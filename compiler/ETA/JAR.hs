{-|
Module      : JAR
Description : Small utility functions for creating Jars from class files.
Copyright   : (c) Christopher Wells 2016
              (c) Rahul Muttineni 2016
License     : MIT
This module provides utility functions for creating Jar archives from JVM class
files.
The general process used for creating a Jar archive is to first create an
empty Jar in the desired location using `createEmptyJar`. After the Jar has
been created, files can be added to it using the `addByteStringToJar` and
`addMultiByteStringsToJar` functions.
When adding multiple files to a Jar, be sure to use the
`addMultiByteStringsToJar` function, as it writes all of the file changes in
one action, while mapping over a list of files with `addByteStringToJar` would
perform the file write actions all seperately.
Here is a quick exampe of how to create a Jar and add a file into it.
@
-- Create the empty jar
let jarLocation = "build/Hello.jar"
createEmptyJar jarLocation
-- Add a single file to the jar
import Data.ByteString.Internal (packChars)
let fileLocation = "hellopackage/Main.class"
let fileContents = packChars "Hello, World!"
addByteStringToJar fileLocation fileContents jarLocation
@
-}
module ETA.JAR where

import Data.Coerce(coerce)
import Codec.Archive.Zip
import Control.Monad (forM_, forM)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Catch (MonadCatch(..), MonadThrow)
import Data.ByteString.Internal (ByteString)
import Path
import Data.Map.Lazy (keys)

class MultiPath m where
  getPath :: FilePath -> Path m File

instance MultiPath Abs
instance MultiPath Rel

type FileAndContents = (Path Rel File, ByteString)
-- | Creates an empty jar archive at the given relative filepath location.
-- The name of the archive and its file ending should be included in the
-- filepath.
--
-- __Throws__: 'PathParseException'
--
-- See 'createArchive' and 'parseRelFile' for more information.
--
-- For example, passing in "src\/Main.jar" would create an empty jar archive
-- named "Main.jar" in the "src" sub-directory of the current directory.
--
-- @
-- createEmptyJar "src/Main.jar"
-- @
--
-- __Before__
--
-- @
-- .
-- └── src
-- @
--
-- __After__
--
-- @
-- .
-- └── src
--     └── Main.jar
-- @
createEmptyJar :: (MonadIO m, MonadCatch m) => FilePath -> m ()
createEmptyJar location = do
  isRelative <- catch (parseRelFile location >> return True) handler
  if isRelative then do
    p <- parseRelFile location
    createArchive p (return ())
  else do
    p <- parseAbsFile location
    createArchive p (return ())
  where handler :: (MonadIO m, MonadCatch m) => PathParseException -> m Bool
        handler _ = return False

-- | Adds the given ByteString as a file at the given location within the given
-- jar archive.
--
-- __Throws__: 'PathParseException', 'EntrySelectorException',
-- isAlreadyInUseError, isDoesNotExistError, isPermissionError, 'ParsingFailed'
--
-- See 'withArchive', 'mkEntrySelector', and 'parseRelFile' for more information.
--
-- For example, running the following would create a file named "Hello.class"
-- containing the string "Hello, World!" within the "src" directory in the jar
-- archive located at "build\/libs\/HelloWorld.jar".
--
-- @
-- let fileLocation = "src\/Hello.class"
-- let contents = packChars "Hello, World!"
-- let jarLocation = "build\/libs\/HelloWorld.jar"
-- addByteStringToJar fileLocation contents jarLocation
-- @
--
-- __Before__
--
-- @
-- .
-- └── build
--     └── libs
--         └── HelloWorld.jar
-- @
--
-- __After__
--
-- @
-- .
-- └── build
--     └── libs
--         └── HelloWorld.jar
--             └── src
--                 └── Hello.class
-- @
addByteStringToJar :: (MonadThrow m, MonadIO m)
  => FilePath      -- ^ Location of the new file within the jar
  -> ByteString    -- ^ Contents of the new file to add
  -> FilePath      -- ^ Location of the jar to add the new file into
  -> m ()
addByteStringToJar fileLocation contents jarLocation = zipAction
  where zipAction = jarPath >>= flip withArchive zipChange
        zipChange = entrySel >>= addEntry Store contents
        entrySel  = filePath >>= mkEntrySelector
        jarPath   = parseRelFile jarLocation
        filePath  = parseRelFile fileLocation

-- | Adds the given files into the given jar. Each file is represented by a
-- tuple containing the location where the file will be added inside the jar,
-- and the contents of the file as a ByteString.
--
-- __Throws__: 'PathParseException', 'EntrySelectorException',
-- isAlreadyInUseError, isDoesNotExistError, isPermissionError, 'ParsingFailed'
--
-- See 'withArchive', 'mkEntrySelector', and 'parseRelFile' for more information.
--
-- For example, running the following would create two files within the jar
-- archive located at "build\/libs\/HelloWorld.jar". The first file would be
-- named "Hello.class" containing the string "Hello, World!" within the
-- "helloworld" directory in the jar archive. The second file would be named
-- \"MANIFEST.MF" containing the string "Manifest-Version: 1.0" within the
-- \"META-INF" directory in the jar archive.
--
-- @
-- let file1Location = "helloworld\/Hello.class"
-- let file1Contents = "Hello, World!"
-- let file1 = (file1Location, file1Contents)
--
-- let file2Location = "META-INF\/MANIFEST.MF"
-- let file2Contents = "Manifest-Version: 1.0"
-- let file2 = (file2Location, file2Contents)
--
-- let files = [file1, file2]
-- let jarLocation = "build\/libs\/HelloWorld.jar"
-- addMultiByteStringsToJar files jarLocation
-- @
--
-- __Before__
--
-- @
-- .
-- └── build
--     └── libs
--         └── HelloWorld.jar
-- @
--
-- __After__
--
-- @
-- .
-- └── build
--     └── libs
--         └── HelloWorld.jar
--             ├── helloworld
--             │   └── Hello.class
--             └── META-INF
--                 └── MANIFEST.MF
-- @
-- addMultiByteStringsToJar
--   :: (MonadThrow m, MonadIO m)
--   => [(FilePath, ByteString)]    -- ^ Filepaths and contents of files to add into the jar
--   -> FilePath                    -- ^ Location of the jar to add the new files into
--   -> m ()
-- addMultiByteStringsToJar files jarLocation = do
--   jarPath <- parseRelFile jarLocation
--   withArchive jarPath $
--     forM_ files $ \(path, contents) -> do
--       filePath <- parseRelFile path
--       entrySel <- mkEntrySelector filePath
--       addEntry Store contents entrySel

addMultiByteStringsToJar'
  :: (MonadThrow m, MonadIO m, MonadCatch m)
  => [(Path Rel File, ByteString)]    -- ^ Filepaths and contents of files to add into the jar
  -> FilePath                    -- ^ Location of the jar to add the new files into
  -> m ()
addMultiByteStringsToJar' files jarLocation = do
  isRelative <- catch (parseRelFile jarLocation >> return True) handler
  if isRelative then do
    p <- parseRelFile jarLocation
    withArchive p action
  else do
    p <- parseAbsFile jarLocation
    withArchive p action
  where action =
          forM_ files $ \(path, contents) -> do
            entrySel <- mkEntrySelector path
            addEntry Store contents entrySel
        handler :: (MonadThrow m, MonadCatch m, MonadIO m)
                => PathParseException -> m Bool
        handler _ = return False

getFilesFromJar
  :: (MonadThrow m, MonadCatch m, MonadIO m)
  => FilePath
  -> m [(Path Rel File, ByteString)]
getFilesFromJar jarLocation = do
  isRelative <- catch (parseRelFile jarLocation >> return True) handler
  if isRelative then do
    p <- parseRelFile jarLocation
    withArchive p action
  else do
    p <- parseAbsFile jarLocation
    withArchive p action
  where action = do
          entrySelectors <- keys <$> getEntries
          forM entrySelectors $ \es -> do
            contents <- getEntry es
            return (unEntrySelector es, contents)
        handler :: (MonadThrow m, MonadCatch m, MonadIO m)
                => PathParseException -> m Bool
        handler _ = return False

mkPath :: (MonadThrow m, MonadIO m) => FilePath -> m (Path Rel File)
mkPath = parseRelFile
