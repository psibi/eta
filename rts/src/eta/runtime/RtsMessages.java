package eta.runtime;

import eta.runtime.Rts;
import eta.runtime.RtsFlags;
import eta.runtime.stg.StgClosure;
import static eta.runtime.Rts.stgExit;
import static eta.runtime.Rts.ExitCode.EXIT_INTERNAL_ERROR;
import static eta.runtime.RtsFlags.progName;
import static eta.runtime.RtsFlags.progArgs;

public class RtsMessages {
    public static void barf(String msg, Object... args) {
        if (RtsFlags.progName != null && RtsFlags.progArgs != null) {
            System.err.print(RtsFlags.progName + ": ");
        }
        System.err.print("internal error: ");
        System.err.format(msg, args);
        System.err.print("\n");
        System.err.print("    (ETA version " + 1.0 /* TODO: ProjectVersion */ + " for " + "Linux" /* TODO: HostPlatform_TYPE */ + ")\n");
        System.err.println("    Please report this as a ETA bug:  https://github.com/typelead/eta/issues");
        stgExit(EXIT_INTERNAL_ERROR);
    }

    public static void errorBelch(String msg, Object... args) {
        if (progName != null) {
            System.err.print(progName + ": ");
        }
        System.err.format(msg, args);
        System.err.print("\n");
    }

    public static void debugBelch(String msg, Object... args) {
        System.err.format(msg, args);
    }

    public static void printClosure(StgClosure closure) {
        // TODO: Override toString and make the closure more readable
        System.out.println("Closure: " + closure);
    }
}