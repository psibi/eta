package eta.runtime.exception;

import java.util.ListIterator;

import eta.runtime.stg.Stg;
import eta.runtime.stg.Capability;
import eta.runtime.stg.StgTSO;
import eta.runtime.stg.StackFrame;
import eta.runtime.stg.StgEnter;
import eta.runtime.stg.StgClosure;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.RtsFun;
import eta.runtime.apply.Apply;
import eta.runtime.apply.ApV;
import eta.runtime.message.MessageThrowTo;
import static eta.runtime.stg.StgTSO.TSO_BLOCKEX;
import static eta.runtime.stg.StgTSO.TSO_INTERRUPTIBLE;
import static eta.runtime.stg.StgTSO.WhatNext.ThreadKilled;
import static eta.runtime.stg.StgTSO.WhyBlocked.BlockedOnMsgThrowTo;

public class StgException extends RuntimeException {

    @Override
    public Throwable fillInStackTrace() {
        return null;
    }

    /* TODO: Should this be volatile/atomic? */
    public static boolean noBreakOnException = false;

    public static StgException stgReturnException = new StgReturnException();
    public static StgException threadYieldException = new ThreadYieldException();
    public static StgException stackReloadException = new StackReloadException();

    public static RtsFun getMaskingState = new RtsFun() {
            @Override
            public void enter(StgContext context) {
                StgTSO tso = context.currentTSO;
                context.I(1, ((tso.hasFlag(TSO_BLOCKEX)? 1: 0) +
                              (tso.hasFlag(TSO_INTERRUPTIBLE)? 1: 0)));
            }
        };

    public static RtsFun maskAsyncExceptions = new RtsFun() {
            @Override
            public void enter(StgContext context) {
                StgTSO tso = context.currentTSO;
                ListIterator<StackFrame> sp = tso.sp;
                if (tso.hasFlag(TSO_BLOCKEX)) {
                    if (!tso.hasFlag(TSO_INTERRUPTIBLE)) {
                        sp.add(new MaskUninterruptibleFrame());
                    }
                } else {
                    StackFrame top = tso.stack.peek();
                    if (top.getClass() == MaskAsyncExceptionsFrame.class) {
                        sp.previous();
                        sp.remove();
                    } else {
                        sp.add(new UnmaskAsyncExceptionsFrame());
                    }
                }
                tso.addFlags(TSO_BLOCKEX | TSO_INTERRUPTIBLE);
                /* TODO: Ensure that R1 is preserved */
                Apply.ap_v_fast.enter(context);
            }
        };

    public static RtsFun maskUninterruptible = new RtsFun() {
            @Override
            public void enter(StgContext context) {
                StgTSO tso = context.currentTSO;
                ListIterator<StackFrame> sp = tso.sp;
                if (tso.hasFlag(TSO_BLOCKEX)) {
                    if (tso.hasFlag(TSO_INTERRUPTIBLE)) {
                        sp.add(new MaskAsyncExceptionsFrame());
                    }
                } else {
                    StackFrame top = tso.stack.peek();
                    if (top.getClass() == MaskUninterruptibleFrame.class) {
                        sp.previous();
                        sp.remove();
                    } else {
                        sp.add(new UnmaskAsyncExceptionsFrame());
                    }
                }
                tso.addFlags(TSO_BLOCKEX);
                tso.removeFlags(TSO_INTERRUPTIBLE);
                /* TODO: Ensure that R1 is preserved */
                Apply.ap_v_fast.enter(context);
            }
        };

    public static RtsFun unmaskAsyncExceptions = new RtsFun() {
            @Override
            public void enter(StgContext context) {
                Capability cap = context.myCapability;
                StgTSO tso = context.currentTSO;
                StgClosure io = context.R(1);
                ListIterator<StackFrame> sp = tso.sp;
                if (tso.hasFlag(TSO_BLOCKEX)) {
                    StackFrame top = tso.stack.peek();
                    if (top.getClass() == UnmaskAsyncExceptionsFrame.class) {
                        sp.previous();
                        sp.remove();
                    } else {
                        if (tso.hasFlag(TSO_INTERRUPTIBLE)) {
                            sp.add(new MaskAsyncExceptionsFrame());
                        } else {
                            sp.add(new MaskUninterruptibleFrame());
                        }
                    }
                    tso.removeFlags(TSO_BLOCKEX | TSO_INTERRUPTIBLE);
                    if (!tso.blockedExceptions.isEmpty()) {
                        sp.add(new ApV());
                        sp.add(new StgEnter(io));
                        boolean performed = cap.maybePerformBlockedException(tso);
                        if (performed) {
                            if (tso.whatNext == ThreadKilled) {
                                Stg.threadFinished.enter(context);
                            } else {
                                /* TODO: Verify R1 is conserved on the next
                                   stack reload. */
                                throw StgException.stackReloadException;
                            }
                        } else {
                            sp.previous();
                            sp.remove();
                            sp.previous();
                            sp.remove();
                        }
                    }
                }
                Apply.ap_v_fast.enter(context);
            }
        };

    public static RtsFun killThread = new RtsFun() {
            @Override
            public void enter(StgContext context) {
                StgTSO target = (StgTSO) context.R(1);
                StgTSO tso = context.currentTSO;
                if (target == tso) {
                    killMyself.enter(context);
                } else {
                    StgClosure exception = context.R(2);
                    Capability cap = context.myCapability;
                    MessageThrowTo msg = cap.throwTo(tso, target, exception);
                    if (msg == null) {
                        return;
                    } else {
                        tso.whyBlocked = BlockedOnMsgThrowTo;
                        tso.blockInfo = msg;
                        block_throwto.enter(context);
                    }
                }
            }
        };

    public static RtsFun killMyself = new RtsFun() {
            @Override
            public void enter(StgContext context) {
                Capability cap = context.myCapability;
                StgTSO tso = context.currentTSO;
                StgTSO target = (StgTSO) context.R(1);
                StgClosure exception = context.R(2);
                cap.throwToSingleThreaded(target, exception);
                if (tso.whatNext == ThreadKilled) {
                    Stg.threadFinished.enter(context);
                } else {
                    throw StgException.stackReloadException;
                }
            }
        };

    public static RtsFun catch_ = new RtsFun() {
            @Override
            public void enter(StgContext context) {
                StgClosure handler = context.R(2);
                StgTSO tso = context.currentTSO;
                ListIterator<StackFrame> sp = tso.sp;
                int exceptionsBlocked = tso.showIfFlags(TSO_BLOCKEX | TSO_INTERRUPTIBLE);
                sp.add(new StgCatchFrame(exceptionsBlocked, handler));
                Apply.ap_v_fast.enter(context);
            }
        };


    public static RtsFun raise = new RtsFun() {
            @Override
            public void enter(StgContext context) {
                Capability cap = context.myCapability;
                StgTSO tso = context.currentTSO;
                StgClosure exception = context.R(1);
                boolean retry = false;
                do {
                    StackFrame frame = cap.raiseExceptionHelper(tso, exception);
                    retry = frame.doRaise(context, cap, tso, exception);
                } while (retry);
            }
        };

    public static RtsFun raiseIO = new RtsFun() {
            @Override
            public void enter(StgContext context) {
                raise.enter(context);
            }
        };

    public static RtsFun block_throwto = new RtsFun() {
            @Override
            public void enter(StgContext context) {
                StgTSO tso = (StgTSO) context.R(1);
                StgClosure exception = context.R(2);
                tso.sp.add(new BlockThrowToFrame(tso, exception));
                MessageThrowTo msg = (MessageThrowTo) tso.blockInfo;
                if (msg.isLocked()) {
                    msg.unlock();
                }
                throw stgReturnException;
            }
        };

}
