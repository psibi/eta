package eta.runtime.concurrent;

import java.util.Deque;
import java.util.ArrayDeque;
import java.util.concurrent.atomic.AtomicBoolean;

import eta.runtime.stg.StgTSO;
import eta.runtime.stg.StgClosure;
import eta.runtime.stg.StgContext;
import static eta.runtime.RtsMessages.barf;
import static eta.runtime.concurrent.Concurrent.SPIN_COUNT;

public class StgMVar extends StgClosure {
    public Deque<StgTSO> tsoQueue = new ArrayDeque<StgTSO>();
    public StgClosure value;
    public AtomicBoolean lock = new AtomicBoolean(false);

    public StgMVar(StgClosure value) {
        this.value = value;
    }

    @Override
    public void enter(StgContext context) {
        barf("MVAR object entered!");
    }

    public boolean isEmpty() {
        return tsoQueue.isEmpty();
    }

    public void pushFirst(StgTSO tso) {
        tsoQueue.offerFirst(tso);
    }

    public void pushLast(StgTSO tso) {
        tsoQueue.offerLast(tso);
    }

    public StgTSO popFromQueue() {
        return tsoQueue.poll();
    }

    public final void lock() {
        do {
            int i = 0;
            do {
                boolean old = lock.getAndSet(true);
                if (!old) return;
            } while (++i < SPIN_COUNT);
            Thread.yield();
        } while (true);
    }

    public final void unlock() {
        lock.set(false);
    }

    public final boolean tryLock() {
        return lock.getAndSet(true);
    }

    @Override
    public StgClosure getEvaluated() { return this; }
}
