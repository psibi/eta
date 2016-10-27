package eta.runtime.apply;

import eta.runtime.stg.StgClosure;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.StackFrame;

public class ApPP extends StackFrame {
    public StgClosure p1;
    public StgClosure p2;

    public ApPP(final StgClosure p1, final StgClosure p2) {
        this.p1 = p1;
        this.p2 = p2;
    }

    @Override
    public void stackEnter(StgContext context) {
        StgClosure fun = context.R(1);
        fun.apply(context, p1, p2);
    }
}
