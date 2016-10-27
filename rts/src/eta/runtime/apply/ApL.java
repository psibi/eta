package eta.runtime.apply;

import eta.runtime.stg.StgClosure;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.StackFrame;

public class ApL extends StackFrame {
    public long l;

    public ApL(long l) {
        this.l = l;
    }

    @Override
    public void stackEnter(StgContext context) {
        StgClosure fun = context.R(1);
        fun.apply(context, l);
    }
}
