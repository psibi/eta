package eta.runtime.thunk;

import eta.runtime.stg.StgClosure;
import eta.runtime.stg.StgContext;
import eta.runtime.apply.Apply;

public class Ap6Upd extends StgInd {
    public StgClosure p1;
    public StgClosure p2;
    public StgClosure p3;
    public StgClosure p4;
    public StgClosure p5;
    public StgClosure p6;

    public Ap6Upd(final StgClosure p1, final StgClosure p2, final StgClosure p3, final StgClosure p4, final StgClosure p5, final StgClosure p6) {
        super();
        this.p1 = p1;
        this.p2 = p2;
        this.p3 = p3;
        this.p4 = p4;
        this.p5 = p5;
        this.p6 = p6;
    }

    @Override
    public void thunkEnter(StgContext context) {
        context.R(1, p1);
        context.R(2, p2);
        context.R(3, p3);
        context.R(4, p4);
        context.R(5, p5);
        context.R(6, p6);
        Apply.ap_ppppp_fast.enter(context);
    }
}
