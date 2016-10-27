package eta.runtime.thunk;

import eta.runtime.stg.StgClosure;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.StgConstr;
import eta.runtime.apply.Apply;

public class SelectorPNoUpd extends SelectorNoUpd {

    public SelectorPNoUpd(int i, StgClosure p) {
        super(i, p);
    }

    @Override
    public void enter(StgContext context) {
        super.enter(context);
        context.R(1, ((StgConstr) context.R(1)).getP(index));
        Apply.ap_0_fast.enter(context);
    }
}
