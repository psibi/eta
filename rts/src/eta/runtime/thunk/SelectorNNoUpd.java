package eta.runtime.thunk;

import eta.runtime.stg.StgClosure;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.StgConstr;
import eta.runtime.apply.Apply;

public class SelectorNNoUpd extends SelectorNoUpd {

    public SelectorNNoUpd(int i, StgClosure p) {
        super(i, p);
    }

    @Override
    public void enter(StgContext context) {
        super.enter(context);
        context.I(1, ((StgConstr) context.R(1)).getN(index));
    }
}
