type elab_result = (Type.t, Contexts.t, Delta.t, DTerm_pat.t);

let elab:
  (TypeInfo_pat.t, Term_pat.t, MetaVarGen.t) => (elab_result, MetaVarGen.t);

let syn: (TypeInfo_pat.t, Term_pat.t) => (Type.t, Contexts.t);
