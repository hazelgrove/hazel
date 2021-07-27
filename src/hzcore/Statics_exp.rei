type elab_result = (Type.t, Delta.t, DTerm_exp.t);

// MetaVarGen used to number type errors
let elab_s:
  (TypeInfo_exp.t, Term_exp.s, MetaVarGen.t) => (elab_result, MetaVarGen.t);
let elab_t:
  (TypeInfo_exp.t, Term_exp.t, MetaVarGen.t) => (elab_result, MetaVarGen.t);

let syn_s: (TypeInfo_exp.t, Term_exp.s) => Type.t;
let syn_t: (TypeInfo_exp.t, Term_exp.t) => Type.t;
