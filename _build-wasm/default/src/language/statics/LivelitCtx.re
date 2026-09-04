open Util;

/* SPIKE (wasm-eval-bench): the livelit `view` field and the BuiltinLivelit
   functor have been removed. They were the last dependency of [language] on
   Virtual_dom, which cannot coexist with wasm_of_ocaml (virtual_dom v0.16
   caps js_of_ocaml < 6.0.0; wasm_of_ocaml starts at 6.0.1). Livelits are out
   of scope for the evaluator benchmark, so the types are kept only so that
   Ctx.LivelitEntry and its pattern matches still typecheck. */

type model_state = {
  get: TermBase.Exp.t,
  set: TermBase.Exp.t => unit,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type model_exp = TermBase.Exp.t /* of type model_t */;
[@deriving (show({with_path: false}), sexp, yojson)]
type expansion_exp = TermBase.Exp.t /* of type expansion_t */;
[@deriving (show({with_path: false}), sexp, yojson)]
type action_exp = TermBase.Exp.t /* of type action_t */;

[@deriving (show({with_path: false}), sexp, yojson)]
type raw_livelit = {
  name: string,
  id: Id.t,
  model_t: TermBase.Typ.t,
  model_default: model_exp,
  expansion_t: TermBase.Typ.t,
  expand: model_exp => option(expansion_exp),
  action_t: TermBase.Typ.t,
  update: (action_exp, model_exp) => model_exp,
  size: ProjectorShape.t,
};
