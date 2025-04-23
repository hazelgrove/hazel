open Util;

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
type send_action = action_exp => Ui_effect.t(unit);

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
  view: (model_exp, send_action) => Virtual_dom.Vdom.Node.t,
  size: ProjectorCore.Shape.t,
};

// referenced in docs/livelits.md
module type BuiltinLivelit = {
  let name: string;
  type model_t;
  type expansion_t;
  type action_t;

  let hazel_model_t: TermBase.Typ.t; /* defines model_exp type */
  let model_to_hazel: model_t => model_exp;
  let model_from_hazel: model_exp => option(model_t);
  let model_default: model_t;

  let hazel_expansion_t: TermBase.Typ.t; /* defines expansion_exp type */
  let expand: model_t => expansion_t;
  let expand_to_hazel: expansion_t => expansion_exp;

  let hazel_action_t: TermBase.Typ.t; /* defines action_exp type */
  let action_to_hazel: action_t => action_exp;
  let action_from_hazel: action_exp => option(action_t);

  let update: (action_t, model_t) => model_t;
  let view:
    (model_t, action_t => Ui_effect.t(unit)) => Virtual_dom.Vdom.Node.t;
  let size: ProjectorCore.Shape.t;
};

/* Convert a BuiltinLivelit module into a rawLivelit record */
let raw_of_builtin = (module B: BuiltinLivelit): raw_livelit => {
  name: B.name,
  id: Id.mk_str(B.name),
  model_t: B.hazel_model_t,
  model_default: B.model_to_hazel(B.model_default),
  expansion_t: B.hazel_expansion_t,
  expand: (exp: model_exp) =>
    switch (B.model_from_hazel(exp)) {
    | Some(m) => Some(B.expand(m) |> B.expand_to_hazel)
    | None => None
    },
  action_t: B.hazel_action_t,
  update: (action: action_exp, model: model_exp) =>
    B.model_to_hazel(
      B.update(
        B.action_from_hazel(action) |> Option.get,
        B.model_from_hazel(model) |> Option.get,
      ),
    ),
  view: (model: model_exp, send_action: send_action) => {
    switch (B.model_from_hazel(model)) {
    | Some(m) => B.view(m, action => send_action(B.action_to_hazel(action)))
    | None => Virtual_dom.Vdom.Node.text("Error: invalid model")
    };
  },
  size: B.size,
};
