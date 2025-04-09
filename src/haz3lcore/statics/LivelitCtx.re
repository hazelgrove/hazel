open Util;

type model_state = {
  get: TermBase.Exp.t,
  set: TermBase.Exp.t => unit,
};

type node_or_list =
  | Node(Virtual_dom.Vdom.Node.t)
  | List(list(Virtual_dom.Vdom.Node.t));

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
  expansion_f: model_exp => expansion_exp,
  action_t: TermBase.Typ.t,
  update: (action_exp, model_exp) => model_exp,
  view: (model_exp, send_action) => node_or_list,
  size: ProjectorCore.Shape.t,
};

module type BuiltinLivelit = {
  let name: string;
  type model_t;
  type expansion_t;
  type action_t;

  let hazel_model_t: TermBase.Typ.t; /* defines model_exp type */
  let model_to_hazel: model_t => model_exp;
  let model_from_hazel: model_exp => model_t;
  let model_default: model_t;

  let hazel_expansion_t: TermBase.Typ.t; /* defines expansion_exp type */
  let expansion_f: model_t => expansion_t;
  let expansion_to_hazel: expansion_t => expansion_exp;

  let hazel_action_t: TermBase.Typ.t; /* defines action_exp type */
  let action_to_hazel: action_t => action_exp;
  let action_from_hazel: action_exp => action_t;

  let update: (action_t, model_t) => model_t;
  let view: (model_t, action_t => Ui_effect.t(unit)) => node_or_list;
  let size: ProjectorCore.Shape.t;
};

/* Convert a BuiltinLivelit module into a rawLivelit record */
let raw_of_builtin = (module B: BuiltinLivelit): raw_livelit => {
  name: B.name,
  id: Id.mk_str(B.name),
  model_t: B.hazel_model_t,
  model_default: B.model_to_hazel(B.model_default),
  expansion_t: B.hazel_expansion_t,
  expansion_f: (exp: model_exp) =>
    B.expansion_to_hazel(B.expansion_f(B.model_from_hazel(exp))),
  action_t: B.hazel_action_t,
  update: (action: action_exp, model: model_exp) =>
    B.model_to_hazel(
      B.update(B.action_from_hazel(action), B.model_from_hazel(model)),
    ),
  view: (model: model_exp, send_action: send_action) =>
    B.view(B.model_from_hazel(model), action =>
      send_action(B.action_to_hazel(action))
    ),
  size: B.size,
};
