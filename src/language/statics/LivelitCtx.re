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

/* What a livelit needs from the typing context in order to expand in
   checking mode, packaged as closures.

   Closures rather than a Ctx.t because Ctx already depends on this module --
   Ctx.entry has a LivelitEntry of raw_livelit -- so naming Ctx.t here would
   be circular. This is also the tighter interface: a livelit needs to resolve
   names and unfold aliases, not to read the whole context. */
[@deriving (show({with_path: false}), sexp, yojson)]
type type_tools = {
  /* The type of constructor [name] at a position expected to have type
     [ana] -- for a constructor carrying a payload, an arrow from the payload
     type. None when the name is not a constructor of any type in scope. */
  resolve_ctr: (~ana: TermBase.Typ.t, string) => option(TermBase.Typ.t),
  /* Unfold type aliases, so an expected type written as a name can be
     destructured. */
  normalize: TermBase.Typ.t => TermBase.Typ.t,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type raw_livelit = {
  name: string,
  id: Id.t,
  model_t: TermBase.Typ.t,
  model_default: model_exp,
  expansion_t: TermBase.Typ.t,
  /* [ana] is the type expected of the expansion. A livelit that sets
     [requires_annotation] is only expanded when that type is known, so it
     may rely on it rather than inventing one. */
  expand:
    (~ana: TermBase.Typ.t, ~tools: type_tools, model_exp) =>
    option(expansion_exp),
  /* When true, this livelit only expands in checking mode: without an
     expected type it cannot know what to produce, and says so rather than
     guessing. */
  requires_annotation: bool,
  action_t: TermBase.Typ.t,
  update: (action_exp, model_exp) => model_exp,
  view: (~id: Id.t, model_exp, send_action) => Virtual_dom.Vdom.Node.t,
  size: ProjectorShape.t,
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
  /* [ana] is the type expected of the expansion, and [tools] resolves
     constructor names and unfolds aliases against the ambient context.
     A livelit that sets [requires_annotation] below is only asked to expand
     when [ana] is known. */
  let expand:
    (~ana: TermBase.Typ.t, ~tools: type_tools, model_t) => expansion_t;
  /* Set when the livelit cannot decide what to produce without an expected
     type -- see the fumola livelit, whose result shape depends on both the
     program it runs and the type asked of it. */
  let requires_annotation: bool;
  let expand_to_hazel: expansion_t => expansion_exp;

  let hazel_action_t: TermBase.Typ.t; /* defines action_exp type */
  let action_to_hazel: action_t => action_exp;
  let action_from_hazel: action_exp => option(action_t);

  let update: (action_t, model_t) => model_t;
  /* [id] is the projector's persistent unique identifier. Livelits whose
     model names external state use it to tell one live projector from
     another, so that duplicating a livelit can be distinguished from
     editing it. Livelits with self-contained models ignore it. */
  let view:
    (~id: Id.t, model_t, action_t => Ui_effect.t(unit)) =>
    Virtual_dom.Vdom.Node.t;
  let size: ProjectorShape.t;
};

/* Convert a BuiltinLivelit module into a rawLivelit record */
let raw_of_builtin = (module B: BuiltinLivelit): raw_livelit => {
  name: B.name,
  id: Id.mk_str(B.name),
  model_t: B.hazel_model_t,
  model_default: B.model_to_hazel(B.model_default),
  expansion_t: B.hazel_expansion_t,
  expand: (~ana: TermBase.Typ.t, ~tools: type_tools, exp: model_exp) =>
    switch (B.model_from_hazel(exp)) {
    | Some(m) => Some(B.expand(~ana, ~tools, m) |> B.expand_to_hazel)
    | None => None
    },
  requires_annotation: B.requires_annotation,
  action_t: B.hazel_action_t,
  update: (action: action_exp, model: model_exp) =>
    B.model_to_hazel(
      B.update(
        B.action_from_hazel(action) |> Option.get,
        B.model_from_hazel(model) |> Option.get,
      ),
    ),
  view: (~id: Id.t, model: model_exp, send_action: send_action) => {
    switch (B.model_from_hazel(model)) {
    | Some(m) =>
      B.view(~id, m, action => send_action(B.action_to_hazel(action)))
    | None => Virtual_dom.Vdom.Node.text("Error: invalid model")
    };
  },
  size: B.size,
};
