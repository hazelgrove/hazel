[@deriving sexp]
type edit_state = (ZExp.t, HTyp.t, MetaVarGen.t);

/**
 * The typing mode for some subexpression in the program
 */
type type_mode =
  | Syn
  | Ana(HTyp.t);

[@deriving sexp]
type livelit_view_data = (UHExp.t /* view */, UHExp.t /* shape */);
[@deriving sexp]
type livelit_def_ctx = VarMap.t_(livelit_view_data);
[@deriving sexp]
type livelit_view_ctx = MetaVarMap.t(livelit_view_data);
[@deriving sexp]
type livelit_web_view_ctx =
  MetaVarMap.t(
    ((int, SerializedModel.t) => option(DHExp.t), LivelitShape.t),
  );
