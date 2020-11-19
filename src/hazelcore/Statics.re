[@deriving sexp]
type edit_state = (ZExp.t, HTyp.t, MetaVarGen.t);

/**
 * The typing mode for some subexpression in the program
 */
type type_mode =
  | Syn
  | Ana(HTyp.t);

type livelit_view_data = (UHExp.t /* view */, UHExp.t /* shape */);
type livelit_def_ctx = VarMap.t_(livelit_view_data);
type livelit_view_ctx = MetaVarMap.t(livelit_view_data);
type livelit_web_view_ctx =
  MetaVarMap.t((SerializedModel.t => string, LivelitShape.t));
