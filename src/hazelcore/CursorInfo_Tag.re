type cursor_term = CursorInfo.cursor_term;
type zoperand = CursorInfo_common.zoperand;

let extract_cursor_term = (CursorTag(cursor_pos, tag): ZTag.t): cursor_term =>
  Tag(cursor_pos, tag);

let cursor_info =
    (~steps as _, ctx: Contexts.t, tag: ZTag.t): option(CursorInfo.t) =>
  Some(CursorInfo_common.mk(OnTag, ctx, extract_cursor_term(tag)));

let get_zoperand = (ztag: ZTag.t): option(zoperand) => Some(ZTag(ztag));

let get_outer_zrules =
    (_: ZTag.t, outer_zrules: option(ZExp.zrules)): option(ZExp.zrules) => outer_zrules;

let syn_cursor_info =
    (
      ~steps: CursorPath.steps,
      ctx: Contexts.t,
      CursorTag(cursor, tag): ZTag.t,
    )
    : option(CursorInfo.t) => {
  let _ = steps;
  Some(CursorInfo_common.mk(OnTag, ctx, Tag(cursor, tag)));
};
