type cursor_term = CursorInfo.cursor_term;
type zoperand = CursorInfo_common.zoperand;

let extract_cursor_term = (CursorTag(cursor_pos, tag): ZTag.t): cursor_term =>
  Tag(cursor_pos, tag);

let cursor_info =
    (~steps as _, ctx: Contexts.t, tag: ZTag.t): option(CursorInfo.t) =>
  Some(CursorInfo_common.mk(OnTag, ctx, extract_cursor_term(tag)));
