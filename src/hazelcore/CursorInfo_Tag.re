type cursor_term = CursorInfo.cursor_term;
type zoperand = CursorInfo_common.zoperand;

let extract_cursor_term = (CursorTag(cursor, tag): ZTag.t): cursor_term =>
  Tag(cursor, tag);

let cursor_info =
    (ctx: Contexts.t, CursorTag(_, tag) as ztag: ZTag.t)
    : option(CursorInfo.t) =>
  switch (tag) {
  | Tag(status, _) =>
    switch (status) {
    | NotInTagHole =>
      Some(CursorInfo_common.mk(OnTag, ctx, extract_cursor_term(ztag)))
    | InTagHole(InvalidTagName, _) =>
      Some(
        CursorInfo_common.mk(
          OnInvalidTag(tag),
          ctx,
          extract_cursor_term(ztag),
        ),
      )
    | InTagHole(DuplicateTagName, _) =>
      Some(
        CursorInfo_common.mk(
          OnDuplicateTag(tag),
          ctx,
          extract_cursor_term(ztag),
        ),
      )
    }
  | EmptyTagHole(_) =>
    Some(CursorInfo_common.mk(OnTagHole, ctx, extract_cursor_term(ztag)))
  };

let get_zoperand = (ztag: ZTag.t): option(zoperand) => Some(ZTag(ztag));

let get_outer_zrules =
    (_: ZTag.t, outer_zrules: option(ZExp.zrules)): option(ZExp.zrules) => outer_zrules;
