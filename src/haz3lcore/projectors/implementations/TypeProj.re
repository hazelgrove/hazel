open Util;
open Virtual_dom.Vdom;
open Node;
open ProjectorBase;

let rec of_segment = (~holes: option(string), seg: Segment.t): string =>
  seg |> List.map(of_piece(~holes)) |> String.concat("")
and of_piece = (~holes, p: Piece.t): string =>
  switch (p) {
  | Tile(t) => of_tile(~holes, t)
  | Grout({shape: Concave, _}) => " "
  | Grout({shape: Convex, _}) when holes != None => Option.get(holes)
  | Grout({shape: Convex, _}) => " "
  | Secondary(w) =>
    Secondary.is_linebreak(w) ? "\n" : Secondary.get_string(w.content)
  | Projector(p) => of_segment(~holes, Piece.unparenthesize(p.syntax))
  }
and of_tile = (~holes, t: Tile.t): string =>
  Aba.mk(t.shards, t.children)
  |> Aba.join(of_delim(t), of_segment(~holes))
  |> String.concat("")
and of_delim = (t: Piece.tile, i: int): string => List.nth(t.label, i);

let expected_ty = (info: option(Info.t)): option(Typ.t) =>
  switch (info) {
  | Some(InfoExp({ana, _}))
  | Some(InfoPat({ana, _})) => Some(ana)
  | _ => None
  };

let self_ty = (info: option(Info.t)): option(Typ.t) =>
  switch (info) {
  | Some(InfoExp({self, ctx, _})) => Self.typ_of_exp(ctx, self)
  | Some(InfoPat({self, ctx, _})) => Self.typ_of_pat(ctx, self)
  | _ => None
  };

let totalize_ty = (expected_ty: option(Typ.t)): Typ.t =>
  switch (expected_ty) {
  | Some(expected_ty) => expected_ty
  | None => Typ.fresh_empty(Unknown(Internal))
  };

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model =
    | Expected
    | Self;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type action =
    | ToggleDisplay;

  let init = (any: Term.Any.t): option(model) => {
    switch (any) {
    | Exp(_)
    | Pat(_) => Some(Expected)
    | Any () => Some(Expected) /* Grout don't have sorts rn */
    | _ => None
    };
  };

  let dynamics = false;
  let focusable = Focusable.non;

  let display_ty = (model, statics): option(Typ.t) =>
    switch (model) {
    | _ when expected_ty(statics) |> totalize_ty |> Typ.is_syn =>
      statics |> self_ty
    | Self => statics |> self_ty
    | Expected => statics |> expected_ty
    };

  let display_mode = (model: model, statics: option(Info.t)): string =>
    switch (model) {
    | _ when self_ty(statics) == expected_ty(statics) => "⇔"
    | _ when expected_ty(statics) |> totalize_ty |> Typ.is_syn => "⇒"
    | Self => "⇒"
    | Expected => "⇐"
    };

  let mode_view = (model, info) =>
    div(
      ~attrs=[Attr.classes(["mode"])],
      [text(display_mode(model, info))],
    );

  let typ_view = (model, info: info, utility, view_seg: View.seg) => {
    let typ = display_ty(model, info.statics) |> totalize_ty;
    div(
      ~attrs=[Attr.classes(["type-cell"])],
      [Typ(typ) |> utility.term_to_seg |> view_seg(Sort.Typ)],
    );
  };

  let update = (model, _, a: action) =>
    switch (a, model) {
    | (ToggleDisplay, Expected) => Self
    | (ToggleDisplay, Self) => Expected
    };

  let syntax_str = (info: info) => {
    let max_len = 30;
    let seg = Segment.unparenthesize(info.syntax);
    let str = of_segment(~holes=Some("?"), seg);
    let str = Re.Str.global_replace(Re.Str.regexp("\n"), " ", str);
    String.length(str) > max_len
      ? String.sub(str, 0, max_len) ++ "..." : str;
  };

  let placeholder = (_m, info) =>
    ProjectorCore.Shape.inline(3 + String.length(syntax_str(info)));

  let syntax_view = (info: info) => info |> syntax_str |> text;

  let icon = div(~attrs=[Attr.classes(["icon"])], []);

  let view = (model, info, ~local, ~parent as _, ~view_seg) =>
    View.{
      inline:
        div(
          ~attrs=[
            Attr.classes(["main"]),
            Attr.on_double_click(_ => local(ToggleDisplay)),
          ],
          [syntax_view(info), icon],
        ),
      offside:
        Some(
          div(
            ~attrs=[Attr.classes(["offside"])],
            [
              mode_view(model, info.statics),
              typ_view(model, info, info.utility, view_seg),
            ],
          ),
        ),
      overlay: None,
    };
};
