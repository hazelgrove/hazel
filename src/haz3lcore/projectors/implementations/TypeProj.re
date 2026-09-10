open Virtual_dom.Vdom;
open Node;
open ProjectorBase;
open Language;
open Util;

let expected_ty = (info: option(Info.t)): option(Typ.t) =>
  switch (info) {
  | Some(InfoExp({ana, _}))
  | Some(InfoPat({ana, _})) => Some(ana)
  | _ => None
  };

let self_ty = (info: option(Info.t)): option(Typ.t) =>
  switch (info) {
  | Some(InfoExp({elab_syn_ty, _}))
  | Some(InfoPat({elab_syn_ty, _})) => Some(elab_syn_ty)
  | _ => None
  };

let totalize_ty = (expected_ty: option(Typ.t)): Typ.t =>
  switch (expected_ty) {
  | Some(expected_ty) => expected_ty
  | None => Typ.fresh(Unknown(Internal))
  };

/* The segment to display in Dynamic mode, and the ids of its tokens that
   came from runtime. Rendered here rather than by the caller because the
   dynamic_ids only describe this one render -- see DynamicTypInfer. */
let get_dynamic_segment =
    (utility: utility, info: info): (Base.segment, Id.Set.t) => {
  let ctx =
    Option.map(Info.ctx_of, info.statics)
    |> Option.value(~default=Builtins.ctx_init(Some(Int)));
  let static_typ =
    Option.value(
      ~default=Typ.fresh(Unknown(Internal)),
      self_ty(info.statics),
    );
  let normalize = utility.normalize_typ(~inline=true);
  let render_normalized = utility.render_normalized_typ(~inline=true);
  switch (info.dynamics) {
  | None => (render_normalized(normalize(static_typ)), Id.Set.empty)
  | Some(d: Dynamics.Info.t) =>
    DynamicTypInfer.displayed_segment_and_dynamic_ids(
      ~normalize,
      ~render_normalized,
      ~ctx,
      ~static_typ,
      ~samples=d.samples,
    )
  };
};

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model =
    | Expected
    | Self
    | Dynamic;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type action =
    | ToggleDisplay;

  let init = (any: Any.t): option(model) => {
    switch (any) {
    | Exp(_)
    | Pat(_) => Some(Expected)
    | Any () => Some(Expected) /* Grout don't have sorts rn */
    | _ => None
    };
  };

  let dynamics = true;
  let elaborate_syntax = false;
  let focusable = Focusable.non;

  let display_mode = (model: model, statics: option(Language.Info.t)): string => {
    switch (model) {
    | Dynamic => "⇓"
    /* ↔ not ⇔: Source Code Pro, the bundled font these render in, has
       no bidirectional double arrow, so ⇔ fell back to a system font and
       rendered differently per browser. */
    | _ when self_ty(statics) == expected_ty(statics) => "↔"
    | _ when expected_ty(statics) |> totalize_ty |> Typ.is_syn => "⇒"
    | Self => "⇒"
    | Expected => "⇐"
    };
  };
  let mode_description =
      (model: model, statics: option(Language.Info.t)): string => {
    switch (model) {
    | Dynamic => "Dynamic type (from runtime values)"
    | _ when self_ty(statics) == expected_ty(statics) => "Self type matches expected type"
    | _ when expected_ty(statics) |> totalize_ty |> Typ.is_syn => "Self type"
    | Self => "Self type"
    | Expected => "Expected type"
    };
  };

  let mode_view = (model, info) =>
    div(
      ~attrs=[
        Attr.classes(["mode"]),
        Attr.title(mode_description(model, info)),
      ],
      [text(display_mode(model, info))],
    );

  let typ_view = (model, info: info, utility, view_seg: View.seg) => {
    /* Every arm yields the segment to display, so Dynamic can hand over the
       exact segment its dynamic_ids were computed from. */
    let render = (t: Typ.t) => utility.term_to_seg(~inline=true, Typ(t));
    let (classes, seg) =
      switch (model) {
      | Dynamic =>
        let (seg, dynamic_ids) = get_dynamic_segment(utility, info);
        ((id => Id.Set.mem(id, dynamic_ids) ? ["dynamic"] : []), seg);
      | Expected when expected_ty(info.statics) |> totalize_ty |> Typ.is_syn => (
          (_ => []),
          render(self_ty(info.statics) |> totalize_ty),
        )
      | Expected => (
          (_ => []),
          render(expected_ty(info.statics) |> totalize_ty),
        )
      | Self => ((_ => []), render(self_ty(info.statics) |> totalize_ty))
      };

    div(
      ~attrs=[Attr.classes(["type-cell"])],
      [seg |> view_seg(~single_line=true, ~classes, Sort.Typ)],
    );
  };

  let update = (model, info, a: action) => {
    let has_expected =
      switch (expected_ty(info.statics)) {
      | Some(ty) => !Typ.is_syn(ty)
      | None => false
      };
    switch (a, model) {
    | (ToggleDisplay, Expected) => if (has_expected) {Self} else {Dynamic}
    | (ToggleDisplay, Self) => Dynamic
    | (ToggleDisplay, Dynamic) => if (has_expected) {Expected} else {Self}
    };
  };

  let placeholder = (_, _) => ProjectorCore.Shape.default;
  let error = (_, _): option(ProjectorBase.error) => None;

  let view = ({model, info, local, view_seg, _}: View.args(model, action)) =>
    View.{
      inline: div([]),
      offside:
        Some(
          div(
            ~attrs=[
              Attr.id(Id.cls(info.id)),
              Attr.tabindex(0),
              Attr.classes(["offside"]),
              Attr.on_double_click(_ => local(ToggleDisplay)),
            ],
            [
              mode_view(model, info.statics),
              typ_view(model, info, info.utility, view_seg),
            ],
          ),
        ),
      overlay: None,
      below: None,
      error: false,
    };
};
