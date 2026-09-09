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

let get_dynamic_typ = (info: info): Typ.t => {
  let ctx =
    Option.map(Info.ctx_of, info.statics)
    |> Option.value(~default=Builtins.ctx_init(Some(Int)));
  info.dynamics
  |> Option.map((d: Dynamics.Info.t) =>
       DynamicTypInfer.dynamic_typ_of_samples_or_unknown(~ctx, d.samples)
     )
  |> Option.value(~default=Typ.fresh(Unknown(Internal)));
};

[@deriving (show({with_path: false}), sexp, yojson)]
type mode =
  | Expected
  | Self
  | Dynamic;

/* Named rather than an option(int): the two states are "as wide as the type
   needs" and "as wide as the user dragged to", and `option` cannot say that.
   (It also keeps `Some`/`None` out of the derived reader -- they are shadowed
   in this scope, which is why ProbeProj writes `Option.None`.) */
[@deriving (show({with_path: false}), sexp, yojson)]
type length =
  | Auto
  | Fixed(int);

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  /* `length` is the abbreviation budget for the rendered type. It lives in
     the model rather than in module-level state the way ProbeProj's sample
     lengths have to: a sample is recreated every evaluation, a projector
     is not. */
  type model = {
    mode,
    length,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type action =
    | ToggleDisplay
    | SetLength(int);

  let init_model = {
    mode: Expected,
    length: Auto,
  };

  /* A model is persisted as a sexp and read back at render time, where
     ProjectorBase.Cook calls deserialize_m with no guard -- so a raise here
     takes the view down. Before this record the model WAS the bare mode, so
     read that shape too and carry the user's mode across the upgrade rather
     than resetting it. */
  let model_of_sexp = (sexp: Sexplib.Sexp.t): model =>
    switch (model_of_sexp(sexp)) {
    | m => m
    | exception _ =>
      switch (mode_of_sexp(sexp)) {
      | mode => {
          mode,
          length: Auto,
        }
      | exception _ => init_model
      }
    };

  let init = (any: Any.t): option(model) => {
    switch (any) {
    | Exp(_)
    | Pat(_)
    | Typ(_) => Some(init_model)
    | Any () => Some(init_model) /* Grout don't have sorts rn */
    | _ => None
    };
  };

  let dynamics = true;
  let elaborate_syntax = false;
  let focusable = Focusable.non;

  let display_mode = (mode: mode, statics: option(Language.Info.t)): string => {
    switch (mode) {
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
      (mode: mode, statics: option(Language.Info.t)): string => {
    switch (mode) {
    | Dynamic => "Dynamic type (from runtime values)"
    | _ when self_ty(statics) == expected_ty(statics) => "Self type matches expected type"
    | _ when expected_ty(statics) |> totalize_ty |> Typ.is_syn => "Self type"
    | Self => "Self type"
    | Expected => "Expected type"
    };
  };

  let mode_view = (mode, info) =>
    div(
      ~attrs=[
        Attr.classes(["mode"]),
        Attr.title(mode_description(mode, info)),
      ],
      [text(display_mode(mode, info))],
    );

  let typ_view = (model: model, info: info, utility, view_seg: View.seg) => {
    let (classes, typ) =
      switch (model.mode) {
      | Dynamic =>
        let dynamic_typ = get_dynamic_typ(info);
        let static_typ =
          Option.value(
            ~default=Typ.fresh(Unknown(Internal)),
            self_ty(info.statics),
          );
        let ctx = Option.map(Info.ctx_of, info.statics);
        PadIds.compute_dynamic_ids(~ctx?, ~static_typ, ~dynamic_typ, ());
      | Expected when expected_ty(info.statics) |> totalize_ty |> Typ.is_syn => (
          (_ => []),
          self_ty(info.statics) |> totalize_ty,
        )
      | Expected => ((_ => []), expected_ty(info.statics) |> totalize_ty)
      | Self => ((_ => []), self_ty(info.statics) |> totalize_ty)
      };

    div(
      ~attrs=[Attr.classes(["type-cell"])],
      [
        Typ(typ)
        |> utility.term_to_seg(~inline=true)
        |> view_seg(~single_line=true, ~classes, Sort.Typ),
      ],
    );
  };

  let update = (model, info, a: action) => {
    let has_expected =
      switch (expected_ty(info.statics)) {
      | Some(ty) => !Typ.is_syn(ty)
      | None => false
      };
    switch (a, model.mode) {
    | (SetLength(n), _) => {
        ...model,
        length: Fixed(max(1, n)),
      }
    | (ToggleDisplay, Expected) => {
        ...model,
        mode: has_expected ? Self : Dynamic,
      }
    | (ToggleDisplay, Self) => {
        ...model,
        mode: Dynamic,
      }
    | (ToggleDisplay, Dynamic) => {
        ...model,
        mode: has_expected ? Expected : Self,
      }
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
              mode_view(model.mode, info.statics),
              typ_view(model, info, info.utility, view_seg),
            ],
          ),
        ),
      overlay: None,
      below: None,
      error: false,
    };
};
