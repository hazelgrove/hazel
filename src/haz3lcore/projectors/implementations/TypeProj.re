open Virtual_dom.Vdom;
open Node;
open ProjectorBase;
open Language;
open Util;

/* The expectation statics holds a term to, where it holds it to one. A term in
   synthetic position is held to none: statics writes Unknown(SynSwitch) for the
   expectation there, and that is the absence of one, not an unknown type. */
let expected_ty = (info: Info.t): option(Typ.t) =>
  switch (info) {
  | InfoExp({ana, _})
  | InfoPat({ana, _}) => Typ.is_syn(ana) ? None : Some(ana)
  | _ => None
  };

let self_ty = (info: Info.t): option(Typ.t) =>
  switch (info) {
  | InfoExp({elab_syn_ty, _})
  | InfoPat({elab_syn_ty, _}) => Some(elab_syn_ty)
  | _ => None
  };

let totalize_ty = (expected_ty: option(Typ.t)): Typ.t =>
  switch (expected_ty) {
  | Some(expected_ty) => expected_ty
  | None => Typ.fresh(Unknown(Internal))
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

  let dynamics = false;
  let elaborate_syntax = false;
  let focusable = Focusable.non;

  /* Whether the two readings are the same type. Not (==): that compares ids
     too, and statics builds types with Typ.temp, so two types it built
     compare equal on the Id.invalid sentinel while a written annotation --
     carrying its own tokens' ids -- never equals its synthesized twin. An
     expression with no expectation has nothing to agree with. */
  let readings_agree = (statics: Info.t): bool =>
    switch (self_ty(statics), expected_ty(statics)) {
    | (Some(self), Some(expected)) => Typ.fast_equal(self, expected)
    | _ => false
    };

  /* What the cell shows: the model says which reading was asked for, this
     says which it came to. The dynamic reading carries the static type
     beside the one runtime supplied, since colouring names the tokens the
     static type does not account for; None where runtime supplied nothing,
     which is not the same as its having supplied `?`. */
  type content =
    | FromRuntime({
        dynamic: option(Typ.t),
        static: Typ.t,
      })
    | OfTyp(Typ.t);

  /* The arrow, its tooltip and the content are one decision, so one cascade
     settles all three and none can drift from the others. */
  type reading = {
    glyph: string,
    description: string,
    content,
  };

  let samples_of = (info: info): list(Sample.t) =>
    switch (info.dynamics) {
    | None => []
    | Some(dynamics: Dynamics.Info.t) => dynamics.samples
    };

  let reading =
      (~samples: list(Sample.t), model: model, statics: Info.t): reading => {
    let self_typ = self_ty(statics) |> totalize_ty;
    let self = () => {
      glyph: "⇒",
      description: "Self type",
      content: OfTyp(self_typ),
    };
    switch (model) {
    | Dynamic => {
        glyph: "⇓",
        description: "Dynamic type (from runtime values)",
        content:
          FromRuntime({
            dynamic:
              DynamicTypInfer.dynamic_typ_of_samples(
                ~ctx=Info.ctx_of(statics),
                samples,
              ),
            static: self_typ,
          }),
      }
    /* ↔ not ⇔: the bundled font has no bidirectional double arrow, and a
       fallback renders differently per browser (see proj-type.css). */
    | _ when readings_agree(statics) => {
        glyph: "↔",
        description: "Self type matches expected type",
        content: OfTyp(self_typ),
      }
    | Self => self()
    | Expected =>
      switch (expected_ty(statics)) {
      | None => self()
      | Some(expected) => {
          glyph: "⇐",
          description: "Expected type",
          content: OfTyp(expected),
        }
      }
    };
  };

  let mode_view = (glyph, description) =>
    div(
      ~attrs=[Attr.classes(["mode"]), Attr.title(description)],
      [text(glyph)],
    );

  let typ_view = (content, utility, view_seg: View.seg, ~ctx) => {
    /* Dynamic hands over the exact segment its ids were computed from:
       preparing mints fresh paren ids, so a second one would not answer to
       them. */
    let (classes, seg) =
      switch (content) {
      | FromRuntime({dynamic, static}) =>
        let (seg, dynamic_ids) =
          DynamicTypInfer.segment_and_dynamic_ids(
            ~typ_to_seg_with_diff_ids=
              utility.typ_to_seg_with_diff_ids(~inline=true),
            ~ctx,
            ~static_typ=static,
            /* Runtime supplied nothing: the static type stands in, and diffs
               against itself, so nothing is coloured. */
            ~dynamic_typ=dynamic |> Option.value(~default=static),
          );
        ((id => Id.Set.mem(id, dynamic_ids) ? ["dynamic"] : []), seg);
      | OfTyp(typ) => (
          (_ => []),
          utility.term_to_seg(~inline=true, Typ(typ)),
        )
      };
    div(
      ~attrs=[Attr.classes(["type-cell"])],
      [seg |> view_seg(~single_line=true, ~classes, Sort.Typ)],
    );
  };

  /* Statics are absent when the user has turned them off, and briefly while
     an edit is being checked. Say so rather than showing `?`, which would
     claim the type is unknown when what is unknown is whether we looked. */
  let unavailable_view = () =>
    div(
      ~attrs=[
        Attr.classes(["type-cell"]),
        Attr.title("No type information for this expression"),
      ],
      [text("unavailable")],
    );

  let update = (model, info, a: action) => {
    let has_expected =
      Option.bind(info.statics, expected_ty) |> Option.is_some;
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
            switch (info.statics) {
            | None => [unavailable_view()]
            | Some(statics) =>
              let {glyph, description, content} =
                reading(~samples=samples_of(info), model, statics);
              [
                mode_view(glyph, description),
                typ_view(
                  content,
                  info.utility,
                  view_seg,
                  ~ctx=Info.ctx_of(statics),
                ),
              ];
            },
          ),
        ),
      overlay: None,
      below: None,
      error: false,
    };
};
