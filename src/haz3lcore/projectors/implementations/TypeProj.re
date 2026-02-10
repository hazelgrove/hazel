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
  | Some(InfoExp({self, _})) => Self.typ_of_exp(self)
  | Some(InfoPat({self, _})) => Self.typ_of_pat(self)
  | _ => None
  };

let totalize_ty = (expected_ty: option(Typ.t)): Typ.t =>
  switch (expected_ty) {
  | Some(expected_ty) => expected_ty
  | None => Typ.fresh(Unknown(Internal))
  };

let get_dynamic_typ = (info: info): Typ.t => {
  let dynamic_typ =
    info.dynamics
    |> Option.bind(
         _,
         (d: Dynamics.Info.t) => {
           let statics =
             Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)));
           let type_of = (c: Sample.t) => {
             IdTagged.rep_id(c.value)
             |> Id.Map.find_opt(_, statics(c.value))
             |> Option.bind(
                  _,
                  fun
                  | InfoExp(e) => {
                      Some(e.ty);
                    }
                  | _ => None,
                );
           };
           let types = List.map(type_of, d.samples) |> Util.OptUtil.sequence;

           Option.bind(
             types,
             Typ.meet_all(~empty=Typ.fresh(Unknown(Internal)), Ctx.empty),
           );
         },
       )
    |> Option.value(~default=Typ.fresh(Unknown(Internal)));
  dynamic_typ;
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
    | Pat(_)
    | Typ(_) => Some(Expected)
    | Any () => Some(Expected) /* Grout don't have sorts rn */
    | _ => None
    };
  };

  let dynamics = true;
  let focusable = Focusable.non;

  let display_mode = (model: model, statics: option(Language.Info.t)): string => {
    switch (model) {
    | Dynamic => "⇓"
    | _ when self_ty(statics) == expected_ty(statics) => "⇔"
    | _ when expected_ty(statics) |> totalize_ty |> Typ.is_syn => "⇒"
    | Self => "⇒"
    | Expected => "⇐"
    };
  };
  let mode_view = (model, info) =>
    div(
      ~attrs=[Attr.classes(["mode"])],
      [text(display_mode(model, info))],
    );

  let typ_view = (model, info: info, utility, view_seg: View.seg) => {
    let (is_dynamic, typ) =
      switch (model) {
      | Dynamic =>
        let dyn_typ =
          get_dynamic_typ(info)
          |> Grammar.map_typ_annotation(_ => IdTagged.IdTag.fresh())
          |> PadIds.pad_typ_ids;
        let self_ty =
          Option.value(
            ~default=Typ.fresh(Unknown(Internal)),
            self_ty(info.statics),
          );
        let dynamic_ids: list(Id.t) = Typ.diff(self_ty, dyn_typ);
        (List.mem(_, dynamic_ids), dyn_typ);
      | Expected when expected_ty(info.statics) |> totalize_ty |> Typ.is_syn => (
          (_ => false),
          self_ty(info.statics) |> totalize_ty,
        )
      | Expected => ((_ => false), expected_ty(info.statics) |> totalize_ty)
      | Self => ((_ => false), self_ty(info.statics) |> totalize_ty)
      };

    div(
      ~attrs=[Attr.classes(["type-cell"])],
      [
        Typ(typ)
        |> utility.term_to_seg
        |> view_seg(~single_line=true, ~is_dynamic, Sort.Typ),
      ],
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
    };
};
