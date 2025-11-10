open Virtual_dom.Vdom;
open Node;
open ProjectorBase;
open Language;
open Util;

let expected_ty = (info: option(Info.t)): option(Typ.t) =>
  switch (info) {
  | Some(InfoExp({ana, _}))
  | Some(InfoPat({ana, _})) => Some(ana)
  | Some(InfoTyp({term, _})) => Some(term) // TODO Expected doesn't make sense for types
  | _ => None
  };

let self_ty = (info: option(Info.t)): option(Typ.t) =>
  switch (info) {
  | Some(InfoExp({self, _})) => Self.typ_of_exp(self)
  | Some(InfoPat({self, _})) => Self.typ_of_pat(self)
  | Some(InfoTyp({term, _})) => Some(term)
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
           let type_of = (c: Dynamics.Probe.Closure.t) => {
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
           let types = List.map(type_of, d) |> Util.OptUtil.sequence;

           Option.map(Typ.consistent_join(Ctx.empty), types);
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
    | Typ(_) // TODO This seems to behave oddly on grout
    | Pat(_) => Some(Expected)
    | Any () => Some(Expected) /* Grout don't have sorts rn */
    | _ => None
    };
  };

  let dynamics = true;
  let focusable = Focusable.non;

  let _display_ty = (model, statics, dyn_typ): option(Typ.t) =>
    switch (model) {
    | Dynamic => Some(dyn_typ)
    | _ when expected_ty(statics) |> totalize_ty |> Typ.is_syn =>
      statics |> self_ty
    | Self => statics |> self_ty
    | Expected => statics |> expected_ty
    };

  let display_mode = (model: model, statics: option(Language.Info.t)): string =>
    switch (model) {
    | Dynamic => "↦"
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
    let (is_dynamic, typ) =
      switch (model) {
      | Dynamic =>
        let self_ty = self_ty(info.statics);
        let dyn_typ =
          get_dynamic_typ(info)
          |> Grammar.map_typ_annotation(_ => IdTagged.IdTag.fresh(), _);
        let ids: list(Id.t) =
          Typ.diff(
            Option.value(~default=Typ.fresh(Unknown(Internal)), self_ty),
            dyn_typ,
          );
        let is_dynamic_id = (id: Id.t): bool => {
          List.mem(id, ids);
        };
        (is_dynamic_id, dyn_typ);
      | Expected => ((_ => false), expected_ty(info.statics) |> totalize_ty)
      | Self => ((_ => false), self_ty(info.statics) |> totalize_ty)
      };

    div(
      ~attrs=[Attr.classes(["type-cell"])],
      [
        Typ(typ) |> utility.term_to_seg |> view_seg(~is_dynamic, Sort.Typ, _),
      ],
    );
  };

  let update = (model, _, a: action) =>
    switch (a, model) {
    | (ToggleDisplay, Expected) => Self
    | (ToggleDisplay, Self) => Dynamic
    | (ToggleDisplay, Dynamic) => Expected
    };

  let syntax_str = (info: info) => {
    let max_len = 30;
    let seg = Segment.unparenthesize(info.syntax);
    let str = info.utility.seg_to_string(seg);
    let str = StringUtil.replace(StringUtil.regexp("\n"), str, " ");
    String.length(str) > max_len
      ? String.sub(str, 0, max_len) ++ "..." : str;
  };

  let placeholder = (_m, info) =>
    ProjectorCore.Shape.inline(3 + String.length(syntax_str(info)));

  let syntax_view = (info: info) => info |> syntax_str |> text;

  let icon = div(~attrs=[Attr.classes(["icon"])], []);

  let view = ({model, info, local, view_seg, _}: View.args(model, action)) =>
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
