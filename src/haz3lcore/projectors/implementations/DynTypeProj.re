open Virtual_dom.Vdom;
open Node;
open ProjectorBase;
open Language;

let self_ty = (info: option(Info.t)): option(Typ.t) =>
  switch (info) {
  | Some(InfoExp({self, _})) => Self.typ_of_exp(self)
  | Some(InfoPat({self, _})) => Self.typ_of_pat(self)
  | _ => None
  };
module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model =
    | Expected
    | Self;

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
  let focusable = Focusable.non;

  let typ_view = (info: info, utility, view_seg: View.seg) => {
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
    div(
      ~attrs=[Attr.classes(["dyntype-cell"])],
      [Typ(dynamic_typ) |> utility.term_to_seg |> view_seg(Sort.Typ)],
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
    let str = info.utility.seg_to_string(seg);
    let str = Re.Str.global_replace(Re.Str.regexp("\n"), " ", str);
    String.length(str) > max_len
      ? String.sub(str, 0, max_len) ++ "..." : str;
  };

  let placeholder = (_m, info) =>
    ProjectorCore.Shape.inline(3 + String.length(syntax_str(info)));

  let syntax_view = (info: info) => info |> syntax_str |> text;

  let icon = div(~attrs=[Attr.classes(["icon"])], []);

  let view =
      (
        _: model,
        info: info,
        ~local: action => Ui_effect.t(unit),
        ~parent as _,
        ~view_seg,
      ) =>
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
            [typ_view(info, info.utility, view_seg)],
          ),
        ),
      overlay: None,
    };
};
