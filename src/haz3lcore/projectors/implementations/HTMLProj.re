open Util;
//open Virtual_dom.Vdom;
open ProjectorBase;

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = Language.Grammar.exp_t(Language.IdTagged.IdTag.t);
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = unit;

  let init = (any: Language.Any.t) =>
    switch (any) {
    //TODO(andrew): be more and less pickier
    | Exp({term: Ap(_, {term: Constructor("Div", _), _}, _), _} as exp) =>
      Some(exp)
    | _ => None
    };

  let focusable = Focusable.non;
  let dynamics = false;
  let placeholder = (_, _) => ProjectorCore.Shape.inline(10);
  let update = (m, _, _) => m;

  let view =
      (
        model,
        _info,
        ~local as _,
        ~parent as _: external_action => Ui_effect.t(unit),
        ~view_seg as _,
      ) =>
    View.mk(Language.MVU.go2(model));
};
