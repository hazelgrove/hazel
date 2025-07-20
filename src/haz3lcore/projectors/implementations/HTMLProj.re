open Util;
open ProjectorBase;

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = Language.Grammar.exp_t(Language.IdTagged.IdTag.t);
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = unit;

  let init = (any: Language.Any.t) =>
    switch (any) {
    //TODO: Be more (and less) picky
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
        model: model,
        info,
        ~local as _,
        ~parent: external_action => Ui_effect.t(unit),
        ~view_seg: View.seg,
      ) => {
    let seed: HazelDOM.t = {
      model:
        switch (info.syntax |> info.utility.seg_to_term) {
        | Some(Exp(term)) => term
        | _ => model
        },
      inject: (new_model: model) =>
        /* Allow HTMLements to replace themselves wholesale. Note that
           this will fail if anything other than a builtin is used in
           a handler */
        parent(SetSyntax(Exp(new_model) |> info.utility.term_to_seg)),
      view_term: term =>
        Exp(term)
        |> info.utility.term_to_seg
        |> view_seg(~background=false, Exp),
    };
    View.mk(HazelDOM.go(seed));
  };
};
