open Util;
open ProjectorBase;
open Language;
open IdTagged.FreshGrammar;

// Detect if expression is an App type: ((HTML, Cmd), HTML -> Sub)
// Returns Some((html_model, init_cmd, subscriptions_fn)) or None
let detect_app =
    (exp: DHExp.t): option((DHExp.t, option(DHExp.t), option(DHExp.t))) => {
  switch (exp.term) {
  | Tuple([init, subs_fn])
  | Parens({term: Tuple([init, subs_fn]), _}) =>
    switch (init.term) {
    | Tuple([html_model, init_cmd])
    | Parens({term: Tuple([html_model, init_cmd]), _}) =>
      Some((html_model, Some(init_cmd), Some(subs_fn)))
    | _ => None
    }
  | _ => None
  };
};

// Evaluate a Hazel expression
let evaluate = exp =>
  fst(
    Evaluator.evaluate(
      ~env=Builtins.env_init,
      fst(
        Elaborator.elaborate(
          Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), exp),
          exp,
        ),
      ),
    ),
  );

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = Grammar.exp_t(IdTagged.IdTag.t);
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = unit;

  let init = (any: Any.t) =>
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

  let view = ({model, info, parent, view_seg, _}: View.args(model, action)) => {
    let current_model =
      switch (info.syntax |> info.utility.seg_to_term) {
      | Some(Exp(term)) => term
      | _ => model
      };

    // Check if model is an App type vs plain Html
    let (html_model, subscriptions) =
      switch (detect_app(current_model)) {
      | Some((html, _init_cmd, Some(subs_fn))) =>
        // It's an App - evaluate subscriptions function with current html
        let subs = evaluate(Exp.ap(Forward, subs_fn, html));
        (html, Some(subs));
      | _ =>
        // Plain Html - no subscriptions
        (current_model, None)
      };

    let seed: HazelDOM.t = {
      model: html_model,
      inject: (new_model: model) =>
        /* Allow HTMLements to replace themselves wholesale. Note that
           this will fail if anything other than a builtin is used in
           a handler */
        parent(SetSyntax(Exp(new_model) |> info.utility.term_to_seg)),
      view_term: term =>
        Exp(term)
        |> info.utility.term_to_seg
        |> view_seg(~background=false, Exp),
      projector_id: Some(info.id),
      subscriptions,
    };
    View.mk(HazelDOM.go(seed));
  };
};
