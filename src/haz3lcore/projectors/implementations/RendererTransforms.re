open ProjectorBase;
open Language;

/* Shared transform pipeline used by the list-shaped rich-probe
 * renderers (TableRenderer, ListRenderer). A `transform` is a
 * function-valued Exp that gets applied to the projected expression
 * via `|>`, so renderers can describe each menu action as a small
 * Exp value and let this module handle composition and lifting. */

type transform =
  | Rowwise(Exp.t)
  | Listwise(Exp.t);

let strip_parens =
  Exp.map_term(~f_exp=(continue, e) =>
    switch (e.term) {
    | Parens(inner) => continue(inner)
    | _ => continue(e)
    }
  );

/* Apply a list of transforms to a base expression, producing the
 * piped result: base |> transform1 |> transform2 |> ... */
let apply_transforms = (base: Exp.t, transforms: list(transform)): Exp.t => {
  open IdTagged.FreshGrammar;
  let to_listwise = (t: transform): Exp.t =>
    switch (t) {
    | Rowwise(row_fn) =>
      Exp.(deferred_ap(var("map"), [deferral(InAp), row_fn]))
    | Listwise(expr) => expr
    };
  let transformations = List.map(to_listwise, transforms);
  let base = strip_parens(base);
  List.fold_left(
    (acc, transformation) => Exp.ap(Reverse, transformation, acc),
    base,
    transformations,
  );
};

/* Single conversion point: transform list → Base.segment.
 * Returns None if the syntax isn't an expression or if lifting fails —
 * callers should treat that as "do nothing" rather than crashing. */
let to_segment =
    (info: info, transforms: list(transform)): option(Base.segment) => {
  let ok = ref(true);
  let lifted =
    info.utility.lift_syntax(
      ~inline=false,
      fun
      | Exp(exp) => Exp(apply_transforms(exp, transforms))
      | other => {
          ok := false;
          other;
        },
      info.syntax,
    );
  ok^ ? lifted : None;
};
