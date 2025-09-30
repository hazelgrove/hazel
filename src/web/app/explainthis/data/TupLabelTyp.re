open Haz3lcore;
open ExplainThisForm;
open Example;

let labeled_example_1: example = {
  sub_id: Label1,
  term: mk_example("(x=1) : (x=Int)"),
  message: "A singleton tuple with label x being ascribed to the type of a singleton tuple with the label x and value type Int.",
};
let unlabeled_example: example = {
  sub_id: Label2,
  term: mk_example("(_=1) : (_=Int)"),
  message: "A singleton unlabeled tuple. Unlabeled tuple items are given explicitly nonlabeled with `_` in label position.",
};
let labeled_example_2: example = {
  sub_id: Label3,
  term: mk_example({|("Alex", age=5) :
  (age=Int, name=String) |}),
  message: "Labels will be rearranged and inserted on tuple literals based on the type expectation.",
};

let lab = typ("x");
let p = typ("p");

let labeled_exps_coloring_ids =
    (~label_id: Id.t, ~typ_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(lab), label_id),
  (Piece.id(p), typ_id),
];
let labeled_typ: form = {
  let explanation =
    "Assigns a [*label*](%s) to a [*type*](%s) appearing as an element within a Tuple type. "
    ++ "Labeled tuple item types cannot exist outside of a Tuple type. Labeled tuple item types that are not contained "
    ++ "within a Tuple are automatically converted into a singleton tuple type. "
    ++ "Singleton tuples can also be unlabeled by using `_` in the label position.";
  {
    id: LabeledTyp,
    syntactic_form: [lab, labeled_typ(), p],
    expandable_id: None,
    explanation,
    examples: [labeled_example_1, unlabeled_example, labeled_example_2],
  };
};
let labeled_typs: group = {
  id: LabeledTyp,
  forms: [labeled_typ],
};
