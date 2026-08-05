open Haz3lcore;
open ExplainThisForm;
open Example;

let ascription_example_1 = {
  sub_id: Asc1,
  term: mk_example("1 : Int"),
  message: "An integer literal 1 ascribed with the type Int.",
};
let ascription_example_2 = {
  sub_id: Asc2,
  term: mk_example("(3 + 4) : Int"),
  message: "An addition expression 3 + 4 ascribed with the type Int.",
};

let ascription_example_3 = {
  sub_id: Asc3,
  term: mk_example({|"hello" : (Int -> Int)|}),
  message: "A string literal \"hello\" ascribed with the type (Int -> Int), indicating it is a function that takes an Int and returns an Int. This is marked with a type error because a string cannot have a function type.",
};
let e = exp("e");
let typ = typ("ty");

let ascription_coloring_ids =
    (~exp_id: Id.t, ~typ_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(e), exp_id),
  (Piece.id(typ), typ_id),
];
let ascription = (~exp_id: Id.t, ~typ_id: Id.t): form => {
  let explanation =
    Printf.sprintf(
      "Represents a syntactic type ascription where an [*expression*](%s) is explicitly ascribed with a [*type*](%s). This is used to clarify or enforce the type of an expression.",
      Id.to_string(exp_id),
      Id.to_string(typ_id),
    );
  {
    id: AscExp,
    syntactic_form: [e, space(), ascription_exp(), space(), typ],
    expandable_id: None,
    explanation,
    examples: [
      ascription_example_1,
      ascription_example_2,
      ascription_example_3,
    ],
  };
};
let ascriptions = (~exp_id: Id.t, ~typ_id: Id.t): group => {
  id: AscExp,
  forms: [ascription(~exp_id, ~typ_id)],
};
