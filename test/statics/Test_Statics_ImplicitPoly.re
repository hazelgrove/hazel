open Language;
open Alcotest;
open Test_Statics_Prelude;

module T = FTemp.Typ;
let a = T.var("a");
let b = T.var("b");
let c = T.var("c");
let poly = (x, t) => T.poly(FTemp.TPat.var(x), t);
let unknown = () => T.unknown(Internal);

/* Every mark in the program. */
let all_marks = (program: string): list(Mark.t) => {
  let exp = parse_exp(program);
  let s = statics(exp);
  let found = ref([]);
  let _ =
    Grammar.map_exp_annotation(
      ({ids, _}: IdTagged.IdTag.t) =>
        List.iter(
          id =>
            switch (Statics.Map.lookup(id, s)) {
            | Some(info) => found := [info, ...found^]
            | None => ()
            },
          ids,
        ),
      exp,
    );
  found^ |> List.concat_map(Info.marks_of);
};

let has_conflict = program =>
  test_case(
    "Conflict localized to a hole: " ++ program,
    `Quick,
    () => {
      let marks = all_marks(program);
      check(
        bool,
        "has InferenceConflict",
        true,
        List.exists(
          fun
          | Mark.InferenceConflict(_) => true
          | _ => false,
          marks,
        ),
      );
      check(
        bool,
        "no ExpectationMismatch",
        false,
        List.exists(
          fun
          | Mark.ExpectationMismatch(_) => true
          | _ => false,
          marks,
        ),
      );
    },
  );

let tests = (
  "Statics.ImplicitPoly",
  [
    fully_consistent_typecheck(
      "Identity is generalized",
      {|let id = fun x -> x in id|},
      Some(poly("a", T.arrow(a, a))),
    ),
    fully_consistent_typecheck(
      "Generalized identity is instantiated at each use",
      {|let id = fun x -> x in (id(1), id(true))|},
      Some(T.prod([T.int(), T.bool()])),
    ),
    fully_consistent_typecheck(
      "A constrained binder is inferred, not gradual",
      {|let f = fun x -> x + 1 in f|},
      Some(T.arrow(T.int(), T.int())),
    ),
    fully_consistent_typecheck(
      "Two independent type variables",
      {|let pair = fun x -> fun y -> (x, y) in pair|},
      Some(poly("a", poly("b", T.arrow(a, T.arrow(b, T.prod([a, b])))))),
    ),
    fully_consistent_typecheck(
      "Higher-order: apply",
      {|let apply = fun f -> fun x -> f(x) in apply|},
      Some(poly("a", poly("b", T.arrow(T.arrow(a, b), T.arrow(a, b))))),
    ),
    fully_consistent_typecheck(
      "Higher-order: compose",
      {|let compose = fun f -> fun g -> fun x -> f(g(x)) in compose|},
      Some(
        poly(
          "a",
          poly(
            "b",
            poly(
              "c",
              T.arrow(
                T.arrow(a, b),
                T.arrow(T.arrow(c, a), T.arrow(c, b)),
              ),
            ),
          ),
        ),
      ),
    ),
    fully_consistent_typecheck(
      "Instantiation result flows on",
      {|let apply = fun f -> fun x -> f(x) in apply(fun y -> y + 1)(2)|},
      Some(T.int()),
    ),
    fully_consistent_typecheck(
      "An explicitly poly function needs no @<>",
      {|let id : poly a -> a -> a = typfun a -> fun x : a -> x in id(1)|},
      Some(T.int()),
    ),
    fully_consistent_typecheck(
      "Explicit type application still works",
      {|let id : poly a -> a -> a = typfun a -> fun x : a -> x in id@<Int>(1)|},
      Some(T.int()),
    ),
    fully_consistent_typecheck(
      "Recursive definitions are monomorphic inside, polymorphic outside",
      {|let len = fun xs -> case xs | [] => 0 | _ :: t => 1 + len(t) end in len|},
      Some(poly("a", T.arrow(T.list(a), T.int()))),
    ),
    fully_consistent_typecheck(
      "Recursive map",
      {|let map = fun f -> fun xs -> case xs | [] => [] | x :: t => f(x) :: map(f)(t) end in map|},
      Some(
        poly(
          "a",
          poly(
            "b",
            T.arrow(T.arrow(a, b), T.arrow(T.list(a), T.list(b))),
          ),
        ),
      ),
    ),
    fully_consistent_typecheck(
      "Recursive map used at two types",
      {|let map = fun f -> fun xs -> case xs | [] => [] | x :: t => f(x) :: map(f)(t) end in (map(fun x -> x + 1)([1]), map(fun b -> !b)([true]))|},
      Some(T.prod([T.list(T.int()), T.list(T.bool())])),
    ),
    fully_consistent_typecheck(
      "An outer binder is not generalized by an inner let",
      {|let f = fun x -> let g = fun y -> (x, y) in g(1) in f|},
      Some(poly("a", T.arrow(a, T.prod([a, T.int()])))),
    ),
    fully_consistent_typecheck(
      "A poly argument to a poly function is instantiated, not passed whole",
      {|let k = fun x -> x in let id = fun y -> y in k(id)(1)|},
      Some(T.int()),
    ),
    fully_consistent_typecheck(
      "Equality's operands share a type",
      {|let eq = fun x -> fun y -> x == y in eq|},
      Some(poly("a", T.arrow(a, T.arrow(a, T.bool())))),
    ),
    fully_consistent_typecheck(
      "Function sugar is generalized",
      {|let id(x) = x in (id(1), id("s"))|},
      Some(T.prod([T.int(), T.string()])),
    ),
    fully_consistent_typecheck(
      "An expression hole blocks generalization (etc)",
      {|let f = fun x -> ? in f|},
      Some(T.arrow(unknown(), unknown())),
    ),
    fully_consistent_typecheck(
      "An annotated let is not generalized",
      {|let f : ? -> ? = fun x -> x in f|},
      Some(T.arrow(unknown(), unknown())),
    ),
    fully_consistent_typecheck(
      "A projected binder is not generalized; its use decides it",
      {|let get = fun r -> r.a in get((a=1, b=true))|},
      Some(T.int()),
    ),
    fully_consistent_typecheck(
      "Elements checked against a type need not agree with each other",
      {|let fs : [Int -> ?] = [fun x -> x, fun y -> [y]] in fs|},
      Some(T.list(T.arrow(T.int(), unknown()))),
    ),
    has_conflict({|let id = fun x -> x in id(1) ++ "a"|}),
    has_conflict({|let f = fun x -> if x then 1 else x + 1 in f|}),
  ],
);
