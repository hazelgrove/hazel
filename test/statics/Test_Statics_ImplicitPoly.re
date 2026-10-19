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

/* The elaboration printed as results and the stepper print it. */
let printed = (~show_ascriptions, program: string): string => {
  let exp = parse_exp(program);
  let (_, elab) =
    Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), exp);
  Haz3lcore.ExpToSegment.exp_to_segment(
    ~settings={
      ...Haz3lcore.ExpToSegment.Settings.editable(~inline=true),
      show_ascriptions,
    },
    elab,
  )
  |> Haz3lcore.Printer.of_segment(~holes="?", ~refractors=[]);
};

let tests = (
  "Statics.ImplicitPoly",
  [
    test_case(
      "Hidden type abstractions and applications stay hidden",
      `Quick,
      () => {
        let program = {|let id = fun x -> x in id(1)|};
        check(
          string,
          "hidden",
          "let id = fun x -> x in id(1)",
          printed(~show_ascriptions=false, program),
        );
        check(
          bool,
          "shown with ascriptions",
          true,
          Util.StringUtil.match(
            Util.StringUtil.regexp("@<"),
            printed(~show_ascriptions=true, program),
          ),
        );
      },
    ),
    test_case(
      "With inference off, unannotated binders stay gradual",
      `Quick,
      () => {
        let exp = parse_exp({|let id = fun x -> x in id|});
        let (s, _) =
          Statics.mk(
            {
              ...CoreSettings.on,
              inference: false,
            },
            Builtins.ctx_init(Some(Int)),
            exp,
          );
        check(
          option(testable_typ),
          "gradual",
          Some(T.arrow(unknown(), unknown())),
          synthesized_type(s, exp),
        );
      },
    ),
    fully_consistent_typecheck(
      "Identity is generalized",
      {|let id = fun x -> x in id|},
      Some(poly("a", T.arrow(a, a))),
    ),
    fully_consistent_typecheck(
      "List builtins are polymorphic",
      {|map|},
      Some(
        poly(
          "a",
          poly(
            "b",
            T.arrow(T.prod([T.list(a), T.arrow(a, b)]), T.list(b)),
          ),
        ),
      ),
    ),
    fully_consistent_typecheck(
      "A builtin call gets its instance",
      {|map([1, 2], fun x -> x + 1)|},
      Some(T.list(T.int())),
    ),
    has_conflict({|append([1], ["a"])|}),
    test_case(
      "With inference off, a builtin call gets ? type arguments",
      `Quick,
      () => {
        let exp = parse_exp({|map([1, 2], string_of_int)|});
        let (s, _) =
          Statics.mk(
            {
              ...CoreSettings.on,
              inference: false,
            },
            Builtins.ctx_init(Some(Int)),
            exp,
          );
        check(bool, "no errors", true, errors(s) == []);
      },
    ),
    fully_consistent_typecheck(
      "Value restriction: a computed definition is not generalized",
      {|let id(x) = x in
let compose(f, g) = fun x -> f(g(x)) in
let both = compose(id, id) in both|},
      Some(T.arrow(unknown(), unknown())),
    ),
    fully_consistent_typecheck(
      "Value restriction: values built from values are generalized",
      {|let pair = (fun x -> x, fun y -> y) in pair|},
      Some(poly("a", poly("b", T.prod([T.arrow(a, a), T.arrow(b, b)])))),
    ),
    test_case(
      "Value restriction: written type variables need a value", `Quick, () =>
      check(
        bool,
        "marked",
        true,
        all_marks({|let x : a = (fun y -> y)(1) in x|}) != [],
      )
    ),
    fully_consistent_typecheck(
      "Written type variables are bound by the definition",
      {|let id(x : a) = x in id|},
      Some(poly("a", T.arrow(a, a))),
    ),
    fully_consistent_typecheck(
      "Written type variable on the let's name",
      {|let id : a -> a = fun x -> x in id|},
      Some(poly("a", T.arrow(a, a))),
    ),
    fully_consistent_typecheck(
      "Written type variables come before inferred ones",
      {|let pair(x : a, y) = (x, y) in pair|},
      Some(
        poly("a", poly("b", T.arrow(T.prod([a, b]), T.prod([a, b])))),
      ),
    ),
    fully_consistent_typecheck(
      "Nested definitions share a written type variable",
      {|let outer(x : a) = let inner(y : a) = y in inner(x) in outer(true)|},
      Some(T.bool()),
    ),
    test_case(
      "With inference off, written type variables stay unbound",
      `Quick,
      () => {
        let exp = parse_exp({|let id(x : a) = x in id|});
        let (s, _) =
          Statics.mk(
            {
              ...CoreSettings.on,
              inference: false,
            },
            Builtins.ctx_init(Some(Int)),
            exp,
          );
        check(bool, "marked", true, errors(s) != []);
      },
    ),
    test_case("Written type variables are rigid", `Quick, () =>
      check(
        bool,
        "marked",
        true,
        all_marks({|let f(x : a) = x + 1 in f|}) != [],
      )
    ),
    fully_consistent_typecheck(
      "Any lowercase name can be a written type variable",
      {|let f(x : elem) = x in f|},
      Some(poly("elem", T.arrow(T.var("elem"), T.var("elem")))),
    ),
    test_case(
      "An unbound type name is one error, not also a conflict", `Quick, () =>
      check(
        int,
        "marks",
        1,
        List.length(all_marks({|let x : Strnig = (fun y -> y)(1) in x|})),
      )
    ),
    test_case(
      "Uppercase names and misspelled built-in types aren't bound",
      `Quick,
      () => {
        check(bool, "T", true, all_marks({|let f(x : T) = x in f|}) != []);
        check(
          bool,
          "int",
          true,
          all_marks({|let f(x : int) = x in f|}) != [],
        );
        check(
          bool,
          "string",
          true,
          all_marks({|let f(x : string) = x in f|}) != [],
        );
      },
    ),
    test_case(
      "Written type variables are not in scope in the body", `Quick, () =>
      check(
        bool,
        "marked",
        true,
        all_marks({|let id(x : a) = x in (1 : a)|}) != [],
      )
    ),
    fully_consistent_typecheck(
      "Function definition sugar is generalized",
      {|let id(x) = x in id|},
      Some(poly("a", T.arrow(a, a))),
    ),
    fully_consistent_typecheck(
      "Recursive function definition sugar is generalized",
      {|let map(f, xs) = case xs | [] => [] | x :: rest => f(x) :: map(f, rest) end in map|},
      Some(
        poly(
          "a",
          poly(
            "b",
            T.arrow(T.prod([T.arrow(a, b), T.list(a)]), T.list(b)),
          ),
        ),
      ),
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
      "Module members are generalized",
      {|module M = { let id = fun x -> x } in (M.id(1), M.id(true))|},
      Some(T.prod([T.int(), T.bool()])),
    ),
    fully_consistent_typecheck(
      "An expression hole blocks generalization (etc)",
      {|let f = fun x -> ? in f|},
      Some(T.arrow(unknown(), unknown())),
    ),
    fully_consistent_typecheck(
      "A tuple-pattern let isn't generalized, nor decided by its uses",
      {|let (f, g) = (fun x -> x, fun y -> y) in (f(1), f(true))|},
      Some(T.prod([unknown(), unknown()])),
    ),
    fully_consistent_typecheck(
      "An annotated let is not generalized",
      {|let f : ? -> ? = fun x -> x in f|},
      Some(T.arrow(unknown(), unknown())),
    ),
    /* Projection gives no constraint and a type variable can't be
       projected, so `r` stays gradual; uses don't decide it either. */
    fully_consistent_typecheck(
      "A projected binder stays gradual",
      {|let get = fun r -> r.a in get((a=1, b=true))|},
      Some(unknown()),
    ),
    fully_consistent_typecheck(
      "A let's uses don't decide its holes",
      {|let f = fun x -> ? in f(1)|},
      Some(unknown()),
    ),
    fully_consistent_typecheck(
      "Branches agree even when checked against ?",
      {|let f = fun x -> ((if true then x else 1) : ?) in f|},
      Some(T.arrow(T.int(), unknown())),
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
