open MenhirParser;
open Alcotest;
open Language;
module Fresh = IdTagged.FreshGrammar;
let alco_check =
  (testable(Fmt.using(Exp.show, Fmt.string)))(
    // This is syntactic with ignore_wrappers=true
    Equality.(
      equality({
        ...syntactic_settings,
        ignore_parens: true,
      })
    ).
      exp,
  )
  |> Alcotest.check;

let strip_wrap =
  Exp.map_term(
    ~f_exp=
      (cont: TermBase.exp_t => TermBase.exp_t, e: TermBase.exp_t) =>
        switch (e.term) {
        | Parens(e)
        | Probe(e, _) => cont(e)
        | _ => cont(e)
        },
    ~f_pat=
      (cont, e) =>
        switch (e.term) {
        | Parens(e)
        | Probe(e, _) => cont(e)
        | _ => cont(e)
        },
    ~f_typ=
      (cont, e) =>
        switch (e.term) {
        | Parens(e) => cont(e)
        | _ => cont(e)
        },
    _,
  );

// Existing recovering parser
let make_term_parse = (s: string) =>
  strip_wrap(
    Haz3lcore.MakeTerm.from_zip_for_sem(
      Option.get(Haz3lcore.Parser.to_zipper(s)),
    ).
      term,
  );

let menhir_matches = (exp: Exp.t, actual: string) =>
  alco_check(
    "menhir matches expected parse",
    exp,
    Grammar.map_exp_annotation(
      _: IdTagged.IdTag.t => {ids: [Id.invalid]},
      Conversion.Exp.of_menhir_ast(Interface.parse_program(actual)),
    ),
  );

let menhir_only_test = (name: string, exp: Exp.t, actual: string) =>
  test_case(name, `Quick, () => {menhir_matches(exp, actual)});

let skip_menhir_maketerm_equivalent_test =
    (~speed_level=`Quick, name: string, _actual: string) =>
  test_case(name, speed_level, () => {Alcotest.skip()});

let full_parser_test = (name: string, exp: Exp.t, actual: string) =>
  test_case(
    name,
    `Quick,
    () => {
      alco_check(
        "expected parse matches MakeTerm parse",
        exp,
        make_term_parse(actual),
      );
      menhir_matches(exp, actual);
    },
  );

let menhir_maketerm_equivalent_test =
    (~speed_level=`Quick, name: string, actual: string) =>
  test_case(name, speed_level, () => {
    alco_check(
      "Menhir parse matches MakeTerm parse",
      make_term_parse(actual),
      Grammar.map_exp_annotation(
        _: IdTagged.IdTag.t => {ids: [Id.invalid]},
        Conversion.Exp.of_menhir_ast(Interface.parse_program(actual)),
      ),
    )
  });

/**
 * QCheck Test to check the equivalence of the Menhir and MakeTerm parsing.
 * We generate an expression, convert it to the core representation, convert it to a segment,
 * serialize it, parse it with MakeTerm, and parse it with Menhir.
 */
let qcheck_menhir_maketerm_equivalent_test =
  QCheck.Test.make(
    ~name="Menhir and maketerm are equivalent",
    ~count=100,
    QCheck_Util.arb_exp(~minimal_idents=false, 7),
    core_exp => {
      let segment =
        Haz3lcore.ExpToSegment.(
          exp_to_segment(~settings=Settings.editable(~inline=true), core_exp)
        );

      let serialized = Haz3lcore.Printer.of_segment(~holes="?", segment);
      let make_term_parsed = make_term_parse(serialized);
      let menhir_parsed = Interface.parse_program(serialized);
      let menhir_parsed_converted =
        Conversion.Exp.of_menhir_ast(menhir_parsed);

      switch (
        Equality.(
          equality({
            ...syntactic_settings,
            ignore_parens: true,
          })
        ).
          exp(
          make_term_parsed,
          Grammar.map_exp_annotation(
            _ => IdTagged.IdTag.fresh(),
            menhir_parsed_converted,
          ),
        )
      ) {
      | true => true
      | false => false
      | exception (Failure(msg)) =>
        print_endline("Error: " ++ msg);
        msg == "Sum type has non-unique constructors";
      };
    },
  );

/**
 * QCheck Test to check that menhir parses out what ExpToSegment serializes.
 * We generate an expression, convert it to the core representation, convert it to a segment,
 * serialize it, parse it with Menhir, and compare to the original.
 *
 *
 * Filter and Test not implemented
 * Deferral serializing as "deferral"
 * Right associated operator
 * https://github.com/hazelgrove/hazel/issues/1452
 * https://github.com/hazelgrove/hazel/issues/1451
 * https://github.com/hazelgrove/hazel/issues/1445
 */
let qcheck_menhir_serialized_equivalent_test =
  QCheck.Test.make(
    ~name="Menhir through ExpToSegment and back",
    ~count=1000,
    AST.arb_exp(7),
    exp => {
      let unit_exp = Conversion.Exp.of_menhir_ast(exp);
      let core_exp =
        Grammar.map_exp_annotation(_ => IdTagged.IdTag.fresh(), unit_exp);
      let segment =
        Haz3lcore.ExpToSegment.exp_to_segment(
          ~settings={
            inline: true,
            fold_case_clauses: false,
            fold_fn_bodies: `NoFold,
            hide_fixpoints: false,
            show_filters: true,
            show_unknown_as_hole: true,
            raise_if_padding: false,
          },
          core_exp,
        );
      let serialized = Haz3lcore.Printer.of_segment(~holes="?", segment);
      let menhir_parsed = Interface.parse_program(serialized);
      AST.equal_exp(menhir_parsed, exp);
    },
  );

let tests =
  Fresh.(
    "MenhirParser",
    Exp.[
      full_parser_test("Integer Literal", int(8), "8"),
      full_parser_test(
        "Fun",
        fn(Pat.var("x"), var("x"), None, None),
        "fun x -> x",
      ),
      full_parser_test(
        "String Literal",
        string("Hello World"),
        {|"Hello World"|},
      ),
      full_parser_test("Bool Literal", bool(true), "true"),
      full_parser_test("Empty Hole", empty_hole(), "?"),
      full_parser_test("Var", var("x"), "x"),
      full_parser_test("Parens", parens(var("y")), "(y)"),
      full_parser_test(
        "bin_op",
        bin_op(Int(Plus), int(4), int(5)),
        "4 + 5",
      ),
      full_parser_test(
        "Let",
        let_(Fresh.Pat.var("x"), int(5), var("x")),
        "let x = 5 in x",
      ),
      full_parser_test("Tuple", tuple([int(4), int(5)]), "(4, 5)"),
      full_parser_test(
        "Match",
        match(
          int(4),
          [(Pat.int(1), string("hello")), (Pat.wild(), string("world"))],
        ),
        {|case 4
       | 1 => "hello"
       | _ => "world"
      end|},
      ),
      full_parser_test(
        "If",
        if_(bool(true), int(8), int(6)),
        "if true then 8 else 6",
      ),
      full_parser_test(
        "Deferred Ap",
        deferred_ap(var("x"), [deferral(InAp)]),
        "x(_)",
      ),
      full_parser_test("Cons", cons(int(1), list_lit([])), "1 :: []"),
      full_parser_test(
        "ListLit",
        list_lit([int(1), int(2), int(3)]),
        "[1, 2, 3]",
      ),
      menhir_only_test("Unit", tuple([]), "()"),
      menhir_only_test("Constructor", constructor("A", None), "A"),
      menhir_only_test(
        "Constructor ascription",
        asc(constructor("A", None), Typ.int()),
        "A : Int",
      ),
      menhir_only_test(
        "Constructor of specific sum type",
        constructor("A", Some(Some(Typ.int()))),
        "A ~ Int",
      ),
      // TODO Fix for the tests below
      menhir_only_test(
        "Constructor with Type Variable",
        constructor("A", Some(Some(Typ.var("T")))),
        "A ~ T",
      ),
      full_parser_test(
        "Type Variable",
        let_(Pat.asc(Pat.var("x"), Typ.var("T")), empty_hole(), var("x")),
        "let x : T = ? in x",
      ),
      full_parser_test(
        "Type Alias",
        ty_alias(TPat.var("x"), Typ.int(), int(1)),
        "type x = Int in 1",
      ),
      full_parser_test(
        "Test",
        test(bin_op(Poly(Equals), int(3), int(3))),
        "test 3 == 3 end",
      ),
      full_parser_test(
        "Filter",
        filter(
          Filter({
            act: (Eval, All),
            pat: int(3),
          }),
          int(3),
        ),
        "eval 3 in 3" // TODO Use other filter commands
      ),
      full_parser_test(
        "List Concat",
        list_concat(
          list_lit([int(1), int(2)]),
          list_lit([int(3), int(4)]),
        ),
        "[1, 2] @ [3, 4]",
      ),
      full_parser_test(
        "times and divide precendence",
        bin_op(Int(Divide), bin_op(Int(Times), int(1), int(2)), int(3)),
        "1 * 2 / 3",
      ),
      full_parser_test(
        "plus and minus precendence",
        bin_op(Int(Plus), bin_op(Int(Minus), int(1), int(2)), int(3)),
        "1 - 2 + 3",
      ),
      full_parser_test(
        "Integer Ops",
        bin_op(
          Int(GreaterThanOrEqual),
          bin_op(
            Int(Minus),
            bin_op(Int(Plus), un_op(Int(Minus), int(1)), int(2)),
            bin_op(
              Int(Times),
              bin_op(Int(Divide), int(3), int(4)),
              bin_op(Int(Power), int(5), int(6)),
            ),
          ),
          int(8),
        ),
        "-1 + 2 - 3 / 4 * 5 ** 6 >= 8",
      ),
      full_parser_test("Float", float(1.), "1."),
      full_parser_test(
        "Float Ops",
        bin_op(
          Float(LessThan),
          bin_op(
            Float(Minus),
            float(2.),
            bin_op(
              Float(Times),
              bin_op(Float(Divide), float(3.), float(4.)),
              bin_op(Float(Power), float(5.), float(6.)),
            ),
          ),
          float(8.),
        ),
        "2. -. 3. /. 4. *. 5. **. 6. <. 8.",
      ),
      full_parser_test(
        "Let binding with type ascription",
        let_(Pat.asc(Pat.var("x"), Typ.int()), int(5), var("x")),
        "let (x: Int) = 5 in x",
      ),
      menhir_only_test(
        "named_function",
        fn(
          Pat.var("x"),
          bin_op(Int(Plus), var("x"), int(5)),
          None,
          Some("f"),
        ),
        "named_fun f x -> x + 5",
      ),
      full_parser_test(
        "basic sum type",
        let_(
          Pat.asc(
            Pat.var("x"),
            Typ.sum([
              Variant("A", [], None),
              Variant("B", [], None),
              Variant("C", [], Some(Typ.int())),
            ]),
          ),
          ap(Forward, constructor("C", None), int(7)),
          var("x"),
        ),
        "let x : +A +B +C(Int) = C(7) in x",
      ),
      menhir_maketerm_equivalent_test("Fold Projector Exp", "^^fold(1)"),
      menhir_maketerm_equivalent_test(
        "Fold Projector Typ",
        "type foo = ^^fold(Int) in 3",
      ),
      menhir_maketerm_equivalent_test(
        "Fold Projector Pat",
        "let ^^fold(x) = 3 in x",
      ),
      menhir_maketerm_equivalent_test("Empty Type Hole", "let g: ? = 7 in g"),
      menhir_maketerm_equivalent_test(
        "Pattern with type ascription",
        "fun (b : Bool) -> b",
      ),
      full_parser_test(
        "Type Hole in arrow ascription",
        fn(
          Pat.asc(
            Pat.var("b"),
            Typ.(
              parens(
                arrow(
                  unknown(TypeProvenance.hole(EmptyHole)),
                  unknown(TypeProvenance.hole(EmptyHole)),
                ),
              )
            ),
          ),
          empty_hole(),
          None,
          None,
        ),
        "fun (b : ? -> ?) -> ?",
      ),
      full_parser_test(
        "multiargument function",
        ap(Forward, var("f"), tuple([int(1), int(2)])),
        "f(1, 2)",
      ),
      menhir_maketerm_equivalent_test(
        "partial sum type",
        "type Partial = +Ok(?) + ? in ?",
      ),
      menhir_maketerm_equivalent_test(
        "Function with type variable",
        "fun (x : a) -> x",
      ),
      menhir_maketerm_equivalent_test(
        "Sequence addition precedence",
        "1+2;3",
      ),
      menhir_maketerm_equivalent_test(
        "And app precedence",
        "exp_equal(e1, e3) && exp_equal(e2, e4)",
      ),
      menhir_maketerm_equivalent_test(
        "Negation precedence with multiplication",
        "-num*1",
      ),
      menhir_maketerm_equivalent_test(
        "Concatenation association",
        "1::2::3::[]",
      ),
      menhir_maketerm_equivalent_test(
        "and less than precedence",
        "true && 23 < int_of_float(51.00)" // TODO This looks like a bug in MakeTerm
      ),
      menhir_maketerm_equivalent_test("Singleton labeled tuple", {|(h = 1)|}),
      menhir_maketerm_equivalent_test(
        ~speed_level=`Slow,
        "Altered Documentation Buffer: Basic Reference",
        {|
let empty_hole = ? in

let non_empty_hole : Int = true in

let bool: Bool = true in
let operators = !true && false || true in
let conditional = if !true then 1 else 2 in

let num: Int = 1 in
let arithmetic = -num*1 + 2/3 - 4**5 in
let comparison =
  (0 == 0, 0 < 1, 1 <= 1, 2 > 1, 1 >= 1)
in

let float: Float = 0.1 in
let arithmetic = 0. *. 1. +. 2. /. 3. -. 4. **. 5. in
let comparison =
  (0. ==. 0., 0. <. 1., 1. <=. 1., 2. >. 1., 1. >=. 1.)
in

let string = "Hello, world!" in
let concatenation  = string ++ " Goodbye." in
let comparison = string$== "Hello, world!" in

let tuple : (Int, Bool, (Bool, Int)) =
(1, true, (false, 3)) in
let (a, b, (c, d)) = tuple in

let y : (Int, Int, Int) -> Int =
fun (m, x, b) -> m * x + b in

let double_recursively : Int -> Int =
  fun n ->
    if n == 0
    then 0
    else double_recursively(n - 1) + 2
in

let (even : Int -> Bool, odd : Int -> Bool) =
  (fun n -> if n == 0 then true else odd(n - 1),
  fun n -> if n == 0 then false else even(n - 1))
in

let empty_list : [Int] = [] in
let non_empty_list : [Int] = 1::2::3::[] in
let list_literals : [Int] = [1, 2, 3] in
let length : [Int] -> Int =
  fun xs ->
    case xs
      | [] => 0
      | hd::tl => 1 + length(tl)
    end
in
let has_at_least_two_elements : [Int] -> Bool =
  fun xs ->
    case xs
      | [] => false
      | hd::[] => false
      | a::b::[] => true
    end
in

type Exp =
  + Var(String)
  + Lam(String, Exp)
+ Ap(Exp, Exp) in
let exp_equal: (Exp, Exp) -> Bool =
  fun es ->
    case es
      | (Var(x), Var(y)) => x$== y
      | (Lam((x1, e1)), Lam((x2, e2))) => x1$== x2 && exp_equal(e1, e2)
      | (Ap((e1, e2)), Ap((e3, e4))) => exp_equal(e1, e3) && exp_equal(e2, e4)
      | _ => false
    end
in

let poly_id: (poly a -> (a -> a)) =
  (typfun a -> (fun (x : a) -> x))
in
let apply_both:
poly a -> poly b -> (poly c -> c -> c) -> ((a, b) -> (a, b)) =
  typfun a -> typfun b ->
    fun (f : poly c -> (c -> c)) ->
      fun ((x, y) : (a, b)) -> (f@<a>(x), f@<b>(y))
in
let list_length: poly a -> ([a] -> Int) =
  typfun a -> fun (l : [a]) ->
    case l
      | [] => 0
      | hd::tl => 1 + list_length@<a>(tl)
    end
in

test 2 + 2 == 4 end;
test 3 + 3 == 6 end;
test 2 + 2 == 5 end;

2 + 2
    |},
      ),
      menhir_maketerm_equivalent_test(
        ~speed_level=`Slow,
        "Altered Documentation Buffer: Projectors",
        {|
let fold = (((((((((((()))))))))))) in
let folds: (Int -> Bool) = ? in
let guard: Bool = true in
let phase: Int = 44 in
let float: Float = 79.00 in
let (a:Int, f: Float) = (true, 28) in
let _ = "" in
let __ = "" in
let ___ = "a" in
let ____ = "shift" in
let _____ = "malicious" in
let ______ = "a shift   malicious" in
let box: Int = "malicious" in
if true && (23 < int_of_float(51.00))
then ______ else "its: " ++ box    |},
      ),
      menhir_maketerm_equivalent_test(
        ~speed_level=`Slow,
        "Altered Documentation Buffer: Types & Static Errors",
        {|
let _ = unbound in
let Undefined = Undefined in
let true = 2 in

let ? = if true then 1 else 1. in
let _ = if true then 1 else 1. in
let _: ? = if true then 1 else 1. in
let _: Int = if true then 1 else 1. in
let _: Fake = if true then 1 else true in
let (_, _) = if true then 1 else 1. in
let (_, _) = ((if true then 1 else 1.),?)    in
let (_: ?, _) = ((if true then 1 else 1.),?)    in
let [_] = [(if true then 1 else 1.)] in
let [_] = (if true then 1 else 1.) in

(?)(if true then 1 else 1.);
1(if true then 1 else 1.);
(1)(if true then 1 else 1.);
(fun ? -> ?)(if true then 1 else 1.);
(fun _ -> ?)(if true then 1 else 1.);
(fun (_: ?) -> ?)(if true then 1 else 1.);
(fun (_: Int) -> ?)(if true then 1 else 1.);

let _ = fun x -> if true then 1 else 1. in
let _: ? = fun x -> if true then 1 else 1. in
let _: ? -> ?  = fun x -> if true then 1 else 1. in
let _: ? -> Int = fun x -> if true then 1 else 1. in
let _: ? -> [?] = fun x -> if true then 1 else 1. in

(?)::[(if true then 1 else 1.)];
1::[(if true then 1 else 1.)];
(1, 1)::[(if true then 1 else 1.)];

let ? = [1, 1., true] in
let _ = [1, 1., true] in
let _: ? = [1, 1., true] in
let _: [?] = [1, 1., true] in
let _: [Int] = [1, 1., true] in

let _: [Int] = 1::[2] in
let _: [Int] = 1.0::[2] in
let _: [Int] = 1::[2.0] in
"BYE"
|},
      ),
      menhir_maketerm_equivalent_test(
        ~speed_level=`Slow,
        "Altered Documentation Buffer: adt dynamics",
        {|
type Exp =
  + Var(String)
  + Lam(String, Exp)
  + Ap(Exp, Exp) in

let exp_equal: (Exp, Exp) -> Bool =
  fun es ->
    case es
      | (Var(x), Var(y)) => x$== y
      | (Lam((x1, e1)), Lam((x2, e2))) => x1$== x2 && exp_equal(e1, e2)
      | (Ap((e1, e2)), Ap((e3, e4))) => exp_equal(e1, e3) && exp_equal(e2, e4)
      | _ => false end in

let subst: (Exp, String, Exp) -> Exp=
  fun (v, name, f) ->
    case f
      | Var(n) =>
        (if n$== name then v else f)
      | Lam((x, body)) =>
        Lam(x, subst(v,name, body))
      | Ap((e1,e2)) =>
  Ap(subst(v, name, e1), subst(v, name, e2)) end in

type Result =
  + Error(String)
  + Ok(Exp)
in

let result_equal: (Result, Result) -> Bool =
  fun rs ->
    case rs
      | (Ok(e1), Ok(e2)) => exp_equal(e1, e2)
      | (Error(e1), Error(e2)) => e1$== e2
| _ => false end in

let go: Exp -> Result =
  fun f ->
    case f
      | Var(n) => Error("Free Variable")
      | Lam((x, body)) => Ok(Lam(x, body))
      | Ap((e1,e2)) =>
      case go(e1)
        | Ok(Lam((x, body)))=>
        case go(e2)
          | Error(err) => Error(err)
        | Ok(arg) => go(subst(arg, x, body)) end
| _ => Error("Not a Function") end end in

test result_equal(
  go(Var("yo")),
Error("Free Variable")) end;

test result_equal(
  go(Ap(Var("no"), Lam("bro", Var("bro")))),
Error("Not a Function")) end;

test result_equal(
  go(Lam("yo", Var("yo"))),
Ok(Lam("yo", Var("yo")))) end;

test result_equal(
  go(Ap(Lam("yo", Var("yo")), Lam("bro", Var("bro")))),
Ok(Lam("bro", Var("bro")))) end
|},
      ),
      menhir_maketerm_equivalent_test(
        // Variable names are renamed due to lexing overtaking e, t, p, and tp
        ~speed_level=`Slow,
        "Altered Documentation Buffer: Polymorphism",
        {|let id = typfun A -> (fun (x : A) -> x) in
let ex1 = id@<Int>(1) in
let const : poly A -> (poly B -> (A -> B -> A)) =
typfun A -> (typfun B -> (fun x -> fun y -> x)) in
let ex2 = const@<Int>@<String>(2)("Hello World") in
let apply_both : poly A -> poly B -> (poly D -> D -> D) -> (A , B) -> (A , B) =
typfun A -> typfun B -> fun f -> fun (x, y) -> (f@<A>(x), f@<B>(y)) in
let ex3 = apply_both@<Int>@<String>(id)(3, "Hello World") in
let emptylist : poly A -> [A] = typfun A -> [] in
let map : poly A -> poly B -> (A -> B) -> ([A] -> [B]) =
  typfun A -> typfun B -> fun (f : (A -> B)) -> fun (l : [A]) ->
    case l
      | (h :: a) => f(h) :: map@<A>@<B>(f)(a)
      | _ => emptylist@<B>
end in
let ex4 = map@<Int>@<String>(string_of_int)([1,2,3]) in
type MyList = rec A -> (+Nil + Cons(Int, A)) in
let x : MyList = Cons(1, Cons(2, Cons(3, Nil))) in
type MyList2 = +Nil + Cons(Int, MyList2) in
type Broken = Int -> (+HasInt(Int) + HasMore(Int, Broken)) in
let list_of_mylist : (MyList -> [Int]) = fun (myl : MyList) ->
  case myl
    | Nil => []
    | Cons((h, a)) => h :: list_of_mylist(a)
end in
let ex5 = list_of_mylist(x) in
(ex1, ex2, ex3, ex4, ex5)
    |},
      ),
      // This fails because MakeTerm can't handle left to right keyword prefixes.
      skip_menhir_maketerm_equivalent_test(
        "Prefixed keyword parses",
        {|let ? = ina in ?|},
      ),
      skip_menhir_maketerm_equivalent_test(
        "Sum type messed up in make term",
        {|type ? = rec ? -> + Aramj -> Bool in ?|},
      ),
      skip_menhir_maketerm_equivalent_test(
        "List concat and typap",
        {|type ? = (+ Ulog, () -> Float) in let (()) = (()) in 0.001536|},
      ),
      skip_menhir_maketerm_equivalent_test(
        "Sum in product in typeap",
        {|((fun _ -> b)) @< [(+ Kfgii, Float)] >|},
      ),
      skip_menhir_maketerm_equivalent_test(
        "Non-unique constructors currently throws in equality",
        {|type ? = ((+ ? + ?)) in []|},
      ),
      QCheck_alcotest.to_alcotest(qcheck_menhir_maketerm_equivalent_test),
      QCheck_alcotest.to_alcotest(qcheck_menhir_serialized_equivalent_test),
    ],
  );
