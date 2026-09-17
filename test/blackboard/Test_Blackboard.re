/* Tests for the Blackboard kernel (src/language/blackboard).  The fixtures
   are the paper's own signatures, with implicit braces expanded and `=`
   written as `eq A a b`.  Two of them are the paper's typos, which the
   checker should find; two others record known limits of the decidable
   fragment, where the paper's examples lean on equations the checker does
   not rewrite with. */

open Alcotest;
open Language;

let located_t = testable(BbError.pp_located, BbError.equal_located);

let parse_doc = (s: string): BbTerm.doc =>
  switch (BbParse.doc(s)) {
  | Ok(d) => d
  | Error(m) => failf("parse error: %s", m)
  };

let parse_term = (s: string): BbTerm.t =>
  switch (BbParse.term(s)) {
  | Ok(t) => t
  | Error(m) => failf("parse error: %s", m)
  };

let reports = (s: string): list(BbCheck.report) =>
  snd(BbCheck.check_doc(parse_doc(s)));

let errors = (s: string): list(BbError.located) =>
  BbCheck.all_errors(reports(s));

let errors_of_entry = (name: string, s: string): list(BbError.t) =>
  errors(s)
  |> List.filter((l: BbError.located) => l.entry == name)
  |> List.map((l: BbError.located) => l.err);

let pending = (s: string): list(string) =>
  List.filter_map((r: BbCheck.report) => r.pending, reports(s));

/* Paper, Section 2 (l. 158-172), braces expanded. */
let eq_block = {|
assume
eq : (A : type) -> (a1 a2 : A) -> type
refl : (A : type) -> (a : A) -> eq A a a
replace : (A1 A2 : type) -> eq type A1 A2 -> A1 -> A2
|};

/* cong-ap exactly as printed at l. 164-171: `(a : A) -> B` with
   B : A -> type, and `(a1 a1 : A)`. */
let cong_ap_printed = {|
cong-ap :
  (A : type) ->
  (B : A -> type) ->
  (f1 f2 : (a : A) -> B) ->
  (a1 a1 : A) ->
  eq ((a : A) -> B) f1 f2 ->
  eq A a1 a2 ->
  eq (B a1) (f1 a1) (f2 a2)
|};

/* cong-ap with both slips repaired. */
let cong_ap_fixed = {|
cong-ap :
  (A : type) ->
  (B : A -> type) ->
  (f1 f2 : (a : A) -> B a) ->
  (a1 a2 : A) ->
  eq ((a : A) -> B a) f1 f2 ->
  eq A a1 a2 ->
  eq (B a1) (f1 a1) (f2 a2)
|};

/* Paper, Section 4, subtyping, refinements, intersections (l. 231-298). */
let section4 = {|
assume
eq : (A : type) -> (a1 a2 : A) -> type
construct
subtype : (A B : type) -> type
subtype-eq : (A B : type) -> eq type (subtype A B) ((x : A) -> x : B)
by definition
assume
type-ext : (A B : type) -> subtype A B -> subtype B A -> eq type A B
assume
refine : (A : type) -> (P : A -> type) -> type
refine-intro : (A : type) -> (P : A -> type) -> (x : A) -> P x -> x : refine A P
refine-elim :
  (A M : type) ->
  (P : A -> type) ->
  (x : refine A P) ->
  ((h : (x : A)) -> P x -> M) ->
  M
assume
intersect : (A B : type) -> type
intersect-intro : (A B : type) -> (x : A) -> (h : (x : B)) -> x : intersect A B
intersect-elim :
  (A B M : type) ->
  (x : intersect A B) ->
  ((h1 : (x : A)) -> (h2 : (x : B)) -> M) ->
  M
construct
refinement-subtype-1 : (A : type) -> (P : A -> type) -> subtype (refine A P) A
refinement-subtype-2 :
  (A : type) ->
  (P1 P2 : A -> type) ->
  ((x : A) -> P1 x -> P2 x) ->
  subtype (refine A P1) (refine A P2)
intersect-subtype-1 : (A B : type) -> subtype (intersect A B) A
intersect-subtype-2 : (A B : type) -> subtype (intersect A B) B
intersect-subtype-3 : (A B C : type) -> subtype C A -> subtype C B -> subtype C (intersect A B)
by proof
|};

/* Paper, l. 315-318: eq-rel is never defined. */
let quot_block = {|
assume
eq : (A : type) -> (a1 a2 : A) -> type
assume
quot : (A : type) -> (R : eq-rel A) -> type
|};

/* Paper, l. 344-372, int-elim exactly as printed: s and p take an int
   first, but sp and ps apply them as `s (p h)` with h : M x. */
let int_block = {|
assume
eq : (A : type) -> (a1 a2 : A) -> type
construct
int : type
zero : int
succ : int -> int
pred : int -> int
succ-pred : (x : int) -> eq int (succ (pred x)) x
pred-succ : (x : int) -> eq int (pred (succ x)) x
int-elim :
  (M : int -> type) ->
  (z : M zero) ->
  (s : (x : int) -> M x -> M (succ x)) ->
  (p : (x : int) -> M x -> M (pred x)) ->
  (sp : (x : int) -> (h : M x) -> eq (M x) (s (p h)) h) ->
  (ps : (x : int) -> (h : M x) -> eq (M x) (p (s h)) h) ->
  (x : int) ->
  M x
by quotient
|};

/* Paper, l. 401-408: A : U is used where a type is needed; the paper
   relies on U = (type | small) and a coercion the checker cannot see. */
let small_block = {|
assume
eq : (A : type) -> (a1 a2 : A) -> type
refine : (A : type) -> (P : A -> type) -> type
small : type -> type
U : type
U-eq : eq type U (refine type small)
arrow-small : (A : U) -> (B : A -> U) -> small ((a : A) -> B a)
|};

let tests = (
  "Blackboard",
  [
    test_case("Section 2 equality block checks", `Quick, () =>
      check(list(located_t), "no errors", [], errors(eq_block))
    ),
    test_case(
      "cong-ap as printed: both slips are reported",
      `Quick,
      () => {
        let errs = errors_of_entry("cong-ap", eq_block ++ cong_ap_printed);
        let b_not_a_type =
          BbError.NotAType(Var("B"), Pi("_", Var("A"), Type));
        check(
          bool,
          "B : A -> type is not a type",
          true,
          List.mem(b_not_a_type, errs),
        );
        check(
          bool,
          "a2 is unbound",
          true,
          List.mem(BbError.Unbound("a2"), errs),
        );
      },
    ),
    test_case(
      "cong-ap repaired: the conclusion needs the equation a1 = a2",
      `Quick,
      () => {
        let errs = errors_of_entry("cong-ap", eq_block ++ cong_ap_fixed);
        switch (errs) {
        | [
            ArgumentMismatch(
              _,
              App(Var("f2"), Var("a2")),
              App(Var("B"), Var("a1")),
              App(Var("B"), Var("a2")),
            ),
          ] =>
          ()
        | _ =>
          failf(
            "expected exactly the mismatch of f2 a2 : B a2 against B a1, got %s",
            String.concat("; ", List.map(BbError.to_string, errs)),
          )
        };
      },
    ),
    test_case(
      "Section 4 subtyping, refinement, intersection check",
      `Quick,
      () => {
        check(list(located_t), "no errors", [], errors(section4));
        check(
          list(string),
          "construct blocks leave their tactics pending",
          ["definition", "proof"],
          pending(section4),
        );
      },
    ),
    test_case("quot: eq-rel is not defined", `Quick, () =>
      check(
        bool,
        "eq-rel unbound",
        true,
        List.mem(
          BbError.Unbound("eq-rel"),
          errors_of_entry("quot", quot_block),
        ),
      )
    ),
    test_case(
      "int-elim as printed: s and p applied to h",
      `Quick,
      () => {
        let errs = errors_of_entry("int-elim", int_block);
        let is_h_for_int =
          fun
          | BbError.ArgumentMismatch(
              _,
              Var("h"),
              Var("int"),
              App(Var("M"), Var("x")),
            ) =>
            true
          | _ => false;
        check(int, "two mismatches", 2, List.length(errs));
        check(
          bool,
          "each is h : M x where an int is expected",
          true,
          List.for_all(is_h_for_int, errs),
        );
      },
    ),
    test_case("small universe: A : U needs a coercion to type", `Quick, () =>
      check(
        bool,
        "A is not a type",
        true,
        List.mem(
          BbError.NotAType(Var("A"), Var("U")),
          errors_of_entry("arrow-small", small_block),
        ),
      )
    ),
    test_case("printer and parser round-trip", `Quick, () =>
      List.iter(
        s => {
          let t = parse_term(s);
          let t' = parse_term(BbTerm.to_string(t));
          check(bool, "round-trip of " ++ s, true, BbTerm.alpha_eq(t, t'));
        },
        [
          "type",
          "(A : type) -> (a1 a2 : A) -> type",
          "(x : A) -> x : B",
          "(A M : type) -> (P : A -> type) -> (x : refine A P) -> ((h : (x : A)) -> P x -> M) -> M",
          "(_ : (x : A)) -> M",
          "(t : T) : type",
          "f (g x) y",
        ],
      )
    ),
    test_case(
      "substitution avoids capture",
      `Quick,
      () => {
        let t = BbTerm.subst("x", Var("y"), Pi("y", Type, Var("x")));
        check(
          bool,
          "(y : type) -> x  [y/x]  is  (z : type) -> y",
          true,
          BbTerm.alpha_eq(t, Pi("z", Type, Var("y"))),
        );
      },
    ),
    test_case(
      "a membership hypothesis establishes typing",
      `Quick,
      () => {
        let ctx: BbCheck.ctx = [
          ("h", Mem(Var("x"), Var("A"))),
          ("x", Var("B")),
          ("B", Type),
          ("A", Type),
        ];
        switch (BbCheck.has_type(ctx, Var("x"), Var("A"))) {
        | Ok(None) => ()
        | _ => fail("x : A should follow from h : (x : A)")
        };
      },
    ),
  ],
);
