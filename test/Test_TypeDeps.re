open Alcotest;
open Haz3lcore;
open Language;

/* Type-side dependency tracking in DefStatics (the "type co_ctx"):
   type-alias/constructor export changes used to re-analyze the WHOLE
   downstream suffix; now they dirty only items whose d_tfree mentions
   the changed names (with shadowing and transitive alias chains).
   Each case asserts BOTH the analyzed count and error parity vs the
   monolithic analysis of the edited program. */

let settings = CoreSettings.on;
let ctx0 = Builtins.ctx_init(Some(Operators.default_mode));

let parse_exp = (src: string): Segment.t =>
  switch (CorpusUtil.parse(~root=Exp, src)) {
  | Some(seg) => seg
  | None =>
    fail(
      "parse failed: " ++ Option.value(FastParse.bail_note^, ~default="?"),
    )
  };

let sorted_ids = CorpusUtil.sorted_ids;

/* run: cold calc on src, apply needle edit, incremental calc; assert
   analyzed count and error parity vs monolithic on the edited term */
let run = (~src, ~needle, ~repl, ~expect_analyzed, name) => {
  let seg = parse_exp(src);
  let term = MakeTerm.go(seg).term;
  let ds0 = DefStatics.calc(~settings, term);
  let (seg2, edited) = CorpusUtil.edit_token(~needle, ~repl, seg);
  check(bool, name ++ ": edit found", true, edited);
  let term2 = MakeTerm.go(seg2).term;
  let ds1 = DefStatics.calc(~settings, ~prev=ds0, term2);
  check(
    int,
    name ++ ": analyzed",
    expect_analyzed,
    DefStatics.last_analyzed^,
  );
  let (mono_map, _) = Statics.mk_unmemoized(settings, ctx0, term2);
  check(
    Alcotest.list(string),
    name ++ ": error parity",
    sorted_ids(Statics.Map.error_ids(mono_map)),
    sorted_ids(DefStatics.all_error_ids(ds1)),
  );
};

/* alias edit: users re-analyze (annotation use, use-through-a-var's
   stored type, transitive alias chain) — non-users stay clean */
let alias_users = () =>
  run(
    ~src=
      "type T = [Int] in
let a : T = [1] in
let b = 2. in
let c = a in
type U = [T] in
let d : U = [[2]] in
let e = \"x\" in
let g = b +. 1. in
1",
    ~needle="Int",
    ~repl="Bool",
    /* T's item, a (annotation), c (a's stored type mentions T),
       U (def mentions T: transitive), d (annotation U) —
       b, e, g, and the tail stay clean */
    ~expect_analyzed=5,
    "alias-users",
  );

/* a shadowing redefinition stops the cascade */
let alias_shadowed = () =>
  run(
    ~src=
      "type T = [Int] in
let a : T = [1] in
type T = Bool in
let z : T = true in
9",
    ~needle="Int",
    ~repl="Float",
    /* first T's item + a; the second T redefines the name with an
       unchanged, unrelated definition, so z stays clean */
    ~expect_analyzed=2,
    "alias-shadowed",
  );

/* constructor-set change: users of the sum re-analyze */
let ctor_change = () =>
  run(
    ~src=
      "type S = Aa + Bb in
let h : S = Aa in
let k = 3 in
case h | Aa => 1 | Bb => 2 end",
    ~needle="Bb",
    ~repl="Cc",
    /* S's item, h (annotation + ctor use), the trailing case
       (ctor patterns + scrutinee type) — k stays clean */
    ~expect_analyzed=3,
    "ctor-change",
  );

/* retyping a module MEMBER changes the module's export type; users
   of the module must re-analyze (the BenchStatics cascade class) */
let member_retype = () =>
  run(
    ~src=
      "module M = {
  let f : () -> Bool = fun _ -> true;
  let g = 1
} in
let consume = M.f(()) in
9",
    ~needle="Bool",
    ~repl="String",
    /* M's item + its f member + the exports-tail member + the
       consumer (mentions M) */
    ~expect_analyzed=4,
    "member-retype",
  );

let tests = (
  "TypeDeps",
  [
    test_case("module member retype", `Quick, member_retype),
    test_case("alias users only", `Quick, alias_users),
    test_case("alias shadowing stops cascade", `Quick, alias_shadowed),
    test_case("constructor change", `Quick, ctor_change),
  ],
);
