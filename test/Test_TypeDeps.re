open Alcotest;
open Haz3lcore;
open Language;

/* DefStatics type dependencies: an alias or constructor edit re-analyzes
   only items whose d_tfree mentions it; errors match monolithic statics */

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
    /* T, a (annotation), c (a's type mentions T), U (transitive), d
       (annotation U); b, e, g and the tail stay clean */
    ~expect_analyzed=5,
    "alias-users",
  );

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
    /* the first T and a; the second T shadows it, so z stays clean */
    ~expect_analyzed=2,
    "alias-shadowed",
  );

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

/* retyping a member changes its module's export type: its users re-analyze */
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
    /* M, its f member, the exports-tail member, and the consumer */
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
