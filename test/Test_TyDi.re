open Alcotest;
open Haz3lcore;
open Language;

/* Build a zipper with caret at the indicated position (¦),
 * compute statics, and return TyDi's suggestion buffer. */
let tydi_suggest = (code: string): option(string) => {
  open Util.OptUtil.Syntax;
  let actions = Test_Editing.mk(code);
  let z = Test_Editing.perform(Zipper.init(), actions);
  let MakeTerm.{term, _} = MakeTerm.from_zip_for_sem(z);
  let info_map =
    Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), term);
  let ci = Indicated.ci_of(z, info_map);
  let* z = TyDi.set_buffer(~ci, z);
  TyDi.get_unparsed_buffer(z);
};

let tydi_test = (~name, ~code, ~expect) =>
  test_case(name, `Quick, () =>
    check(option(string), name, expect, tydi_suggest(code))
  );

let dot_label_tests = (
  "TyDi.DotLabel",
  [
    tydi_test(
      ~name="Labeled tuple: prefix match",
      ~code="let m : (x=Int, length=Int) = (x=1, length=5) in m.le¦",
      ~expect=Some("ngth"),
    ),
    tydi_test(
      ~name="Labeled tuple: exact match, no suffix",
      ~code="let m : (x=Int, y=Int) = (x=1, y=2) in m.x¦",
      ~expect=None,
    ),
    tydi_test(
      ~name="Labeled tuple: second label",
      ~code="let m : (x=Int, yy=Int) = (x=1, yy=2) in m.y¦",
      ~expect=Some("y"),
    ),
    tydi_test(
      ~name="Labeled tuple: no match",
      ~code="let m : (x=Int, y=Int) = (x=1, y=2) in m.z¦",
      ~expect=None,
    ),
    tydi_test(
      ~name="Unknown type: no suggestions",
      ~code="m.x¦",
      ~expect=None,
    ),
    tydi_test(
      ~name="List dot projection: prefix match",
      ~code="let xs : [(name=String, age=Int)] = [] in xs.na¦",
      ~expect=Some("me"),
    ),
  ],
);

let variable_tests = (
  "TyDi.Variables",
  [
    tydi_test(
      ~name="Complete variable prefix",
      ~code="let myvar = 5 in my¦",
      ~expect=Some("var"),
    ),
    tydi_test(
      ~name="Complete variable exact match",
      ~code="let x = 5 in x¦",
      ~expect=None,
    ),
    tydi_test(
      ~name="Multiple candidates: picks first alphabetically",
      ~code="let ab = 1 in let ac = 2 in a¦",
      ~expect=Some("b"),
    ),
    /* When the top alphabetical match is exact, no suffix is returned,
     * even if longer matches exist. This is expected set_buffer behavior:
     * it tries only the first match. */
    tydi_test(
      ~name="Exact match at top suppresses longer matches",
      ~code="let x = 1 in let xy = 2 in x¦",
      ~expect=None,
    ),
    /* Builtins are included in suggestions */
    tydi_test(
      ~name="Builtin variable: string_of_int",
      ~code="let x : String = st¦",
      ~expect=Some("ring_capitalize("),
    ),
  ],
);

let constructor_tests = (
  "TyDi.Constructors",
  [
    tydi_test(
      ~name="Bool constructor: false",
      ~code="let x : Bool = f¦",
      ~expect=Some("alse"),
    ),
    tydi_test(
      ~name="Sum type: None constructor",
      ~code="let x : +None +Some(Int) = N¦",
      ~expect=Some("one"),
    ),
    tydi_test(
      ~name="Sum type: Some constructor application",
      ~code="let x : +None +Some(Int) = S¦",
      ~expect=Some("ome("),
    ),
    tydi_test(
      ~name="Constructor in pattern",
      ~code="fun x : +None +Some(Int) -> case x | N¦",
      ~expect=Some("one"),
    ),
  ],
);

let operand_tests = (
  "TyDi.Operands",
  [
    tydi_test(
      ~name="true for Bool",
      ~code="let x : Bool = tr¦",
      ~expect=Some("ue"),
    ),
    tydi_test(
      ~name="false for Bool",
      ~code="let x : Bool = fa¦",
      ~expect=Some("lse"),
    ),
  ],
);

let leading_form_tests = (
  "TyDi.LeadingForms",
  [
    tydi_test(
      ~name="fun suggested for arrow type",
      ~code="let f : Int -> Int = fu¦",
      ~expect=Some("n "),
    ),
    tydi_test(
      ~name="test suggested for unit type",
      ~code="let t : () = te¦",
      ~expect=Some("st "),
    ),
  ],
);

let operator_tests = (
  "TyDi.Operators",
  [
    tydi_test(
      ~name="Plus after int is exact match",
      ~code="let x : Int = 1 +¦",
      ~expect=None,
    ),
    tydi_test(
      ~name="String concat operator",
      ~code={|let x : String = "a" +¦|},
      ~expect=Some("+"),
    ),
  ],
);

let type_tests = (
  "TyDi.Types",
  [
    tydi_test(
      ~name="Type variable completion: Int",
      ~code="let x : I¦",
      ~expect=Some("nt"),
    ),
    tydi_test(
      ~name="Type variable completion: Bool",
      ~code="let x : B¦",
      ~expect=Some("ool"),
    ),
    tydi_test(
      ~name="Type variable completion: String",
      ~code="let x : St¦",
      ~expect=Some("ring"),
    ),
    tydi_test(
      ~name="Qualified type: dot label prefix match",
      ~code="module Mod = { type MyType = Int } in let x : Mod.My¦",
      ~expect=Some("Type"),
    ),
    tydi_test(
      ~name="Qualified type: dot label exact match",
      ~code="module Mod = { type MyType = Int } in let x : Mod.MyType¦",
      ~expect=None,
    ),
    tydi_test(
      ~name="Qualified type: dot label no match",
      ~code="module Mod = { type MyType = Int } in let x : Mod.Z¦",
      ~expect=None,
    ),
  ],
);

let suppression_tests = (
  "TyDi.Suppression",
  [
    /* In dot position, only dot_labels are suggested, not context variables */
    tydi_test(
      ~name="Dot position suppresses general suggestions",
      ~code="let m : (x=Int) = (x=1) in let ab = 1 in m.a¦",
      ~expect=None /* "ab" from context is suppressed */
    ),
    /* label_sort=true suppresses even when cls is not Label */
    tydi_test(
      ~name="Dot position: non-matching label prefix",
      ~code="let m : (abc=Int) = (abc=1) in let xyz = 1 in m.x¦",
      ~expect=None /* "xyz" suppressed because label_sort=true */
    ),
  ],
);

let qualified_tests = (
  "TyDi.Qualified",
  [
    /* Direct match: module member type matches expected type */
    tydi_test(
      ~name="Qualified: direct field match",
      ~code="let m : (x=Int, y=Int) = (x=1, y=2) in let n : Int = m.¦",
      ~expect=None /* No prefix typed after dot yet — dot_labels take over */
    ),
    tydi_test(
      ~name="Qualified: direct field via module prefix",
      ~code="let mm : (empty=String) = (empty=\"\") in let x : String = mm¦",
      ~expect=Some(".empty"),
    ),
    tydi_test(
      ~name="Qualified: function application field",
      ~code=
        "let mm : (double=Int -> Int) = (double=fun n -> n * 2) in let x : Int = mm¦",
      ~expect=Some(".double("),
    ),
    /* Prefix matching: partial module name */
    tydi_test(
      ~name="Qualified: partial module name prefix",
      ~code=
        "let math : (square=Int -> Int) = (square=fun n -> n * n) in let x : Int = ma¦",
      ~expect=Some("th.square("),
    ),
    /* Multiple fields: picks first alphabetically */
    tydi_test(
      ~name="Qualified: alphabetical ordering",
      ~code=
        "let mm : (beta=Int, alpha=Int) = (beta=2, alpha=1) in let x : Int = mm¦",
      ~expect=Some(".alpha"),
    ),
    /* Only consistent fields suggested */
    tydi_test(
      ~name="Qualified: only type-consistent fields",
      ~code=
        "let mm : (name=String, age=Int) = (name=\"a\", age=1) in let x : Int = mm¦",
      ~expect=Some(".age"),
    ),
    /* Lookahead: qualified suggestion in Bool context via comparison */
    tydi_test(
      ~name="Qualified: lookahead in Bool context",
      ~code="let mm : (count=Int) = (count=0) in let b : Bool = mm¦",
      ~expect=Some(".count"),
    ),
    /* No qualified suggestions when module has no matching fields */
    tydi_test(
      ~name="Qualified: no match when field types don't match",
      ~code="let mm : (name=String) = (name=\"a\") in let x : Int = mm¦",
      ~expect=None,
    ),
    /* Module keyword syntax */
    tydi_test(
      ~name="Qualified: module keyword with capitalized name",
      ~code="module M = {let x = 1} in let n : Int = M.¦",
      ~expect=None /* In dot position, dot_labels handle it */
    ),
    tydi_test(
      ~name="Qualified: module keyword prefix match",
      ~code="module Mm = {let val = 1} in let n : Int = Mm¦",
      ~expect=Some(".val"),
    ),
    /* Module name that shadows a builtin type alias should not
     * cause the module itself to be suggested as an operand */
    tydi_test(
      ~name="Qualified: type-name module, no matching fields",
      ~code="module String = {let empty = \"\"} in let i : Int = Str¦",
      ~expect=None,
    ),
    tydi_test(
      ~name="Qualified: type-name module with matching field",
      ~code=
        "module String = {let empty = \"\"; let length = string_length} in let i : Int = Str¦",
      ~expect=Some("ing.length("),
    ),
  ],
);

/* Base type names (Int, String, Bool, Float, Nat, SInt) have Exp/Pat-sort
 * molds but no type entry in TyDiForms, so without filtering they'd get
 * Unknown type and match any expected type. Verify they're suppressed in
 * Exp and Pat positions but still work in Typ position. */
let base_typ_suppression_tests = (
  "TyDi.BaseTypSuppression",
  [
    tydi_test(
      ~name="Nat not suggested as Exp operand",
      ~code="let x : Int = N¦",
      ~expect=None,
    ),
    tydi_test(
      ~name="Nat not suggested as Pat operand over constructor",
      ~code="fun x : +None +Some(Int) -> case x | N¦",
      ~expect=Some("one"),
    ),
    tydi_test(
      ~name="SInt not suggested as Exp operand",
      ~code="let x : Int = S¦",
      ~expect=None,
    ),
    tydi_test(
      ~name="Nat still suggested in Typ position",
      ~code="let x : N¦",
      ~expect=Some("at"),
    ),
  ],
);

let tests = [
  dot_label_tests,
  variable_tests,
  constructor_tests,
  operand_tests,
  leading_form_tests,
  operator_tests,
  type_tests,
  suppression_tests,
  qualified_tests,
  base_typ_suppression_tests,
];
