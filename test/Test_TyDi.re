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
    check(
      option(string),
      name,
      expect,
      tydi_suggest(code),
    )
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
  ],
);

let suppression_tests = (
  "TyDi.Suppression",
  [
    /* In dot position, only dot_labels are suggested, not context variables */
    tydi_test(
      ~name="Dot position suppresses general suggestions",
      ~code="let m : (x=Int) = (x=1) in let ab = 1 in m.a¦",
      ~expect=None, /* "ab" from context is suppressed */
    ),
    /* label_sort=true suppresses even when cls is not Label */
    tydi_test(
      ~name="Dot position: non-matching label prefix",
      ~code="let m : (abc=Int) = (abc=1) in let xyz = 1 in m.x¦",
      ~expect=None, /* "xyz" suppressed because label_sort=true */
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
];
