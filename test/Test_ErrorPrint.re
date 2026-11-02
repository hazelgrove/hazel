/* Printing a static error, and a node's type, for the user to read. */

open Alcotest;
open Haz3lcore;
open Language;

let info_map = (code: string): Statics.Map.t =>
  switch (Parser.to_zipper(~root=Exp, code)) {
  | None => fail("parse failure: " ++ code)
  | Some(z) =>
    fst(
      Statics.mk(
        CoreSettings.on,
        Builtins.ctx_init(Some(Operators.default_mode)),
        MakeTerm.from_zip_for_sem(z, ~root=Exp).term,
      ),
    )
  };

/* Programs where statics renames a shadowing type binder (Typ.unshadow),
   each with an error mentioning a type it renamed. */
let shadowing = [
  "typfun T -> fun (t : T) -> typfun T -> fun (u : T) -> (t : T)",
  "typfun T -> fun (t : T) -> typfun T -> fun (u : T) -> ((t, u) : (T, T))",
  "(typfun T -> fun (t : T) -> typfun T -> t) && true",
  "type a' = Bool in (typfun a -> fun (x : a) -> typfun a -> fun (y : a') -> fun (z : a) -> (x, y, z)) && true",
  "type T = Int in let x : T = 1 in type T = Bool in !x",
  "typfun T -> fun (t : T) -> type T = Int -> T in fun (g : T) -> (g, t) && true",
];

let shows_no_renamed_name = (what, shown) =>
  check(
    list(string),
    what,
    [],
    List.filter(s => String.contains(s, '#'), shown),
  );

/* Whether one of [code]'s messages matches [pattern]. */
let reports = (code: string, pattern: string) => {
  let messages = ErrorPrint.all(info_map(code));
  let matches = m =>
    switch (Str.search_forward(Str.regexp(pattern), m, 0)) {
    | _ => true
    | exception Not_found => false
    };
  check(
    bool,
    String.concat("\n", messages),
    true,
    List.exists(matches, messages),
  );
};

let tests = (
  "ErrorPrint.TypeNames",
  [
    test_case("a message never shows a renamed binder's name", `Quick, () =>
      List.iter(
        code => shows_no_renamed_name(code, ErrorPrint.all(info_map(code))),
        shadowing,
      )
    ),
    test_case("a node's type never shows a renamed binder's name", `Quick, () =>
      List.iter(
        code =>
          shows_no_renamed_name(
            code,
            Id.Map.fold(
              (_, info: Info.t, shown) =>
                switch (info) {
                | InfoExp({ty, _}) => [ErrorPrint.Print.typ(ty), ...shown]
                | _ => shown
                },
              info_map(code),
              [],
            ),
          ),
        shadowing,
      )
    ),
    test_case(
      "a mismatch between two bindings of one name tags the one not meant here",
      `Quick,
      () =>
      reports(
        "typfun T -> fun (t : T) -> typfun T -> fun (u : T) -> (t : T)",
        "Expecting type T but got inconsistent type T~[0-9a-f][0-9a-f][0-9a-f]$",
      )
    ),
    /* https://github.com/hazelgrove/hazel/issues/2478 */
    test_case(
      "a type shown alone is tagged when its name means another binding here",
      `Quick,
      () =>
      reports(
        "type T = Int in let x : T = 1 in type T = Bool in !x",
        "Expecting type Bool but got inconsistent type T~[0-9a-f][0-9a-f][0-9a-f]$",
      )
    ),
  ],
);
