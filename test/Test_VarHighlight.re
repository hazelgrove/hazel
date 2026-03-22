open Alcotest;
open Language;

let statics = Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)));

let parse_exp = (s: string) =>
  switch (Haz3lcore.Parser.to_term(s)) {
  | Some(e) => e
  | None => fail("Failed to parse: " ++ s)
  };

/* Build info_map and find all InfoExp Var entries with the given name */
let find_var_refs =
    (info_map: Statics.Map.t, name: string): list((Id.t, Info.t)) =>
  Id.Map.fold(
    (id, info: Info.t, acc) =>
      switch (info) {
      | InfoExp({term: {term: Var(n), _}, _}) when n == name => [
          (id, info),
          ...acc,
        ]
      | _ => acc
      },
    info_map,
    [],
  );

/* Find the InfoPat Var entry for a binding with the given name.
 * If multiple bindings exist, returns the first found. */
let find_var_binding =
    (info_map: Statics.Map.t, name: string): option((Id.t, Info.t)) =>
  Id.Map.fold(
    (id, info: Info.t, acc) =>
      switch (acc) {
      | Some(_) => acc
      | None =>
        switch (info) {
        | InfoPat({term: {term: Var(n), _}, _}) when n == name =>
          Some((id, info))
        | _ => None
        }
      },
    info_map,
    None,
  );

/* Find InfoTPat Var entry for a type variable binding */
let find_tvar_binding =
    (info_map: Statics.Map.t, name: string): option((Id.t, Info.t)) =>
  Id.Map.fold(
    (id, info: Info.t, acc) =>
      switch (acc) {
      | Some(_) => acc
      | None =>
        switch (info) {
        | InfoTPat({term: {term: Var(n), _}, _}) when n == name =>
          Some((id, info))
        | _ => None
        }
      },
    info_map,
    None,
  );

/* Find InfoTyp Var entries for type variable references */
let find_tvar_refs =
    (info_map: Statics.Map.t, name: string): list((Id.t, Info.t)) =>
  Id.Map.fold(
    (id, info: Info.t, acc) =>
      switch (info) {
      | InfoTyp({term: {term: Var(n), _}, _}) when n == name => [
          (id, info),
          ...acc,
        ]
      | _ => acc
      },
    info_map,
    [],
  );

let highlight_ids = (info_map, info) =>
  Statics.Map.var_highlight_ids(info_map, info);

/* Helper: check that highlight_ids contains expected_id */
let has_id = (ids, expected_id) => List.exists(Id.equal(expected_id), ids);

let test_let_ref_to_binding =
  test_case("Let: reference highlights binding + sibling uses", `Quick, () => {
    let exp = parse_exp("let x = 1 in x");
    let info_map = statics(exp);
    let refs = find_var_refs(info_map, "x");
    check(bool, "found x reference", true, List.length(refs) >= 1);
    let (_, ref_info) = List.hd(refs);
    let ids = highlight_ids(info_map, ref_info);
    /* Should include the binding site */
    let binding = find_var_binding(info_map, "x");
    check(bool, "found x binding", true, binding != None);
    let (binding_id, _) = Option.get(binding);
    check(bool, "highlights binding", true, has_id(ids, binding_id));
  });

let test_let_binding_to_refs =
  test_case("Let: binding highlights all uses", `Quick, () => {
    let exp = parse_exp("let x = 1 in x + x");
    let info_map = statics(exp);
    let (_, binding_info) = Option.get(find_var_binding(info_map, "x"));
    let ids = highlight_ids(info_map, binding_info);
    let refs = find_var_refs(info_map, "x");
    check(
      bool,
      "found multiple x references",
      true,
      List.length(refs) >= 2,
    );
    /* All references should be highlighted */
    List.iter(
      ((ref_id, _)) =>
        check(
          bool,
          "highlights ref " ++ Id.to_string(ref_id),
          true,
          has_id(ids, ref_id),
        ),
      refs,
    );
  });

let test_fun_param =
  test_case("Fun: parameter binding highlights body uses", `Quick, () => {
    let exp = parse_exp("let f = fun x -> x + x in f(1)");
    let info_map = statics(exp);
    let (_, binding_info) = Option.get(find_var_binding(info_map, "x"));
    let ids = highlight_ids(info_map, binding_info);
    let refs = find_var_refs(info_map, "x");
    check(bool, "found x references", true, List.length(refs) >= 2);
    List.iter(
      ((ref_id, _)) =>
        check(
          bool,
          "highlights ref " ++ Id.to_string(ref_id),
          true,
          has_id(ids, ref_id),
        ),
      refs,
    );
  });

let test_match_case =
  test_case("Match: case binding highlights body uses", `Quick, () => {
    let exp = parse_exp("case 1 | x => x + x end");
    let info_map = statics(exp);
    let (_, binding_info) = Option.get(find_var_binding(info_map, "x"));
    let ids = highlight_ids(info_map, binding_info);
    let refs = find_var_refs(info_map, "x");
    check(bool, "found x references", true, List.length(refs) >= 2);
    List.iter(
      ((ref_id, _)) =>
        check(
          bool,
          "highlights ref " ++ Id.to_string(ref_id),
          true,
          has_id(ids, ref_id),
        ),
      refs,
    );
  });

let test_match_ref_to_binding =
  test_case("Match: reference highlights case binding", `Quick, () => {
    let exp = parse_exp("case 1 | x => x end");
    let info_map = statics(exp);
    let refs = find_var_refs(info_map, "x");
    check(bool, "found x reference", true, List.length(refs) >= 1);
    let (_, ref_info) = List.hd(refs);
    let ids = highlight_ids(info_map, ref_info);
    let (binding_id, _) = Option.get(find_var_binding(info_map, "x"));
    check(bool, "highlights binding", true, has_id(ids, binding_id));
  });

let test_shadowing =
  test_case("Shadowing: inner binding doesn't highlight outer", `Quick, () => {
    let exp = parse_exp("let x = 1 in let x = 2 in x");
    let info_map = statics(exp);
    /* The reference x should resolve to the inner binding */
    let refs = find_var_refs(info_map, "x");
    check(bool, "found x reference", true, List.length(refs) >= 1);
    let (_, ref_info) = List.hd(refs);
    let ids = highlight_ids(info_map, ref_info);
    /* Should highlight exactly 1 binding (the inner one) */
    let bindings =
      Id.Map.fold(
        (id, info: Info.t, acc) =>
          switch (info) {
          | InfoPat({term: {term: Var("x"), _}, _}) when has_id(ids, id) => [
              id,
              ...acc,
            ]
          | _ => acc
          },
        info_map,
        [],
      );
    check(int, "highlights exactly one binding", 1, List.length(bindings));
  });

let test_multiple_refs =
  test_case("Multiple references all highlighted", `Quick, () => {
    let exp = parse_exp("let x = 1 in x + x + x");
    let info_map = statics(exp);
    let refs = find_var_refs(info_map, "x");
    check(bool, "found 3+ x references", true, List.length(refs) >= 3);
    /* From any one reference, all others should be highlighted */
    let (ref_id, ref_info) = List.hd(refs);
    let ids = highlight_ids(info_map, ref_info);
    List.iter(
      ((other_id, _)) =>
        if (!Id.equal(other_id, ref_id)) {
          check(
            bool,
            "highlights sibling " ++ Id.to_string(other_id),
            true,
            has_id(ids, other_id),
          );
        },
      refs,
    );
  });

let test_tvar_ref_to_binding =
  test_case(
    "Type variable: reference highlights binding",
    `Quick,
    () => {
      let exp = parse_exp("type T = Int in let x : T = 1 in x");
      let info_map = statics(exp);
      let tvar_refs = find_tvar_refs(info_map, "T");
      check(bool, "found T reference", true, List.length(tvar_refs) >= 1);
      let (_, ref_info) = List.hd(tvar_refs);
      let ids = highlight_ids(info_map, ref_info);
      let binding = find_tvar_binding(info_map, "T");
      check(bool, "found T binding", true, binding != None);
      let (binding_id, _) = Option.get(binding);
      check(bool, "highlights binding", true, has_id(ids, binding_id));
    },
  );

let test_tvar_binding_to_refs =
  test_case(
    "Type variable: binding highlights all uses",
    `Quick,
    () => {
      let exp = parse_exp("type T = Int in let x : T = 1 in let y : T = 2 in x + y");
      let info_map = statics(exp);
      let binding = find_tvar_binding(info_map, "T");
      check(bool, "found T binding", true, binding != None);
      let (_, binding_info) = Option.get(binding);
      let ids = highlight_ids(info_map, binding_info);
      let tvar_refs = find_tvar_refs(info_map, "T");
      check(bool, "found T references", true, List.length(tvar_refs) >= 2);
      List.iter(
        ((ref_id, _)) =>
          check(
            bool,
            "highlights ref " ++ Id.to_string(ref_id),
            true,
            has_id(ids, ref_id),
          ),
        tvar_refs,
      );
    },
  );

let tests = (
  "VarHighlight",
  [
    test_let_ref_to_binding,
    test_let_binding_to_refs,
    test_fun_param,
    test_match_case,
    test_match_ref_to_binding,
    test_shadowing,
    test_multiple_refs,
    test_tvar_ref_to_binding,
    test_tvar_binding_to_refs,
  ],
);
