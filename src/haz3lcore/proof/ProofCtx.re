open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type entry = {
  name: string,
  exp: Exp.t,
  rule: ProofRule.t,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t = list(entry);

let empty = [];

let add_entry = (name: string, exp: Exp.t, ctx: t) => {
  let rule = ProofRule.exp_to_rule(exp);
  [{name, exp, rule}, ...ctx];
};

let rec get_empty_bindings = (ctx: Ctx.t) =>
  switch (ctx) {
  | [] => []
  | [VarEntry(var_entry), ...rs] => [
      (var_entry.name, None),
      ...get_empty_bindings(rs),
    ]
  | [_, ...rs] => get_empty_bindings(rs)
  };

let rec get_rewrites = (ctx: t, exp: Exp.t) =>
  switch (ctx) {
  | [] => []
  | [
      {rule: {bindings, assumptions: _, conclusion: Equality(a, b)}, _},
      ...rs,
    ] =>
    let bindings = get_empty_bindings(bindings);
    switch (MatchExp.match_exp([], bindings, a, exp)) {
    | Some(m) => [b |> MatchExp.substitute_exp(m), ...get_rewrites(rs, exp)]
    | None =>
      switch (MatchExp.match_exp([], bindings, b, exp)) {
      | Some(m) => [
          a |> MatchExp.substitute_exp(m),
          ...get_rewrites(rs, exp),
        ]
      | None =>
        print_endline("NOPE");
        get_rewrites(rs, exp);
      }
    };
  | [_, ...rs] => get_rewrites(rs, exp)
  };

let get_rewrites_and_locations =
    (ctx: t, exp: Exp.t): list((Id.t, Exp.t, Exp.t)) => {
  let result: ref(list((Id.t, Exp.t, Exp.t))) = ref([]);
  let _ =
    Exp.map_term(
      ~f_exp=
        (cont, exp) => {
          result :=
            (
              get_rewrites(ctx, exp)
              |> List.map(e => (exp |> Exp.rep_id, exp, e))
            )
            @ result^;
          cont(exp);
        },
      exp,
    );
  print_endline("RESULT");
  print_endline(result^ |> List.length |> string_of_int);
  result^;
};
