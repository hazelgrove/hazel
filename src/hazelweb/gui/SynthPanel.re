open Virtual_dom.Vdom;
open Sexplib.Std;
open Shmyth;

let rec natlist_dhexp_to_string_list = (dhexp: DHExp.t): list(string) => {
  switch (dhexp) {
  | ListNil(_) => []
  | Cons(IntLit(n), cdr) =>
    [string_of_int(n)] @ natlist_dhexp_to_string_list(cdr)
  | _ => failwith("ERROR: natlist_dhexp_to_string: malformed natlist lit")
  };
};

let rec constraint_dhexp_to_string = (constraint_dhexp: DHExp.t): string => {
  switch (constraint_dhexp) {
  | IntLit(n) => string_of_int(n)
  | BoolLit(n) => string_of_bool(n)
  | BoundVar(str) => str
  | ListNil(_) => "[]"
  | Cons(_) =>
    "["
    ++ String.concat(", ", natlist_dhexp_to_string_list(constraint_dhexp))
    ++ "]"
  | _ =>
    print_endline("ERROR: constraint_dhexp_to_string:");
    print_endline(
      Sexplib.Sexp.to_string_hum(DHExp.sexp_of_t(constraint_dhexp)),
    );
    "?";
  };
}
and constraint_ex_to_string = (constraint_ex: Shmyth.hexample): string => {
  switch (constraint_ex) {
  | Ex(dhexp) => constraint_dhexp_to_string(dhexp)
  | ExIO(xs) =>
    let strs = List.map(constraint_dhexp_to_string, xs);
    "λ." ++ String.concat("→", strs);
  };
};

let constraints_to_text_table = (constraints: constraint_data) => {
  let headers =
    switch (constraints) {
    | [] => []
    | [(_ex, env), ..._] =>
      let env_names = List.map(((id_str, _v)) => id_str, env);
      // reversals are temp hack to get result at back -andrew
      List.rev(["=", ...env_names]);
    };
  let rows =
    constraints
    |> List.map(((ex, env)) => {
         let ex_str = constraint_ex_to_string(ex);
         let env_values =
           List.map(((_id_str, v)) => constraint_dhexp_to_string(v), env);
         // see above re: reversal
         List.rev([ex_str, ...env_values]);
       });
  switch (headers) {
  | [] => []
  | h => [h] @ rows
  };
};

[@deriving sexp]
type constraint_str_table = list(list(string));

let process_constraints = (hconstraints, hole_number) => {
  /* the constraints i'm extracting from smyth have this outer nesting
     list that i'm not sure i'm interpreting correctly. in the examples
     i've looked at so far, whenever there is more than one element in this
     outer list, the second element is a duplicate. for sake of time,
     i am shelving my attempt to figure this out and just taking the first element. */
  let one_world_constraints =
    switch (hconstraints) {
    | [] => []
    | [x, ..._] => x
    };
  let constraints_for_hole =
    one_world_constraints
    |> List.filter(((hole_name, _)) => hole_name == hole_number)
    |> (
      fun
      | [] => []
      | [(_hole_name, data), ..._] => data
    );
  constraints_to_text_table(constraints_for_hole);
};

let make_row = (row_fn, row_data) => {
  Node.tr(
    [Attr.classes([])],
    List.map(entry => row_fn([], [Node.text(entry)]), row_data),
  );
};

let constraints_table = (u, constraints) => {
  let contents =
    switch (process_constraints(constraints, u)) {
    | [] => []
    | [header, ...rows] => [
        make_row(Node.th, header),
        ...List.map(make_row(Node.td), rows),
      ]
    };
  Node.table([Attr.classes(["synth-constraints"])], contents);
};

let view = (~inject as _, ~view_of_text, j, es, u, constraints) => {
  let constraint_table = constraints_table(u, constraints);
  let fillings =
    es
    |> List.map(
         Lazy.force(UHDoc_Exp.mk, ~memoize=false, ~enforce_inline=false),
       )
    |> List.map(Pretty.LayoutOfDoc.layout_of_doc(~width=40, ~pos=0))
    |> List.map(OptUtil.get(() => failwith("failed layout")))
    |> List.mapi((i, l) =>
         Node.div(
           [Attr.classes(i == j ? ["selected-filling"] : [])],
           view_of_text(l),
         )
       );
  Node.span(
    [],
    [
      Node.div([Attr.classes(["synth-panel"])], fillings),
      constraint_table,
    ],
  );
};
