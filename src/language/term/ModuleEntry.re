[@deriving (show({with_path: false}), sexp, yojson)]
type t = TermBase.module_entry_t;
type term = TermBase.module_entry_term;
let term_of: t => term = IdTagged.term_of;

let rep_id = ({annotation: {ids, _}, _}: t) =>
  switch (ids) {
  | [] => raise(Invalid_argument("Exp.rep_id"))
  | [id, ..._] => id
  };

let temp: term => t =
  term => {
    term,
    annotation: {
      ids: [Id.invalid],
    },
  };

let env_to_entries = (env: Environment.t): list(t) =>
  Environment.to_listo(env)
  |> List.map(((s, entry)) => {
       Grammar.ValBinding(
         (Grammar.Var(s): Grammar.pat_term('a)) |> IdTagged.fresh,
         entry,
       )
       |> IdTagged.fresh
     });
