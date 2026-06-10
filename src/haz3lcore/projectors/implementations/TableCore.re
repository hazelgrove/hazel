open Util;
open Language;

/* TableCore: shared table parsing logic for TableProj and TableRenderer.
   The cell/assembly rendering half lives in web TableCoreView. */

let max_column_length = 12;

/* --- Table Parsing --- */

type table_data = (list(option(string)), list(list(Exp.t)));

let rec extract_entry = (e: Exp.t): option((option(string), Exp.t)) =>
  switch (e.term) {
  | Parens(inner) => extract_entry(inner)
  | TupLabel({term: Label(l), _}, v) => Some((Some(l), v))
  | TupLabel({term: EmptyHole, _}, v) => Some((None, v))
  | _ => None
  };

/* Peel Parens and push outer Asc wrappers into the tuple so labeled
 * entries surface in their normal shape. Revisit if elaboration changes
 * how it adds ascriptions to list rows. */
let rec normalize_row = (e: Exp.t): Exp.t =>
  switch (e.term) {
  | Parens(inner) => normalize_row(inner)
  | Asc(_, _) =>
    let stepped = Ascriptions.transition_multiple(e);
    stepped === e ? e : normalize_row(stepped);
  | _ => e
  };

let parse_table = (exp: Exp.t): option(table_data) =>
  switch (exp.term) {
  | ListLit(es) =>
    let data =
      List.map(
        (e: Exp.t) =>
          switch (normalize_row(e).term) {
          | Tuple(ds) =>
            OptUtil.traverse(extract_entry, ds) |> Option.map(List.split)
          | _ => None
          },
        es,
      );

    let data_opt = OptUtil.sequence(data);
    switch (data_opt) {
    | Some(data) =>
      let (headers, rows) = List.split(data);
      switch (headers) {
      | [] => None
      | [h, ..._]
          when
            List.for_all(List.equal(Option.equal(String.equal), h), headers) =>
        Some((h, rows))
      | _ => None
      };
    | None => None
    };
  | _ => None
  };
