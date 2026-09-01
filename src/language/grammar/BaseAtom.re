/* The bidirectional association between base-type TOKEN TEXT and Typ
 * term constructors: the base atoms ("Bool" etc.), "Void" (the empty
 * sum), and the Drv quote types (token text from DrvSort.to_string).
 * The single home of these strings: Token.base_typs is derived from
 * this table. Tokens are typed as plain strings here (Token.t ==
 * string; referencing Token would be circular), and the table is
 * built over TermBase rather than Typ to stay out of the statics
 * dependency closure. */

/* Token text of each atomic type class; exhaustive, so adding an
 * Atom.cls forces a choice of token here. */
let atom_token: Atom.cls => string =
  fun
  | Bool => "Bool"
  | Int => "Int"
  | SInt => "SInt"
  | Nat => "Nat"
  | Float => "Float"
  | String => "String";

/* Token text of the empty sum type */
let void_token = "Void";

/* token -> term rows, in Token.base_typs order: atoms sorted
 * alphabetically, then Void, then Drv quote types in DrvSort.all
 * order. */
let table: list((string, TermBase.Typ.term)) =
  (
    Atom.all_of_cls
    |> List.map((a: Atom.cls) =>
         (atom_token(a), Atom(a): TermBase.Typ.term)
       )
    |> List.sort(((s, _), (s', _)) => String.compare(s, s'))
  )
  @ [(void_token, Sum([]): TermBase.Typ.term)]
  @ (
    DrvSort.all
    |> List.map((s: DrvSort.t) =>
         (DrvSort.to_string(s), DrvQuoteTy(s): TermBase.Typ.term)
       )
  );

let base_typs: list(string) = List.map(fst, table);

let typ_term_of = (t: string): option(TermBase.Typ.term) =>
  List.assoc_opt(t, table);

let token_of_typ = (tm: TermBase.Typ.term): option(string) =>
  switch (tm) {
  | Atom(a) => Some(atom_token(a))
  | Sum([]) => Some(void_token)
  | DrvQuoteTy(s) => Some(DrvSort.to_string(s))
  | _ => None
  };
