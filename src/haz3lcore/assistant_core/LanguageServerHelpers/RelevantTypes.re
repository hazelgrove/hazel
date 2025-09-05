open Language;

/* Assembles relevant type definitions per the Static Contextualization paper */

let format_def = (alias: string, ty: Typ.t): string =>
  Printf.sprintf("type %s = %s in", alias, ErrorPrint.Print.typ(ty));

let subst_if_rec = ((name: string, ty: Typ.t)): (string, Typ.t) => {
  switch (ty) {
  | {term: Rec(name', ty'), _} => (
      name,
      Typ.subst(Typ.fresh(Var(name)), name', ty'),
    )
  | _ => (name, ty)
  };
};

let collate_aliases = (ctx: Ctx.t, expected_ty': Typ.t): option(string) => {
  let defs =
    Typ.aliases_deep(ctx, expected_ty')
    |> Util.ListUtil.dedup
    |> List.map(subst_if_rec)
    |> List.map(((alias, ty)) => format_def(alias, ty));
  switch (defs) {
  | [] => None
  | _ => Some(defs |> String.concat("\n"))
  };
};

let get = (ctx: Ctx.t, ana_ty: Typ.t, hole_label: string): string => {
  let prefix = "# The expected type of the hole " ++ hole_label ++ " is: ";
  let defs =
    switch (collate_aliases(ctx, ana_ty)) {
    | Some(defs) =>
      "# The following type definitions are likely relevant: #\n" ++ defs
    | None => "\n"
    };
  prefix
  ++ "a type consistent with "
  ++ ErrorPrint.Print.typ(ana_ty)
  ++ " #\n"
  ++ defs;
};
