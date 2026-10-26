/* Γ shows a type whole only when it is small enough to print and lay out
   (^ticket(396)): on the Scenes slide one entry, a module of livelit
   definitions, printed to 474,304 characters and froze the page. */
open Alcotest;
open Haz3lcore;
open Language;
module CI = Web.ContextInspector;

let slide_ctx = (file: string): Ctx.t => {
  let path = "../../../hazel-programs/docs/livelits/playground/" ++ file;
  let ic = open_in_bin(path);
  let src = really_input_string(ic, in_channel_length(ic));
  close_in(ic);
  switch (
    FastParse.of_text(
      ~materialize=Triggers.invoked_projector,
      ~collect_refractors=true,
      ~root=Exp,
      src,
    )
  ) {
  | None => fail("could not parse " ++ file)
  | Some(seg) =>
    let (map, _) =
      Statics.mk(
        CoreSettings.on,
        Builtins.ctx_init(Some(Operators.default_mode)),
        MakeTerm.go(seg).term,
      );
    /* the program's innermost context: the one with the most entries */
    Id.Map.fold(
      (_, info, best: Ctx.t) => {
        let c = Info.ctx_of(info);
        List.length(c.entries) > List.length(best.entries) ? c : best;
      },
      map,
      Ctx.empty,
    );
  };
};

let typ_of = (ctx: Ctx.t, name: string): Typ.t =>
  switch (
    List.find_map(
      (e: Ctx.entry) =>
        switch (e) {
        | VarEntry({name: n, typ, _})
        | ConstructorEntry({name: n, typ, _}) when n == name => Some(typ)
        | _ => None
        },
      ctx.entries,
    )
  ) {
  | Some(t) => t
  | None => fail(name ++ " is not in the context")
  };

let contains = (hay: string, needle: string): bool => {
  let n = String.length(needle);
  let rec go = i =>
    i
    + n <= String.length(hay)
    && (String.sub(hay, i, n) == needle || go(i + 1));
  go(0);
};

let name_of = (e: Ctx.entry): string =>
  switch (e) {
  | VarEntry({name, _})
  | ConstructorEntry({name, _})
  | TVarEntry({name, _}) => name
  | LivelitEntry({name, _}) => "^" ++ name
  };

let typ_of_entry = (e: Ctx.entry): option(Typ.t) =>
  switch (e) {
  | VarEntry({typ, _})
  | ConstructorEntry({typ, _}) => Some(typ)
  | LivelitEntry({expansion_t, _}) => Some(expansion_t)
  | TVarEntry(_) => None
  };

let tests = (
  "ContextInspector",
  [
    test_case(
      "Scenes: the Drawing module is outlined, not printed",
      `Slow,
      () => {
        let ctx = slide_ctx("scenes.hz");
        let drawing = typ_of(ctx, "Drawing");
        check(
          bool,
          "Drawing is over the budget",
          false,
          CI.typ_within(CI.typ_budget, drawing),
        );
        check(
          bool,
          "Html.T fits",
          true,
          CI.typ_within(CI.typ_budget, typ_of(ctx, "Html")),
        );
        check(
          bool,
          "the outline names ^polygons",
          true,
          contains(CI.outline_text(drawing), "let ^polygons"),
        );
      },
    ),
    test_case(
      "Polygons: every entry fits and prints whole",
      `Slow,
      () => {
        let ctx = slide_ctx("polygons.hz");
        List.iter(
          (e: Ctx.entry) =>
            switch (typ_of_entry(e)) {
            | Some(t) =>
              check(
                bool,
                name_of(e) ++ " fits",
                true,
                CI.typ_within(CI.typ_budget, t),
              )
            | None => ()
            },
          ctx.entries,
        );
      },
    ),
  ],
);
