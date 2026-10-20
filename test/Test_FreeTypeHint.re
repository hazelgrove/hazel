/* ^ticket(469): on the livelits slides, `type X = Html in` (the way to ask
   the full view what Html is) said only "Html not found". Html is a
   module; its type is Html.T, where the old global alias HTML lives now.
   The inspector says so, and the full view of `type X = Html.T in` shows
   what Html.T is. */
open Alcotest;
open Haz3lcore;
open Language;
module CI = Web.CursorInspector;

/* The context a type in [src] is checked in, and the type infos. */
let typ_infos = (src: string): list(Info.typ) =>
  switch (
    FastParse.of_text(
      ~materialize=Triggers.invoked_projector,
      ~collect_refractors=true,
      ~root=Exp,
      src,
    )
  ) {
  | None => fail("could not parse " ++ src)
  | Some(seg) =>
    let (map, _) =
      Statics.mk(
        CoreSettings.on,
        Builtins.ctx_init(Some(Operators.default_mode)),
        MakeTerm.go(seg).term,
      );
    Id.Map.fold(
      (_, info: Info.t, acc) =>
        switch (info) {
        | InfoTyp(t) => [t, ...acc]
        | _ => acc
        },
      map,
      [],
    );
  };

let var_info = (src, name) =>
  switch (
    List.find_opt(
      (t: Info.typ) => t.user_term.term == Var(name),
      typ_infos(src),
    )
  ) {
  | Some(t) => t
  | None => fail("no type " ++ name ++ " in " ++ src)
  };

let hint =
  testable(
    Fmt.of_to_string(
      fun
      | Some(CI.ModuleTypes(ts)) =>
        "a module, its types " ++ String.concat(", ", ts)
      | Some(CI.MovedTo(p)) => "moved to " ++ p
      | None => "no hint",
    ),
    (==),
  );

let tests = (
  "FreeTypeHint",
  [
    test_case(
      "Html as a type: a module, whose types are named",
      `Quick,
      () => {
        let t = var_info("type X = Html in 1", "Html");
        check(
          bool,
          "Html is not found as a type",
          true,
          List.mem(Mark.TypFreeTypeVariable("Html"), t.marks),
        );
        check(
          hint,
          "the hint",
          Some(CI.ModuleTypes(["Html.T", "Html.App"])),
          CI.free_type_hint(t.ctx, "Html"),
        );
      },
    ),
    test_case(
      "HTML, the old global alias: it is Html.T now",
      `Quick,
      () => {
        let t = var_info("type X = HTML in 1", "HTML");
        check(
          hint,
          "the hint",
          Some(CI.MovedTo("Html.T")),
          CI.free_type_hint(t.ctx, "HTML"),
        );
      },
    ),
    test_case(
      "a name that is nothing: no hint",
      `Quick,
      () => {
        let t = var_info("type X = Nope in 1", "Nope");
        check(hint, "the hint", None, CI.free_type_hint(t.ctx, "Nope"));
      },
    ),
    test_case(
      "Html.T has a definition to show",
      `Quick,
      () => {
        let t = var_info("type X = Html.T in 1", "Html");
        let path =
          Typ.temp(
            ProdProjection(Typ.temp(Var("Html")), Typ.temp(Label("T"))),
          );
        check(
          bool,
          "Html.T stands for more than its name",
          false,
          Typ.fast_equal(Typ.weak_head_normalize(t.ctx, path), path),
        );
      },
    ),
  ],
);
