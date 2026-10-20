/* ^ticket(469), a probe, not for merging: what statics says about each
   way of naming the HTML type, as the cursor inspector would show it. */
open Alcotest;
open Haz3lcore;
open Language;

let report = (src: string): string =>
  switch (
    FastParse.of_text(
      ~materialize=Triggers.invoked_projector,
      ~collect_refractors=true,
      ~root=Exp,
      src,
    )
  ) {
  | None => src ++ " => did not parse"
  | Some(seg) =>
    let (map, _) =
      Statics.mk(
        CoreSettings.on,
        Builtins.ctx_init(Some(Operators.default_mode)),
        MakeTerm.go(seg).term,
      );
    let cut = (n, s) =>
      String.length(s) > n ? String.sub(s, 0, n) ++ "..." : s;
    let lines =
      Id.Map.fold(
        (_, info: Info.t, acc) =>
          switch (info) {
          | InfoTyp({user_term, marks, message, expects, _}) =>
            let what =
              switch (user_term.term) {
              | Var(n) => "Var " ++ n
              | ProdProjection(_, {term: Label(l), _}) => "Projection ." ++ l
              | Label(l) => "Label " ++ l
              | _ => ""
              };
            what == ""
              ? acc
              : [
                Printf.sprintf(
                  "  %s [expects %s] marks=[%s] message=%s",
                  what,
                  cut(60, TypExpectation.show(expects)),
                  String.concat("; ", List.map(Mark.show, marks)),
                  cut(
                    160,
                    Option.fold(~none="none", ~some=Message.show, message),
                  ),
                ),
                ...acc,
              ];
          | InfoTPat({user_term, marks, message, _}) => [
              Printf.sprintf(
                "  TPat %s marks=[%s] message=%s",
                cut(40, TPat.show(user_term)),
                String.concat("; ", List.map(Mark.show, marks)),
                cut(
                  200,
                  Option.fold(~none="none", ~some=Message.show, message),
                ),
              ),
              ...acc,
            ]
          | _ => acc
          },
        map,
        [],
      );
    src ++ "\n" ++ String.concat("\n", List.sort_uniq(compare, lines));
  };

let tests = (
  "Diag469",
  [
    test_case(
      "what each spelling of the HTML type gets",
      `Quick,
      () => {
        let all =
          [
            "type X = Html in 1",
            "type X = HTML in 1",
            "type X = Html.T in 1",
            "type X = Html.App in 1",
          ]
          |> List.map(report)
          |> String.concat("\n\n");
        check(string, "the report (a probe: it always fails)", "", all);
      },
    ),
  ],
);
