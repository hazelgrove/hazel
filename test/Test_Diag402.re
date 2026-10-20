/* ^ticket(402), a probe, not for merging: what Dress Up's game logic would
   cost in Hazel. A lever pulled (an event) flips every trapdoor a table of
   lever-to-trapdoor links names. Timed: statics and elaboration once, then
   evaluating `run(init, pulls)` for 1 and 100 pulls, for tables of 10, 100
   and 1000 links. Always fails; the failure is the report. */
open Alcotest;
open Haz3lcore;
open Language;

let program = (~links: int, ~doors: int, ~pulls: int): string => {
  let link = i => Printf.sprintf("(%d, %d)", i mod (doors * 2), i mod doors);
  let pull = i => Printf.sprintf("Pull(%d)", i * 7 mod (doors * 2));
  let list = (n, f) => "[" ++ String.concat(", ", List.init(n, f)) ++ "]";
  String.concat(
    "\n",
    [
      "type Model = [Bool] in",
      "type Action = + Pull(Int) in",
      "let links : [(Int, Int)] = " ++ list(links, link) ++ " in",
      "let flip_at : ([Bool], Int) -> [Bool] = fun (ds, i) -> case ds | [] => [] | d :: tl => if i == 0 then (if d then false else true) :: tl else d :: flip_at(tl, i - 1) end in",
      "let doors_of : ([(Int, Int)], Int) -> [Int] = fun (ls, l) -> case ls | [] => [] | (l2, d) :: tl => if l2 == l then d :: doors_of(tl, l) else doors_of(tl, l) end in",
      "let flip_all : (Model, [Int]) -> Model = fun (m, ds) -> case ds | [] => m | d :: tl => flip_all(flip_at(m, d), tl) end in",
      "let update : (Model, Action) -> Model = fun (m, a) -> case a | Pull(l) => flip_all(m, doors_of(links, l)) end in",
      "let run : (Model, [Action]) -> Model = fun (m, as) -> case as | [] => m | a :: tl => run(update(m, a), tl) end in",
      "let init : Model = " ++ list(doors, _ => "false") ++ " in",
      "run(init, " ++ list(pulls, pull) ++ ")",
    ],
  );
};

/* The same rule, monadic (Matthew: "we need global variables for simple
   games --- most scripts use them. So we need to be monadic."): the rule
   returns commands as data, Flip(door, then) ending in Done, which a game
   would interpret against its globals. [interp] says whether Hazel also
   interprets them (true) or only builds them (false: the game would). */
let monadic = (~links: int, ~doors: int, ~pulls: int, ~interp: bool): string => {
  let link = i => Printf.sprintf("(%d, %d)", i mod (doors * 2), i mod doors);
  let pull = i => Printf.sprintf("Pull(%d)", i * 7 mod (doors * 2));
  let list = (n, f) => "[" ++ String.concat(", ", List.init(n, f)) ++ "]";
  String.concat(
    "\n",
    [
      "type Cmd = + Done + Flip(Int, Cmd) in",
      "type Action = + Pull(Int) in",
      "let links : [(Int, Int)] = " ++ list(links, link) ++ " in",
      "let flip_at : ([Bool], Int) -> [Bool] = fun (ds, i) -> case ds | [] => [] | d :: tl => if i == 0 then (if d then false else true) :: tl else d :: flip_at(tl, i - 1) end in",
      "let doors_of : ([(Int, Int)], Int) -> [Int] = fun (ls, l) -> case ls | [] => [] | (l2, d) :: tl => if l2 == l then d :: doors_of(tl, l) else doors_of(tl, l) end in",
      "let flips : [Int] -> Cmd = fun ds -> case ds | [] => Done | d :: tl => Flip(d, flips(tl)) end in",
      "let rule : Action -> Cmd = fun a -> case a | Pull(l) => flips(doors_of(links, l)) end in",
      "let interp : ([Bool], Cmd) -> [Bool] = fun (g, c) -> case c | Done => g | Flip(d, k) => interp(flip_at(g, d), k) end in",
      "let count : Cmd -> Int = fun c -> case c | Done => 0 | Flip(_, k) => 1 + count(k) end in",
      interp
        ? "let run : ([Bool], [Action]) -> [Bool] = fun (g, as) -> case as | [] => g | a :: tl => run(interp(g, rule(a)), tl) end in"
        : "let run : (Int, [Action]) -> Int = fun (n, as) -> case as | [] => n | a :: tl => run(n + count(rule(a)), tl) end in",
      "let init : [Bool] = " ++ list(doors, _ => "false") ++ " in",
      interp
        ? "run(init, " ++ list(pulls, pull) ++ ")"
        : "run(0, " ++ list(pulls, pull) ++ ")",
    ],
  );
};

let ms = span => Core.Time_ns.Span.to_ms(span);

let measure = (~label="pure", ~text=?, ~links, ~doors, ~pulls, ()) => {
  let text =
    switch (text) {
    | Some(t) => t
    | None => program(~links, ~doors, ~pulls)
    };
  switch (
    FastParse.of_text(
      ~materialize=Triggers.invoked_projector,
      ~collect_refractors=true,
      ~root=Exp,
      text,
    )
  ) {
  | None =>
    Printf.sprintf("%s links %d pulls %d: no parse", label, links, pulls)
  | Some(seg) =>
    let term = MakeTerm.go(seg).term;
    let (st, (map, elab)) =
      Util.TimeUtil.timed(() =>
        Statics.mk(
          CoreSettings.on,
          Builtins.ctx_init(Some(Operators.default_mode)),
          term,
        )
      );
    let errs = ErrorPrint.all(map);
    let (ev, (result, _)) =
      Util.TimeUtil.timed(() =>
        Evaluator.evaluate(~env=Builtins.env_init, elab)
      );
    let shown = DHExp.show(result);
    Printf.sprintf(
      "%-16s links %4d doors %3d pulls %3d: statics+elab %8.1f ms, evaluate %8.1f ms, %d errors%s; result %s",
      label,
      links,
      doors,
      pulls,
      ms(st),
      ms(ev),
      List.length(errs),
      errs == []
        ? ""
        : " ("
          ++ String.sub(
               List.hd(errs),
               0,
               min(200, String.length(List.hd(errs))),
             )
          ++ ")",
      String.sub(shown, 0, min(80, String.length(shown))),
    );
  };
};

let tests = (
  "Diag402",
  [
    test_case(
      "levers and trapdoors, timed",
      `Quick,
      () => {
        let rows =
          List.concat_map(
            ((links, doors)) =>
              List.concat_map(
                pulls =>
                  [
                    measure(~links, ~doors, ~pulls, ()),
                    measure(
                      ~label="monadic, built",
                      ~text=monadic(~links, ~doors, ~pulls, ~interp=false),
                      ~links,
                      ~doors,
                      ~pulls,
                      (),
                    ),
                    measure(
                      ~label="monadic, run",
                      ~text=monadic(~links, ~doors, ~pulls, ~interp=true),
                      ~links,
                      ~doors,
                      ~pulls,
                      (),
                    ),
                  ],
                [1, 100],
              ),
            [(10, 5), (100, 20), (1000, 50)],
          );
        check(
          string,
          "the report (a probe: it always fails)",
          "",
          String.concat("\n", rows),
        );
      },
    ),
  ],
);
