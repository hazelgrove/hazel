/* Incremental evaluation of the Livelit Playground slides against fresh
   evaluation, after the edit a slider makes: one int literal, a livelit's
   model, changed in place with every id kept (as the projector's commit
   keeps them). The slides found an incremental run that came back stuck
   where a fresh one completes. */
open Alcotest;
open Language;

let playground = "hazel-programs/docs/livelits/playground/";

let parse_seg_of = (src: string): Haz3lcore.Segment.t =>
  switch (
    Haz3lcore.FastParse.of_text(
      ~materialize=Haz3lcore.Triggers.invoked_projector,
      ~collect_refractors=true,
      ~root=Exp,
      src,
    )
  ) {
  | Some(seg) => seg
  | None => failwith("IncrSlides: parse failed")
  };

let elab_of = (src: string) => {
  let seg = parse_seg_of(src);
  let term = Haz3lcore.MakeTerm.go(seg).term;
  let (map, elab) =
    Statics.mk(
      CoreSettings.on,
      Builtins.ctx_init(Some(Operators.default_mode)),
      term,
    );
  (term, map, elab);
};

/* every Atom(Int n) with n = [value], by its rep id */
let int_lits = (value: int, exp: Exp.t): list(Id.t) => {
  let acc = ref([]);
  let f_exp = (continue, e: Exp.t): Exp.t => {
    switch (e.term) {
    | Atom(Int(n)) when Bigint.equal(n, Bigint.of_int(value)) =>
      acc := [Exp.rep_id(e), ...acc^]
    | _ => ()
    };
    continue(e);
  };
  let _ = TermBase.Exp.map_term(~f_exp, exp);
  List.rev(acc^);
};

let replace_int = (~target: Id.t, ~to_: int, exp: Exp.t): Exp.t =>
  TermBase.Exp.map_term(
    ~f_exp=
      (continue, e: Exp.t) =>
        if (Id.equal(Exp.rep_id(e), target)) {
          switch (e.term) {
          | Atom(Int(_)) => {
              ...e,
              term: Atom(Int(Bigint.of_int(to_))),
            }
          | _ => continue(e)
          };
        } else {
          continue(e);
        },
    exp,
  );

let eval = (~prev=IncrEval.empty, map, elab) => {
  let ei =
    EvalInfo.of_info_map(~probe_all=false, ~targets=Sample.no_targets, map);
  Evaluator.evaluate(~prev, ~eval_info=ei, ~env=Builtins.env_init, elab);
};

/* a result's outermost form: a stuck run shows a Let or Match where
   a completed one shows its value (printing either whole can exceed a
   JS string) */
let summary = (e: Exp.t): string =>
  switch (e.term) {
  | Let(_) => "Let (stuck)"
  | Match(_) => "Match (stuck)"
  | Ap(_) => "Ap (stuck)"
  | Tuple(es) => Printf.sprintf("Tuple of %d", List.length(es))
  | _ => "other"
  };

/* [slide]'s int literal [from] (the [nth] one) set to [to_]: the
   incremental result is the fresh one */
let case = (~slide, ~from, ~to_, ~nth=0, ()) => {
  let path = playground ++ slide ++ ".hz";
  let path = Sys.file_exists(path) ? path : "../" ++ path;
  let src = In_channel.with_open_bin(path, In_channel.input_all);
  let (term, map, elab) = elab_of(src);
  let (_, state) = eval(map, elab);
  switch (List.nth_opt(int_lits(from, term), nth)) {
  | None => fail(Printf.sprintf("%s: no int literal %d", slide, from))
  | Some(target) =>
    let edited = replace_int(~target, ~to_, term);
    let (map2, elab2) =
      Statics.mk(
        CoreSettings.on,
        Builtins.ctx_init(Some(Operators.default_mode)),
        edited,
      );
    let (fresh, _) = eval(map2, elab2);
    let (incr, _) = eval(~prev=state.incr_eval, map2, elab2);
    let same = Equality.semantic.exp(fresh, incr);
    if (!same) {
      Printf.printf(
        "%s %d -> %d\n  fresh: %s\n  incr:  %s\n",
        slide,
        from,
        to_,
        summary(fresh),
        summary(incr),
      );
    };
    check(bool, slide ++ ": incremental agrees with fresh", true, same);
  };
};

/* The marks a slide's statics leave, as the editor would print them:
   every playground slide has none. */
let no_errors = (slide, ()) => {
  let path = playground ++ slide ++ ".hz";
  let path = Sys.file_exists(path) ? path : "../" ++ path;
  let (_, map, _) =
    elab_of(In_channel.with_open_bin(path, In_channel.input_all));
  let marks =
    Id.Map.fold(
      (_, info, acc) =>
        switch (Info.marks_of(info)) {
        | [] => acc
        | marks => [
            Haz3lcore.ErrorPrint.string_of_marks(~info_map=map, info, marks),
            ...acc,
          ]
        },
      map,
      [],
    )
    |> List.sort_uniq(String.compare);
  check(list(string), slide ++ ": no static errors", [], marks);
};

let tests = (
  "IncrSlides",
  List.map(
    slide =>
      test_case(slide ++ ": no static errors", `Quick, no_errors(slide)),
    [
      "hair",
      "shoes",
      "arms",
      "hands",
      "eyes",
      "mouth",
      "full-body",
      "witch",
      "scenes",
    ],
  )
  @ [
    /* each slide's first ^choice is an inline use with a type
       argument, Drawing.^choice@<T>, whose type application's result
       is an open function: reused as a value, it stuck the run */
    test_case(
      "Arms: arm length",
      `Quick,
      case(~slide="arms", ~from=95, ~to_=150),
    ),
    test_case(
      "Hands: hand size",
      `Quick,
      case(~slide="hands", ~from=14, ~to_=30),
    ),
    test_case(
      "Shoes: shoe size",
      `Quick,
      case(~slide="shoes", ~from=100, ~to_=200),
    ),
    test_case(
      "Eyes: eye size",
      `Quick,
      case(~slide="eyes", ~from=50, ~to_=100),
    ),
  ],
);
