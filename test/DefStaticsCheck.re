open Haz3lcore;
open Language;

/* how [t] differs from a cold calc and from monolithic statics: error and
   warning ids, and per id the type, elaboration, runtime free variables
   and probe targets. spine roots keep their hollow item's type, so their
   types are compared only against the cold calc */
let divergences = (~settings, ~cold=true, t: DefStatics.t): list(string) => {
  let analyzed = DefStatics.last_analyzed^;
  let cold =
    cold
      ? Some(DefStatics.calc(~settings, ~probe_ids=t.probe_ids, t.term))
      : None;
  DefStatics.last_analyzed := analyzed;
  let (mono, _) =
    Statics.mk_unmemoized(
      ~probe_ids=t.probe_ids,
      settings,
      DefStatics.ctx0,
      t.term,
    );
  /* monolithic lowering mints ids of its own each run, and a Mod
     root's wrapper id has no tile */
  let own = Hashtbl.create(1024);
  let grab = (cont, x) => {
    List.iter(id => Hashtbl.replace(own, id, ()), IdTagged.ids(x));
    cont(x);
  };
  ignore(
    Exp.map_term(
      ~f_exp=grab,
      ~f_pat=grab,
      ~f_typ=grab,
      ~f_tpat=grab,
      ~f_rul=grab,
      ~f_mod=grab,
      ~f_sig=grab,
      ~f_mpat=grab,
      t.term,
    ),
  );
  switch (t.term.term) {
  | Module(_) => Hashtbl.remove(own, Exp.rep_id(t.term))
  | _ => ()
  };
  let rec roots = items =>
    List.concat_map(
      (it: DefStatics.item) =>
        Option.to_list(Option.map(_ => it.d_id, it.d_hole))
        @ roots(it.d_members),
      items,
    );
  let spine = Hashtbl.create(64);
  List.iter(id => Hashtbl.replace(spine, id, ()), roots(t.items));
  let sorted = xs => List.sort_uniq(compare, xs);
  let runtime = co =>
    sorted(List.filter(IncrEval.is_runtime_dependency, CoCtx.names(co)));
  let differ = (~types, a: Info.t, b: Info.t): option(string) =>
    switch (a, b) {
    | (InfoExp(a), InfoExp(b)) =>
      if (types && !Typ.fast_equal(a.ty, b.ty)) {
        Some(
          "type "
          ++ Typ.pretty_print(a.ty)
          ++ " / "
          ++ Typ.pretty_print(b.ty),
        );
      } else if (!Exp.fast_equal(a.elab_term, b.elab_term)
                 || Exp.lexeme_trace(a.elab_term)
                 != Exp.lexeme_trace(b.elab_term)) {
        Some("elaboration");
      } else if (runtime(a.co_ctx) != runtime(b.co_ctx)) {
        let names = co => String.concat(" ", runtime(co));
        Some(
          "free variables {"
          ++ names(a.co_ctx)
          ++ "} / {"
          ++ names(b.co_ctx)
          ++ "}",
        );
      } else if (!SubexpProbeTargets.equal(a.probe_targets, b.probe_targets)) {
        Some("probe targets");
      } else {
        None;
      }
    | (InfoPat(a), InfoPat(b)) =>
      !types || Typ.fast_equal(a.ty, b.ty)
        ? None
        : Some(
            "pattern type "
            ++ Typ.pretty_print(a.ty)
            ++ " / "
            ++ Typ.pretty_print(b.ty),
          )
    | _ => Info.sort_of(a) == Info.sort_of(b) ? None : Some("sort")
    };
  let out = ref([]);
  let note = s => out := [s, ...out^];
  let ids = (what, reference, got) => {
    /* statics mints ids for terms it builds (labels filled into a tuple
       against a labeled type), new each run: those count, not match */
    let mine = List.filter(id => Hashtbl.mem(own, id));
    let minted = xs =>
      List.length(sorted(xs)) - List.length(sorted(mine(xs)));
    if (minted(reference) != minted(got)) {
      note(
        what
        ++ ": "
        ++ string_of_int(minted(reference))
        ++ " on minted ids, "
        ++ string_of_int(minted(got))
        ++ " here",
      );
    };
    let (r, g) = (sorted(mine(reference)), sorted(mine(got)));
    let only = (xs, ys) =>
      List.filter(x => !List.mem(x, ys), xs)
      |> List.map(id =>
           switch (Id.Map.find_opt(id, mono)) {
           | Some(i) => Cls.show(Info.cls_of(i))
           | None => Id.show(id)
           }
         )
      |> String.concat(", ");
    if (r != g) {
      note(
        what
        ++ ": missing ["
        ++ only(r, g)
        ++ "] extra ["
        ++ only(g, r)
        ++ "]",
      );
    };
  };
  Option.iter(
    cold => {
      ids(
        "error ids vs cold",
        DefStatics.all_error_ids(cold),
        DefStatics.all_error_ids(t),
      );
      ids(
        "warning ids vs cold",
        DefStatics.all_warning_ids(cold),
        DefStatics.all_warning_ids(t),
      );
    },
    cold,
  );
  ids(
    "error ids vs monolithic",
    Statics.Map.error_ids(mono),
    DefStatics.all_error_ids(t),
  );
  ids(
    "warning ids vs monolithic",
    Statics.Map.warning_ids(mono),
    DefStatics.all_warning_ids(t),
  );
  Id.Map.iter(
    (id, m) =>
      if (Hashtbl.mem(own, id)) {
        let at = what =>
          note(
            what ++ " at " ++ Cls.show(Info.cls_of(m)) ++ " " ++ Id.show(id),
          );
        switch (
          Id.Map.find_opt(id, t.merged),
          Option.map(
            (c: DefStatics.t) => Id.Map.find_opt(id, c.merged),
            cold,
          ),
        ) {
        | (None, _) => at("no info")
        | (Some(got), c) =>
          Option.iter(
            d => at(d ++ " vs monolithic"),
            differ(~types=!Hashtbl.mem(spine, id), m, got),
          );
          switch (c) {
          | None => ()
          | Some(None) => at("no cold info")
          | Some(Some(c)) =>
            Option.iter(
              d => at(d ++ " vs cold"),
              differ(~types=true, c, got),
            )
          };
        };
      },
    mono,
  );
  List.rev(out^);
};

/* installed by the test runner: calc_auto results of up to [cap] infos
   are checked against monolithic statics, raising on divergence */
let cap = 3000;

exception Divergence(list(string));

let install = () =>
  DefStatics.after_calc :=
    Some(
      (settings, t: DefStatics.t) =>
        if (Id.Map.cardinal(t.merged) <= cap) {
          switch (divergences(~settings, ~cold=false, t)) {
          | [] => ()
          | ds => raise(Divergence(ds))
          };
        },
    );
