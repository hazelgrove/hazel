open Haz3lcore;
open Language;

/* how two infos for one id differ, if they do: type ([types]),
   elaboration, class, runtime free variables, probe targets */
let sorted = xs => List.sort_uniq(compare, xs);
let runtime = co =>
  sorted(List.filter(IncrEval.is_runtime_dependency, CoCtx.names(co)));
/* probe targets as a set: their witness's shape follows how each
   analysis built it, which may differ (MerkleSet's false negatives) */
let probe_set = (t: SubexpProbeTargets.t): list(Id.t) => {
  let rec go = (s: Util.MerkleSet.t(Id.t)) =>
    switch (s.node) {
    | Empty => []
    | Leaf(x) => [x]
    | Branch(a, b) => go(a) @ go(b)
    };
  sorted(go(t.probe_ids));
};
let differ = (~types, a: Info.t, b: Info.t): option(string) =>
  switch (a, b) {
  | (InfoExp(a), InfoExp(b)) =>
    if (types && !Typ.fast_equal(a.ty, b.ty)) {
      Some(
        "type " ++ Typ.pretty_print(a.ty) ++ " / " ++ Typ.pretty_print(b.ty),
      );
    } else if (!Exp.fast_equal(a.elab_term, b.elab_term)
               || Exp.lexeme_trace(a.elab_term)
               != Exp.lexeme_trace(b.elab_term)) {
      Some("elaboration");
    } else if (a.cls != b.cls) {
      /* the inspector and ExplainThis read it */
      Some(
        "class " ++ Cls.show(a.cls) ++ " / " ++ Cls.show(b.cls),
      );
    } else if (runtime(a.co_ctx) != runtime(b.co_ctx)) {
      let names = co => String.concat(" ", runtime(co));
      Some(
        "free variables {"
        ++ names(a.co_ctx)
        ++ "} / {"
        ++ names(b.co_ctx)
        ++ "}",
      );
    } else if (probe_set(a.probe_targets) != probe_set(b.probe_targets)) {
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

let rec dedup_adjacent = (ids: list(Id.t)): list(Id.t) =>
  switch (ids) {
  | [x, y, ...rest] when Id.equal(x, y) => dedup_adjacent([y, ...rest])
  | [x, ...rest] => [x, ...dedup_adjacent(rest)]
  | [] => []
  };

/* a binding form's parts, which say what its body is */
let parts = (e: Exp.t): list(Id.t) =>
  switch (e.term) {
  | Let(p, d, b) => [Pat.rep_id(p), Exp.rep_id(d), Exp.rep_id(b)]
  | TyAlias(tp, ty, b) => [TPat.rep_id(tp), Typ.rep_id(ty), Exp.rep_id(b)]
  | ModuleExp(mp, d, b) => [MPat.rep_id(mp), Exp.rep_id(d), Exp.rep_id(b)]
  | Seq(e, b) => [Exp.rep_id(e), Exp.rep_id(b)]
  | Filter(_, b) => [Exp.rep_id(b)]
  | _ => []
  };

/* how [a]'s and [b]'s places in their programs differ, if they do: all
   their ancestors (Info.ancestors) and a binding form's parts, both
   among [own]'s ids when given. a monolithic analysis lists the id of a
   `module` binding twice above its body: adjacent repeats count once */
let place_differs =
    (~own=?, ~a_map, a: Info.t, ~b_map, b: Info.t): option(string) => {
  let mine = ids =>
    switch (own) {
    | Some(own) => List.filter(id => Hashtbl.mem(own, id), ids)
    | None => ids
    };
  let ancestors = (map, i) =>
    dedup_adjacent(mine(Info.ancestors(~info_map=map, i)));
  let parts_of = (i: Info.t) =>
    switch (i) {
    | InfoExp({user_term, _}) => mine(parts(user_term))
    | _ => []
    };
  let show = ids => "[" ++ String.concat(" ", List.map(Id.str8, ids)) ++ "]";
  let (aa, ba) = (ancestors(a_map, a), ancestors(b_map, b));
  let (ap, bp) = (parts_of(a), parts_of(b));
  if (aa != ba) {
    Some("ancestors " ++ show(aa) ++ " / " ++ show(ba));
  } else if (ap != bp) {
    Some("parts " ++ show(ap) ++ " / " ++ show(bp));
  } else {
    None;
  };
};

/* how [t] differs from a cold calc and from monolithic statics: error and
   warning ids, and per id the type, elaboration, runtime free variables,
   probe targets and place in the program; from the cold calc, also its
   items and their exports. spine roots keep their hollow item's type, so
   their types are compared only against the cold calc */
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
  /* items and members by id, each with its exports' names, in order */
  let rec shape = items =>
    List.concat_map(
      (it: DefStatics.item) =>
        [
          Id.show(it.d_id)
          ++ " exports "
          ++ String.concat(
               " ",
               List.map(DefStatics.entry_name, it.d_exports),
             ),
          ...shape(it.d_members),
        ],
      items,
    );
  Option.iter(
    (cold: DefStatics.t) => {
      let rec first_diff = (xs, ys) =>
        switch (xs, ys) {
        | ([], []) => ()
        | ([x, ...xs], [y, ...ys]) when x == y => first_diff(xs, ys)
        | ([x, ..._], _) => note("item " ++ x ++ " differs from cold")
        | ([], [y, ..._]) => note("item " ++ y ++ " missing")
        };
      first_diff(shape(t.items), shape(cold.items));
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
          Option.iter(
            d => at(d ++ " vs monolithic"),
            place_differs(~own, ~a_map=mono, m, ~b_map=t.merged, got),
          );
          switch (c) {
          | None => ()
          | Some(None) => at("no cold info")
          | Some(Some(c)) =>
            Option.iter(
              d => at(d ++ " vs cold"),
              differ(~types=true, c, got),
            );
            Option.iter(
              d => at(d ++ " vs cold"),
              place_differs(
                ~a_map=Option.get(cold).merged,
                c,
                ~b_map=t.merged,
                got,
              ),
            );
          };
        };
      },
    mono,
  );
  List.rev(out^);
};

/* how [t] differs from a cold calc of its term: the merged map by key,
   every item's map (members included) by its keys and, where the item
   is new since [prev], its infos; and the items' aggregates */
let delta_diffs = (~settings, ~prev=?, t: DefStatics.t): list(string) => {
  let analyzed = DefStatics.last_analyzed^;
  let cold = DefStatics.calc(~settings, ~probe_ids=t.probe_ids, t.term);
  DefStatics.last_analyzed := analyzed;
  let out = ref([]);
  let note = s => out := [s, ...out^];
  let cls = i => Cls.show(Info.cls_of(i));
  /* [places]: the merged map, whose roots read as in the whole program
     (an item's own map holds its hollow analysis) */
  let maps =
      (
        ~infos=true,
        ~places=false,
        what,
        got: Statics.Map.t,
        want: Statics.Map.t,
      ) => {
    let n = ref(0);
    let at = (id, s) => {
      incr(n);
      if (n^ <= 3) {
        note(what ++ ": " ++ s ++ " " ++ Id.str8(id));
      };
    };
    ignore(
      Id.Map.merge(
        (id, g, w) => {
          switch (g, w) {
          | (Some(g), Some(w)) when infos =>
            Option.iter(
              d => at(id, d ++ " at " ++ cls(w)),
              differ(~types=true, w, g),
            );
            if (places) {
              Option.iter(
                d => at(id, d ++ " at " ++ cls(w)),
                place_differs(~a_map=want, w, ~b_map=got, g),
              );
            };
          | (Some(_), Some(_)) => ()
          | (Some(g), None) => at(id, "stale " ++ cls(g))
          | (None, Some(w)) => at(id, "missing " ++ cls(w))
          | (None, None) => ()
          };
          None;
        },
        got,
        want,
      ),
    );
    if (n^ > 3) {
      note(what ++ ": " ++ string_of_int(n^) ++ " keys in all");
    };
  };
  let ids = xs => List.sort(compare, List.map(Id.str8, xs));
  let same = (what, show, a, b) =>
    if (a != b) {
      note(what ++ ": " ++ show(b) ++ " / " ++ show(a));
    };
  let names = String.concat(" ");
  maps(~places=true, "merged", t.merged, cold.merged);
  same(
    "errors",
    names,
    ids(DefStatics.all_error_ids(t)),
    ids(DefStatics.all_error_ids(cold)),
  );
  same(
    "warnings",
    names,
    ids(DefStatics.all_warning_ids(t)),
    ids(DefStatics.all_warning_ids(cold)),
  );
  /* maps the last step had, already compared then */
  let seen = Hashtbl.create(64);
  let rec remember = (items: list(DefStatics.item)) =>
    List.iter(
      (it: DefStatics.item) => {
        Hashtbl.replace(seen, Id.to_string(it.d_id), it.d_map);
        remember(it.d_members);
      },
      items,
    );
  Option.iter((p: DefStatics.t) => remember(p.items), prev);
  let fresh = (it: DefStatics.item) =>
    switch (Hashtbl.find_opt(seen, Id.to_string(it.d_id))) {
    | Some(m) => m !== it.d_map
    | None => true
    };
  let rec items = (path, got: list(DefStatics.item), want) =>
    if (List.length(got) != List.length(want)) {
      note(path ++ ": item count");
    } else {
      List.iter2(
        (g: DefStatics.item, w: DefStatics.item) => {
          let at = path ++ "/" ++ Id.str8(w.d_id);
          if (g.d_id != w.d_id) {
            note(at ++ ": id " ++ Id.str8(g.d_id));
          } else {
            maps(~infos=fresh(g), at, g.d_map, w.d_map);
            same(
              at ++ " errors",
              names,
              ids(g.d_error_ids),
              ids(w.d_error_ids),
            );
            same(
              at ++ " warnings",
              names,
              ids(g.d_warning_ids),
              ids(w.d_warning_ids),
            );
            same(at ++ " free", names, g.d_free, w.d_free);
            same(at ++ " tfree", names, g.d_tfree, w.d_tfree);
            same(
              at ++ " exports",
              names,
              List.map(DefStatics.entry_name, g.d_exports),
              List.map(DefStatics.entry_name, w.d_exports),
            );
            items(at, g.d_members, w.d_members);
          };
        },
        got,
        want,
      );
    };
  items("", t.items, cold.items);
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

/* [f] with the check installed, whether or not the runner installed it
   (a quick run doesn't): for tests whose assertion the check is */
let armed = (f: unit => unit, ()): unit => {
  let saved = DefStatics.after_calc^;
  install();
  Fun.protect(~finally=() => DefStatics.after_calc := saved, f);
};
