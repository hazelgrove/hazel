/* TypHash: a Merkle hash of a type, ignoring ids, remembered per type
   object. Separate copies of one type hash alike, which is what lets the
   evaluator answer is_consistent once per pair of types instead of once
   per copy (Ascriptions.consistent_by_hash). */
open Alcotest;
open Language;

let t = (term: Typ.term): Typ.t => Typ.fresh(term);
let ty_int = () => t(Atom(Int));
let ty_bool = () => t(Atom(Bool));
let arrow = (a, b) => t(Arrow(a, b));
let labeled = (l, ty) => t(TupLabel(t(Label(l)), ty));

let hash = (ty: Typ.t): TypHash.t =>
  switch (TypHash.hash(ty)) {
  | Some(h) => h
  | None => fail("no hash")
  };

let same = (name, a, b) =>
  test_case(
    name,
    `Quick,
    () => {
      check(bool, "separate objects", false, a === b);
      check(triple(int, int, int), "hash alike", hash(a), hash(b));
    },
  );

let differ = (name, a, b) =>
  test_case(name, `Quick, () =>
    check(bool, "hash apart", false, hash(a) == hash(b))
  );

/* (^report(21), ^ticket(411)) Over the corpus and at random: the hash is a
   cache key for Ascriptions.consistent_by_hash, so equal hashes must mean
   types the consistency check cannot tell apart. */
let rec hz_files = dir =>
  Sys.readdir(dir)
  |> Array.to_list
  |> List.sort(compare)
  |> List.concat_map(f => {
       let path = Filename.concat(dir, f);
       Sys.is_directory(path)
         ? hz_files(path) : Filename.check_suffix(f, ".hz") ? [path] : [];
     });
/* every type in every program's info map: what each expression and
   pattern has and is expected to have, and each type annotation */
let corpus_types =
  lazy(
    switch (
      List.find_opt(
        Sys.file_exists,
        ["hazel-programs", "../../../hazel-programs"],
      )
    ) {
    | None => []
    | Some(root) =>
      hz_files(root)
      |> List.concat_map(file => {
           let ic = open_in_bin(file);
           let text = really_input_string(ic, in_channel_length(ic));
           close_in(ic);
           switch (
             Haz3lcore.PersistentZipper.parse_text(
               ~source="t",
               ~root=Exp,
               text,
             )
           ) {
           | None => []
           | Some(z) =>
             let Haz3lcore.MakeTerm.{term, _} =
               Haz3lcore.MakeTerm.from_zip_for_sem(z, ~root=Exp);
             let (info_map, _) =
               Statics.mk(
                 CoreSettings.on,
                 Builtins.ctx_init(Some(Int)),
                 term,
               );
             Id.Map.fold(
               (_, info: Info.t, acc) =>
                 switch (info) {
                 | InfoExp(e) => [e.ty, e.ana, e.elab_syn_ty, ...acc]
                 | InfoPat(p) => [p.ty, p.ana, p.elab_syn_ty, ...acc]
                 | InfoTyp(t) => [t.user_term, ...acc]
                 | _ => acc
                 },
               info_map,
               [],
             );
           };
         })
    }
  );

/* Same hash, the same type up to ids and provenance (Typ.fast_equal). */
let census = () => {
  let groups = Hashtbl.create(4096);
  let collisions = ref([]);
  let tys = Lazy.force(corpus_types);
  tys
  |> List.iter(ty =>
       switch (TypHash.hash(ty)) {
       | None => ()
       | Some(h) =>
         switch (Hashtbl.find_opt(groups, h)) {
         | None => Hashtbl.add(groups, h, ty)
         | Some(first) =>
           if (!Typ.fast_equal(first, ty)) {
             collisions := [Typ.show(ty), ...collisions^];
           }
         }
       }
     );
  check(
    bool,
    Printf.sprintf(
      "the corpus's types hashed: %d, %d distinct",
      List.length(tys),
      Hashtbl.length(groups),
    ),
    true,
    List.length(tys) > 1000,
  );
  check(
    list(string),
    "types of one hash that are not the same type",
    [],
    collisions^,
  );
};

/* A copy with every unknown's provenance changed: new objects, the same
   hash, so a remembered answer is what it gets. */
let twin = (ty: Typ.t): Typ.t =>
  Typ.map_term(
    ~f_typ=
      (continue, t: Typ.t) =>
        switch (t.term) {
        | Unknown(_) => {
            ...t,
            term: Unknown(Internal),
          }
        | _ => continue(t)
        },
    ty,
  );
let remembered_is_direct =
  QCheck_alcotest.to_alcotest(
    QCheck.Test.make(
      ~name="the remembered consistency answer is the direct one, both ways",
      ~count=500,
      QCheck.pair(
        QCheck_Util.arb_typ(~minimal_idents=true, 5),
        QCheck_Util.arb_typ(~minimal_idents=true, 5),
      ),
      ((a, b)) => {
        let direct = (x, y) => Typ.is_consistent(Ctx.empty, x, y);
        let memo = (x, y) =>
          Ascriptions.consistent_by_hash(x, y, () => direct(x, y));
        List.for_all(
          ((x, y)) => memo(x, y) == direct(x, y),
          [(a, b), (twin(a), twin(b)), (b, a), (twin(b), twin(a))],
        );
      },
    ),
  );

let tests = (
  "TypHash",
  [
    test_case("the corpus: one hash, one type", `Slow, census),
    remembered_is_direct,
    same(
      "two copies of a type",
      arrow(ty_int(), ty_bool()),
      arrow(ty_int(), ty_bool()),
    ),
    same(
      "a copy with every id replaced",
      t(Prod([labeled("x", ty_int()), labeled("y", ty_bool())])),
      t(Prod([labeled("x", ty_int()), labeled("y", ty_bool())])),
    ),
    same(
      "an unknown's provenance does not count",
      t(Unknown(Internal)),
      t(Unknown(SynSwitch)),
    ),
    differ("Int and Bool", ty_int(), ty_bool()),
    differ(
      "arguments swapped",
      arrow(ty_int(), ty_bool()),
      arrow(ty_bool(), ty_int()),
    ),
    differ(
      "different labels",
      labeled("x", ty_int()),
      labeled("y", ty_int()),
    ),
    differ("a list and its element", t(List(ty_int())), ty_int()),
    differ(
      "one more field",
      t(Prod([ty_int(), ty_bool()])),
      t(Prod([ty_int(), ty_bool(), ty_int()])),
    ),
    test_case(
      "the same object, asked twice",
      `Quick,
      () => {
        let ty = arrow(ty_int(), t(List(ty_bool())));
        check(triple(int, int, int), "remembered", hash(ty), hash(ty));
      },
    ),
    test_case(
      "parts that differ only in the same bit do not cancel", `Quick, ()
      /* With a multiply and no finalizer, a difference in a part only
         moved toward the high bits, so two of three parts flipped in
         bit 28 or 29 hashed alike. Every bit, and every two of three
         parts. */
      =>
        for (k in 0 to 29) {
          let f = 1 lxor 1 lsl k;
          let h = parts =>
            TypHash.combine(8, List.map(TypHash.of_int, parts));
          let name = what => Printf.sprintf("bit %d, parts %s", k, what);
          check(bool, name("1, 2"), false, h([1, 1, 1]) == h([f, f, 1]));
          check(bool, name("1, 3"), false, h([1, 1, 1]) == h([f, 1, f]));
          check(bool, name("2, 3"), false, h([1, 1, 1]) == h([1, f, f]));
        }
      ),
    test_case("a type holding an expression gets no hash", `Quick, () =>
      check(
        bool,
        "None, so callers compute",
        true,
        TypHash.hash(t(ProofOf(Exp.fresh(Atom(Bool(true)))))) == None,
      )
    ),
  ],
);
