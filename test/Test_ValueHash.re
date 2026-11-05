/* Util.ValueHash: a Merkle hash of an immutable value by its structure,
   ids included, remembered per object, and answers remembered per pair.
   Equality's ~hash_shortcut uses both, to recognize identical subterms
   without walking them and to answer a repeated comparison from memory,
   which made the eval worker's reuse check cheap (IncrEval.reuse_check).
   So the hash must never call two different values the same, and the
   hashed comparison must agree with the plain one, asked once or again. */
open Alcotest;
open Haz3lcore;
open Language;

let elab = text =>
  switch (PersistentZipper.parse_text(~source="t", ~root=Exp, text)) {
  | None => fail("did not parse")
  | Some(z) =>
    let MakeTerm.{term, _} = MakeTerm.from_zip_for_sem(z, ~root=Exp);
    snd(Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), term));
  };

/* A deep copy, as unmarshaling a request makes one: new objects, same
   structure, same ids. */
let copy = (e: Exp.t): Exp.t =>
  Marshal.from_string(Marshal.to_string(e, []), 0);

let program = "let f = fun x -> x + 1 in\nlet g = fun (y : Int) -> f(y) * 2 in\n(g(3), [f(1), f(2)], \"text\")";
let edited = "let f = fun x -> x + 1 in\nlet g = fun (y : Int) -> f(y) * 3 in\n(g(3), [f(1), f(2)], \"text\")";

/* (^report(21), ^ticket(411)) Over the corpus. Every program in
   hazel-programs elaborated, and every subterm of it with a copy (new
   objects, same ids), a copy under new ids, and a copy with its first
   integer literal changed. */
let rec hz_files = dir =>
  Sys.readdir(dir)
  |> Array.to_list
  |> List.sort(compare)
  |> List.concat_map(f => {
       let path = Filename.concat(dir, f);
       Sys.is_directory(path)
         ? hz_files(path) : Filename.check_suffix(f, ".hz") ? [path] : [];
     });
let corpus =
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
      |> List.filter_map(file => {
           let ic = open_in_bin(file);
           let text = really_input_string(ic, in_channel_length(ic));
           close_in(ic);
           switch (PersistentZipper.parse_text(~source="t", ~root=Exp, text)) {
           | None => None
           | Some(z) =>
             let MakeTerm.{term, _} =
               MakeTerm.from_zip_for_sem(z, ~root=Exp);
             Some((
               Filename.basename(file),
               snd(
                 Statics.mk(
                   CoreSettings.on,
                   Builtins.ctx_init(Some(Int)),
                   term,
                 ),
               ),
             ));
           };
         })
    }
  );
let subterms = (e: Exp.t): list(Exp.t) => {
  let acc = ref([]);
  let _ =
    Exp.map_term(
      ~f_exp=
        (continue, e) => {
          acc := [e, ...acc^];
          continue(e);
        },
      e,
    );
  List.rev(acc^);
};
/* the first integer literal, plus one: an edit that leaves the rest */
let bump_first_int = (e: Exp.t): Exp.t => {
  let done_ = ref(false);
  Exp.map_term(
    ~f_exp=
      (continue, e) =>
        switch (e.term) {
        | Atom(Int(n)) when ! done_^ =>
          done_ := true;
          {
            ...e,
            term: Atom(Int(Bigint.succ(n))),
          };
        | _ => continue(e)
        },
    e,
  );
};

/* No two different subterms share a hash: grouped by hash, every member of
   a group is the same value as the first, ids and all. */
let census = () => {
  let groups = Hashtbl.create(4096);
  let hashed = ref(0);
  let collisions = ref([]);
  Lazy.force(corpus)
  |> List.iter(((name, e)) =>
       [e, copy(e)]
       |> List.concat_map(subterms)
       |> List.iter(s =>
            switch (Util.ValueHash.hash_of(s)) {
            | None => ()
            | Some(h) =>
              incr(hashed);
              switch (Hashtbl.find_opt(groups, h)) {
              | None => Hashtbl.add(groups, h, s)
              | Some(first) =>
                if (first != s) {
                  collisions := [name, ...collisions^];
                }
              };
            }
          )
     );
  check(
    bool,
    Printf.sprintf(
      "the corpus hashed: %d programs, %d subterms, %d distinct",
      List.length(Lazy.force(corpus)),
      hashed^,
      Hashtbl.length(groups),
    ),
    true,
    hashed^ > 1000,
  );
  check(
    list(string),
    "programs with two different subterms of one hash",
    [],
    collisions^,
  );
};

/* The reuse check's two answers, at every node: the hashed one-walk check
   (IncrEval.reuse_check now) and the trace check it replaced. The hashed
   one may be stricter (a reuse lost) but never laxer, except the pinned
   corner of (e) against e, which evaluate alike: so wherever it says
   reuse, the old check says so too, or the two differ only as fast_equal
   ignores (parentheses, projectors). */
let decisions = () => {
  let old_check = (a, b) => Exp.fast_equal_with_lexemes(a, b);
  let new_check = (a, b) =>
    Util.ValueHash.same(a, b) || Exp.fast_equal_hashed_with_lexemes(a, b);
  let laxer = ref([]);
  let stricter = ref(0);
  let pairs = ref(0);
  Lazy.force(corpus)
  |> List.iter(((name, e)) =>
       [
         ("a copy", copy(e)),
         ("new ids", Exp.replace_all_ids(e)),
         ("a literal edited", bump_first_int(e)),
       ]
       |> List.iter(((how, e')) =>
            List.iter2(
              (a, b) => {
                incr(pairs);
                let o = old_check(a, b);
                let n = new_check(a, b);
                if (n && !o && !Exp.fast_equal(a, b)) {
                  laxer := [name ++ ", " ++ how, ...laxer^];
                } else if (o && !n) {
                  incr(stricter);
                };
              },
              subterms(e),
              subterms(e'),
            )
          )
     );
  check(
    bool,
    Printf.sprintf(
      "pairs compared: %d (the hashed check stricter at %d)",
      pairs^,
      stricter^,
    ),
    true,
    pairs^ > 1000,
  );
  check(
    list(string),
    "where the hashed check reuses and the old one would not",
    [],
    laxer^,
  );
};

let tests = (
  "ValueHash",
  [
    test_case(
      "the corpus: no two different subterms share a hash",
      `Slow,
      census,
    ),
    test_case(
      "the corpus: the hashed reuse check is never laxer",
      `Slow,
      decisions,
    ),
    test_case(
      "a copy hashes the same",
      `Quick,
      () => {
        let e = elab(program);
        let c = copy(e);
        check(bool, "separate objects", false, e === c);
        check(bool, "same", true, Util.ValueHash.same(e, c));
      },
    ),
    test_case(
      "a different value does not",
      `Quick,
      () => {
        check(
          bool,
          "1 vs 2",
          false,
          Util.ValueHash.same([1, 2, 3], [1, 2, 4]),
        );
        check(bool, "strings", false, Util.ValueHash.same("ab", "ba"));
        check(
          bool,
          "nested",
          false,
          Util.ValueHash.same([[1], [2]], [[2], [1]]),
        );
        /* the same text elaborated twice: fresh ids, so not the same build */
        check(
          bool,
          "fresh ids",
          false,
          Util.ValueHash.same(elab(program), elab(program)),
        );
      },
    ),
    test_case(
      "parts that differ only in the same bit do not cancel", `Quick, () => {
      /* Each step of the hash used to be an xor and a multiply, which
         carries a difference only toward the high bits: one in bit 30 or
         31 stayed in bits 30 and 31, and a second such difference undid
         it, so (1, 1, 1) and (1 lxor 1 lsl 30, 1 lxor 1 lsl 30, 1) hashed
         alike, as did any two of the three fields, at bit 30 or 31. Every
         bit, and every two of three fields. */
      for (k in 0 to 31) {
        let f = x => x lxor 1 lsl k;
        let name = what => Printf.sprintf("bit %d, %s", k, what);
        check(
          bool,
          name("fields 1, 2"),
          false,
          Util.ValueHash.same((1, 1, 1), (f(1), f(1), 1)),
        );
        check(
          bool,
          name("fields 1, 3"),
          false,
          Util.ValueHash.same((1, 1, 1), (f(1), 1, f(1))),
        );
        check(
          bool,
          name("fields 2, 3"),
          false,
          Util.ValueHash.same((1, 1, 1), (1, f(1), f(1))),
        );
        check(
          bool,
          name("nested"),
          false,
          Util.ValueHash.same(((1, 1), 1), ((f(1), 1), f(1))),
        );
      }
    }),
    test_case(
      "a function has no hash: never the same",
      `Quick,
      () => {
        let f = (x: int) => x + 1;
        check(
          bool,
          "two closures",
          false,
          Util.ValueHash.same((1, f), (1, f)),
        );
      },
    ),
    test_case(
      "the hashed comparison agrees with fast_equal",
      `Quick,
      () => {
        let e = elab(program);
        let c = copy(e);
        let d = elab(edited);
        check(bool, "copy: plain", true, Exp.fast_equal(e, c));
        check(bool, "copy: hashed", true, Exp.fast_equal_hashed(e, c));
        check(bool, "edited: plain", false, Exp.fast_equal(e, d));
        check(bool, "edited: hashed", false, Exp.fast_equal_hashed(e, d));
        check(
          bool,
          "re-elaborated: hashed agrees with plain",
          Exp.fast_equal(e, elab(program)),
          Exp.fast_equal_hashed(e, elab(program)),
        );
        /* asked again: answered from memory, the same */
        check(bool, "copy, again", true, Exp.fast_equal_hashed(e, c));
        check(bool, "edited, again", false, Exp.fast_equal_hashed(e, d));
      },
    ),
    test_case(
      "pairs: an answer is remembered for exactly that pair",
      `Quick,
      () => {
        let m = Util.ValueHash.Pairs.create();
        let a = [1, 2];
        let b = [1, 2];
        let c = [1, 2];
        check(
          option(bool),
          "unknown",
          None,
          Util.ValueHash.Pairs.find(m, a, b),
        );
        Util.ValueHash.Pairs.add(m, a, b, true);
        check(
          option(bool),
          "a, b",
          Some(true),
          Util.ValueHash.Pairs.find(m, a, b),
        );
        check(
          option(bool),
          "a, c",
          None,
          Util.ValueHash.Pairs.find(m, a, c),
        );
        check(
          option(bool),
          "b, a",
          None,
          Util.ValueHash.Pairs.find(m, b, a),
        );
        Util.ValueHash.Pairs.add(m, a, c, false);
        check(
          option(bool),
          "a, c now",
          Some(false),
          Util.ValueHash.Pairs.find(m, a, c),
        );
        check(
          option(bool),
          "not an object",
          None,
          {
            Util.ValueHash.Pairs.add(m, 3, 3, true);
            Util.ValueHash.Pairs.find(m, 3, 3);
          },
        );
      },
    ),
  ],
);
