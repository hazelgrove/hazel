open Alcotest;
open Haz3lcore;

/* A member level's skel, and a program's top level's, are kept from the
   last parse while the level's pieces read alike (MakeTerm.Incr.reskel).
   Over an edit script typed into List, Mega-Mod 1k and a 4k-member level
   (Mega 4k's items as one module's members): each parse gives the record
   the same parse gives with every skel built afresh; every skel kept is
   the one Segment.skel builds; keys that leave the levels' shapes alone
   build none; and the last parse is a cold parse's */

module MA = Test_ModAudit;
module S = ItemSpans;

let root = Sort.Mod;

/* Mega 4k's items as World's members */
let world_src = (): string =>
  "module World = {\n"
  ++ (
    String.split_on_char('\n', Test_EditCaches.mega_4k(~root=Mod))
    |> List.map(l => l == "" ? l : "  " ++ l)
    |> String.concat("\n")
  )
  ++ "\n}";

type program = {
  name: string,
  src: unit => string,
  /* needles, the caret [off] columns in: inside a member's tile, and in
     a token of the member level; then, the caret after them, a member's
     `;` ending its line, and another's */
  inner: (string, int),
  token: (string, int),
  line_end: string,
  semi: string,
  /* two adjacent members that read alike (their tokens' starts) */
  pair: (string, string),
  /* typed on the new line and deleted: a member, then an unfinished one */
  member: string,
  unfinished: string,
  speed: speed_level,
};

let programs = [
  {
    name: "List",
    src: () => Option.get(MA.stdlib("List.hz")),
    inner: ("    | [] => 0", 13),
    token: ("    fold_left(xs, fun (acc, x)", 6),
    line_end: "    end;",
    semi: "    fold_left(xs, fun (acc, x) -> acc @ f(x), []);",
    pair: ("letmap:", "letfilter:"),
    member: "let q = 1;",
    unfinished: "let q = (1 + 2",
    speed: `Quick,
  },
  {
    name: "Mega-Mod 1k",
    src: () => Option.get(CorpusUtil.mega_src("mega-mod-1k.hz")),
    inner: ("dew = Int", 3),
    token: ("final.ok == 9", 13),
    line_end: "test Text.selfcheck(()) end;",
    semi: "test DewLedger.selfcheck(()) end;",
    pair: ("testText.", "testDewLedger."),
    member: "let q = 1;",
    unfinished: "let q = (1 + 2",
    speed: `Quick,
  },
  {
    name: "a 4k-member level",
    src: world_src,
    /* in MetaRunner, near the other needles: the caret's walk across
       World from DewLedger takes seconds */
    inner: ("ran = Int", 3),
    token: ("final.ok == 87", 14),
    line_end: "test Text.selfcheck(()) end;",
    semi: "test DewLedger.selfcheck(()) end;",
    pair: ("testText.", "testDewLedger."),
    /* the shortest of each, as a key here costs ~0.5s */
    member: "q;",
    unfinished: "(1",
    speed: `Slow,
  },
];

/* the program's members: a lone module literal's, else its top level's;
   and the program with them rearranged */
let rearranged =
    (f: list(Segment.t) => list(Segment.t), seg: Segment.t): Segment.t =>
  switch (S.module_item(~root, seg)) {
  | Some((slices, put, _)) => put(List.concat(f(slices)))
  | None => List.concat(f(S.slices(S.top(~root, seg))))
  };
let members = (seg: Segment.t): list(Segment.t) => {
  let found = ref([]);
  ignore(
    rearranged(
      ms => {
        found := ms;
        ms;
      },
      seg,
    ),
  );
  found^;
};
let index = (start: string, seg: Segment.t): int => {
  let rec go = (k, ms) =>
    switch (ms) {
    | [] => fail("no member " ++ start)
    | [m, ...ms] =>
      CorpusUtil.contains(start, String.concat("", MA.tokens(m)))
        ? k : go(k + 1, ms)
    };
  go(0, members(seg));
};
let swap = (k: int, ms: list('a)): list('a) =>
  List.mapi(
    (i, m) =>
      i == k ? List.nth(ms, k + 1) : i == k + 1 ? List.nth(ms, k) : m,
    ms,
  );
let drop = (k: int, ms: list('a)): list('a) =>
  List.filteri((i, _) => i != k, ms);
/* member [k] moved before the last */
let move_down = (k: int, ms: list('a)): list('a) => {
  let m = List.nth(ms, k);
  let rest = drop(k, ms);
  let n = List.length(rest);
  List.concat(List.mapi((i, x) => i == n - 1 ? [m, x] : [x], rest));
};

/* the cache as it was, its skels forgotten: a parse from it builds every
   skel, and takes the same entries */
let skelless = (c: MakeTerm.Incr.cache): MakeTerm.Incr.cache => {
  let rec strip = ((k, e: MakeTerm.Incr.entry_full)) => (
    k,
    {
      ...e,
      f_kids: Option.map(((b, ks)) => (b, List.map(strip, ks)), e.f_kids),
      f_skel: None,
    },
  );
  {
    c_prev:
      Option.map(
        ((r, e, entries, m, td, pj)) =>
          (r, e, List.map(strip, entries), m, td, pj),
        c.c_prev,
      ),
    c_skel: None,
  };
};

/* each field structurally; [compare] skips what the records share */
let agree = (what: string, a: MakeTerm.t, b: MakeTerm.t): unit => {
  let same = (x, y) => compare(x, y) == 0;
  check(bool, what ++ ": term", true, same(a.term, b.term));
  check(bool, what ++ ": terms", true, Id.Map.equal(same, a.terms, b.terms));
  check(
    bool,
    what ++ ": term_data",
    true,
    Id.Map.equal(same, a.term_data, b.term_data),
  );
  check(
    bool,
    what ++ ": projectors",
    true,
    Id.Map.equal(same, a.projectors, b.projectors)
    && a.projector_list == b.projector_list,
  );
};

/* kept skels against Segment.skel, each level's once */
let check_skels = (~seen, what: string, cache: MakeTerm.Incr.cache) => {
  let skel_is = (sort, seg, sk) =>
    if (!List.memq(seg, seen^)) {
      seen := [seg, ...seen^];
      check(
        bool,
        what ++ ": the skel Segment.skel builds",
        true,
        sk == Segment.skel(~sort, seg),
      );
    };
  let rec entry = (e: MakeTerm.Incr.entry_full) => {
    Option.iter(((seg, sk)) => skel_is(Sort.Mod, seg, sk), e.f_skel);
    switch (e.f_kids) {
    | Some((_, kids)) => List.iter(((_, k)) => entry(k), kids)
    | None => ()
    };
  };
  switch (cache.c_prev) {
  | Some((_, _, entries, _, _, _)) =>
    List.iter(((_, e)) => entry(e), entries)
  | None => fail(what ++ ": no entries")
  };
  switch (cache.c_skel) {
  | Some((sort, seg, sk)) => skel_is(sort, seg, sk)
  | None => fail(what ++ ": no top-level skel")
  };
};

/* the editor's state: its zipper, and the syntax its next key reads */
type typing = {
  z: Zipper.t,
  syntax: CachedSyntax.t,
};

let script = (p: program) => {
  let src = p.src();
  let z = MA.load(~root, src);
  let t0 = {
    z,
    syntax: CachedSyntax.init(~root, z),
  };
  let cache = MakeTerm.Incr.mk_cache();
  let parse = (cache, seg) =>
    MakeTerm.Incr.go_incr(~ends=Read, ~root, ~cache, seg);
  ignore(parse(cache, t0.syntax.segment));
  let seen = ref([]);
  let keys = ref(0);
  let read = (~same: bool, what: string, seg: Segment.t): MakeTerm.t => {
    let what = p.name ++ ", " ++ what;
    let twin = skelless(cache);
    let (b0, fb) = (MakeTerm.Incr.skels_built^, MakeTerm.Incr.fell_back^);
    let r = parse(cache, seg);
    check(int, what ++ ": no fallback", fb, MakeTerm.Incr.fell_back^);
    if (same) {
      check(
        int,
        what ++ ": no skel built",
        0,
        MakeTerm.Incr.skels_built^ - b0,
      );
    };
    check_skels(~seen, what, cache);
    agree(what, parse(twin, seg), r);
    incr(keys);
    r;
  };
  let act = (~same, what, t: typing, a: Action.t): typing => {
    let z =
      switch (
        MA.perform_why(
          ~root,
          ~syntax=t.syntax,
          ~statics=CachedStatics.empty,
          t.z,
          a,
        )
      ) {
      | Ok(z) => z
      | Error(why) => fail(p.name ++ ", " ++ what ++ ": " ++ why)
      };
    let syntax =
      CachedSyntax.init(
        ~root,
        ~m_cache=t.syntax.m_cache,
        ~t_cache=t.syntax.t_cache,
        z,
      );
    ignore(read(~same, what, syntax.segment));
    {
      z,
      syntax,
    };
  };
  let at = (what, (needle, off), t: typing) =>
    switch (MA.point_of(~off, MA.text_of(t.z), needle)) {
    | Some(pt) => act(~same=true, what, t, Move(Point(pt, None)))
    | None => fail(p.name ++ ": no " ++ needle)
    };
  let type_ = (~same, what, s: string, t: typing) =>
    List.fold_left(
      (t, c) => act(~same, what ++ " " ++ String.escaped(c), t, Insert(c)),
      t,
      Token.to_list(s),
    );
  let back = (~same, what, n: int, t: typing) =>
    List.fold_left(
      (t, _) => act(~same, what, t, Destruct(Local(Left, ByChar))),
      t,
      List.init(n, _ => ()),
    );
  let t =
    t0
    /* keys inside a member, and in a token of the member level */
    |> at("to a member", p.inner)
    |> type_(~same=true, "in a member", "12")
    |> back(~same=true, "in a member, deleted", 2)
    |> at("to a token of the level", p.token)
    |> type_(~same=true, "in a token of the level", "1")
    |> back(~same=true, "in a token of the level, deleted", 1)
    /* a new line: its whitespace moves the pieces after it */
    |> at("to a line's end", (p.line_end, String.length(p.line_end)))
    |> type_(~same=true, "a new line", "\n")
    /* a member typed on it and deleted, then an unfinished one */
    |> type_(~same=false, "a member", p.member)
    |> back(~same=false, "a member, deleted", String.length(p.member))
    |> type_(~same=false, "an unfinished member", p.unfinished)
    |> back(
         ~same=false,
         "an unfinished member, deleted",
         String.length(p.unfinished),
       )
    /* a missing `;`, and keys in a member while it is */
    |> at("to a `;`", (p.semi, String.length(p.semi)))
    |> back(~same=false, "a `;` deleted", 1);
  let missing = Zipper.Caret.point(t.syntax.measured, t.z);
  let t =
    t
    |> at("to a member", p.inner)
    |> type_(~same=true, "in a member, a `;` missing", "3")
    |> back(~same=true, "in a member, a `;` missing, deleted", 1);
  let t =
    act(~same=true, "to the missing `;`", t, Move(Point(missing, None)));
  let t = type_(~same=false, "the `;` again", ";", t);
  /* members swapped, moved and removed, as outline ops leave them */
  let seg = t.syntax.segment;
  let (a, b) = p.pair;
  let k = index(a, seg);
  check(int, p.name ++ ": the pair is adjacent", k + 1, index(b, seg));
  ignore(
    read(
      ~same=true,
      "two members alike, swapped",
      rearranged(swap(k), seg),
    ),
  );
  ignore(
    read(~same=false, "a member moved down", rearranged(move_down(1), seg)),
  );
  ignore(read(~same=false, "a member removed", rearranged(drop(1), seg)));
  agree(
    p.name ++ ", after the script, cold",
    parse(MakeTerm.Incr.mk_cache(), seg),
    read(~same=false, "as it was", seg),
  );
  check(
    int,
    p.name ++ ": keys read",
    21 + 2 * String.length(p.member ++ p.unfinished),
    keys^,
  );
};

/* a level, the same with a hole for its first token, and with a grout
   for its first `;` (a missing `;`): each with one piece put in another's
   place (a piece of each kind they have: tiles by form, sort and shards;
   grout of each shape), or a space put in or taken out. Each time reskel
   from the level's skel gives Segment.skel's, or both raise */
let swaps = ((sort: Sort.t, src: string), ()) => {
  let parsed = CorpusUtil.seg_of(~root=sort, src);
  let grout = shape =>
    Piece.Grout({
      id: Id.mk(),
      shape,
    });
  /* [seg] with its first piece [is] replaced by [by] */
  let first = (is: Piece.t => bool, by: Piece.t, seg: Segment.t) => {
    let found = ref(false);
    List.map(
      p =>
        if (! found^ && is(p)) {
          found := true;
          by;
        } else {
          p;
        },
      seg,
    );
  };
  let token = (p: Piece.t) =>
    switch (p) {
    | Tile({form: Tok(_), _} as t) => Tile.shapes(t) == (Convex, Convex)
    | _ => false
    };
  let bases = [
    parsed,
    first(token, grout(Convex), parsed),
    first(Piece.is_semi, grout(Concave), parsed),
  ];
  let kind = (p: Piece.t) =>
    switch (p) {
    | Tile(t) => `Tile((t.form, t.sort, t.shards))
    | Grout(g) => `Grout(g.shape)
    | Secondary(_) => `Secondary
    | Projector(_) => `Projector
    };
  let pool =
    List.fold_left(
      (pool, p) =>
        List.exists(q => kind(q) == kind(p), pool) ? pool : pool @ [p],
      [Piece.Secondary(Secondary.mk_space(Id.mk()))],
      List.concat(bases),
    );
  let read = (f: unit => Skel.t) =>
    switch (f()) {
    | sk => Some(sk)
    | exception _ => None
    };
  let kept = MakeTerm.Incr.skels_kept^;
  List.iteri(
    (b, seg) => {
      let skel = Segment.skel(~sort, seg);
      let trial = (what, seg') =>
        check(
          bool,
          Printf.sprintf("base %d: %s", b, what),
          true,
          read(() => Segment.skel(~sort, seg'))
          == read(() =>
               MakeTerm.Incr.reskel(~sort, ~prev=Some((seg, skel)), seg')
             ),
        );
      List.iteri(
        (i, p) => {
          List.iteri(
            (k, q) =>
              trial(
                Printf.sprintf("pool piece %d put in for piece %d", k, i),
                List.mapi((j, r) => j == i ? q : r, seg),
              ),
            pool,
          );
          trial(
            Printf.sprintf("a space put in before piece %d", i),
            List.concat(
              List.mapi(
                (j, r) =>
                  j == i
                    ? [Piece.Secondary(Secondary.mk_space(Id.mk())), r]
                    : [r],
                seg,
              ),
            ),
          );
          if (Piece.is_secondary(p)) {
            trial(
              Printf.sprintf("piece %d taken out", i),
              List.filteri((j, _) => j != i, seg),
            );
          };
        },
        seg,
      );
    },
    bases,
  );
  check(bool, "some skels kept", true, MakeTerm.Incr.skels_kept^ > kept);
};

/* member levels' pieces and an expression's, at a Mod root and an Exp
   root's top level */
let swap_levels = [
  (
    "a Mod level",
    (
      Sort.Mod,
      "let a = 1 + 2 + 3 * 4 - 5;\nlet b = a :: [] @ [5];\ntest a == 7 end;\nlet f = fun x -> x / 2;\nf(a) > 2 || a < 1 && true;\nlet c = if a < 3 then 1 else 2;\nmodule M = { let d = 1 };\nM.d",
    ),
  ),
  (
    "an Exp level",
    (
      Sort.Exp,
      "let a = 1 in\ntest a == 1 end;\na + 2 + 3 * 4 - 5 :: [] @ [5] == b || c && d > 2",
    ),
  ),
];

/* the runner's normalization oracle (a global pass a key, seconds at 4k)
   is off: other groups run it */
let edit_script = (p: program, ()) => {
  let saved = Zipper.normalize_check^;
  Zipper.normalize_check := None;
  Fun.protect(
    ~finally=() => Zipper.normalize_check := saved,
    () => script(p),
  );
};

let tests = (
  "LevelSkels",
  List.map(
    ((name, level)) =>
      test_case(name ++ ": pieces swapped", `Quick, swaps(level)),
    swap_levels,
  )
  @ List.map(
      (p: program) =>
        test_case(p.name ++ ": an edit script", p.speed, edit_script(p)),
      programs,
    ),
);
