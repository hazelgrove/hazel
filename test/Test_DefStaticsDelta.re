open Alcotest;
open Haz3lcore;
open Language;

/* DefStatics updates a chain's bookkeeping where items changed: after
   member edits (literals, retypes, renames, adds, removes, moves within
   and across modules) its maps and aggregates equal a calc from scratch.
   and a clean member keeps its term, so its head costs no structural
   compare */

let settings = CoreSettings.on;

/* how [t] differs from a cold calc of its term: the merged map by key,
   every item's map (members included) by its keys and, where the item
   is new since [prev], its infos; and the items' aggregates */
let delta_diffs = (~prev=?, t: DefStatics.t): list(string) => {
  let analyzed = DefStatics.last_analyzed^;
  let cold = DefStatics.calc(~settings, ~probe_ids=t.probe_ids, t.term);
  DefStatics.last_analyzed := analyzed;
  let out = ref([]);
  let note = s => out := [s, ...out^];
  let cls = i => Cls.show(Info.cls_of(i));
  let maps = (~infos=true, what, got: Statics.Map.t, want: Statics.Map.t) => {
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
              DefStaticsCheck.differ(~types=true, w, g),
            )
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
  maps("merged", t.merged, cold.merged);
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

/* each step's calc from the last one's equals a cold calc; [mono] also
   checks it against monolithic statics */
let script =
    (
      ~root=Sort.Exp,
      ~mono=false,
      src: string,
      steps: list(Test_DefStaticsParity.step),
      (),
    ) => {
  let calc = (~prev=?, seg) =>
    DefStatics.calc(
      ~settings,
      ~prev?,
      MakeTerm.Incr.term_of_root(~root, seg),
    );
  let seg = CorpusUtil.seg_of(~root, src);
  ignore(
    List.fold_left(
      ((i, seg, prev), s: Test_DefStaticsParity.step) => {
        let seg = s.edit(~root, seg);
        let ds = calc(~prev, seg);
        let name = "step " ++ string_of_int(i);
        check(list(string), name, [], delta_diffs(~prev, ds));
        if (mono) {
          check(
            list(string),
            name ++ " vs monolithic",
            [],
            DefStaticsCheck.divergences(~settings, ~cold=false, ds),
          );
        };
        (i + 1, seg, ds);
      },
      (1, seg, calc(seg)),
      steps,
    ),
  );
};

let mega = (~root, file, steps, ()) =>
  switch (CorpusUtil.mega_src(file)) {
  | Some(src) => script(~root, src, steps, ())
  | None => fail("corpus unreadable: " ++ file)
  };

/* the stdlib's shape: Option and List (cut down), a type member, a
   nested module, a let-chain member and member tests */
let lib = "module Option = {
  type t = None + Some(?);
  let map : (Option, ? -> ?) -> ? = fun (opt, f) ->
    case opt
    | None => None
    | Some(x) => Some(f(x))
    end;
  let bind : (Option, ? -> ?) -> ? = fun (opt, f) ->
    case opt
    | None => None
    | Some(x) => f(x)
    end;
  let to_list : Option -> [?] = fun opt ->
    case opt
    | None => []
    | Some(x) => [x]
    end
};

module List = {
  let length : [?] -> Int = fun xs ->
    case xs
    | [] => 0
    | _ :: xs => 1 + length(xs)
    end;
  let map : ([?], ? -> ?) -> [?] = fun (xs, f) ->
    case xs
    | [] => []
    | x :: xs => f(x) :: map(xs, f)
    end;
  let filter : ([?], ? -> Bool) -> [?] = fun (xs, f) ->
    case xs
    | [] => []
    | x :: xs => if f(x) then x :: filter(xs, f) else filter(xs, f)
    end;
  let fold_left : ([?], (?, ?) -> ?, ?) -> ? = fun (xs, f, acc) ->
    case xs
    | [] => acc
    | x :: xs => fold_left(xs, f, f(acc, x))
    end;
  let flat_map : ([?], ? -> [?]) -> [?] = fun (xs, f) ->
    fold_left(xs, fun (acc, x) -> acc @ f(x), []);
  type Pair = (Float, ?);
  let unzip : [Pair] -> ([?], [?]) = fun xs ->
    case xs
    | [] => ([], [])
    | (a, b) :: xs =>
      let (as, bs) = unzip(xs) in
      (a :: as, b :: bs)
    end;
  let reverse : [?] -> [?] = fun xs ->
    case xs
    | [] => []
    | x :: xs => reverse(xs) @ [x]
    end;
  let range : (Int, Int) -> [Int] = fun (start, stop) ->
    if start > stop then [] else start :: range(start + 1, stop);
  let enumerate : [?] -> [(Int, ?)] = fun xs ->
    let enumerate_helper = fun (xs, index) ->
      case xs
      | [] => []
      | x :: xs => (index, x) :: enumerate_helper(xs, index + 1)
      end
    in
    enumerate_helper(xs, 0);
  let any : ([?], ? -> Bool) -> Bool = fun (xs, pred) ->
    case xs
    | [] => false
    | x :: xs => if pred(x) then true else any(xs, pred)
    end;
  let exists = any;
  module Sorted = {
    let insert : (Int, [Int]) -> [Int] = fun (x, xs) ->
      case xs
      | [] => [x]
      | y :: ys => if x <= y then x :: xs else y :: insert(x, ys)
      end;
    let sort : [Int] -> [Int] = fun xs ->
      fold_left(xs, fun (acc, x) -> insert(x, acc), [])
  };
  let k = 10 in
  test length(range(1, k)) == k end;
  let init : (Int, Int -> ?) -> [?] = fun (n, f) ->
    let init_helper = fun (n, f, i) ->
      if i >= n then [] else f(i) :: init_helper(n, f, i + 1)
    in
    init_helper(n, f, 0);
  test length(init(3, fun i -> i)) == 3 end;
  let last : [?] -> ? = fun xs -> fold_left(xs, fun (acc, x) -> x, 0)
};

let pair : List.Pair = (1.5, 2);

test List.length([1, 2, 3]) == 3 end;

List.length(List.map([1, 2, 3], fun x -> x + 1))";

module P = Test_DefStaticsParity;
let (token, op, move) = (P.token, P.op, P.move);

/* an outline op across levels (Tab, ⇧Tab), which go through
   ItemEdit.edit */
let level = (op: Web.OutlineEdit.def_op, path): P.step =>
  P.step((~root, seg) => {
    let term = MakeTerm.Incr.term_of_root(~root, seg);
    let ctx: Web.ItemEdit.ctx = {
      mod_root: root == Sort.Mod,
      term,
      info_map: lazy(Statics.Map.empty),
      is_open: _ => true,
    };
    switch (Web.ItemEdit.edit(ctx, Op(op, CorpusUtil.row(term, path)), seg)) {
    | Ok((seg, _)) => seg
    | Error(why) =>
      fail("op failed at " ++ String.concat("/", path) ++ ": " ++ why)
    };
  });

/* [token]'s edit where the program has the token */
let token_if = (~at, needle, repl): P.step =>
  P.step((~root as _, seg) =>
    fst(CorpusUtil.edit_token(~at, ~keep_id=true, ~needle, ~repl, seg))
  );

/* edits within members: monolithic statics agrees too */
let lib_edits = [
  /* a literal in the first member, its id kept as typing does */
  token(~at=`First, ~keep_id=true, "0", "5"),
  /* a retype: its users go dirty */
  token(~at=`First, "Int", "Bool"),
  /* renames: a binder alone, and a binder with its uses */
  token(~at=`First, "range", "span"),
  token("reverse", "rev"),
  /* in a nested module and in a let-chain member */
  token("<=", "<"),
  token("10", "11"),
  /* a type member, which a later member's annotation reads */
  token("Float", "String"),
];

/* adds, removes and moves (a duplicated member included: its copy's
   recursive call is a use of the member above, as monolithically) */
let lib_restructures = [
  op(NewBelow, ["List", "map"]),
  op(Duplicate, ["List", "filter"]),
  op(Delete, ["List", "flat_map"]),
  op(Delete, ["List", "Sorted"]),
  op(MoveDown, ["List", "length"]),
  op(MoveUp, ["List", "any"]),
  level(Outdent, ["List", "last"]),
  move(~up=true, ["List", "map"]),
  level(Indent, ["last"]),
];

let mega_steps = [
  token("180", "181"),
  token(~at=`First, "pad", "pad2"),
  op(NewBelow, ["WateringTimer", "format"]),
  op(Delete, ["WateringTimer", "format"]),
  op(MoveDown, ["HarvestLedger", "cropValue"]),
  level(Outdent, ["WateringTimer", "selfcheck"]),
];

/* a member edit in List: MakeTerm keeps the other members' terms and
   DefStatics their wrappers, so the only structural compares are the
   edited member's head and List's own; the sums recomputed are the
   member's, List's and the item above List's */
let clean_heads = () => {
  let term_of = seg => MakeTerm.Incr.term_of_root(~root=Mod, seg);
  let seg = CorpusUtil.seg_of(~root=Mod, lib);
  let t0 = term_of(seg);
  let ds0 = DefStatics.calc(~settings, t0);
  let (edited, _) =
    CorpusUtil.edit_token(
      ~at=`First,
      ~keep_id=true,
      ~needle="0",
      ~repl="5",
      seg,
    );
  let t1 = term_of(edited);
  let members = (t: Exp.t) =>
    switch (DefStatics.strip(t).term) {
    | Module(items) =>
      List.concat_map(
        (m: Mod.t) =>
          switch (m.term) {
          | ModuleMod(_, {term: Module(ms), _}) => ms
          | _ => []
          },
        items,
      )
    | _ => fail("not a module root")
    };
  let kept =
    List.fold_left2(
      (n, a, b) => a === b ? n + 1 : n,
      0,
      members(t0),
      members(t1),
    );
  check(
    int,
    "MakeTerm keeps every member's term but the edited one",
    List.length(members(t0)) - 1,
    kept,
  );
  DefStatics.head_compares := 0;
  let ds1 = DefStatics.calc(~settings, ~prev=ds0, t1);
  /* each head's pattern and definition */
  check(int, "structural head compares", 4, DefStatics.head_compares^);
  check(int, "sums recomputed", 3, DefStatics.summed^);
  let module_members = (ds: DefStatics.t) =>
    List.concat_map((it: DefStatics.item) => it.d_members, ds.items);
  let same_nodes =
    List.fold_left2(
      (n, a: DefStatics.item, b: DefStatics.item) =>
        a.d_node === b.d_node ? n + 1 : n,
      0,
      module_members(ds0),
      module_members(ds1),
    );
  check(
    int,
    "members keep their nodes",
    List.length(module_members(ds0)) - 1,
    same_nodes,
  );
  check(
    list(string),
    "and the result is a cold calc's",
    [],
    delta_diffs(ds1),
  );
};

/* co_union is CoCtx.union([a, b]) on the corpus's co_ctxs, order and
   uses included */
let co_union_is_union = () => {
  let ds =
    DefStatics.calc(
      ~settings,
      MakeTerm.Incr.term_of_root(
        ~root=Mod,
        CorpusUtil.seg_of(~root=Mod, lib),
      ),
    );
  let cos =
    Id.Map.fold(
      (_, info, acc) =>
        switch (info) {
        | Info.InfoExp({co_ctx, _}) when co_ctx != [] => [co_ctx, ...acc]
        | _ => acc
        },
      ds.merged,
      [],
    );
  let show = (co: CoCtx.t) =>
    String.concat(
      " ",
      List.map(
        ((n, es: list(CoCtx.entry))) =>
          n
          ++ ":"
          ++ String.concat(
               ",",
               List.map((e: CoCtx.entry) => Id.str8(e.id), es),
             ),
        co,
      ),
    );
  let n = ref(0);
  List.iteri(
    (i, a) =>
      List.iteri(
        (j, b) =>
          if ((i + j) mod 7 == 0) {
            incr(n);
            check(
              string,
              "union",
              show(CoCtx.union([a, b])),
              show(DefStatics.co_union(a, b)),
            );
          },
        cos,
      ),
    cos,
  );
  check(bool, "pairs checked", true, n^ > 100);
};

let tests = (
  "DefStaticsDelta",
  [
    test_case("clean members compare in O(1)", `Quick, clean_heads),
    test_case("co_union is CoCtx.union", `Quick, co_union_is_union),
    test_case(
      "a lib-like module, member edits",
      `Quick,
      script(~root=Mod, ~mono=true, lib, lib_edits),
    ),
    test_case(
      "a lib-like module, adds, removes and moves",
      `Quick,
      script(~root=Mod, ~mono=true, lib, lib_edits @ lib_restructures),
    ),
    test_case(
      "the lib-like module at an Exp root",
      `Quick,
      script(
        ~mono=true,
        "module Wrap = {\n" ++ lib ++ "\n} in\nWrap.List.length([1])",
        [
          token(~at=`First, ~keep_id=true, "0", "5"),
          token("reverse", "rev"),
          op(Delete, ["Wrap", "List", "flat_map"]),
          op(MoveDown, ["Wrap", "List", "length"]),
        ],
      ),
    ),
    test_case("mega-1k", `Quick, mega(~root=Exp, "mega-1k.hz", mega_steps)),
    test_case(
      "mega-mod-1k",
      `Quick,
      mega(~root=Mod, "mega-mod-1k.hz", mega_steps),
    ),
  ]
  @ List.map(
      ((path, src)) =>
        test_case(
          Filename.basename(path),
          `Quick,
          script(
            ~root=Mod,
            src,
            [token_if(~at=`First, "0", "5"), token_if(~at=`Last, "1", "2")],
          ),
        ),
      Test_ItemSpans.stdlib_corpus(),
    ),
);
