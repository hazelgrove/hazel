open Alcotest;
open Haz3lcore;
open Language;

/* DefStatics updates a chain's bookkeeping where items changed: after
   member edits (literals, retypes, renames, adds, removes, moves within
   and across modules) its maps and aggregates equal a calc from scratch
   (DefStaticsCheck.delta_diffs, which DefStaticsParity's mega scripts
   run too). and a clean member keeps its term, so its head costs no
   structural compare */

let settings = CoreSettings.on;

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
        check(
          list(string),
          name,
          [],
          DefStaticsCheck.delta_diffs(~settings, ~prev, ds),
        );
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

/* typed key by key into [src] after its line [after], each key's program
   as semantics reads it (its completion), each calc from the last key's */
let typed = (~root, src, after, keys, ()) => {
  module MC = Test_ModuleCompletion;
  let term_of = z =>
    MakeTerm.semantic_term(~root, MakeTerm.semantic_source(z));
  let z0 = MC.at_line(~root, src, after);
  ignore(
    List.fold_left(
      ((z, prev, typed), c) => {
        let z = MC.perform(~root, z, Insert(c));
        let typed = typed ++ c;
        let ds = DefStatics.calc(~settings, ~prev, term_of(z));
        let name = String.escaped(typed);
        check(
          list(string),
          name,
          [],
          DefStaticsCheck.delta_diffs(~settings, ~prev, ds),
        );
        check(
          list(string),
          name ++ " vs monolithic",
          [],
          DefStaticsCheck.divergences(~settings, ~cold=false, ds),
        );
        (z, ds, typed);
      },
      (z0, DefStatics.calc(~settings, term_of(z0)), ""),
      List.map(String.make(1), List.of_seq(String.to_seq(keys))),
    ),
  );
};

/* an id that moves from a module's own entries into a member: a `;`
   typed after the member of a module whose `}` isn't typed separates the
   outer module's members until a member follows it there, and the grout
   a missing `;` leaves is the `+` typed in its place */
let moved_ids = [
  (
    "a `;` into a module whose `}` isn't typed",
    Sort.Exp,
    Test_ModuleCompletion.counter,
    "  let start = 0;",
    "\nmodule N = {\nlet a = 1;\nl",
  ),
  (
    "a `;` into a module whose `}` isn't typed, at a Mod root",
    Sort.Mod,
    Test_ModAudit.counter_mod,
    "  let start = 0;",
    "\nmodule N = {\nlet a = 1;\nl",
  ),
  (
    "a missing `;` typed over as a `+`",
    Sort.Exp,
    Test_ModuleCompletion.counter,
    "module Counter = {",
    "\nlet q = fun x -> x +",
  ),
  (
    "a missing `;` typed over as a `+`, at a Mod root",
    Sort.Mod,
    Test_ModAudit.counter_mod,
    "module Counter = {",
    "\nlet q = fun x -> x +",
  ),
];

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

/* an outline op across levels (Tab, ⇧Tab) */
let level = P.edit_op;

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
    DefStaticsCheck.delta_diffs(~settings, ds1),
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
    /* lib_edits first: their own run would be this one's prefix */
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
    /* an item the rest don't use: the items below stay clean, and only
       the root after it gets a new parent */
    test_case(
      "a let chain, adds and removes",
      `Quick,
      script(
        ~mono=true,
        "let a = 1 in\nlet b = a + 1 in\ntype T = Int in\nlet c : T = b in\nc",
        [
          op(NewBelow, ["a"]),
          op(Delete, ["new_def"]),
          op(NewTestBelow, ["b"]),
        ],
      ),
    ),
  ]
  @ List.map(
      ((name, root, src, after, keys)) =>
        test_case(
          "typed, an id moved into a member: " ++ name,
          `Quick,
          typed(~root, src, after, keys),
        ),
      moved_ids,
    )
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
