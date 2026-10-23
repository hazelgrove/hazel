open Alcotest;
open Haz3lcore;
open Language;

/* The eval request in pieces: after every edit the worker composes the
   program a whole request would carry, posting only the changed pieces;
   a piece it lacks or that doesn't match its hash gets the request
   posted whole; and evaluation reads the same */

module Split = Web.EvalSplit;
module Client = Web.WorkerClient;
module Parity = Test_DefStaticsParity;
module Delta = Test_DefStaticsDelta;

let settings = CoreSettings.on;

/* structural equality, a memo of the pairs found equal so that shared
   subterms are compared once (the whole program, and every root entry's
   suffix of it). a pair counts as equal while its fields are compared:
   one found unequal fails the test, so no later check reads the memo */
type same = {same: 'a. ('a, 'a) => bool};
let same_graph = (): same => {
  let memo =
    Js_of_ocaml.Js.Unsafe.new_obj(
      Js_of_ocaml.Js.Unsafe.pure_js_expr("Map"),
      [||],
    );
  let paired = (a: Obj.t, b: Obj.t): bool =>
    Js_of_ocaml.Js.Unsafe.meth_call(
      memo,
      "get",
      [|Js_of_ocaml.Js.Unsafe.inject(a)|],
    )
    === b;
  let pair = (a: Obj.t, b: Obj.t): unit =>
    ignore(
      Js_of_ocaml.Js.Unsafe.meth_call(
        memo,
        "set",
        [|
          Js_of_ocaml.Js.Unsafe.inject(a),
          Js_of_ocaml.Js.Unsafe.inject(b),
        |],
      ),
    );
  let rec go = (a: Obj.t, b: Obj.t): bool =>
    a === b
    || (
      if (Obj.is_int(a) || Obj.is_int(b)) {
        a == b;
      } else if (Obj.tag(a) != Obj.tag(b)) {
        false;
      } else if (Obj.tag(a) >= Obj.no_scan_tag) {
        compare(a, b) == 0;
      } else if (Obj.size(a) != Obj.size(b)) {
        false;
      } else if (paired(a, b)) {
        true;
      } else {
        pair(a, b);
        let n = Obj.size(a);
        let rec fields = i =>
          i >= n || go(Obj.field(a, i), Obj.field(b, i)) && fields(i + 1);
        fields(0);
      }
    );
  {same: (x, y) => go(Obj.repr(x), Obj.repr(y))};
};

/* the program's last expression: the ⇓ toggle's probe */
let tail_id = (term: Exp.t): Id.t => {
  let rec tail_of = (e: Exp.t) =>
    switch (AutoProbePerform.top_level_rest(e)) {
    | Some(body) => tail_of(body)
    | None => e
    };
  Exp.rep_id(tail_of(term));
};

/* statics as the editor computes them, the ⇓ probe on ([ctx_in]: a
   world's, WorldSlide.ctx) */
let statics_of = (~ctx_in=?, term: Exp.t): CachedStatics.t => {
  let probe_ids = Id.Map.singleton(tail_id(term), ());
  CachedStatics.of_def_statics(
    ~settings,
    ~probe_ids,
    term,
    DefStatics.calc_auto(~settings, ~ctx_in?, ~probe_ids, term),
  );
};

/* what the base posted: the whole program */
let whole = (st: CachedStatics.t): (Exp.t, EvalInfo.t) => (
  st.elaborated,
  EvalInfo.of_info_map(
    ~probe_all=settings.probe_all,
    ~targets=st.targets,
    st.info_map,
  ),
);

let request_of = (st: CachedStatics.t): WorkerServer.Request.t => {
  request_id: 1,
  batch: [
    (
      "",
      {
        program:
          Split.program(
            ~probe_all=settings.probe_all,
            ~targets=st.targets,
            st,
          ),
        prev: UseResident,
        stream: Full,
      },
    ),
  ],
};

/* a new worker, and the client's view of it */
let fresh_worker = () => {
  Hashtbl.reset(WorkerServer.held);
  Hashtbl.reset(Client.held);
};

/* posted as the client posts it, across the encoding */
let post =
    (~full=false, request: WorkerServer.Request.t): WorkerServer.Posted.t => {
  let posted = Client.posted_of(~full, request);
  Client.hold(request);
  switch (
    WorkerServer.Active.decode_request(
      WorkerServer.Active.encode_request(Evaluate(posted)),
    )
  ) {
  | Evaluate(posted) => posted
  | InstallLibrary(_) => Alcotest.fail("a library install")
  };
};

let composed = (r: WorkerServer.received): WorkerServer.Evaluation.value =>
  switch (r) {
  | Composed({batch: [(_, v)], _}) => v
  | Composed(_) => fail("a batch of other than one")
  | Missing(keys) =>
    fail(
      "missing " ++ String.concat(", ", List.map(EvalPieces.show_key, keys)),
    )
  | Malformed(why) => fail("malformed: " ++ why)
  };

/* the worker's program is the whole request's, and the program went in
   pieces */
let check_exact =
    (name, st: CachedStatics.t, v: WorkerServer.Evaluation.value) => {
  let (expr, info) = whole(st);
  let {same} = same_graph();
  check(option(string), name ++ ": in pieces", None, Split.went_whole^);
  check(bool, name ++ ": the program", true, same(expr, v.expr));
  check(
    int,
    name ++ ": eval info entries",
    Id.Map.cardinal(info.statics),
    Id.Map.cardinal(v.eval_info_map.statics),
  );
  check(
    bool,
    name ++ ": eval info",
    true,
    same(
      Id.Map.bindings(info.statics),
      Id.Map.bindings(v.eval_info_map.statics),
    ),
  );
  check(
    bool,
    name ++ ": targets",
    true,
    same(
      Id.Map.bindings(info.targets),
      Id.Map.bindings(v.eval_info_map.targets),
    ),
  );
};

/* each step's program through one worker, from a fresh one */
let script = (~root=Sort.Exp, src: string, steps: list(Parity.step), ()) => {
  fresh_worker();
  let term_of = seg => MakeTerm.Incr.term_of_root(~root, seg);
  let run = (name, seg) => {
    let st = statics_of(term_of(seg));
    check_exact(
      name,
      st,
      composed(WorkerServer.receive(post(request_of(st)))),
    );
  };
  let seg = CorpusUtil.seg_of(~root, src);
  run("step 0", seg);
  ignore(
    List.fold_left(
      ((i, seg), s: Parity.step) => {
        let seg = s.edit(~root, seg);
        run("step " ++ string_of_int(i), seg);
        (i + 1, seg);
      },
      (1, seg),
      steps,
    ),
  );
};

/* typed key by key after [after]'s line: mid-typing states */
let typed = (~root, src, after, keys, ()) => {
  module MC = Test_ModuleCompletion;
  fresh_worker();
  let run = (name, z) => {
    let st =
      statics_of(
        MakeTerm.semantic_term(~root, MakeTerm.semantic_source(z)),
      );
    check_exact(
      name,
      st,
      composed(WorkerServer.receive(post(request_of(st)))),
    );
  };
  let z = MC.at_line(~root, src, after);
  run("untyped", z);
  ignore(
    List.fold_left(
      ((z, typed), c) => {
        let z = MC.perform(~root, z, Insert(c));
        let typed = typed ++ c;
        run(String.escaped(typed), z);
        (z, typed);
      },
      (z, ""),
      List.map(String.make(1), List.of_seq(String.to_seq(keys))),
    ),
  );
};

/* List to `cons` (OpenMember's short_src) */
let list_src = (~root) => Test_OpenMember.short_src(~root);

/* [src]'s module body as the only member of [depth] modules */
let nest = (~depth, src: string): string =>
  List.fold_left(
    (src, i) =>
      "module Nest" ++ string_of_int(i) ++ " = {\n" ++ src ++ "\n}",
    src,
    List.init(depth, i => i),
  );

/* a literal retyped and back */
let back_and_forth = (~at=?, needle, repl) => [
  Parity.token(~at?, ~keep_id=true, needle, repl),
  Parity.token(~at?, ~keep_id=true, repl, needle),
];

let mega = (~root, file, steps) =>
  script(~root, Option.get(CorpusUtil.mega_src(file)), steps);

/* a module's members of every kind, its value their bindings: a type
   member (none), a function, a destructuring, a sub-module, a sealed
   one, a let chain, a test, and a member shadowing one the earlier ones
   read */
let kinds = "type T = Int;
let a : T = 1;
let f(x) = x + a;
let (b, c) = (2, 3);
module N = {
  let d = 4;
  let g(y) = y * d
};
module S : { let e : Int } = { let e = 5 };
let q = 1 in q + a;
test f(1) == 2 end;
let a = a + 10;
let h = N.g(b) + S.e + a";

let kinds_in_module = "module M = {\n" ++ kinds ++ "\n} in\nM.h";

/* [kinds]' edits, its paths under [above] */
let kinds_steps = (~above) =>
  back_and_forth(~at=`First, "1", "7")
  @ back_and_forth(~at=`First, "Int", "Bool")
  @ [
    Parity.token(~at=`First, ~keep_id=true, "4", "6"),
    Parity.token(~at=`First, ~keep_id=true, "5", "8"),
    Parity.op(NewBelow, above @ ["N", "d"]),
    Parity.op(Delete, above @ ["f"]),
    Parity.move(~up=true, above @ ["h"]),
  ];

let exact_tests =
  [
    test_case(
      "Mega 1k: retypes, an insert, a delete, a move, an outdent",
      `Quick,
      mega(
        ~root=Exp,
        "mega-1k.hz",
        Parity.mega_steps @ back_and_forth(~at=`Last, "9", "8"),
      ),
    ),
    test_case(
      "Mega-Mod 1k: the same at a Mod root",
      `Quick,
      mega(
        ~root=Mod,
        "mega-mod-1k.hz",
        Parity.mega_steps @ back_and_forth(~at=`Last, "9", "8"),
      ),
    ),
    test_case(
      "Mega 4k: retypes top, middle and end, and back",
      `Slow,
      mega(
        ~root=Exp,
        "mega-4k.hz",
        back_and_forth(~at=`First, "60", "61")
        @ back_and_forth(~at=`First, "2104", "2105")
        @ back_and_forth(~at=`Last, "87", "88"),
      ),
    ),
    test_case(
      "the List module at a Mod root",
      `Quick,
      script(
        ~root=Mod,
        list_src(~root=Mod),
        back_and_forth(~at=`First, "0", "5")
        @ [
          Parity.op(NewBelow, ["List", "map"]),
          Parity.op(Delete, ["List", "filter"]),
          Parity.op(MoveDown, ["List", "length"]),
          Parity.move(~up=true, ["List", "zip"]),
          Parity.token(~at=`First, "length", "size"),
        ],
      ),
    ),
    test_case(
      "the List module at an Exp root",
      `Quick,
      script(
        ~root=Exp,
        list_src(~root=Exp),
        back_and_forth(~at=`First, "0", "5")
        @ [Parity.op(Duplicate, ["List", "map"])],
      ),
    ),
    test_case(
      "a lib-like program: member edits, adds, removes, moves",
      `Slow,
      script(~root=Mod, Delta.lib, Delta.lib_edits @ Delta.lib_restructures),
    ),
    test_case(
      "the lib-like program in a module at an Exp root",
      `Quick,
      script(
        "module Wrap = {\n" ++ Delta.lib ++ "\n} in\nWrap.List.length([1])",
        [
          Parity.token(~at=`First, ~keep_id=true, "0", "5"),
          Parity.token("reverse", "rev"),
          Parity.op(Delete, ["Wrap", "List", "flat_map"]),
          Parity.op(MoveDown, ["Wrap", "List", "length"]),
        ],
      ),
    ),
    test_case(
      "List one and two modules deep, at a Mod root",
      `Slow,
      () => {
        script(
          ~root=Mod,
          nest(~depth=1, list_src(~root=Mod)),
          back_and_forth(~at=`First, "0", "5")
          @ [Parity.op(NewBelow, ["Nest0", "List", "map"])],
          (),
        );
        script(
          ~root=Mod,
          nest(~depth=2, list_src(~root=Mod)),
          back_and_forth(~at=`Last, "1", "2")
          @ [Parity.op(Delete, ["Nest1", "Nest0", "List", "zip"])],
          (),
        );
      },
    ),
    test_case(
      "Mega-Mod 1k's items as one module's members, two deep",
      `Quick,
      script(
        ~root=Mod,
        nest(~depth=2, Option.get(CorpusUtil.mega_src("mega-mod-1k.hz"))),
        back_and_forth(~at=`Last, "9", "8"),
      ),
    ),
    test_case(
      "a module's members of every kind, at a Mod root",
      `Quick,
      script(~root=Mod, kinds, kinds_steps(~above=[])),
    ),
    test_case(
      "... in a module at an Exp root",
      `Quick,
      script(kinds_in_module, kinds_steps(~above=["M"])),
    ),
    test_case(
      "sealed modules: met, unmet (whole) and met again",
      `Quick,
      script(
        "module M : {type T = Int; let x : T; let y : Int} = {\n  type T = Int;\n  let x = 0;\n  let y = x + 1\n} in\nM.y",
        back_and_forth(~at=`First, "0", "5")
        @ back_and_forth(~at=`Last, "Int", "Bool")
        @ back_and_forth(~at=`Last, "y", "z"),
      ),
    ),
    test_case(
      "a let chain: adds and removes",
      `Quick,
      script(
        "let a = 1 in\nlet b = a + 1 in\ntype T = Int in\nlet c : T = b in\nc",
        [
          Parity.op(NewBelow, ["a"]),
          Parity.op(Delete, ["new_def"]),
          Parity.op(NewTestBelow, ["b"]),
        ],
      ),
    ),
  ]
  @ List.map(
      ((name, root, src, after, keys)) =>
        test_case("typed: " ++ name, `Quick, typed(~root, src, after, keys)),
      Delta.moved_ids,
    )
  @ [
    test_case("edge and mid-typing programs", `Quick, () =>
      List.iter(
        ((name, src)) =>
          switch (CorpusUtil.parse(src)) {
          | Some(seg) =>
            fresh_worker();
            let st = statics_of(MakeTerm.Incr.term_of_root(~root=Exp, seg));
            check_exact(
              name,
              st,
              composed(WorkerServer.receive(post(request_of(st)))),
            );
          | None => ()
          },
        CorpusUtil.edge_programs @ CorpusUtil.incomplete_programs,
      )
    ),
  ];

/* the tile [needle] names first: its term's id */
let token_id = (needle: string, seg: Segment.t): Id.t => {
  let rec go = (seg: Segment.t) =>
    List.find_map(
      (p: Piece.t) =>
        switch (p) {
        | Tile(t) when Tile.label(t) == [needle] => Some(t.id)
        | Tile(t) => List.find_map(go, t.children)
        | _ => None
        },
      seg,
    );
  switch (go(seg)) {
  | Some(id) => id
  | None => fail("no token " ++ needle)
  };
};

/* the item, a member at whatever depth, whose own map has [id] */
let owner = (id: Id.t, ds: DefStatics.t): Id.t => {
  let rec find = (items: list(DefStatics.item)) =>
    List.find_map(
      (it: DefStatics.item) =>
        switch (find(it.d_members)) {
        | Some(_) as found => found
        | None => Id.Map.mem(id, it.d_map) ? Some(it.d_id) : None
        },
      items,
    );
  switch (find(ds.items)) {
  | Some(id) => id
  | None => fail("no item has it")
  };
};

/* a literal retyped in a member: only that member's piece is cut again,
   and only it is posted */
let member_edit = (~root, src, needle, repl, ()) => {
  fresh_worker();
  let term_of = seg => MakeTerm.Incr.term_of_root(~root, seg);
  let seg = CorpusUtil.seg_of(~root, src);
  let st = statics_of(term_of(seg));
  ignore(WorkerServer.receive(post(request_of(st))));
  let id = token_id(needle, seg);
  let seg =
    fst(
      CorpusUtil.edit_token(~at=`First, ~keep_id=true, ~needle, ~repl, seg),
    );
  let st = statics_of(term_of(seg));
  let request = request_of(st);
  let ds = Option.get(DefStatics.cached(term_of(seg)));
  let member = owner(id, ds);
  check(
    bool,
    "a member's piece",
    false,
    List.exists((it: DefStatics.item) => it.d_id == member, ds.items),
  );
  check(
    list(string),
    "re-encoded",
    [EvalPieces.show_key(Item(member))],
    List.map(EvalPieces.show_key, Split.encoded^),
  );
  let posted = post(request);
  check(int, "posted pieces", 1, List.length(posted.pieces));
  check_exact("after", st, composed(WorkerServer.receive(posted)));
};

let encoding_tests = [
  test_case(
    "a member's literal re-encodes only that member: List at a Mod root",
    `Quick,
    member_edit(~root=Mod, list_src(~root=Mod), "0", "5"),
  ),
  test_case(
    "... List two modules deep",
    `Quick,
    member_edit(~root=Mod, nest(~depth=2, list_src(~root=Mod)), "0", "5"),
  ),
  test_case(
    "... List in a module at an Exp root",
    `Quick,
    member_edit(
      ~root=Exp,
      "module Wrap = {\n"
      ++ list_src(~root=Mod)
      ++ "\n} in\nWrap.List.length([1])",
      "0",
      "5",
    ),
  ),
  test_case(
    "... a module sealed with a signature",
    `Quick,
    member_edit(
      ~root=Exp,
      "module M : {let x : Int; let y : Int} = {\n  let x = 0;\n  let y = x + 1\n} in\nM.y",
      "0",
      "5",
    ),
  ),
  /* one member re-encoded is the same claim at 1k; the Mega 4k script
     composes 4k's pieces */
  test_case(
    "... Mega 1k",
    `Quick,
    member_edit(
      ~root=Exp,
      Option.get(CorpusUtil.mega_src("mega-1k.hz")),
      "180",
      "181",
    ),
  ),
];

/* the protocol: whatever the worker holds, it composes the request's
   program or asks for every piece */
let protocol = () => {
  let term_of = seg => MakeTerm.Incr.term_of_root(~root=Mod, seg);
  let seg0 = CorpusUtil.seg_of(~root=Mod, list_src(~root=Mod));
  let edit = (~at, seg, needle, repl) =>
    fst(CorpusUtil.edit_token(~at, ~keep_id=true, ~needle, ~repl, seg));
  /* two members' edits: the second request reuses the first's piece */
  let seg1 = edit(~at=`First, seg0, "0", "5");
  let seg2 = edit(~at=`Last, seg1, "1", "2");
  let st0 = statics_of(term_of(seg0));
  let r0 = request_of(st0);
  let st1 = statics_of(term_of(seg1));
  let r1 = request_of(st1);
  let st2 = statics_of(term_of(seg2));
  let r2 = request_of(st2);
  let missing = (what, r: WorkerServer.received) =>
    switch (r) {
    | Missing([_, ..._]) => ()
    | _ => fail(what ++ ": composed without the piece")
    };
  /* in order: each delta composes */
  fresh_worker();
  check_exact("first", st0, composed(WorkerServer.receive(post(r0))));
  check_exact("delta", st1, composed(WorkerServer.receive(post(r1))));
  /* a restarted worker holds nothing: asked, the client posts all */
  Hashtbl.reset(WorkerServer.held);
  missing("restarted", WorkerServer.receive(post(r2)));
  check_exact(
    "restarted, posted whole",
    st2,
    composed(WorkerServer.receive(post(~full=true, r2))),
  );
  /* a dropped message: the next one names its pieces */
  fresh_worker();
  ignore(WorkerServer.receive(post(r0)));
  /* posted, never received */
  ignore(post(r1));
  missing("after a dropped one", WorkerServer.receive(post(r2)));
  check_exact(
    "after a dropped one, posted whole",
    st2,
    composed(WorkerServer.receive(post(~full=true, r2))),
  );
  /* out of order: the later one first */
  fresh_worker();
  ignore(WorkerServer.receive(post(r0)));
  let p1 = post(r1);
  let p2 = post(r2);
  missing("out of order", WorkerServer.receive(p2));
  ignore(WorkerServer.receive(p1));
  check_exact(
    "out of order, posted whole",
    st2,
    composed(WorkerServer.receive(post(~full=true, r2))),
  );
  /* a piece whose bytes don't match its hash isn't taken */
  fresh_worker();
  ignore(WorkerServer.receive(post(r0)));
  let p1 = post(r1);
  let corrupt = (bytes: string) =>
    String.mapi(
      (i, c) =>
        i == String.length(bytes) - 1 ? Char.chr(Char.code(c) lxor 1) : c,
      bytes,
    );
  missing(
    "a mismatched piece",
    WorkerServer.receive({
      ...p1,
      pieces: List.map(((h, bytes)) => (h, corrupt(bytes)), p1.pieces),
    }),
  );
  check_exact(
    "a mismatched piece, posted whole",
    st1,
    composed(WorkerServer.receive(post(~full=true, r1))),
  );
};

/* evaluation reads the same from the composed program as from the
   whole: the value (the ⇓ probe's), the tests and every probe's samples */
let evaluate = ((expr, eval_info): (Exp.t, EvalInfo.t)) =>
  switch (Evaluator.evaluate(~eval_info, ~env=Builtins.env_init(), expr)) {
  | (value, state) => Ok((value, state))
  | exception exn => Error(Printexc.to_string(exn))
  };

/* values as the worker ships them: closures without their environments
   (which hold ids minted per evaluation) */
let shipped = TermPrune.prune_closure_envs;

/* the projection as the base made it: every use, "$hole" ones too */
let with_holes = (st: CachedStatics.t): (Exp.t, EvalInfo.t) => {
  let w =
    EvalInfo.witnessing(
      ~probe_all=settings.probe_all,
      ~external_ids=[],
      ~targets=st.targets,
    );
  (
    st.elaborated,
    {
      statics:
        Id.Map.filter_map(
          (_, info: Info.t) =>
            switch (info) {
            | InfoExp({co_ctx, _}) =>
              EvalInfo.entry_of_info(w, info)
              |> Option.map((e: EvalInfo.entry) =>
                   {
                     ...e,
                     co_ctx,
                   }
                 )
            | _ => None
            },
          st.info_map,
        ),
      targets: st.targets,
    },
  );
};

/* two evaluations' value, tests and samples */
let compare_results = (name, a, b) => {
  switch (a, b) {
  | (Ok((value, state)), Ok((value', state'))) =>
    check(
      bool,
      name ++ ": value",
      true,
      Exp.fast_equal_with_lexemes(shipped(value), shipped(value')),
    );
    check(
      list(pair(string, string)),
      name ++ ": tests",
      Parity.tests_of(state),
      Parity.tests_of(state'),
    );
    let samples = (s: EvaluatorState.t) =>
      Id.Map.bindings(s.probes)
      |> List.map(((id, samples)) =>
           (
             Id.show(id),
             List.map((x: Sample.t) => shipped(x.value), samples),
           )
         );
    let (ps, ps') = (samples(state), samples(state'));
    check(
      list(string),
      name ++ ": probes",
      List.map(fst, ps),
      List.map(fst, ps'),
    );
    List.iter2(
      ((id, xs), (_, ys)) =>
        check(
          bool,
          name ++ ": samples at " ++ id,
          true,
          List.length(xs) == List.length(ys)
          && List.for_all2(Exp.fast_equal_with_lexemes, xs, ys),
        ),
      ps,
      ps',
    );
  | (Error(e), Error(e')) => check(string, name ++ ": error", e, e')
  | _ => fail(name ++ ": one evaluated, the other raised")
  };
};

let same_results = (name, st: CachedStatics.t) => {
  fresh_worker();
  let v = composed(WorkerServer.receive(post(request_of(st))));
  let as_whole = evaluate(whole(st));
  /* as before the split: the base's projection, whole */
  compare_results(
    name ++ " (without $hole)",
    evaluate(with_holes(st)),
    as_whole,
  );
  compare_results(
    name ++ " (in pieces)",
    as_whole,
    evaluate((v.expr, v.eval_info_map)),
  );
};

let results = () => {
  let n = ref(0);
  List.iter(
    ((path, root, src)) =>
      switch (CorpusUtil.parse(~root, src)) {
      | Some(seg) =>
        same_results(
          path,
          statics_of(MakeTerm.Incr.term_of_root(~root, seg)),
        );
        incr(n);
      | None => ()
      },
    CorpusUtil.slide_corpus(),
  );
  check(bool, "the corpus was there", true, n^ >= 40);
  /* Mega-Mod 1k is Mega 1k at a Mod root, which List and lib cover: its
     ⇓ probe samples the whole module, ~20s to evaluate and compare */
  List.iter(
    ((name, root, src)) =>
      same_results(
        name,
        statics_of(
          MakeTerm.Incr.term_of_root(~root, CorpusUtil.seg_of(~root, src)),
        ),
      ),
    [
      ("List (Mod)", Sort.Mod, list_src(~root=Mod)),
      ("lib (Mod)", Sort.Mod, Delta.lib),
      ("members of every kind (Mod)", Sort.Mod, kinds),
      ("members of every kind (Exp)", Sort.Exp, kinds_in_module),
      ("Mega 1k", Sort.Exp, Option.get(CorpusUtil.mega_src("mega-1k.hz"))),
    ],
  );
};

let world_src = "let a = 1;
module Documentation = {
  let helper = a + 1;
  let unused = 0;
  module Holes : {} = {
    test false end;
    1
  };
  module Intro : {} = {
    let y = helper;
    test y == 2 end;
    Holes;
    y
  }
};
module Scratch = {
  let z = 0
}";

/* the ids of the modules on [path] (names from the top level down) in a
   world's elaboration */
let id_in = (path, e: Exp.t) => {
  let items = (e: Exp.t) =>
    switch (e.term) {
    | Module(items) => items
    | _ => fail("not a module")
    };
  List.fold_left(
    ((ids, e), name) => {
      let i = List.find(i => List.mem(name, WorldTree.binds(i)), items(e));
      ([Mod.rep_id(i), ...ids], Option.get(WorldTree.def_of(i)));
    },
    ([], e),
    path,
  )
  |> fst
  |> List.rev;
};

/* [seg]'s world run zoomed to the module ids [zoom] gives it: its
   statics as cut, analyzed in a world's context as the editor does, and
   its request in pieces as the worker composes it */
let world_run_at = (~zoom, seg) => {
  let st =
    statics_of(
      ~ctx_in=Web.WorldSlide.ctx(),
      MakeTerm.Incr.term_of_root(~root=Mod, seg),
    );
  let (rs, cut) = Web.WorldSlide.run_statics(~zoom=zoom(st), st);
  check(bool, "cut", true, Option.is_some(cut));
  let request: WorkerServer.Request.t = {
    request_id: 1,
    batch: [
      (
        "",
        {
          program:
            Split.program(
              ~probe_all=settings.probe_all,
              ~targets=rs.targets,
              ~run=?cut,
              rs,
            ),
          prev: UseResident,
          stream: Full,
        },
      ),
    ],
  };
  let encoded = Split.encoded^;
  (st, rs, encoded, composed(WorkerServer.receive(post(request))));
};

/* zoomed to the modules named on [zoom], from the top level down */
let world_run = (~zoom, seg) =>
  world_run_at(
    ~zoom=(st: CachedStatics.t) => id_in(zoom, st.elaborated),
    seg,
  );

/* a world's run (WorldSlide.run_statics), its request cut alike: the
   worker composes what runs, with the eval info a whole request of it
   carries, and what doesn't run is never encoded */
let world = () => {
  fresh_worker();
  let run = (name, ~zoom, seg) => {
    let (st, rs, encoded, v) = world_run(~zoom, seg);
    let {same} = same_graph();
    check(option(string), name ++ ": in pieces", None, Split.went_whole^);
    check(bool, name ++ ": what runs", true, same(rs.elaborated, v.expr));
    let (_, info) = whole(rs);
    check(
      list(string),
      name ++ ": the run's eval info",
      [],
      Id.Map.fold(
        (id, e, acc) =>
          switch (Id.Map.find_opt(id, v.eval_info_map.statics)) {
          | Some(e') when same(e, e') => acc
          | _ => [Id.show(id), ...acc]
          },
        info.statics,
        [],
      ),
    );
    (st, encoded);
  };
  let item_key = (st: CachedStatics.t, path): EvalPieces.key =>
    Item(List.nth(id_in(path, st.elaborated), List.length(path) - 1));
  let has = (key, encoded) => List.mem(key, encoded);
  let seg = CorpusUtil.seg_of(~root=Mod, world_src);
  let intro = ["Documentation", "Intro"];
  let (st, encoded) = run("the slide", ~zoom=intro, seg);
  let holes = List.nth(id_in(["Documentation", "Holes"], st.elaborated), 1);
  check(
    bool,
    "the sealed one, empty",
    true,
    has(Empty(holes): EvalPieces.key, encoded),
  );
  check(
    bool,
    "what nothing mentions, unencoded",
    false,
    has(item_key(st, ["Documentation", "unused"]), encoded),
  );
  let seg =
    Parity.token(~at=`First, ~keep_id=true, "2", "3").edit(~root=Mod, seg);
  let (st, encoded) = run("the slide edited", ~zoom=intro, seg);
  check(
    bool,
    "what it mentions, kept",
    false,
    has(item_key(st, ["Documentation", "helper"]), encoded),
  );
  let seg =
    Parity.token(~at=`First, ~keep_id=true, "0", "5").edit(~root=Mod, seg);
  let (st, encoded) = run("what doesn't run, edited", ~zoom=intro, seg);
  check(
    bool,
    "still unencoded",
    false,
    has(item_key(st, ["Documentation", "unused"]), encoded),
  );
  let (_, encoded) = run("the folder", ~zoom=["Documentation"], seg);
  check(
    bool,
    "the sealed one, whole",
    true,
    has(Item(holes): EvalPieces.key, encoded),
  );
  ignore(run("the whole program", ~zoom=[], seg));
};

/* zoomed into a slide, then out to its folder, the worker's results kept
   between: the folder's are a cold run's, a module the first run cut or
   emptied not reused (the evaluator reuses a result while its entry's
   elab_term is the same), whether the request went in pieces or whole */
let world_zoom_out = () => {
  let seg = CorpusUtil.seg_of(~root=Mod, world_src);
  let evaluate = (~prev=?, (expr, info)) => {
    let (_, state) =
      Evaluator.evaluate(
        ~prev?,
        ~eval_info=info,
        ~env=Builtins.env_init(),
        expr,
      );
    (Parity.tests_of(state), state.incr_eval);
  };
  fresh_worker();
  let (_, slide, _, slide_v) =
    world_run(~zoom=["Documentation", "Intro"], seg);
  let (_, folder, _, folder_v) = world_run(~zoom=["Documentation"], seg);
  let (cold, _) = evaluate(whole(folder));
  check(int, "the folder's tests", 2, List.length(cold));
  let (_, prev) = evaluate((slide_v.expr, slide_v.eval_info_map));
  check(
    list(pair(string, string)),
    "in pieces",
    cold,
    fst(evaluate(~prev, (folder_v.expr, folder_v.eval_info_map))),
  );
  let (_, prev) = evaluate(whole(slide));
  check(
    list(pair(string, string)),
    "whole",
    cold,
    fst(evaluate(~prev, whole(folder))),
  );
};

/* the shipped world zoomed into its first slide: requests go in pieces,
   an edit re-encoding a few */
let shipped_world = () =>
  Test_WorldSlide.shipped(() => {
    module Deck = Web.WorldDeck;
    Web.HazelDB.kv_clear();
    fresh_worker();
    let m = Deck.load(~settings);
    let (name, code) =
      switch (List.nth(m.scratchpads, m.current)) {
      | {name, kind: Code(code), _} => (name, code)
      | _ => fail("no code")
      };
    let path = Deck.path(name);
    let zoom = _ =>
      List.init(List.length(path) - 1, i =>
        WorldTree.folder_id(Util.ListUtil.take(i + 1, path))
      )
      @ [Deck.slide_id(name)];
    let run = (label, seg) => {
      let (_, rs, encoded, v) = world_run_at(~zoom, seg);
      let {same} = same_graph();
      check(option(string), label ++ ": in pieces", None, Split.went_whole^);
      check(bool, label ++ ": what runs", true, same(rs.elaborated, v.expr));
      encoded;
    };
    let seg = Web.Program.document(code.program);
    ignore(run("loaded", seg));
    let encoded =
      run(
        "edited",
        Parity.token(~at=`First, ~keep_id=true, "1", "7").edit(
          ~root=Mod,
          seg,
        ),
      );
    check(bool, "a few pieces encoded", true, List.length(encoded) <= 8);
  });

let tests = (
  "EvalPieces",
  exact_tests
  @ encoding_tests
  @ [
    test_case("a world's run, cut alike", `Quick, world),
    test_case("the shipped world in pieces", `Slow, shipped_world),
    test_case(
      "a world zoomed out reuses nothing it cut",
      `Quick,
      world_zoom_out,
    ),
    test_case(
      "missing, restarted, dropped, out of order, mismatched",
      `Quick,
      protocol,
    ),
    test_case(
      "evaluation reads the same: tests, probes, the value",
      `Slow,
      results,
    ),
  ],
);
