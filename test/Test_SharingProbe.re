/* (probe, ^ticket(666)) What the worker boundary costs with ShareCodec, on
   Test_ShareCodec's program of n definitions: the bytes Marshal sends with
   and without sharing, a first send, the same value again, and after an
   edit to one literal (ids kept, as an edit does); the ms of each; and
   whether the value sent again decodes to the very object it did before.
   Prints one line per size; asserts nothing. */
open Test_ShareCodec;

let ms = (f: unit => 'a): ('a, float) => {
  let t0 = Sys.time();
  let r = f();
  (r, (Sys.time() -. t0) *. 1000.);
};

let raw = (v: Language.Exp.t): int =>
  String.length(Marshal.to_string(v, []));

let run = (n: int) => {
  reset();
  let e = elab(program(n));
  let ((b1, d1), t1) = ms(() => send(e));
  let ((b2, d2), t2) = ms(() => send(e));
  let e' = edited(e);
  let ((b3, _), t3) = ms(() => send(e'));
  Printf.printf(
    "SHARE n=%d: raw %d B | first %d B %.1f ms | again %d B %.1f ms, same object %b | edited: raw %d B, sent %d B %.1f ms\n",
    n,
    raw(e),
    b1,
    t1,
    b2,
    t2,
    d1 === d2,
    raw(e'),
    b3,
    t3,
  );
};

/* one Int literal changed, the first one found, ids kept */
let bump = (e: Language.Exp.t): Language.Exp.t => {
  let seen = ref(false);
  Language.TermBase.Exp.map_term(
    ~f_exp=
      (continue, d: Language.Exp.t) =>
        switch (d.term) {
        | Atom(Int(n)) when ! seen^ =>
          seen := true;
          {
            ...d,
            term: Atom(Int(Bigint.of_int(Bigint.to_int_exn(n) + 1))),
          };
        | _ => continue(d)
        },
    e,
  );
};

/* the worker's reply: its sender's tables encode, the page's decode, as a
   request's do (WorkerServer: Client.encode), so here the same send, from
   tables reset for it */
let reply = send;

/* a slide's program, by its file: the request (its elaborated program) and
   the reply (its value), each first, again, and after an edit */
let run_file = (name: string, path: string) => {
  reset();
  let text = In_channel.with_open_bin(path, In_channel.input_all);
  let e = elab(text);
  let ((b1, _), t1) = ms(() => send(e));
  let ((b2, _), t2) = ms(() => send(e));
  let ((b3, _), t3) = ms(() => send(bump(e)));
  Printf.printf(
    "SHARE %s request: raw %d B | first %d B %.1f ms | again %d B %.1f ms | edited %d B %.1f ms\n",
    name,
    raw(e),
    b1,
    t1,
    b2,
    t2,
    b3,
    t3,
  );
  let value = exp => {
    let (v, _, _) = Test_PerfBudgets.work(exp);
    v;
  };
  reset();
  let exp = Test_Evaluator_Prelude.parse_exp(text);
  let v = value(exp);
  let ((r1, c1), u1) = ms(() => reply(v));
  let ((r2, c2), u2) = ms(() => reply(v));
  let v' = value(bump(exp));
  let ((r3, _), u3) = ms(() => reply(v'));
  Printf.printf(
    "SHARE %s reply: raw %d B | first %d B %.1f ms | again %d B %.1f ms, same object %b | edited: raw %d B, sent %d B %.1f ms\n",
    name,
    raw(v),
    r1,
    u1,
    r2,
    u2,
    c1 === c2,
    raw(v'),
    r3,
    u3,
  );
};

/* (^ticket(666), 11-03) The request as the app posts it, not the whole
   program: cut into pieces (EvalSplit), the pieces the worker holds left
   out (WorkerClient.posted_of), and shared as WorkerClient.post_evaluate
   shares them, against the same pieces plain. The steps as the editor
   makes them: first, the same program again, the first numeral retyped
   (its id kept), the undo (the old segment again) and the redo. */
module Pieces = Test_EvalPieces;

let now = Util.JsUtil.precise_timestamp;
let timed = (f: unit => 'a): ('a, float) => {
  let t0 = now();
  let r = f();
  (r, now() -. t0);
};

let first_numeral = (seg: Haz3lcore.Segment.t): option(string) => {
  open Haz3lcore;
  let is_num = s => s != "" && String.for_all(c => c >= '0' && c <= '9', s);
  let rec go = (seg: Segment.t): option(string) =>
    List.fold_left(
      (found, p: Piece.t) =>
        switch (found, p) {
        | (Some(_), _) => found
        | (None, Tile(t)) =>
          switch (Tile.label(t)) {
          | [s] when is_num(s) => Some(s)
          | _ =>
            List.fold_left(
              (f, kid) => f == None ? go(kid) : f,
              None,
              t.children,
            )
          }
        | (None, _) => None
        },
      None,
      seg,
    );
  go(seg);
};

/* one request posted as the app posts it, and taken in by the worker */
let post_request = (~reset, ~id, st: Haz3lcore.CachedStatics.t) => {
  let (r, cut) =
    timed(() => {
      let r: WorkerServer.Request.t = {
        ...Pieces.request_of(st),
        request_id: id,
      };
      r;
    });
  let cut_new = List.length(Web.EvalSplit.encoded^);
  let size = e =>
    Core.Byte_units.bytes_int_exn(WorkerServer.Active.size_request(e));
  let (plain, t_plain) =
    timed(() =>
      size(
        WorkerServer.Active.encode_request(
          Evaluate(Web.WorkerClient.posted_of(~full=false, r)),
        ),
      )
    );
  if (reset) {
    Util.ShareCodec.Client.reset();
  };
  let (encoded, t_shared) =
    timed(() =>
      WorkerServer.Active.encode_request(
        EvaluateShared(
          reset,
          Web.WorkerClient.posted_of(~full=false, ~shared=true, r),
        ),
      )
    );
  Web.WorkerClient.hold(r);
  let (composed, t_worker) =
    timed(() =>
      switch (WorkerServer.Active.decode_request(encoded)) {
      | EvaluateShared(reset, p) =>
        if (reset) {
          Util.ShareCodec.Worker.reset();
        };
        switch (WorkerServer.receive(p)) {
        | Composed(_) => true
        | _ => false
        };
      | _ => false
      }
    );
  (
    cut,
    cut_new,
    plain,
    t_plain,
    size(encoded),
    t_shared,
    t_worker,
    composed,
  );
};

/* (^ticket(666)) why EvalSplit's count of keys fails ("pieces hold other
   keys than the statics"): the statics' keys no top-level item's map
   holds, and the items' keys the statics lack, each by its class, with
   the commonest first */
let unaccounted = (st: Haz3lcore.CachedStatics.t): string =>
  switch (Haz3lcore.DefStatics.cached(st.term)) {
  | None => " | no analysis to compare"
  | Some(ds) =>
    let held = Hashtbl.create(4096);
    List.iter(
      (it: Haz3lcore.DefStatics.item) =>
        Language.Id.Map.iter(
          (id, info) => Hashtbl.replace(held, id, info),
          it.d_map,
        ),
      ds.items,
    );
    let tally = (pairs: list(Language.Info.t)) => {
      let t = Hashtbl.create(16);
      List.iter(
        info => {
          let k = Language.Cls.show(Language.Info.cls_of(info));
          Hashtbl.replace(
            t,
            k,
            1 + Option.value(Hashtbl.find_opt(t, k), ~default=0),
          );
        },
        pairs,
      );
      Hashtbl.fold((k, v, acc) => [(v, k), ...acc], t, [])
      |> List.sort((a, b) => compare(b, a))
      |> Util.ListUtil.take(8)
      |> List.map(((v, k)) => Printf.sprintf("%s %d", k, v))
      |> String.concat(", ");
    };
    /* keys in more than one item's map, by class: counted twice */
    let seen = Hashtbl.create(4096);
    let twice = ref([]);
    List.iter(
      (it: Haz3lcore.DefStatics.item) =>
        Language.Id.Map.iter(
          (id, info) =>
            if (Hashtbl.mem(seen, id)) {
              twice := [info, ...twice^];
            } else {
              Hashtbl.replace(seen, id, ());
            },
          it.d_map,
        ),
      ds.items,
    );
    let (counted, actual) = Web.EvalSplit.last_count^;
    let missing =
      Language.Id.Map.fold(
        (id, info, acc) => Hashtbl.mem(held, id) ? acc : [info, ...acc],
        st.info_map,
        [],
      );
    let extra =
      Hashtbl.fold(
        (id, info, acc) =>
          Language.Id.Map.mem(id, st.info_map) ? acc : [info, ...acc],
        held,
        [],
      );
    Printf.sprintf(
      " | statics %d keys, items %d: in the statics, in no item: %d (%s); in an item, not the statics: %d (%s) | the pieces counted %d against %d; in two items or more: %d (%s)",
      Language.Id.Map.cardinal(st.info_map),
      Hashtbl.length(held),
      List.length(missing),
      tally(missing),
      List.length(extra),
      tally(extra),
      counted,
      actual,
      List.length(twice^),
      tally(twice^),
    );
  };

let request_steps = (name: string, text: string) => {
  open Haz3lcore;
  let term_of = seg => MakeTerm.Incr.term_of_root(~root=Exp, seg);
  Pieces.fresh_worker();
  let seg0 = CorpusUtil.seg_of(~root=Exp, text);
  let seg1 =
    switch (first_numeral(seg0)) {
    | Some(s) =>
      fst(
        CorpusUtil.edit_token(
          ~at=`First,
          ~keep_id=true,
          ~needle=s,
          ~repl=string_of_int(int_of_string(s) + 1),
          seg0,
        ),
      )
    | None => Alcotest.fail(name ++ ": no numeral to retype")
    };
  /* (^ticket(666), why the cut is redone) the top-level items of this
     analysis against the last one's, by id: which are new objects */
  let last: ref(option(DefStatics.t)) = ref(None);
  let items_against_last = (ds: option(DefStatics.t)) =>
    switch (last^, ds) {
    | (Some(old), Some(ds)) =>
      let olds =
        List.map((it: DefStatics.item) => (it.d_id, it), old.items);
      let (n, elab, hole, map) =
        List.fold_left(
          ((n, e, h, m), it: DefStatics.item) =>
            switch (List.assoc_opt(it.d_id, olds)) {
            | Some(o) => (
                n + 1,
                e + (o.d_elab === it.d_elab ? 0 : 1),
                h + (o.d_hole == it.d_hole ? 0 : 1),
                m + (o.d_map === it.d_map ? 0 : 1),
              )
            | None => (n, e, h, m)
            },
          (0, 0, 0, 0),
          ds.items,
        );
      Printf.sprintf(
        " | items %d (%d as before by id): a new elab %d, a new hole id %d, a new map %d",
        List.length(ds.items),
        n,
        elab,
        hole,
        map,
      );
    | _ => ""
    };
  let recut = () => {
    let reasons =
      Hashtbl.fold(
        (k, v, acc) => [(k, v), ...acc],
        Web.EvalSplit.recut,
        [],
      )
      |> List.sort(compare);
    Hashtbl.reset(Web.EvalSplit.recut);
    (
      switch (Web.EvalSplit.went_whole^) {
      | Some(why) => " | WHOLE: " ++ why
      | None => ""
      }
    )
    ++ (
      reasons == []
        ? ""
        : " | cut again because: "
          ++ String.concat(
               ", ",
               List.map(
                 ((k, v)) => Printf.sprintf("%s %d", k, v),
                 reasons,
               ),
             )
    );
  };
  let step = (label, ~reset, ~id, seg) => {
    let (st, statics) = timed(() => Pieces.statics_of(term_of(seg)));
    let ds = DefStatics.cached(st.term);
    let items = items_against_last(ds);
    last := ds;
    Hashtbl.reset(Web.EvalSplit.recut);
    let (cut, cut_new, plain, t_plain, shared, t_shared, t_worker, ok) =
      post_request(~reset, ~id, st);
    let recut = recut();
    let keys =
      Web.EvalSplit.went_whole^ != None
      && (label == "first" || label == "edit")
        ? unaccounted(st) : "";
    Printf.printf(
      "SHARE-WHY %s %s:%s%s%s\n%!",
      name,
      label,
      recut,
      items,
      keys,
    );
    Printf.printf(
      "SHARE-REQ %s %s: statics %.0f ms | cut %.1f ms, %d pieces cut anew | posted plain %d B %.1f ms | shared %d B %.1f ms | worker takes it in %.1f ms%s\n%!",
      name,
      label,
      statics,
      cut,
      cut_new,
      plain,
      t_plain,
      shared,
      t_shared,
      t_worker,
      ok ? "" : " (NOT COMPOSED)",
    );
  };
  step("first", ~reset=true, ~id=1, seg0);
  step("again", ~reset=false, ~id=2, seg0);
  step("edit", ~reset=false, ~id=3, seg1);
  step("undo", ~reset=false, ~id=4, seg0);
  step("redo", ~reset=false, ~id=5, seg1);
};

/* how many of [next]'s blocks (JS arrays) are the very blocks of [prev],
   of how many it has: what the page's identity-keyed memos can hit */
let identity: (Language.Exp.t, Language.Exp.t) => (int, int) =
  (prev, next) =>
    Js_of_ocaml.Js.Unsafe.fun_call(
      Js_of_ocaml.Js.Unsafe.pure_js_expr(
        {js|(function (a, b) {
  const old = new Set(), todo = [a];
  while (todo.length) {
    const v = todo.pop();
    if (!Array.isArray(v) || old.has(v)) continue;
    old.add(v);
    for (let i = 1; i < v.length; i++) todo.push(v[i]);
  }
  const mine = new Set(), next = [b];
  let same = 0;
  while (next.length) {
    const v = next.pop();
    if (!Array.isArray(v) || mine.has(v)) continue;
    mine.add(v);
    if (old.has(v)) same++;
    for (let i = 1; i < v.length; i++) next.push(v[i]);
  }
  return [0, same, mine.size];
})|js},
      ),
      [|
        Js_of_ocaml.Js.Unsafe.inject(prev),
        Js_of_ocaml.Js.Unsafe.inject(next),
      |],
    );

/* the worker's side (hash, walk, Marshal) and the page's (unmarshal,
   resolve, and hash what it keeps), timed apart */
let send_split = (v: Language.Exp.t) => {
  let (wire, enc) =
    timed(() => Marshal.to_string(Util.ShareCodec.Client.encode(v), []));
  let (d, dec) =
    timed(() => Util.ShareCodec.Worker.decode(Marshal.from_string(wire, 0)));
  (String.length(wire), d, enc, dec);
};

/* the reply: the value of the program, then of the edited one and of the
   old one again (the undo), each evaluated with the last one's
   incremental cache as the worker's is, and sent as WorkerServer
   shares a response */
let reply_steps = (name: string, text: string) => {
  reset();
  let exp0 = Test_Evaluator_Prelude.parse_exp(text);
  let exp1 = bump(exp0);
  let ((v0, inc0, _), w0) = timed(() => Test_PerfBudgets.work(exp0));
  let (b0, d0, e0, c0) = send_split(v0);
  let ((v1, inc1, _), w1) =
    timed(() => Test_PerfBudgets.work(~prev=inc0, exp1));
  let (b1, d1, e1, c1) = send_split(v1);
  let ((v2, _, _), w2) =
    timed(() => Test_PerfBudgets.work(~prev=inc1, exp0));
  let (b2, d2, e2, c2) = send_split(v2);
  let (s1, n1) = identity(d0, d1);
  let (s2, n2) = identity(d1, d2);
  let (s20, _) = identity(d0, d2);
  Printf.printf(
    "SHARE-REPLY %s: first: evaluate %.0f ms, %d B, worker encodes %.1f ms, page decodes %.1f ms | edit: evaluate %.0f ms, %d B, encode %.1f, decode %.1f, %d of %d blocks the very ones decoded before | undo: evaluate %.0f ms, %d B, encode %.1f, decode %.1f, %d of %d blocks kept from the edit, %d from the first | raw %d B\n%!",
    name,
    w0,
    b0,
    e0,
    c0,
    w1,
    b1,
    e1,
    c1,
    s1,
    n1,
    w2,
    b2,
    e2,
    c2,
    s2,
    n2,
    s20,
    raw(v0),
  );
};

let slides = [
  ("emotion", "hazel-programs/docs/livelits/emotion.hz"),
  ("emotion-kids", "hazel-programs/docs/livelits/playground/emotion-kids.hz"),
  ("full-body", "hazel-programs/docs/livelits/playground/full-body.hz"),
];
let read = path => In_channel.with_open_bin(path, In_channel.input_all);

let tests = (
  "SharingProbe",
  [
    Alcotest.test_case(
      "the app's request in pieces, shared and plain",
      `Quick,
      () => {
        List.iter(
          ((name, path)) => request_steps(name, read(path)),
          slides,
        );
        request_steps(
          "mega-1k",
          Option.get(CorpusUtil.mega_src("mega-1k.hz")),
        );
      },
    ),
    Alcotest.test_case("the reply, and what the page keeps", `Quick, () =>
      List.iter(((name, path)) => reply_steps(name, read(path)), slides)
    ),
    Alcotest.test_case("bytes and time across the boundary", `Quick, () =>
      List.iter(run, [50, 200, 800])
    ),
    Alcotest.test_case(
      "a slide's program and its value",
      `Quick,
      () => {
        run_file("emotion", "hazel-programs/docs/livelits/emotion.hz");
        run_file(
          "full-body",
          "hazel-programs/docs/livelits/playground/full-body.hz",
        );
      },
    ),
  ],
);
