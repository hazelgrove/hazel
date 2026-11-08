/* (^report(46), ^ticket(694)) What the worker's answer holds. While the page
   keeps its benchmark timeline (Util.BenchMarks), each large message from
   the worker is kept as it arrived; globalThis.hazelAnswerParts(), called by
   the Firefox check once an edit's timing is done, breaks each down by its
   parts' sizes, as they crossed: Util.ShareCodec's references to values the
   page already held count as nothing ("held"), and a part's size is its own
   Marshal (so parts that share values each count them). It returns JSON,
   and forgets the messages. */
open WorkerServer;

let min_bytes = 100_000;
let kept: ref(list(Active.response)) = ref([]);

/* a reference as Util.ShareCodec makes one: [0, MARK, hash, hash] */
let is_ref_fn =
  lazy(
    Js.Unsafe.js_expr(
      {js|(function (v) {
  return Array.isArray(v) && v.length === 4 && v[1] === "\u0001hazel-share-ref";
})|js},
    )
  );
let is_ref = (v: 'a): bool =>
  Js.to_bool(
    Js.Unsafe.fun_call(Lazy.force(is_ref_fn), [|Js.Unsafe.inject(v)|]),
  );

let size = (v: 'a): int => String.length(Marshal.to_string(v, []));

/* a list as it crossed: from a cell the page held, the rest is a reference */
let iter_list = (~held: ref(int), f: Obj.t => unit, l: Obj.t): unit => {
  let cur = ref(l);
  while (!Obj.is_int(cur^)) {
    if (is_ref(cur^)) {
      incr(held);
      cur := Obj.repr(0);
    } else {
      f(Obj.field(cur^, 0));
      cur := Obj.field(cur^, 1);
    };
  };
};

/* a Stdlib map (Node: l, v, d, r, h) as it crossed */
let rec iter_map =
        (~held: ref(int), f: (Obj.t, Obj.t) => unit, m: Obj.t): unit =>
  if (Obj.is_int(m)) {
    ();
  } else if (is_ref(m)) {
    incr(held);
  } else {
    iter_map(~held, f, Obj.field(m, 0));
    f(Obj.field(m, 1), Obj.field(m, 2));
    iter_map(~held, f, Obj.field(m, 3));
  };

/* sizes by name, summed, with a count */
let add = (t: Hashtbl.t(string, (int, int)), k: string, b: int): unit => {
  let (b0, n0) = Option.value(Hashtbl.find_opt(t, k), ~default=(0, 0));
  Hashtbl.replace(t, k, (b0 + b, n0 + 1));
};
let json_of_sums = (t: Hashtbl.t(string, (int, int))): Yojson.Safe.t =>
  `List(
    Hashtbl.fold((k, (b, n), acc) => [(k, b, n), ...acc], t, [])
    |> List.sort(((_, a, _), (_, b, _)) => compare(b, a))
    |> List.map(((k, b, n)) =>
         `Assoc([
           ("part", `String(k)),
           ("bytes", `Int(b)),
           ("n", `Int(n)),
         ])
       ),
  );

/* one message's walk */
type walk = {
  held: ref(int),
  parts: Hashtbl.t(string, (int, int)),
  /* each probe: its samples, and their size */
  probes: Hashtbl.t(string, (int, int)),
  /* the samples' fields, gathered (Marshal'd together, so shared once) */
  values: ref(list(Obj.t)),
  envs: ref(list(Obj.t)),
  stacks: ref(list(Obj.t)),
  args: ref(list(Obj.t)),
  frames: ref(list(Obj.t)),
  samples: ref(int),
  samples_held: ref(int),
  /* the observation trace: its events by kind, gathered */
  events: Hashtbl.t(string, ref(list(Obj.t))),
  close_values: ref(list(Obj.t)),
  close_envs: ref(list(Obj.t)),
};

let gather = (r: ref(list(Obj.t)), v: Obj.t) => r := [v, ...r^];

let sample = (w: walk, o: Obj.t) =>
  if (is_ref(o)) {
    incr(w.samples_held);
  } else {
    let s: Language.Sample.t = Obj.obj(o);
    incr(w.samples);
    gather(w.values, Obj.repr(s.value));
    gather(w.envs, Obj.repr(s.env));
    gather(w.stacks, Obj.repr(s.call_stack));
    gather(w.args, Obj.repr(s.args));
    gather(w.frames, Obj.repr(s.frame));
  };

let probes = (w: walk, m: Obj.t) =>
  iter_map(
    ~held=w.held,
    (k, samples) => {
      let n = ref(0);
      iter_list(
        ~held=w.held,
        o => {
          incr(n);
          sample(w, o);
        },
        samples,
      );
      let id = Util.Id.show(Obj.obj(k));
      let (b0, n0) =
        Option.value(Hashtbl.find_opt(w.probes, id), ~default=(0, 0));
      Hashtbl.replace(w.probes, id, (b0 + size(samples), n0 + n^));
    },
    m,
  );

let trace = (w: walk, l: Obj.t) =>
  iter_list(
    ~held=w.held,
    o =>
      if (is_ref(o)) {
        incr(w.held);
      } else {
        let kind =
          switch ((Obj.obj(o): Language.ObsTrace.event)) {
          | SpanOpen(_) => "span open"
          | SpanClose({value, env, _}) =>
            gather(w.close_values, Obj.repr(value));
            gather(w.close_envs, Obj.repr(env));
            "span close";
          | CallEnter(_) => "call enter"
          | Minted(_) => "minted"
          };
        let r =
          switch (Hashtbl.find_opt(w.events, kind)) {
          | Some(r) => r
          | None =>
            let r = ref([]);
            Hashtbl.replace(w.events, kind, r);
            r;
          };
        gather(r, o);
      },
    l,
  );

/* a state's fields, under [where] */
let state = (w: walk, ~where: string, o: Obj.t) =>
  if (is_ref(o)) {
    add(w.parts, where ++ " (held)", 0);
  } else {
    let st: Language.EvaluatorState.t = Obj.obj(o);
    add(w.parts, where ++ ".theorems", size(st.theorems));
    add(w.parts, where ++ ".tests", size(st.tests));
    add(w.parts, where ++ ".probes", size(st.probes));
    add(w.parts, where ++ ".obs_opens", size(st.obs_opens));
    add(w.parts, where ++ ".obs_trace", size(st.obs_trace));
    add(w.parts, where ++ ".incr_eval", size(st.incr_eval));
    probes(w, Obj.repr(st.probes));
    trace(w, Obj.repr(st.obs_trace));
  };

let entries = (w: walk, ~where: string, o: Obj.t) =>
  if (is_ref(o)) {
    add(w.parts, where ++ " (held)", 0);
  } else {
    let t: Language.IncrEval.t(Language.EvaluatorState.t) = Obj.obj(o);
    add(w.parts, where, size(t.entries));
    iter_map(
      ~held=w.held,
      (_, e) =>
        if (is_ref(e)) {
          incr(w.held);
        } else {
          let e: Language.IncrEval.entry(Language.EvaluatorState.t) =
            Obj.obj(e);
          add(w.parts, where ++ " entry.value", size(e.value));
          add(w.parts, where ++ " entry.prev_elab", size(e.prev_elab));
          state(w, ~where=where ++ " entry.state", Obj.repr(e.state));
        },
      Obj.repr(t.entries),
    );
  };

let outbox = (w: walk, ~where: string, o: Obj.t) =>
  if (is_ref(o)) {
    add(w.parts, where ++ " (held)", 0);
  } else {
    let u: ServerMessage.stream_update = Obj.obj(o);
    entries(w, ~where=where ++ ".completed", Obj.repr(u.completed));
    switch (u.current) {
    | Some(c) when !is_ref(c) =>
      state(w, ~where=where ++ ".current.state", Obj.repr(c.state))
    | _ => ()
    };
  };

let streams = (w: walk, ~where: string, l: Obj.t) =>
  iter_list(
    ~held=w.held,
    o =>
      if (is_ref(o)) {
        incr(w.held);
      } else {
        let s: ServerMessage.stream = Obj.obj(o);
        add(w.parts, where, size(s.update));
        outbox(w, ~where, Obj.repr(s.update));
      },
    l,
  );

let response = (w: walk, l: Obj.t) =>
  iter_list(
    ~held=w.held,
    o =>
      if (is_ref(o)) {
        incr(w.held);
      } else {
        let (_key, v): (string, Response.value) = Obj.obj(o);
        if (is_ref(v)) {
          add(w.parts, "result (held)", 0);
        } else {
          switch (v) {
          | Ok(pair) when !is_ref(pair) =>
            let (exp, st) = pair;
            add(w.parts, "result.value", size(exp));
            state(w, ~where="result.state", Obj.repr(st));
          | Ok(_) => add(w.parts, "result (held)", 0)
          | Error(e) => add(w.parts, "result.error", size(e))
          };
        };
      },
    l,
  );

let breakdown = (raw: Active.response): Yojson.Safe.t => {
  let bytes =
    int_of_float(Core.Byte_units.bytes_float(Active.size_response(raw)));
  let msg = Active.decode_response(raw);
  let (reset, inner) =
    switch (msg) {
    | ServerMessage.Shared(reset, m) => (Some(reset), m)
    | m => (None, m)
    };
  let w = {
    held: ref(0),
    parts: Hashtbl.create(16),
    probes: Hashtbl.create(64),
    values: ref([]),
    envs: ref([]),
    stacks: ref([]),
    args: ref([]),
    frames: ref([]),
    samples: ref(0),
    samples_held: ref(0),
    events: Hashtbl.create(4),
    close_values: ref([]),
    close_envs: ref([]),
  };
  let kind = kind_of(inner);
  if (!is_ref(inner)) {
    switch (inner) {
    | ServerMessage.Result(r) when !is_ref(r) =>
      response(w, Obj.repr(r.response));
      add(w.parts, "result.final_streams", size(r.final_streams));
      streams(w, ~where="final stream", Obj.repr(r.final_streams));
    | ServerMessage.Stream(s) when !is_ref(s) =>
      outbox(w, ~where="stream", Obj.repr(s.update))
    | ServerMessage.ReusePlan(p) when !is_ref(p) =>
      iter_list(
        ~held=w.held,
        o =>
          if (!is_ref(o)) {
            let (_, plan): (
              key,
              Language.IncrEval.t(Language.EvaluatorState.t),
            ) =
              Obj.obj(o);
            entries(w, ~where="plan", Obj.repr(plan));
          },
        Obj.repr(p.initial),
      )
    | _ => ()
    };
  };
  /* the same message with the page's held values put back: what it would
     have been unshared */
  let unshared =
    switch (msg) {
    | ServerMessage.Shared(_, _) =>
      switch (Active.decode_response(raw)) {
      | ServerMessage.Shared(_, m) =>
        switch (Util.ShareCodec.Worker.decode(m)) {
        | m => `Int(size(m))
        | exception _ => `Null
        }
      | _ => `Null
      }
    | m => `Int(size(m))
    };
  let probes =
    Hashtbl.fold((k, (b, n), acc) => [(k, b, n), ...acc], w.probes, [])
    |> List.sort(((_, a, _), (_, b, _)) => compare(b, a));
  let top =
    List.filteri((i, _) => i < 12, probes)
    |> List.map(((k, b, n)) =>
         `Assoc([
           ("id", `String(k)),
           ("bytes", `Int(b)),
           ("samples", `Int(n)),
         ])
       );
  let events =
    Hashtbl.fold(
      (k, r, acc) =>
        [
          `Assoc([
            ("kind", `String(k)),
            ("n", `Int(List.length(r^))),
            ("bytes", `Int(size(r^))),
          ]),
          ...acc,
        ],
      w.events,
      [],
    );
  `Assoc([
    ("kind", `String(kind)),
    ("bytes", `Int(bytes)),
    ("unshared_bytes", unshared),
    (
      "reset",
      switch (reset) {
      | Some(b) => `Bool(b)
      | None => `Null
      },
    ),
    ("held_refs", `Int(w.held^)),
    ("parts", json_of_sums(w.parts)),
    (
      "probes",
      `Assoc([
        ("count", `Int(List.length(probes))),
        ("samples", `Int(w.samples^)),
        ("samples_held", `Int(w.samples_held^)),
        ("value", `Int(size(w.values^))),
        ("env", `Int(size(w.envs^))),
        ("call_stack", `Int(size(w.stacks^))),
        ("args", `Int(size(w.args^))),
        ("frame", `Int(size(w.frames^))),
        ("top", `List(top)),
      ]),
    ),
    (
      "trace",
      `Assoc([
        ("events", `List(events)),
        ("span_close_values", `Int(size(w.close_values^))),
        ("span_close_envs", `Int(size(w.close_envs^))),
      ]),
    ),
  ]);
};

let registered = ref(false);
let register = () =>
  if (! registered^) {
    registered := true;
    Js.Unsafe.set(
      Js.Unsafe.global,
      "hazelAnswerParts",
      Js.wrap_callback(() => {
        let msgs = List.rev(kept^);
        kept := [];
        Js.string(
          Yojson.Safe.to_string(
            `List(
              List.map(
                raw =>
                  switch (breakdown(raw)) {
                  | j => j
                  | exception exn =>
                    `Assoc([("error", `String(Printexc.to_string(exn)))])
                  },
                msgs,
              ),
            ),
          ),
        );
      }),
    );
  };

/* a message from the worker, as it arrived: kept while the timeline is */
let keep = (raw: Active.response): unit =>
  if (Util.BenchMarks.on()) {
    register();
    if (Core.Byte_units.bytes_float(Active.size_response(raw))
        >= float_of_int(min_bytes)) {
      kept := [raw, ...kept^];
    };
  };
