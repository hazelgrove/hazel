open Util;
module Js = Js_of_ocaml.Js;

[@deriving (show, sexp, yojson)]
type key = string;

module Request = {
  /* the incremental cache lives in the worker, per batch key. a stale or
     missing one is safe (reuse_check re-verifies every entry), just a
     colder eval */
  [@deriving (show, sexp, yojson)]
  type prev_source =
    | UseResident;
  /* what to stream during this evaluation. only the pending-eval
     highlight reads effect-free entries, so with it off [Effects] ships
     just the effect-bearing ones; the completion response is unaffected */
  [@deriving (show, sexp, yojson)]
  type stream_interest =
    | Full
    | Effects;
  /* a cell's evaluation as the main thread queues it: its program in
     pieces (EvalPieces), each posted once */
  [@deriving (show, sexp, yojson)]
  type value = {
    program: EvalPieces.program,
    prev: prev_source,
    stream: stream_interest,
  };
  [@deriving (show, sexp, yojson)]
  type batch = list((key, value));
  [@deriving (show, sexp, yojson)]
  type t = {
    request_id: int,
    batch,
  };
};

/* a request as posted: every program's layout (each piece's key and
   hash), and the pieces the worker doesn't hold */
module Posted = {
  [@deriving (show, sexp, yojson)]
  type value = {
    layout: EvalPieces.layout,
    prev: Request.prev_source,
    stream: Request.stream_interest,
  };
  [@deriving (show, sexp, yojson)]
  type t = {
    request_id: int,
    batch: list((key, value)),
    pieces: list((EvalPieces.hash, EvalPieces.posted)),
  };
};

/* a request as the worker evaluates it: its programs composed */
module Evaluation = {
  [@deriving (show, sexp, yojson)]
  type value = {
    expr: Language.Exp.t,
    eval_info_map: Language.EvalInfo.t,
    prev: Request.prev_source,
    stream: Request.stream_interest,
  };
  [@deriving (show, sexp, yojson)]
  type batch = list((key, value));
  [@deriving (show, sexp, yojson)]
  type t = {
    request_id: int,
    batch,
  };
};

module Response = {
  [@deriving (show, sexp, yojson)]
  type value =
    Result.t(
      (Language.Exp.t, Language.EvaluatorState.t),
      Language.ProgramResult.error,
    );
  [@deriving (show, sexp, yojson)]
  type t = list((string, value));

  let (sexp_of_t, t_of_sexp) =
    Util.StructureShareSexp.structure_share_in(sexp_of_t, t_of_sexp);
};

module ClientMessage = {
  [@deriving (show, sexp, yojson)]
  type t =
    | Evaluate(Posted.t)
    /* (^ticket(513)) the same, its pieces shared: the values the worker
       already has replaced by references (Util.ShareCodec); true: reset the
       worker's table first */
    | EvaluateShared(bool, Posted.t)
    /* the client could not resolve a shared response: start the responses'
       table over (the client asks for the evaluation again) */
    | ResetResponses
    /* (^report(46)) keep the worker's own timeline, and send it after each
       result (ServerMessage.Timeline); false: stop */
    | Record(bool);
};

module ServerMessage = {
  /* (^report(46)) one step of the worker's own timeline: [at] in ms since
     the epoch (performance's timeOrigin and now), so the page can place it
     on its own clock; [extra]: a size, or the evaluator's share */
  [@deriving (show, sexp, yojson)]
  type mark = {
    what: string,
    at: float,
    ms: float,
    extra: list((string, float)),
  };

  /* Reuse-pass predictions sent before evaluation slices begin. */
  [@deriving (show, sexp, yojson)]
  type reuse_predictions =
    list((key, Language.IncrEval.t(Language.EvaluatorState.t)));

  /* Cache entries completed so far for one batch item. */
  [@deriving (show, sexp, yojson)]
  type stream_update = Language.IncrEval.outbox(Language.EvaluatorState.t);

  /* Instant liveness ping — must not do ReusePass or other batch work. */
  [@deriving (show, sexp, yojson)]
  type ack = {request_id: int};

  /* Predicted reusable cache entries for initializing streamed state. */
  [@deriving (show, sexp, yojson)]
  type reuse_plan = {
    request_id: int,
    initial: reuse_predictions,
  };

  [@deriving (show, sexp, yojson)]
  type stream = {
    request_id: int,
    key,
    update: stream_update,
  };

  [@deriving (show, sexp, yojson)]
  type result = {
    request_id: int,
    response: Response.t,
    /* Time the worker spent inside the evaluator for this batch, so the
     * Evaluation panel can separate evaluation from the queue + result
     * serialization + transfer that the client's round trip also covers.
     * A TimeUtil.span rather than a Core one so the derived converters resolve;
     * see TimeUtil. */
    eval_time: TimeUtil.span,
    /* The last item's final stream, applied just before the result. Posted
       on its own, it reached the client as a separate message, and the
       client drew it and the result in two frames. */
    final_streams: list(stream),
  };

  /* the request named pieces the worker doesn't hold (a restart, a
     message lost or out of order): post it again with all of them */
  [@deriving (show, sexp, yojson)]
  type resend = {request_id: int};

  /* (^ticket(718), ^report(50)'s next step 7) a message's probe samples, their first-order values carried as data
     (Language.FirstOrder): each such value is a placeholder in the message, a term whose annotation is [marker] (one
     block in the message, so Marshal keeps it shared, and the page knows it by identity) and whose literal is its
     index in [datas] (Wire) */
  [@deriving (show, sexp, yojson)]
  type wire_data = {
    marker: Language.IdTagged.IdTag.t,
    datas: array(Language.FirstOrder.t),
  };

  [@deriving (show, sexp, yojson)]
  type t =
    | Ack(ack)
    | ReusePlan(reuse_plan)
    | Stream(stream)
    | Result(result)
    | Resend(resend)
    /* a message with every value the client already holds replaced by a
       reference (Util.ShareCodec); true: the client resets its table
       first */
    | Shared(bool, t)
    /* The worker's coverage counters so far, after each Result; see
     * CoverageHook. */
    | Coverage(string)
    /* (^report(46)) the worker's own timeline since the last one, after a
       result, while the client asks (ClientMessage.Record) */
    | Timeline(list(mark))
    /* a Stream or a Result, its samples' values carried as data (Wire) */
    | WithData(wire_data, t);
};

/* (^ticket(718), ^report(50)'s next step 7: values as data where they cross) The worker's answer on the Witch slide is
   0.59 MB, encoded in 70 ms in the worker and decoded in 13 ms in the page (Firefox run 42566328043), nearly all of it
   probe samples: 9,099 of them, their values and environments terms with an id on every node. A sample's value, its
   environment's values and its argument, when closed and first-order (Language.FirstOrder.of_term), go as data: in
   the worker, [out] puts each in a table beside the message and a placeholder where it was (one for each value, the
   same value in many samples once); in the page, [in_] makes each a term again, once, and puts it back. Values that
   are not first-order, and a bare literal or constructor (no larger than its placeholder), stay terms. */
module Wire = {
  open Language;
  /* Off (Firefox run 42657604986, 1524e96da3): on the Witch only 5 values a message were first-order by of_term's
     strict reading, and rebuilding every sample record lost the sharing Marshal had kept (the 216 completed entries'
     sample maps share subtrees), so the answer grew from 0.59 to 0.89 MB. Kept for the next slice, which makes data
     where samples are made (Sample.Env.elide), and surveyed meanwhile (survey). */
  let enabled = ref(false);
  module Phys =
    Hashtbl.Make({
      type t = Exp.t;
      let equal = (===);
      let hash = Hashtbl.hash;
    });
  let map_samples =
      (f: Exp.t => Exp.t, st: EvaluatorState.t): EvaluatorState.t => {
    let elided = (v: CallStack.elided_value): CallStack.elided_value =>
      switch (v) {
      | Val(d) => Val(f(d))
      | Opaque => Opaque
      };
    {
      ...st,
      probes:
        Util.Id.Map.map(
          List.map((s: Sample.t) =>
            {
              ...s,
              value: f(s.value),
              env:
                List.map(
                  (en: Sample.Env.entry) =>
                    {
                      ...en,
                      value: elided(en.value),
                    },
                  s.env,
                ),
              args: Option.map(elided, s.args),
            }
          ),
          st.probes,
        ),
    };
  };
  let map_states =
      (f: EvaluatorState.t => EvaluatorState.t, msg: ServerMessage.t)
      : ServerMessage.t => {
    let outbox = (o: ServerMessage.stream_update): ServerMessage.stream_update => {
      completed: {
        entries:
          Util.Id.Map.map(
            (en: IncrEval.entry(EvaluatorState.t)) =>
              {
                ...en,
                state: f(en.state),
              },
            o.completed.entries,
          ),
      },
      current:
        Option.map(
          (c: IncrEval.current(EvaluatorState.t)) =>
            {
              ...c,
              state: f(c.state),
            },
          o.current,
        ),
    };
    let stream = (s: ServerMessage.stream): ServerMessage.stream => {
      ...s,
      update: outbox(s.update),
    };
    switch (msg) {
    | Stream(s) => Stream(stream(s))
    | Result(r) =>
      Result({
        ...r,
        final_streams: List.map(stream, r.final_streams),
        response:
          List.map(
            ((k, v): (string, Response.value)) =>
              (
                k,
                switch (v) {
                | Ok((e, st)) => Ok((e, f(st)))
                | Error(_) => v
                },
              ),
            r.response,
          ),
      })
    | m => m
    };
  };
  /* in the worker: the message, and how many values went as data and how many of them did not come back the same
     (checked only when [check]: made a term again, compared as Exp.fast_equal compares, ids aside) */
  let out = (~check=false, msg: ServerMessage.t): (ServerMessage.t, int, int) =>
    switch (msg) {
    | Stream(_)
    | Result(_) =>
      let marker = IdTagged.IdTag.fresh();
      let datas = ref([]);
      let n = ref(0);
      let differ = ref(0);
      let memo = Phys.create(256);
      let conv = (v: Exp.t): Exp.t =>
        switch (v.term) {
        | Atom(_)
        | Constructor(_) => v
        | _ =>
          switch (Phys.find_opt(memo, v)) {
          | Some(r) => r
          | None =>
            let r =
              switch (FirstOrder.of_term(v)) {
              | Some(d) =>
                if (check && !Exp.fast_equal(FirstOrder.to_term(d), v)) {
                  incr(differ);
                };
                let i = n^;
                incr(n);
                datas := [d, ...datas^];
                let p: Exp.t = {
                  term: Atom(SInt(i)),
                  annotation: marker,
                };
                p;
              | None => v
              };
            Phys.add(memo, v, r);
            r;
          }
        };
      let msg' = map_states(map_samples(conv), msg);
      n^ == 0
        ? (msg, 0, 0)
        : (
          ServerMessage.WithData(
            {
              marker,
              datas: Array.of_list(List.rev(datas^)),
            },
            msg',
          ),
          n^,
          differ^,
        );
    | m => (m, 0, 0)
    };
  /* While the worker keeps its timeline: each distinct sample value in a Stream or a Result (by identity), whether it
     is first-order data, and if not, why (FirstOrder.why_not), as counts; the message unchanged */
  let survey = (msg: ServerMessage.t): list((string, float)) => {
    let seen = Phys.create(256);
    let counts = Hashtbl.create(8);
    let bump = k =>
      Hashtbl.replace(
        counts,
        k,
        1 + Option.value(~default=0, Hashtbl.find_opt(counts, k)),
      );
    let look = (v: Exp.t): Exp.t => {
      if (!Phys.mem(seen, v)) {
        Phys.add(seen, v, v);
        switch (v.term) {
        | Atom(_)
        | Constructor(_) => bump("a bare literal or constructor")
        | _ =>
          switch (FirstOrder.why_not(v)) {
          | None => bump("data")
          | Some(why) => bump("not data: " ++ why)
          }
        };
      };
      v;
    };
    ignore(map_states(map_samples(look), msg));
    Hashtbl.fold(
      (k, n, acc) => [(k, float_of_int(n)), ...acc],
      counts,
      [],
    );
  };
  /* (^ticket(718)) While the worker keeps its timeline: a Result's bytes by
     part, each part's objects marshalled together (so shared within a part,
     as the whole is), to see what the answer carries before changing how it
     is made. Parts overlap the whole, and a state's parts are counted once
     however many entries hold it */
  module PhysO =
    Hashtbl.Make({
      type t = Obj.t;
      let equal = (===);
      let hash = Hashtbl.hash;
    });
  let parts = (msg: ServerMessage.t): list((string, float)) =>
    switch (msg) {
    | Result(r) =>
      let seen = PhysO.create(256);
      let bags: Hashtbl.t(string, list(Obj.t)) = Hashtbl.create(16);
      let put = (k: string, o: Obj.t): unit =>
        Hashtbl.replace(
          bags,
          k,
          [o, ...Option.value(~default=[], Hashtbl.find_opt(bags, k))],
        );
      let entries = ref(0);
      let states = ref(0);
      let samples = ref(0);
      /* each probe's distinct samples (by identity), for the largest */
      let seen_samples = PhysO.create(1024);
      let per_probe: Hashtbl.t(string, list(Obj.t)) = Hashtbl.create(64);
      let rec state = (st: EvaluatorState.t): unit =>
        if (!PhysO.mem(seen, Obj.repr(st))) {
          PhysO.add(seen, Obj.repr(st), Obj.repr(st));
          incr(states);
          Util.Id.Map.iter(
            (pid, ss: list(Sample.t)) =>
              List.iter(
                (smp: Sample.t) => {
                  incr(samples);
                  if (!PhysO.mem(seen_samples, Obj.repr(smp))) {
                    PhysO.add(seen_samples, Obj.repr(smp), Obj.repr(smp));
                    let k = String.sub(Util.Id.to_string(pid), 0, 8);
                    Hashtbl.replace(
                      per_probe,
                      k,
                      [
                        Obj.repr(smp),
                        ...Option.value(
                             ~default=[],
                             Hashtbl.find_opt(per_probe, k),
                           ),
                      ],
                    );
                  };
                  put("sample values", Obj.repr(smp.value));
                  put("sample environments", Obj.repr(smp.env));
                  put(
                    "samples' other fields",
                    Obj.repr((
                      smp.call_stack,
                      smp.args,
                      smp.frame,
                      smp.syntax_id,
                    )),
                  );
                },
                ss,
              ),
            st.probes,
          );
          put("tests and theorems", Obj.repr((st.tests, st.theorems)));
          put("observation traces", Obj.repr((st.obs_trace, st.obs_opens)));
          incr_eval(st.incr_eval);
        }
      and incr_eval = (ie: IncrEval.t(EvaluatorState.t)): unit =>
        Util.Id.Map.iter(
          (_, en: IncrEval.entry(EvaluatorState.t)) => {
            incr(entries);
            put("entries' elaborated pieces", Obj.repr(en.prev_elab));
            put("entries' reuse maps", Obj.repr(en.prev_reuse_map));
            put("entries' probe targets", Obj.repr(en.prev_probe_targets));
            put("entries' values", Obj.repr(en.value));
            state(en.state);
          },
          ie.entries,
        );
      let outbox = (o: ServerMessage.stream_update) => {
        incr_eval(o.completed);
        Option.iter(
          (c: IncrEval.current(EvaluatorState.t)) => state(c.state),
          o.current,
        );
      };
      List.iter(
        (s: ServerMessage.stream) => outbox(s.update),
        r.final_streams,
      );
      List.iter(
        ((_, v): (string, Response.value)) =>
          switch (v) {
          | Ok((e, st)) =>
            put("the program's values", Obj.repr(e));
            state(st);
          | Error(_) => ()
          },
        r.response,
      );
      let bytes = (o: Obj.t): float =>
        float_of_int(String.length(Marshal.to_string(o, [])));
      /* the eight probes with the most distinct samples: their count, and
         their bytes (value, environment and all) */
      let top =
        Hashtbl.fold((k, os, acc) => [(k, os), ...acc], per_probe, [])
        |> List.map(((k, os)) =>
             (k, List.length(os), bytes(Obj.repr(os)))
           )
        |> List.sort(((_, _, a), (_, _, b)) => Float.compare(b, a))
        |> List.filteri((i, _) => i < 8)
        |> List.concat_map(((k, n, b)) =>
             [
               ("probe " ++ k ++ " samples", float_of_int(n)),
               ("probe " ++ k ++ " bytes", b),
             ]
           );
      /* the result's own states' probes: with their environments left out,
         and with their values left out */
      let response_probes = (keep: Sample.t => Sample.t) =>
        switch (msg) {
        | Result(r) =>
          List.filter_map(
            ((_, v): (string, Response.value)) =>
              switch (v) {
              | Ok((_, st)) =>
                Some(Util.Id.Map.map(List.map(keep), st.probes))
              | Error(_) => None
              },
            r.response,
          )
        | _ => []
        };
      let bare: Exp.t = {
        term: Atom(SInt(0)),
        annotation: IdTagged.IdTag.fresh(),
      };
      let values_only =
        bytes(
          Obj.repr(
            response_probes(smp =>
              {
                ...smp,
                env: [],
              }
            ),
          ),
        );
      let envs_only =
        bytes(
          Obj.repr(
            response_probes(smp =>
              {
                ...smp,
                value: bare,
              }
            ),
          ),
        );
      let neither =
        bytes(
          Obj.repr(
            response_probes(smp =>
              {
                ...smp,
                value: bare,
                env: [],
              }
            ),
          ),
        );
      /* what two cuts would leave, each keeping the rest as it is (so as
         shared): (1) the result without its final streams, the cache
         entries finished since the last stream, applied just before it;
         (2) also without the cache in its states (each entry's state holds
         every probe so far, and the page reads only the result's own) */
      let no_cache = (st: EvaluatorState.t): EvaluatorState.t => {
        ...st,
        incr_eval: IncrEval.empty,
      };
      let cut1 =
        switch (msg) {
        | Result(r) =>
          ServerMessage.Result({
            ...r,
            final_streams: [],
          })
        | m => m
        };
      let cut2 = map_states(no_cache, cut1);
      [
        ("result probes, values (no environments)", values_only),
        ("result probes, environments (no values)", envs_only),
        ("result probes, neither", neither),
        ("cut 1: no final streams", bytes(Obj.repr(cut1))),
        ("cut 2: and no cache in the states", bytes(Obj.repr(cut2))),
        ("distinct samples", float_of_int(PhysO.length(seen_samples))),
        ("probes", float_of_int(Hashtbl.length(per_probe))),
        ...top,
      ]
      @ [
        ("the whole", bytes(Obj.repr(msg))),
        ("entries", float_of_int(entries^)),
        ("states", float_of_int(states^)),
        ("samples", float_of_int(samples^)),
        ...Hashtbl.fold(
             (k, os, acc) => [(k, bytes(Obj.repr(os))), ...acc],
             bags,
             [],
           ),
      ];
    | _ => []
    };
  /* in the page: the message as the worker meant it, each value made a term once */
  let in_ = (msg: ServerMessage.t): ServerMessage.t =>
    switch (msg) {
    | WithData({marker, datas}, inner) =>
      let terms = Array.make(Array.length(datas), None);
      let conv = (v: Exp.t): Exp.t =>
        if (v.annotation === marker) {
          switch (v.term) {
          | Atom(SInt(i)) when i >= 0 && i < Array.length(datas) =>
            switch (terms[i]) {
            | Some(t) => t
            | None =>
              let t = FirstOrder.to_term(datas[i]);
              terms[i] = Some(t);
              t;
            }
          | _ => v
          };
        } else {
          v;
        };
      map_states(map_samples(conv), inner);
    | m => m
    };
};

/* Candidate encodings for the worker payloads; `Marshal` is active (see
 * `Active`), the rest are benchmarked against it (WorkerMetrics). Strings and
 * `all_of_encoding` are ppx-derived — no hand-maintained name lists. */
[@deriving (show({with_path: false}), sexp, yojson, enumerate)]
type encoding =
  | Direct
  | Marshal
  | Sexp;

/* An encode/decode pair per direction over the wire messages (the request
 * direction carries ClientMessage.t, the response direction ServerMessage.t);
 * the forms are abstract so only encode/decode can cross the boundary and the
 * directions can't be swapped. */
module type ENCODING = {
  type request;
  type response;
  let encode_request: ClientMessage.t => request;
  let decode_request: request => ClientMessage.t;
  let encode_response: ServerMessage.t => response;
  let decode_response: response => ServerMessage.t;
  /* Size of the encoded form (exact for the string encodings, an estimate for
   * the identity one), reported here since only the encoding knows its form. */
  let size_request: request => Core.Byte_units.t;
  let size_response: response => Core.Byte_units.t;
};

/* Post the live value graph unchanged (pre-#2368 behavior): unsafe as the
 * active encoding since Chrome's clone overflows on deep payloads, kept as the
 * benchmark baseline (the overflow surfaces there as a caught exception). */
module DirectEncoding: ENCODING = {
  open Js_of_ocaml;
  type request = ClientMessage.t;
  type response = ServerMessage.t;
  let encode_request = Fun.id;
  let decode_request = Fun.id;
  let encode_response = Fun.id;
  let decode_response = Fun.id;
  /* No serialized form, so estimate the in-memory footprint by an iterative
   * walk with a visited set (a relative measure, not exact bytes). */
  let size_fn =
    Js.Unsafe.pure_js_expr(
      {|(function (root) {
           var seen = new Set(), stack = [root], total = 0;
           while (stack.length) {
             var v = stack.pop();
             if (v === null || v === undefined) continue;
             var t = typeof v;
             if (t === 'number') { total += 8; }
             else if (t === 'string') { total += v.length; }
             else if (t === 'boolean') { total += 4; }
             else if (t === 'object') {
               if (seen.has(v)) continue;
               seen.add(v);
               if (ArrayBuffer.isView(v)) { total += v.byteLength; }
               else if (Array.isArray(v)) {
                 total += 8 * v.length;
                 for (var i = 0; i < v.length; i++) stack.push(v[i]);
               } else {
                 var ks = Object.keys(v);
                 for (var i = 0; i < ks.length; i++) {
                   total += ks[i].length + 8;
                   stack.push(v[ks[i]]);
                 }
               }
             }
           }
           return total;
         })|},
    );
  let size: 'a. 'a => Core.Byte_units.t =
    x =>
      Core.Byte_units.of_bytes_int(
        Js.Unsafe.fun_call(size_fn, [|Js.Unsafe.inject(x)|]),
      );
  let size_request = (r: request) => size(r);
  let size_response = (r: response) => size(r);
};

/* jsoo Marshal (iterative → depth-safe) to a flat string clone copies without
 * recursing. The active encoding and the #2368 fix. */
module MarshalEncoding: ENCODING = {
  type request = string;
  type response = string;
  let encode_request = (req: ClientMessage.t): request =>
    Marshal.to_string(req, []);
  let decode_request = (w: request): ClientMessage.t =>
    Marshal.from_string(w, 0);
  let encode_response = (resp: ServerMessage.t): response =>
    Marshal.to_string(resp, []);
  let decode_response = (w: response): ServerMessage.t =>
    Marshal.from_string(w, 0);
  let size_request = (w: request) =>
    Core.Byte_units.of_bytes_int(String.length(w));
  let size_response = (w: response) =>
    Core.Byte_units.of_bytes_int(String.length(w));
};

/* Serialize via the derived sexp converters; recurses per AST level, so deep
 * expressions overflow — a benchmark comparison only. */
module SexpEncoding: ENCODING = {
  type request = string;
  type response = string;
  let encode_request = (req: ClientMessage.t): request =>
    Sexplib.Sexp.to_string(ClientMessage.sexp_of_t(req));
  let decode_request = (w: request): ClientMessage.t =>
    ClientMessage.t_of_sexp(Sexplib.Sexp.of_string(w));
  let encode_response = (resp: ServerMessage.t): response =>
    Sexplib.Sexp.to_string(ServerMessage.sexp_of_t(resp));
  let decode_response = (w: response): ServerMessage.t =>
    ServerMessage.t_of_sexp(Sexplib.Sexp.of_string(w));
  let size_request = (w: request) =>
    Core.Byte_units.of_bytes_int(String.length(w));
  let size_response = (w: response) =>
    Core.Byte_units.of_bytes_int(String.length(w));
};

/* Behavior for an encoding tag (exhaustive — a new variant must be added). */
let module_of_encoding = (e: encoding): (module ENCODING) =>
  switch (e) {
  | Direct => (module DirectEncoding)
  | Marshal => (module MarshalEncoding)
  | Sexp => (module SexpEncoding)
  };

/* The active encoding on the boundary (WorkerClient / on_request); swap to
 * change it. */
module Active = MarshalEncoding;

let error_response = exn =>
  switch (exn) {
  | Language.EvaluatorError.Exception(reason) =>
    print_endline("EvaluatorError:" ++ Language.EvaluatorError.show(reason));
    Error(Language.ProgramResult.EvaulatorError(reason));
  | exn =>
    print_endline("EXN:" ++ Printexc.to_string(exn));
    Error(Language.ProgramResult.UnknownException(Printexc.to_string(exn)));
  };

/* the worker's own per-key incremental caches (async path) */
let resident: Hashtbl.t(key, Language.EvaluatorState.incr_eval) =
  Hashtbl.create(4);

let resolve_prev =
    (~key: key, prev: Request.prev_source): Language.EvaluatorState.incr_eval =>
  switch (prev) {
  | UseResident =>
    Option.value(
      Hashtbl.find_opt(resident, key),
      ~default=Language.IncrEval.empty,
    )
  };

let store_resident = (key: key, response: Response.value): unit =>
  switch (response) {
  | Ok((_, state)) =>
    Hashtbl.replace(resident, key, state.Language.EvaluatorState.incr_eval)
  | Error(_) => ()
  };

/* the main thread's evaluation: cold, with nothing to stream */
let evaluate_sync =
    (~eval_info: Language.EvalInfo.t, expr: Language.Exp.t): Response.value => {
  switch (
    Language.Evaluator.evaluate(
      ~eval_info,
      ~env=Language.Builtins.env_init,
      expr,
    )
  ) {
  | exception exn => error_response(exn)
  | (result, state) =>
    /* Drop transient eval-only data (probe enter-data can hold large arg
     * values) before serializing over postMessage. */
    Ok((result, Language.EvaluatorState.clear_transient(state)))
  };
};

type evaluation_start =
  | Yielding(Language.Evaluator.yielding_evaluation)
  | CompletedImmediately(Response.value);

type running = {
  request_id: int,
  key,
  remaining: Evaluation.batch,
  completed: Response.t,
  evaluation: Language.Evaluator.yielding_evaluation,
};

type runtime =
  | Idle
  | Planning
  | Starting
  | Running(running);

type model = {
  latest_request: option(Evaluation.t),
  runtime,
  slice_already_scheduled: bool,
};

let slice_step_budget = 5000;
/* Cumulative trampoline-step backstop across slices for a single batch item.
 * Wall-clock timeout + worker terminate is the primary runaway guard; this
 * stops pathological cases that somehow evade the client timer.
 *
 * Units are trampoline TRANSITIONS (one per Bind/Next/Done), not evaluator
 * steps — hundreds of transitions per reduction, roughly 1M transitions ≈ 2s
 * of sliced evaluation in-browser. The backstop must sit well beyond the 20s
 * client timer or ordinary heavy programs (e.g. range(1,5000)) get killed at
 * ~2s and surface as spurious "Evaluation timed out". */
let total_step_limit = 100_000_000;
let initial_model = {
  latest_request: None,
  runtime: Idle,
  slice_already_scheduled: false,
};

/* Worker execution model (three phases per request):
 * 1. Instant `Ack` — liveness only; no ReusePass.
 * 2. `ReusePlan` — predicted reusable entries for streamed state.
 * 3. Eval slices — `Stream` updates, then `Result`.
 * A newer request replaces `latest_request`; the next slice abandons stale work. */

/* Incremental reuse pays for itself only past a certain program size:
   the reuse pre-pass is a full evaluation-shaped walk and every
   evaluated node runs a reuse check, which for a demo-sized program
   (a few hundred statics entries) cost 2-3x the evaluation itself.
   Below the threshold, evaluate from scratch. (Measured 2026-09-08 on
   the graph livelit slide: 2.0s -> see the commit message.) */
let incremental_min_statics = 2500;

let wants_incremental = (req_value: Evaluation.value): bool =>
  Util.Id.Map.cardinal(req_value.eval_info_map.statics)
  >= incremental_min_statics;

/* the reuse plan computed for a batch item, handed to its evaluation so
   the pre-pass runs once per request (keyed by the item's expr identity:
   the plan and the evaluation see the same decoded request) */
let planned_reuse:
  ref(
    list((Language.Exp.t, Language.IncrEval.t(Language.EvaluatorState.t))),
  ) =
  ref([]);

let predict_reuse_for_request = ((key, req_value): (key, Evaluation.value)) => {
  let Evaluation.{expr, eval_info_map, prev, _} = req_value;
  let stream =
    if (!wants_incremental(req_value)) {
      Language.IncrEval.empty;
    } else {
      switch (
        Language.ReusePass.reuse_pass(
          ~prev=resolve_prev(~key, prev),
          ~eval_info=eval_info_map,
          ~env=Language.Builtins.env_init,
          expr,
        )
      ) {
      | exception _ => Language.IncrEval.empty
      | stream => stream
      };
    };
  /* a superseded request's plan is never taken: keep only the newest
     few so abandoned plans (whole reuse streams) don't accumulate */
  planned_reuse :=
    [(expr, stream), ...Util.ListUtil.take(3, planned_reuse^)];
  (key, stream);
};

let take_planned_reuse =
    (expr: Language.Exp.t)
    : option(Language.IncrEval.t(Language.EvaluatorState.t)) => {
  let found = List.find_opt(((e, _)) => e === expr, planned_reuse^);
  planned_reuse := List.filter(((e, _)) => e !== expr, planned_reuse^);
  Option.map(snd, found);
};

/* Evaluator time accumulated for the request in flight, reported back in the
 * result. Timed unconditionally — the worker cannot see whether the Evaluation
 * panel is open, and two clock reads per slice (5000 trampoline steps) is noise
 * against the slice itself. */
let eval_total = ref(Core.Time_ns.Span.zero);

let timed_eval: 'a. (unit => 'a) => 'a =
  f => {
    let (span, x) = TimeUtil.timed(f);
    eval_total := Core.Time_ns.Span.(eval_total^ + span);
    x;
  };

let stream_min_interval_ms: ref(float) = ref(100.);

/* the running batch item's interest (items run one at a time) */
let current_stream_interest: ref(Request.stream_interest) =
  ref(Request.Full);

let start_evaluation =
    (~key: key, req_value: Evaluation.value): evaluation_start => {
  let Evaluation.{expr, eval_info_map, prev, stream} = req_value;
  current_stream_interest := stream;
  let planned = take_planned_reuse(expr);
  /* stream cadence scales with program size: each posted chunk costs
     the client a stream-collection + recalc cycle that grows with it */
  stream_min_interval_ms :=
    max(
      100.,
      min(
        1000.,
        float_of_int(Util.Id.Map.cardinal(eval_info_map.statics)) /. 12.,
      ),
    );
  switch (
    Language.Evaluator.start_yielding_evaluation(
      ~prev=
        wants_incremental(req_value)
          ? resolve_prev(~key, prev) : Language.IncrEval.empty,
      ~eval_info=eval_info_map,
      ~env=Language.Builtins.env_init,
      ~reuse_stream=?planned,
      expr,
    )
  ) {
  | exception exn => CompletedImmediately(error_response(exn))
  | evaluation => Yielding(evaluation)
  };
};

let is_latest = (model, request_id) =>
  switch (model.latest_request) {
  | Some({request_id: latest_request_id, _}) =>
    request_id == latest_request_id
  | None => false
  };

/* All worker→client messages cross postMessage in the Active encoding
 * (see ENCODING above): posting the live value graph overflows Chrome's
 * structured clone on deep results (#2368). */
/* (^report(46)) the worker's own timeline, kept while the client asks
   (ClientMessage.Record) and posted after each result: what the worker
   does between a request and its answer. Off, a step costs a test. */
let recording = ref(false);
let marks: ref(list(ServerMessage.mark)) = ref([]);
let epoch_now = (): float =>
  Util.BenchMarks.time_origin() +. Util.BenchMarks.now();
let wtime = (~extra=() => [], what: string, f: unit => 'r): 'r =>
  if (! recording^) {
    f();
  } else {
    let t0 = epoch_now();
    let r = f();
    let t1 = epoch_now();
    marks :=
      [
        ServerMessage.{
          what,
          at: t0,
          ms: t1 -. t0,
          extra: extra(),
        },
        ...marks^,
      ];
    r;
  };
/* (^report(47), experiment 3) how many ids this realm has minted: Id.mk
   writes its running count into each id's bytes (0 to 3: the low 30 bits;
   4, 5, 6's low nibble and 7: the high), so a fresh id says how many came
   before it. Two readings, less one for the second reading's own id, are
   the ids minted between them, without touching Id.mk. */
let id_count = (): int => {
  let b = Uuidm.unsafe_to_bytes(Util.Id.mk());
  let c = i => Char.code(b.[i]);
  let lo = c(0) lor c(1) lsl 8 lor c(2) lsl 16 lor (c(3) land 0x3f) lsl 24;
  let hi = c(4) lor c(5) lsl 8 lor (c(6) land 0x0f) lsl 16 lor c(7) lsl 20;
  hi * 1073741824 + lo;
};

let kind_of = (msg: ServerMessage.t): string =>
  switch (msg) {
  | ServerMessage.Ack(_) => "ack"
  | ServerMessage.ReusePlan(_) => "reuse plan"
  | ServerMessage.Stream(_) => "stream"
  | ServerMessage.Result(_) => "result"
  | ServerMessage.Resend(_) => "resend"
  | ServerMessage.Shared(_) => "shared"
  | ServerMessage.Coverage(_) => "coverage"
  | ServerMessage.Timeline(_) => "timeline"
  | ServerMessage.WithData(_) => "result or stream, values as data"
  };

/* Responses were shared (Util.ShareCodec, ^ticket(513)): a value the client
   already held went as a reference. Encoding the whole response was as much
   of the worker's time per release as evaluating (Kids' Choice, 44 ms
   profiled), and decoding it a main-thread cost on the way to the redraw.
   (^report(46), Matthew, 2026-11-08: "drop sharing") The worker's answers
   are not shared any more: on the Witch slide, once its samples no longer
   wrote out a module whole, sharing a 0.45 MB result took 154 to 255 ms in
   the worker to save 0.04 MB, more than encoding and decoding all of it, and
   the hashing of every value it needs is paid again on the page. What the
   answers carry is meant to be first-order data, which is not expected to
   share (^ticket(718)). share_response stays for its tests, and the client
   still decodes a Shared message, until the response side's sharing is
   removed. The requests keep theirs: the program's pieces, open code that
   mostly does not change between edits. */
let response_reset = ref(true);
let share_response = (msg: ServerMessage.t): ServerMessage.t => {
  if (Util.ShareCodec.Client.size() > Util.ShareCodec.max_entries) {
    response_reset := true;
  };
  let reset = response_reset^;
  if (reset) {
    Util.ShareCodec.Client.reset();
  };
  switch (Util.ShareCodec.Client.encode(msg)) {
  | shared =>
    response_reset := false;
    ServerMessage.Shared(reset, shared);
  | exception _ =>
    response_reset := true;
    msg;
  };
};
let post_message = (msg: ServerMessage.t): unit => {
  /* (^ticket(718)) its samples' first-order values as data (Wire), checked
     made terms again while the timeline is kept */
  let as_data = ref((0, 0));
  let msg =
    switch (msg) {
    | ServerMessage.Stream(_)
    | ServerMessage.Result(_) when ! Wire.enabled^ =>
      if (recording^) {
        let counts = ref([]);
        wtime(
          ~extra=() => counts^,
          "samples' values surveyed",
          () => counts := Wire.survey(msg),
        );
      };
      /* (^ticket(718)) a result's bytes by part, marked with no time (the
         marshalling it takes is the diagnostic's own) */
      if (recording^) {
        switch (msg) {
        | ServerMessage.Result(_) =>
          let ps = Wire.parts(msg);
          wtime(~extra=() => ps, "the answer's parts, in bytes", () => ());
        | _ => ()
        };
      };
      msg;
    | ServerMessage.Stream(_)
    | ServerMessage.Result(_) =>
      wtime(
        ~extra=
          () =>
            [
              ("values", float_of_int(fst(as_data^))),
              ("not the same back", float_of_int(snd(as_data^))),
            ],
        "samples' values as data",
        () => {
          let (m, n, differ) = Wire.out(~check=recording^, msg);
          as_data := (n, differ);
          m;
        },
      )
    | m => m
    };
  /* (^report(46)) each message's encoding and posting, on the worker's
     timeline, with its size */
  let kind = kind_of(msg);
  let bytes = ref(0.);
  let encoded =
    wtime(
      ~extra=() => [("bytes", bytes^)],
      "encode " ++ kind,
      () => {
        let e = Active.encode_response(msg);
        if (recording^) {
          bytes := Core.Byte_units.bytes_float(Active.size_response(e));
        };
        e;
      },
    );
  wtime("post " ++ kind, () => Js_of_ocaml.Worker.post_message(encoded));
};

/* the timeline so far, and start over (not itself on it) */
let post_timeline = () => {
  let tl = List.rev(marks^);
  marks := [];
  Js_of_ocaml.Worker.post_message(
    Active.encode_response(ServerMessage.Timeline(tl)),
  );
};

/* A completed item's last stream, held for the result message when no item
   follows it (see ServerMessage.result.final_streams). */
let held_final_streams: ref(list(ServerMessage.stream)) = ref([]);

let post_batch_result = (model, request_id, completed) => {
  let final_streams =
    List.filter(
      (s: ServerMessage.stream) => s.request_id == request_id,
      List.rev(held_final_streams^),
    );
  held_final_streams := [];
  if (is_latest(model, request_id)) {
    post_message(
      ServerMessage.Result({
        request_id,
        response: List.rev(completed),
        eval_time: eval_total^,
        final_streams,
      }),
    );
    switch (Option.bind(CoverageHook.snapshot, snapshot => snapshot())) {
    | Some(coverage) => post_message(ServerMessage.Coverage(coverage))
    | None => ()
    };
    if (recording^) {
      post_timeline();
    };
  };
};

/* main-thread consumers read only entry keys, [seq] and each state's
   probes/tests/steps; the reuse-cache payload (prev_elab, reuse map,
   probe targets, value, nested incr_eval) stays in the worker, as
   decoding it would dominate main-thread heap churn */
let slim_hole: Lazy.t(Language.Exp.t) = lazy(Language.Exp.fresh(EmptyHole));
/* (^report(46)) and its observation trace cleared, as the main thread's own
   evaluation clears it (clear_transient): the sliced evaluation returns its
   state as it is, and the trace went to the page, where nothing reads it
   (1.4 MB of the Witch slide's 5.4 MB answer) */
let slim_state = (state: Language.EvaluatorState.t) =>
  Language.EvaluatorState.clear_transient({
    ...state,
    incr_eval: Language.IncrEval.empty,
  });
let slim_entries =
    (incr: Language.IncrEval.t(Language.EvaluatorState.t))
    : Language.IncrEval.t(Language.EvaluatorState.t) =>
  Language.IncrEval.{
    entries:
      Id.Map.map(
        (e: Language.IncrEval.entry(Language.EvaluatorState.t)) =>
          Language.IncrEval.{
            prev_elab: Lazy.force(slim_hole),
            prev_reuse_map: Language.IncrEval.empty_reuse_map,
            prev_probe_targets:
              Language.EvalInfo.ProbeTargets(
                Language.SubexpProbeTargets.empty,
              ),
            value: Lazy.force(slim_hole),
            state: slim_state(e.state),
            seq: e.seq,
          },
        incr.entries,
      ),
  };
let slim_stream_update =
    (u: Language.IncrEval.outbox(Language.EvaluatorState.t))
    : Language.IncrEval.outbox(Language.EvaluatorState.t) =>
  Language.IncrEval.{
    completed: slim_entries(u.completed),
    current:
      Option.map(
        (c: Language.IncrEval.current(Language.EvaluatorState.t)) =>
          Language.IncrEval.{
            ...c,
            state: slim_state(c.state),
          },
        u.current,
      ),
  };

let post_stream_update =
    (
      ~allow_empty=false,
      model,
      request_id,
      key,
      update: Language.IncrEval.outbox(Language.EvaluatorState.t),
    ) =>
  if (is_latest(model, request_id)
      && (allow_empty || !Language.IncrEval.outbox_is_empty(update))) {
    post_message(
      ServerMessage.Stream({
        request_id,
        key,
        update: slim_stream_update(update),
      }),
    );
  };

/* stream posts are throttled: each costs the client a full
   update/calculate/render cycle. undrained entries accumulate in the
   outbox; at completion they ride with the result (hold_final_stream) */
let last_stream_post: ref(float) = ref(0.);

let entry_has_effects =
    (e: Language.IncrEval.entry(Language.EvaluatorState.t)): bool =>
  Language.EvaluatorState.(
    e.state.tests != []
    || !Util.Id.Map.is_empty(e.state.probes)
    || e.state.theorems != []
  );

/* [Effects]: husks (ids + step counts) only feed the pending-eval
   highlight, which is off; a chunk filtered to empty isn't posted */
let filter_stream_interest =
    (u: Language.IncrEval.outbox(Language.EvaluatorState.t))
    : Language.IncrEval.outbox(Language.EvaluatorState.t) =>
  switch (current_stream_interest^) {
  | Full => u
  | Effects =>
    Language.IncrEval.{
      completed: {
        entries:
          Util.Id.Map.filter(
            (_, e) => entry_has_effects(e),
            u.completed.entries,
          ),
      },
      current: None,
    }
  };

let flush_stream_update = (model, request_id, key, evaluation) => {
  let now: float = Js.Unsafe.global##.Date##now();
  if (now -. last_stream_post^ >= stream_min_interval_ms^) {
    last_stream_post := now;
    let update =
      Language.Evaluator.drain_streaming_outbox(evaluation)
      |> filter_stream_interest;
    post_stream_update(model, request_id, key, update);
  };
};

/* At completion: what is left of the outbox, slimmed as a stream post is,
   held for the result message (or for the next item's start). */
let hold_final_stream = (model, request_id, key, evaluation) => {
  let update =
    Language.Evaluator.drain_streaming_outbox(evaluation)
    |> filter_stream_interest;
  if (is_latest(model, request_id)
      && !Language.IncrEval.outbox_is_empty(update)) {
    held_final_streams :=
      [
        ServerMessage.{
          request_id,
          key,
          update: slim_stream_update(update),
        },
        ...held_final_streams^,
      ];
  };
};

/* ACK must be cheap: the client treats missing ACK as a dead worker and will
 * terminate/respawn. ReusePass belongs in `ReusePlan`, not here. */
let post_ack = (request_id: int) =>
  post_message(ServerMessage.Ack({request_id: request_id}));

let post_reuse_plan = (model, request: Evaluation.t) =>
  if (is_latest(model, request.request_id)) {
    post_message(
      ServerMessage.ReusePlan({
        request_id: request.request_id,
        /* the evaluation keeps the full plan; the client gets what it
           reads, as with streams */
        initial:
          List.map(
            item => {
              let (key, plan) = predict_reuse_for_request(item);
              (key, slim_entries(plan));
            },
            request.batch,
          ),
      }),
    );
  };

/* Dom_html.window is unavailable in a worker, so go through the global
 * object for setTimeout. */
let schedule_async = callback =>
  ignore(Js.Unsafe.global##setTimeout(Js.wrap_callback(callback), 0.));

/* the main thread only displays a budget-pruned copy, so anything past
   the budget is marshal/decode dead weight. its elisions are marked, so
   the display still notes them when what's left fits its own budget */
let value_ship_budget = 6_000;
let prune_value_size = (e: Language.Exp.t): Language.Exp.t =>
  Language.TermPrune.prune_noting(
    ~prefix="[worker] ",
    ~budget=value_ship_budget,
    ~purpose="shipping",
    e,
  );

/* a finished item's cache stays resident; its response drops it (the UI
   never reads it: prev is worker-resident, predictions come via
   ReusePlan) */
let complete = (key: key, response: Response.value): (key, Response.value) =>
  wtime("make result", () => {
    store_resident(key, response);
    (
      key,
      switch (response) {
      | Ok((exp, state)) =>
        Ok((
          exp |> Language.TermPrune.prune_closure_envs |> prune_value_size,
          slim_state(state),
        ))
      | Error(_) as e => e
      },
    );
  });

let rec evaluate_next_batch_item = (model, request_id, completed, remaining) =>
  switch (remaining) {
  | [] =>
    let model = {
      ...model,
      runtime: Idle,
    };
    post_batch_result(model, request_id, completed);
    model;
  | [(key, req_value), ...remaining] =>
    List.iter(
      (st: ServerMessage.stream) =>
        if (is_latest(model, st.request_id)) {
          post_message(ServerMessage.Stream(st));
        },
      List.rev(held_final_streams^),
    );
    held_final_streams := [];
    switch (timed_eval(() => start_evaluation(~key, req_value))) {
    | CompletedImmediately(response) =>
      evaluate_next_batch_item(
        model,
        request_id,
        [complete(key, response), ...completed],
        remaining,
      )
    | Yielding(evaluation) =>
      let model = {
        ...model,
        runtime:
          Running({
            request_id,
            key,
            remaining,
            completed,
            evaluation,
          }),
      };
      model;
    };
  }
and begin_latest_batch = model =>
  switch (model.latest_request) {
  | None => {
      ...model,
      runtime: Idle,
    }
  | Some({request_id, batch}) =>
    evaluate_next_batch_item(model, request_id, [], batch)
  }
and finish_current_item = (model, running, response) =>
  evaluate_next_batch_item(
    model,
    running.request_id,
    [complete(running.key, response), ...running.completed],
    running.remaining,
  )
and plan_latest_batch = model =>
  switch (model.latest_request) {
  | None => {
      ...model,
      runtime: Idle,
    }
  | Some(request) =>
    wtime("reuse plan", () => post_reuse_plan(model, request));
    {
      ...model,
      runtime: Starting,
    };
  }
and run_scheduled_slice = model => {
  let model = {
    ...model,
    slice_already_scheduled: false,
  };
  switch (model.runtime) {
  | Idle => model
  | Planning => plan_latest_batch(model)
  | Starting => begin_latest_batch(model)
  | Running(running) when !is_latest(model, running.request_id) =>
    plan_latest_batch(model)
  | Running(running) =>
    switch (
      timed_eval(() =>
        Language.Evaluator.run_yielding_slice(
          ~step_budget=slice_step_budget,
          running.evaluation,
        )
      )
    ) {
    | exception exn =>
      finish_current_item(model, running, error_response(exn))
    | EvaluationCompleted(value) =>
      hold_final_stream(
        model,
        running.request_id,
        running.key,
        running.evaluation,
      );
      finish_current_item(model, running, Ok(value));
    | EvaluationYielded(evaluation) =>
      flush_stream_update(model, running.request_id, running.key, evaluation);
      if (Language.Evaluator.yielding_step_count(evaluation)
          >= total_step_limit) {
        finish_current_item(
          model,
          running,
          Error(Language.ProgramResult.Timeout),
        );
      } else {
        let model = {
          ...model,
          runtime:
            Running({
              ...running,
              evaluation,
            }),
        };
        model;
      };
    }
  };
};

/* Slices run back to back for up to this long before the worker yields
   to its event loop. Yielding after every slice (5000 trampoline steps,
   about 3 ms) paid the ~4 ms clamp on chained setTimeouts each time: on
   the Scenes slide's woods, 1,292 slices spent 5.8 s between slices
   against 4.2 s evaluating. A new request still gets in within this
   budget. */
let slice_time_budget_ms = 30.;
let run_slices_for_budget = model => {
  let t0 = Util.JsUtil.precise_timestamp();
  /* (^report(46)) on the worker's timeline, with the evaluator's share;
     (^report(47)) and the ids minted meanwhile */
  let e0 = eval_total^;
  let n = ref(0);
  let i0 = recording^ ? id_count() : 0;
  let rec go = model => {
    incr(n);
    let next = run_scheduled_slice(model);
    switch (next.runtime) {
    | Idle => next
    | Planning
    | Starting
    | Running(_) =>
      Util.JsUtil.precise_timestamp() -. t0 < slice_time_budget_ms
        ? go(next) : next
    };
  };
  wtime(
    ~extra=
      () =>
        [
          ("eval_ms", Core.Time_ns.Span.(to_ms(eval_total^ - e0))),
          ("slices", float_of_int(n^)),
          ("ids", float_of_int(id_count() - i0 - 1)),
        ],
    "slices",
    () => go(model),
  );
};

/* the pieces of the last request received, by hash: what the client
   counts on the worker holding */
let held: Hashtbl.t(EvalPieces.hash, EvalPieces.content) =
  Hashtbl.create(256);

/* (^ticket(666)) the programs composed lately, by their layouts, the most
   recent first. a layout names each piece by its content's hash, so the
   same layout composes to the same program: an undo's or a redo's layout,
   or the same program posted again, is taken as it was composed, the very
   same objects, which the worker's identity-keyed caches find again, and
   not composed anew (Full Body: 47 ms a request, on GitHub's runner, dev
   profile) */
let composed_lately:
  ref(list((EvalPieces.layout, (Language.Exp.t, Language.EvalInfo.t)))) =
  ref([]);
let composed_kept = 4;

let assemble = (~piece, layout: EvalPieces.layout) =>
  switch (List.assoc_opt(layout, composed_lately^)) {
  | Some(program) =>
    composed_lately :=
      [(layout, program), ...List.remove_assoc(layout, composed_lately^)];
    Ok(program);
  | None =>
    let r = EvalPieces.assemble(~piece, layout);
    switch (r) {
    | Ok(program) =>
      composed_lately :=
        List.filteri(
          (i, _) => i < composed_kept,
          [(layout, program), ...composed_lately^],
        )
    | Error(_) => ()
    };
    r;
  };

type received =
  | Composed(Evaluation.t)
  | Missing(list(EvalPieces.key))
  | Malformed(string);

/* [posted]'s programs, from the pieces it carries and those held. a
   piece that isn't the one its listed hash names isn't taken. the
   pieces it lists are held next, whatever it composes to */
let receive = (posted: Posted.t): received => {
  let fresh = Hashtbl.create(16);
  /* in order: a piece posted shared can refer to the values of one
     before it */
  List.iter(
    ((hash, p)) =>
      switch (
        EvalPieces.take(~resolve=Util.ShareCodec.Worker.decode, hash, p)
      ) {
      | Some(content) => Hashtbl.replace(fresh, hash, content)
      | None => ()
      },
    posted.pieces,
  );
  let next = Hashtbl.create(Hashtbl.length(held) + 16);
  List.iter(
    ((_, v: Posted.value)) =>
      List.iter(
        (s: EvalPieces.slot) =>
          switch (
            Hashtbl.find_opt(fresh, s.hash),
            Hashtbl.find_opt(held, s.hash),
          ) {
          | (Some(c), _)
          | (None, Some(c)) => Hashtbl.replace(next, s.hash, c)
          | (None, None) => ()
          },
        EvalPieces.slots(v.layout),
      ),
    posted.batch,
  );
  Hashtbl.reset(held);
  Hashtbl.iter(Hashtbl.replace(held), next);
  let piece = Hashtbl.find_opt(held);
  switch (
    List.map(
      ((key, v: Posted.value)) => (key, v, assemble(~piece, v.layout)),
      posted.batch,
    )
  ) {
  | exception (EvalPieces.Malformed(why)) => Malformed(why)
  | composed =>
    let missing =
      List.concat_map(
        ((_, _, r)) =>
          switch (r) {
          | Error(keys) => keys
          | Ok(_) => []
          },
        composed,
      );
    missing != []
      ? Missing(missing)
      : Composed({
          request_id: posted.request_id,
          batch:
            List.filter_map(
              ((key, v: Posted.value, r)) =>
                Stdlib.Result.to_option(r)
                |> Option.map(((expr, eval_info_map)) =>
                     (
                       key,
                       Evaluation.{
                         expr,
                         eval_info_map,
                         prev: v.prev,
                         stream: v.stream,
                       },
                     )
                   ),
              composed,
            ),
        });
  };
};

let install_message_handler = () => {
  let model = ref(initial_model);

  let rec commit = next_model => {
    let should_schedule_slice =
      switch (next_model.runtime) {
      | Idle => false
      | Planning
      | Starting
      | Running(_) => !next_model.slice_already_scheduled
      };
    model :=
      should_schedule_slice
        ? {
          ...next_model,
          slice_already_scheduled: true,
        }
        : next_model;
    if (should_schedule_slice) {
      schedule_async(() => commit(run_slices_for_budget(model^)));
    };
  };

  let start = (posted: Posted.t): unit => {
    post_ack(posted.request_id);
    switch (wtime("request composed", () => receive(posted))) {
    | Composed(request) =>
      eval_total := Core.Time_ns.Span.zero;
      commit({
        ...model^,
        latest_request: Some(request),
        runtime: Planning,
      });
    | Missing(_) =>
      post_message(Resend({request_id: posted.request_id}));
      commit({
        ...model^,
        latest_request: None,
        runtime: Idle,
      });
    | Malformed(why) =>
      post_message(
        Result({
          request_id: posted.request_id,
          response:
            List.map(
              ((key, _)) =>
                (
                  key,
                  Error(
                    Language.ProgramResult.UnknownException(
                      "eval request: " ++ why,
                    ),
                  ),
                ),
              posted.batch,
            ),
          eval_time: Core.Time_ns.Span.zero,
          final_streams: [],
        }),
      );
      commit({
        ...model^,
        latest_request: None,
        runtime: Idle,
      });
    };
  };
  let on_request = (req: Active.request): unit =>
    switch (
      wtime(
        ~extra=
          () =>
            [
              (
                "bytes",
                Core.Byte_units.bytes_float(Active.size_request(req)),
              ),
            ],
        "request decoded",
        () => Active.decode_request(req),
      )
    ) {
    | ClientMessage.Evaluate(posted) => start(posted)
    | ClientMessage.EvaluateShared(reset, posted) =>
      /* (a piece with a reference it cannot resolve isn't taken: the
         request is then missing it, and the client posts it in full, the
         tables reset) */
      if (reset) {
        Util.ShareCodec.Worker.reset();
      };
      start(posted);
    | ClientMessage.ResetResponses => response_reset := true
    | ClientMessage.Record(on) =>
      recording := on;
      if (!on) {
        marks := [];
      };
    };

  Js_of_ocaml.Worker.set_onmessage(on_request);
};
