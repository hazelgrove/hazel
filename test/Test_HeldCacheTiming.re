/* A trial, not for merging (branch trial/2606-timing): how long the
   messages between the page and the eval worker take to encode and decode
   with the incremental cache in them, as on dev, and without it, as on
   hazelgrove/hazel#2606 ("Keep the incremental cache in the eval worker"),
   on the programs #2606's description measures. For each program: one cold
   run makes the cache; then, with the active encoding (Marshal), the next
   request with the cache (dev) and without it (#2606), and the answer with
   the cache (dev) and without it (#2606), each encoded and decoded, the
   eight measures interleaved, the median of 21 rounds of 5 calls; and, for
   scale, a re-run against the cache with nothing changed. Writes
   HELDTIMING lines to heldtiming.out, and asserts only that the messages
   without the cache are smaller. */
open Alcotest;
open Language;

let now = (): float =>
  Js_of_ocaml.Js.Unsafe.meth_call(
    Js_of_ocaml.Js.Unsafe.js_expr("performance"),
    "now",
    [||],
  );

let program = (path: string): Exp.t => {
  let root =
    Sys.file_exists("hazel-programs")
      ? "hazel-programs" : "../hazel-programs";
  let full = Filename.concat(root, path);
  let ic = open_in_bin(full);
  let src = really_input_string(ic, in_channel_length(ic));
  close_in(ic);
  switch (
    Haz3lcore.PersistentZipper.parse_text(
      ~source="held-cache-timing",
      ~root=Exp,
      src,
    )
  ) {
  | Some(z) => Haz3lcore.MakeTerm.from_zip_for_sem(z, ~root=Exp).term
  | None => fail(full ++ ": did not parse")
  };
};

let value_of = (exp: Exp.t): WorkerServer.Request.value => {
  let (info_map, elab) =
    Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), exp);
  WorkerServer.Request.{
    expr: elab,
    eval_info_map:
      EvalInfo.of_info_map(
        ~probe_all=CoreSettings.on.probe_all,
        ~targets=Id.Map.empty,
        info_map,
      ),
    prev: IncrEval.empty,
    use_held_prev: false,
    doc: "held-cache-timing",
  };
};

let request_of = (v: WorkerServer.Request.value): WorkerServer.ClientMessage.t =>
  WorkerServer.ClientMessage.Evaluate(
    WorkerServer.Request.{
      request_id: 1,
      batch: [("", v)],
    },
  );

let response_of =
    (r: WorkerServer.Response.value): WorkerServer.ServerMessage.t =>
  WorkerServer.ServerMessage.Result(
    WorkerServer.ServerMessage.{
      request_id: 1,
      response: [("", r)],
      eval_time: Core.Time_ns.Span.zero,
    },
  );

let median = (xs: list(float)): float => {
  let a = Array.of_list(xs);
  Array.sort(compare, a);
  a[Array.length(a) / 2];
};

/* the eight measures, interleaved: each round calls each measure k times */
let timed = (~rounds: int, ~k: int, fs: list(unit => unit)): list(float) => {
  List.iter(f => f(), fs);
  let samples = Array.make(List.length(fs), []);
  for (_ in 1 to rounds) {
    List.iteri(
      (i, f) => {
        let t0 = now();
        for (_ in 1 to k) {
          f();
        };
        samples[i] = [(now() -. t0) /. float_of_int(k), ...samples[i]];
      },
      fs,
    );
  };
  Array.to_list(Array.map(median, samples));
};

let out = (line: string): unit => {
  let oc =
    open_out_gen(
      [Open_append, Open_creat, Open_text],
      0o644,
      "heldtiming.out",
    );
  output_string(oc, line ++ "\n");
  close_out(oc);
};

let case = (path: string): test_case(unit) =>
  test_case(
    path,
    `Quick,
    () => {
      module E = WorkerServer.MarshalEncoding;
      let v0 = value_of(program(path));
      let cold = WorkerServer.evaluate_sync(v0);
      let cache =
        switch (cold) {
        | Ok((_, state)) => state.incr_eval
        | Error(_) => fail(path ++ ": the cold run failed")
        };
      let req_dev =
        request_of({
          ...v0,
          prev: cache,
        });
      let req_2606 =
        request_of({
          ...v0,
          use_held_prev: true,
        });
      let resp_dev = response_of(cold);
      let resp_2606 =
        response_of(
          WorkerServer.Held.keep(~doc="held-cache-timing", "", cold),
        );
      let enc_req_dev = E.encode_request(req_dev);
      let enc_req_2606 = E.encode_request(req_2606);
      let enc_resp_dev = E.encode_response(resp_dev);
      let enc_resp_2606 = E.encode_response(resp_2606);
      let bytes_req = r => Core.Byte_units.bytes_int_exn(E.size_request(r));
      let bytes_resp = r =>
        Core.Byte_units.bytes_int_exn(E.size_response(r));
      let keep = x => ignore(Sys.opaque_identity(x));
      let ms =
        timed(
          ~rounds=21,
          ~k=5,
          [
            () => keep(E.encode_request(req_dev)),
            () => keep(E.encode_request(req_2606)),
            () => keep(E.decode_request(enc_req_dev)),
            () => keep(E.decode_request(enc_req_2606)),
            () => keep(E.encode_response(resp_dev)),
            () => keep(E.encode_response(resp_2606)),
            () => keep(E.decode_response(enc_resp_dev)),
            () => keep(E.decode_response(enc_resp_2606)),
          ],
        );
      let rerun =
        timed(
          ~rounds=7,
          ~k=1,
          [
            () =>
              keep(
                WorkerServer.evaluate_sync({
                  ...v0,
                  prev: cache,
                }),
              ),
          ],
        );
      let names = [
        ("request", "encode", "dev", bytes_req(enc_req_dev)),
        ("request", "encode", "2606", bytes_req(enc_req_2606)),
        ("request", "decode", "dev", bytes_req(enc_req_dev)),
        ("request", "decode", "2606", bytes_req(enc_req_2606)),
        ("response", "encode", "dev", bytes_resp(enc_resp_dev)),
        ("response", "encode", "2606", bytes_resp(enc_resp_2606)),
        ("response", "decode", "dev", bytes_resp(enc_resp_dev)),
        ("response", "decode", "2606", bytes_resp(enc_resp_2606)),
      ];
      List.iter2(
        ((msg, op, build, bytes), t) =>
          out(
            Printf.sprintf(
              "HELDTIMING %s %s %s %s bytes=%d ms=%.3f",
              path,
              msg,
              op,
              build,
              bytes,
              t,
            ),
          ),
        names,
        ms,
      );
      out(
        Printf.sprintf(
          "HELDTIMING %s rerun-with-cache - - bytes=0 ms=%.3f",
          path,
          List.hd(rerun),
        ),
      );
      check(
        bool,
        "the request without the cache is smaller",
        true,
        bytes_req(enc_req_2606) < bytes_req(enc_req_dev),
      );
      check(
        bool,
        "the answer without the cache is smaller",
        true,
        bytes_resp(enc_resp_2606) < bytes_resp(enc_resp_dev),
      );
    },
  );

let tests = [
  (
    "HeldCacheTiming",
    List.map(
      case,
      [
        "docs/reference/probes.hz",
        "docs/b2t2/table-api-constructors-leftjoin.hz",
        "docs/reference/tuples.hz",
        "docs/reference/tables.hz",
      ],
    ),
  ),
];
