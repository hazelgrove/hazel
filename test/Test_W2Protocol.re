/* W2a protocol loopback: drives WorkerServer.handle_sync directly (no
   postMessage), so the full sync→derive→summarize→verdict path runs
   headless. Plus a marshal round-trip on the new message shapes. */

open Alcotest;
open Haz3lcore;
open Language;

let settings = CoreSettings.on;

let src = "let a = 1 in
let b : String = a in
b";

let src' = "let a = 1 in
let b : String = \"s\" in
b";

let parse = src =>
  switch (CorpusUtil.parse(~root=Sort.Exp, src)) {
  | Some(seg) => seg
  | None => Alcotest.fail("parse failed")
  };

let full_sync = (~key="k", ~generation=1, seg): WorkerServer.SyncProgram.t => {
  version: WorkerServer.w2_protocol_version,
  key,
  generation,
  payload: Full(Sort.Exp, settings, seg),
};

let verdict_of = (msg: WorkerServer.ServerMessage.t) =>
  switch (msg) {
  | Summary({verdict, _}) => verdict
  | _ => Alcotest.fail("expected Summary message")
  };

let expect_ok = (msg): ResidentProgram.Summary.t =>
  switch (verdict_of(msg)) {
  | SyncOk(s) => s
  | NeedResync(r) => Alcotest.fail("unexpected NeedResync: " ++ r)
  };

let expect_resync = (name, expected_reason, msg) =>
  switch (verdict_of(msg)) {
  | NeedResync(r) => check(string, name, expected_reason, r)
  | SyncOk(_) => Alcotest.fail(name ++ ": expected NeedResync")
  };

let tests = [
  (
    "W2Protocol",
    [
      test_case("full sync answers a parity-true summary", `Quick, () => {
        let seg = parse(src);
        let (resident, msg) =
          WorkerServer.handle_sync(None, full_sync(seg));
        check(bool, "slot filled", true, resident != None);
        let expected =
          ResidentProgram.Summary.of_def_statics(
            ~generation=1,
            ~piece_ids=ResidentProgram.piece_ids(seg),
            DefStatics.calc(~settings, MakeTerm.go(seg).term),
          );
        check(
          bool,
          "summary parity",
          true,
          ResidentProgram.Summary.equal(expected, expect_ok(msg)),
        );
      }),
      test_case("items before any resident demands resync", `Quick, () => {
        let (resident, msg) =
          WorkerServer.handle_sync(
            None,
            {
              version: WorkerServer.w2_protocol_version,
              key: "k",
              generation: 1,
              payload: Items([], []),
            },
          );
        check(bool, "slot untouched", true, resident == None);
        expect_resync("no resident", "no-resident-program", msg);
      }),
      test_case("protocol version skew demands resync", `Quick, () => {
        let seg = parse(src);
        let sync = {
          ...full_sync(seg),
          WorkerServer.SyncProgram.version:
            WorkerServer.w2_protocol_version + 1,
        };
        let (_, msg) = WorkerServer.handle_sync(None, sync);
        expect_resync("version skew", "protocol-version-skew", msg);
      }),
      test_case("full then one-item delta round trip", `Quick, () => {
        let seg = parse(src);
        let (resident, _) = WorkerServer.handle_sync(None, full_sync(seg));
        let rp =
          switch (resident) {
          | Some((_, _, rp)) => rp
          | None => Alcotest.fail("no resident")
          };
        let items' = ResidentProgram.items_of_segment(parse(src'));
        let changed =
          List.combine(rp.items, items')
          |> List.filter_map(
               (((old: ResidentProgram.item), (nu: ResidentProgram.item))) =>
               old.i_print == nu.i_print
                 ? None : Some((old.i_id, nu.i_seg, nu.i_print))
             );
        check(int, "one change", 1, List.length(changed));
        let roster =
          List.combine(rp.items, items')
          |> List.map(
               (((old: ResidentProgram.item), (nu: ResidentProgram.item))) =>
               old.i_print == nu.i_print
                 ? (old.i_id, old.i_print) : (nu.i_id, nu.i_print)
             );
        let (resident', msg) =
          WorkerServer.handle_sync(
            resident,
            {
              version: WorkerServer.w2_protocol_version,
              key: "k",
              generation: 2,
              payload: Items(changed, roster),
            },
          );
        let rp' =
          switch (resident') {
          | Some((_, _, rp')) => rp'
          | None => Alcotest.fail("no resident after delta")
          };
        let spliced = ResidentProgram.segment_of_items(rp'.items);
        let expected =
          ResidentProgram.Summary.of_def_statics(
            ~generation=2,
            ~piece_ids=ResidentProgram.piece_ids(spliced),
            DefStatics.calc(~settings, MakeTerm.go(spliced).term),
          );
        check(
          bool,
          "delta summary parity",
          true,
          ResidentProgram.Summary.equal(expected, expect_ok(msg)),
        );
      }),
      test_case("sync messages survive the marshal encoding", `Quick, () => {
        let seg = parse(src);
        let req =
          WorkerServer.ClientMessage.Sync(full_sync(seg))
          |> WorkerServer.MarshalEncoding.encode_request
          |> WorkerServer.MarshalEncoding.decode_request;
        check(
          bool,
          "client roundtrip",
          true,
          switch (req) {
          | Sync({generation: 1, key: "k", _}) => true
          | _ => false
          },
        );
        let (_, msg) = WorkerServer.handle_sync(None, full_sync(seg));
        let msg' =
          msg
          |> WorkerServer.MarshalEncoding.encode_response
          |> WorkerServer.MarshalEncoding.decode_response;
        check(
          bool,
          "server roundtrip",
          true,
          switch (msg') {
          | Summary({verdict: SyncOk(_), _}) => true
          | _ => false
          },
        );
      }),
    ],
  ),
];
