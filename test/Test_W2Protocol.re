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
  probe_ids: [],
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

let has_substr = (needle: string, hay: string): bool =>
  switch (Str.search_forward(Str.regexp_string(needle), hay, 0)) {
  | _ => true
  | exception Not_found => false
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
      test_case(
        "full sync answers a parity-true summary",
        `Quick,
        () => {
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
        },
      ),
      test_case(
        "items before any resident demands resync",
        `Quick,
        () => {
          let (resident, msg) =
            WorkerServer.handle_sync(
              None,
              {
                version: WorkerServer.w2_protocol_version,
                key: "k",
                generation: 1,
                probe_ids: [],
                payload: Items([], []),
              },
            );
          check(bool, "slot untouched", true, resident == None);
          expect_resync("no resident", "no-resident-program", msg);
        },
      ),
      test_case(
        "protocol version skew demands resync",
        `Quick,
        () => {
          let seg = parse(src);
          let sync = {
            ...full_sync(seg),
            WorkerServer.SyncProgram.version:
              WorkerServer.w2_protocol_version + 1,
          };
          let (_, msg) = WorkerServer.handle_sync(None, sync);
          expect_resync("version skew", "protocol-version-skew", msg);
        },
      ),
      test_case(
        "full then one-item delta round trip",
        `Quick,
        () => {
          let seg = parse(src);
          let (resident, _) =
            WorkerServer.handle_sync(None, full_sync(seg));
          let rp =
            switch (resident) {
            | Some((_, _, rp)) => rp
            | None => Alcotest.fail("no resident")
            };
          let items' = ResidentProgram.items_of_segment(parse(src'));
          let changed =
            List.combine(rp.items, items')
            |> List.filter_map(
                 ((old: ResidentProgram.item, nu: ResidentProgram.item)) =>
                 old.i_print == nu.i_print
                   ? None : Some((old.i_id, nu.i_seg, nu.i_print))
               );
          check(int, "one change", 1, List.length(changed));
          let roster =
            List.combine(rp.items, items')
            |> List.map(
                 ((old: ResidentProgram.item, nu: ResidentProgram.item)) =>
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
                probe_ids: [],
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
        },
      ),
      test_case(
        "sync messages survive the marshal encoding",
        `Quick,
        () => {
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
        },
      ),
      test_case(
        "rejected delta: eval for its generation must not run the old program",
        `Quick,
        () => {
          /* production shape (codex review, #2480 P0-1): Full(g1) ok,
             Items(g2) rejected (roster mismatch) — Sync/Evaluate are
             FIFO, so the eval for g2 arrives next and must ERROR, not
             silently evaluate g1's program under g2's label */
          let seg = parse(src);
          let (resident, _) =
            WorkerServer.handle_sync(None, full_sync(seg));
          let (resident', msg) =
            WorkerServer.handle_sync(
              resident,
              {
                version: WorkerServer.w2_protocol_version,
                key: "k",
                generation: 2,
                probe_ids: [],
                payload: Items([], [(Id.mk(), 0)]),
              },
            );
          expect_resync("rejected delta", "roster-mismatch", msg);
          WorkerServer.resident_slot := resident';
          let resolve = g =>
            WorkerServer.resolve_payload(
              ~key="k",
              Resident({
                generation: g,
                probe_all: false,
              }),
            );
          switch (resolve(2)) {
          | Error(e) =>
            check(
              bool,
              "g2 errors with a generation mismatch",
              true,
              has_substr("generation mismatch", e),
            )
          | Ok(_) => Alcotest.fail("stale program evaluated as g2")
          };
          check(
            bool,
            "g1 still resolvable",
            true,
            Result.is_ok(resolve(1)),
          );
          WorkerServer.resident_slot := None;
          switch (resolve(1)) {
          | Error(e) =>
            check(
              bool,
              "blank worker (restart class) errors",
              true,
              has_substr("no resident program", e),
            )
          | Ok(_) => Alcotest.fail("resolved against an empty slot")
          };
        },
      ),
      test_case(
        "reset_caches busts the per-document slots table",
        `Quick,
        () => {
          /* codex review #2480 P1-4: set_flip cleared only the active
             slot while calc_auto reads the keyed table — a clamped
             chain survived toggle-off */
          let whole = MakeTerm.go(parse(src)).term;
          let _ = DefStatics.calc_auto(~settings, whole);
          let _ = DefStatics.calc_auto(~settings, whole);
          check(int, "second pass is warm", 0, DefStatics.last_analyzed^);
          DefStatics.reset_caches();
          let _ = DefStatics.calc_auto(~settings, whole);
          check(
            bool,
            "post-reset pass is cold (keyed entry really gone)",
            true,
            DefStatics.last_analyzed^ > 0,
          );
        },
      ),
      test_case(
        "grafted summary survives a warm calc_auto",
        `Quick,
        () => {
          /* codex review #2480 P1-5: a slot-only graft was reverted by
             the next ordinary calc_auto, whose prev comes from the
             keyed table */
          let seg = parse(src);
          let whole = MakeTerm.go(seg).term;
          let t = DefStatics.calc_auto(~settings, whole);
          let first =
            switch (t.items) {
            | [it, ..._] => it
            | [] => Alcotest.fail("no items")
            };
          let fake_err = Id.mk();
          let theirs =
            ResidentProgram.Summary.{
              s_generation: 5,
              s_items: [
                {
                  s_id: first.d_id,
                  s_errors: [fake_err],
                  s_warnings: [],
                  s_synth_errors: 0,
                  s_synth_warnings: 0,
                },
              ],
            };
          Web.ShadowResidency.generation := 5;
          Web.ShadowResidency.last_piece_ids := ResidentProgram.piece_ids(seg);
          Web.ShadowResidency.graft_summary(
            {
              version: WorkerServer.w2_protocol_version,
              key: "",
              generation: 5,
              verdict: SyncOk(theirs),
            },
            theirs,
          );
          let has_fake = (t: DefStatics.t) =>
            List.exists(
              (it: DefStatics.item) => List.mem(fake_err, it.d_error_ids),
              t.items,
            );
          switch (DefStatics.current()) {
          | Some(t) =>
            check(bool, "graft visible in slot", true, has_fake(t))
          | None => Alcotest.fail("no slot after graft")
          };
          let t' = DefStatics.calc_auto(~settings, whole);
          check(int, "recalc is warm", 0, DefStatics.last_analyzed^);
          check(bool, "graft survives the warm recalc", true, has_fake(t'));
        },
      ),
    ],
  ),
];
