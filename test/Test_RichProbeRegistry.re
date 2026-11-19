/* Round-trip tests for the live RichProbeRegistry.
 *
 * RichProbe packs renderer state existentially with a fresh Type.Id witness
 * allocated per `pack_renderer` call. Persisted state (localStorage / file
 * save) goes through the *registry* dispatchers in RichProbeRegistry, which
 * decode by re-binding to the witness held by the live registered renderer.
 *
 * The packed_renderer_tests in Test_TableTransforms cover a locally-allocated
 * pack_renderer with its own witness — they can't catch a registry/witness
 * mismatch. The tests here go through the live registry, then re-encode, and
 * verify the decoded value is usable with the registry's live renderer
 * (witness preserved). */
open Alcotest;
open Haz3lcore;
open Language;

module G = IdTagged.FreshGrammar.Exp;

let mk_table = (rows: list(list((string, Exp.t)))): Exp.t =>
  G.(
    list_lit(
      List.map(
        fields =>
          parens(
            tuple(List.map(((l, v)) => tup_label(label(l), v), fields)),
          ),
        rows,
      ),
    )
  );

let live_renderer =
  switch (RichProbeRegistry.find("table")) {
  | Some(r) => r
  | None => failwith("table renderer not registered")
  };

let table_exp = mk_table([[("x", G.int(1)), ("y", G.int(2))]]);

let initial_pm =
  switch (live_renderer.init_model(Sort.Exp, table_exp)) {
  | Some(pm) => pm
  | None => failwith("init_model should succeed on a valid table")
  };

/* `update_model` only mutates if the cast succeeds — so an observed
   menu_state change after a roundtrip proves the witness survived. */
let menu_state_after = (pm, action) =>
  live_renderer.update_model(pm, action)
  |> live_renderer.sexp_of_model_payload
  |> TableRenderer.model_of_sexp
  |> (m => m.menu_state);

let show_menu_action =
  ShowMenu(2)
  |> TableRenderer.sexp_of_action
  |> live_renderer.action_payload_of_sexp;

let registry_tests = [
  test_case(
    "sexp model round-trip is idempotent",
    `Quick,
    () => {
      let s1 = RichProbeRegistry.sexp_of_packed_model(initial_pm);
      let pm2 = RichProbeRegistry.packed_model_of_sexp(s1);
      let s2 = RichProbeRegistry.sexp_of_packed_model(pm2);
      check(
        string,
        "sexp stable",
        Sexplib.Sexp.to_string(s1),
        Sexplib.Sexp.to_string(s2),
      );
    },
  ),
  test_case(
    "sexp model round-trip preserves renderer id",
    `Quick,
    () => {
      let pm2 =
        initial_pm
        |> RichProbeRegistry.sexp_of_packed_model
        |> RichProbeRegistry.packed_model_of_sexp;
      check(string, "id", "table", RichProbe.renderer_id_of_model(pm2));
    },
  ),
  test_case(
    "decoded model is usable by live renderer (sexp)",
    `Quick,
    () => {
      /* If the Type.Id witness is lost on decode, update_model's cast
         fails and ShowMenu is silently dropped. */
      let pm2 =
        initial_pm
        |> RichProbeRegistry.sexp_of_packed_model
        |> RichProbeRegistry.packed_model_of_sexp;
      switch (menu_state_after(pm2, show_menu_action)) {
      | Some((2, Some({selected_idx: 0, path: []}))) => ()
      | _ =>
        fail(
          "expected Some((2, Menu.opened)) after applying ShowMenu(2) to sexp-decoded model",
        )
      };
    },
  ),
  test_case(
    "yojson model round-trip is idempotent",
    `Quick,
    () => {
      let j1 = RichProbeRegistry.yojson_of_packed_model(initial_pm);
      let pm2 = RichProbeRegistry.packed_model_of_yojson(j1);
      let j2 = RichProbeRegistry.yojson_of_packed_model(pm2);
      check(
        string,
        "yojson stable",
        Yojson.Safe.to_string(j1),
        Yojson.Safe.to_string(j2),
      );
    },
  ),
  test_case(
    "decoded model is usable by live renderer (yojson)",
    `Quick,
    () => {
      let pm2 =
        initial_pm
        |> RichProbeRegistry.yojson_of_packed_model
        |> RichProbeRegistry.packed_model_of_yojson;
      switch (menu_state_after(pm2, show_menu_action)) {
      | Some((2, Some({selected_idx: 0, path: []}))) => ()
      | _ =>
        fail(
          "expected Some((2, Menu.opened)) after applying ShowMenu(2) to yojson-decoded model",
        )
      };
    },
  ),
  test_case(
    "sexp action round-trip is idempotent",
    `Quick,
    () => {
      let s1 = RichProbeRegistry.sexp_of_packed_action(show_menu_action);
      let pa2 = RichProbeRegistry.packed_action_of_sexp(s1);
      let s2 = RichProbeRegistry.sexp_of_packed_action(pa2);
      check(
        string,
        "sexp stable",
        Sexplib.Sexp.to_string(s1),
        Sexplib.Sexp.to_string(s2),
      );
    },
  ),
  test_case(
    "decoded action drives the live renderer (sexp)",
    `Quick,
    () => {
      let pa2 =
        show_menu_action
        |> RichProbeRegistry.sexp_of_packed_action
        |> RichProbeRegistry.packed_action_of_sexp;
      switch (menu_state_after(initial_pm, pa2)) {
      | Some((2, Some({selected_idx: 0, path: []}))) => ()
      | _ =>
        fail(
          "expected Some((2, Menu.opened)) after applying sexp-decoded action",
        )
      };
    },
  ),
  test_case(
    "unknown renderer id raises Unknown_renderer",
    `Quick,
    () => {
      let bogus = Sexplib.Sexp.of_string("(does_not_exist (CloseMenu))");
      check_raises(
        "Unknown_renderer",
        RichProbeRegistry.Unknown_renderer("does_not_exist"),
        () =>
        ignore(RichProbeRegistry.packed_model_of_sexp(bogus))
      );
    },
  ),
];

let tests = ("RichProbeRegistry", registry_tests);
