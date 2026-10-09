open Alcotest;
open Web;

let step = (action, model) => {
  let scheduled = ref([]);
  let next =
    AgentGlobals.Update.update(action, model, action =>
      scheduled := scheduled^ @ [action]
    );
  (next, scheduled^);
};

/* A controllable IndexedDB boundary: requests can succeed before a transaction
   commits. Authorization and reset must wait for the latter. */
let with_database = run => {
  let fixture =
    Js_of_ocaml.Js.Unsafe.eval_string(
      {js|
    (() => {
      const previous = {indexedDB: globalThis.indexedDB, auth: globalThis.hazelAgentAuth};
      const opens = [], transactions = [];
      let browserClears = 0;
      const database = {transaction(names) {
        const tx = {name: names[0], puts: [], clears: 0};
        tx.objectStore = () => ({transaction: tx,
          put(value, key) {tx.puts.push([key, value]); return {};},
          clear() {tx.clears++; return {};}});
        transactions.push(tx); return tx;
      }};
      globalThis.indexedDB = {open() {const request = {result: database}; opens.push(request); return request;}};
      globalThis.hazelAgentAuth = {clearEditorStorage() {browserClears++;}};
      return {
        open() {opens.shift().onsuccess({});},
        complete(i) {transactions[i].oncomplete?.({});},
        abort(i) {transactions[i].onabort?.({});},
        puts(i) {return JSON.stringify(transactions[i].puts);},
        cleared() {return transactions.filter(tx => tx.clears === 1).map(tx => tx.name).sort().join(',');},
        browserClears() {return browserClears;},
        restore() {globalThis.indexedDB = previous.indexedDB; globalThis.hazelAgentAuth = previous.auth;},
      };
    })()
  |js},
    );
  let cache = Web.HazelDB.cache^;
  Fun.protect(
    ~finally=
      () => {
        Web.HazelDB.cache := cache;
        ignore(Js_of_ocaml.Js.Unsafe.meth_call(fixture, "restore", [||]));
      },
    () => run(fixture),
  );
};

let tests = (
  "Agent local key",
  [
    test_case(
      "authorization save waits for transaction commit and reports abort",
      `Quick,
      () =>
      with_database(db => {
        let call = (name, args) =>
          Js_of_ocaml.Js.Unsafe.meth_call(db, name, args);
        let index = n => [|Js_of_ocaml.Js.Unsafe.inject(n)|];
        HazelDB.kv_save("auth-save-test", "latest editor data");
        ignore(call("open", [||]));
        let results = ref([]);
        HazelDB.flush(~callback=ok => results := results^ @ [ok], ());
        check(list(bool), "no early navigation", [], results^);
        ignore(call("open", [||]));
        let puts = call("puts", index(1)) |> Js_of_ocaml.Js.to_string;
        check(
          bool,
          "latest data included",
          true,
          try(
            {
              ignore(
                Str.search_forward(
                  Str.regexp_string("latest editor data"),
                  puts,
                  0,
                ),
              );
              true;
            }
          ) {
          | Not_found => false
          },
        );
        check(list(bool), "requests alone do not navigate", [], results^);
        ignore(call("complete", index(1)));
        check(list(bool), "commit allows navigation", [true], results^);
        HazelDB.flush(~callback=ok => results := results^ @ [ok], ());
        ignore(call("open", [||]));
        ignore(call("abort", index(2)));
        ignore(call("complete", index(2)));
        check(
          list(bool),
          "abort reports failure once",
          [true, false],
          results^,
        );
      })
    ),
    test_case(
      "reset waits for both editor stores and uses credential-preserving cleanup",
      `Quick,
      () =>
      with_database(db => {
        let call = (name, args) =>
          Js_of_ocaml.Js.Unsafe.meth_call(db, name, args);
        let done_ = ref(false);
        HazelDB.clear_all(~callback=() => done_ := true, ());
        check(bool, "no early reload", false, done_^);
        check(
          int,
          "dedicated browser cleanup used",
          1,
          call("browserClears", [||]),
        );
        ignore(call("open", [||]));
        check(
          string,
          "both editor stores cleared",
          "kv,log",
          call("cleared", [||]) |> Js_of_ocaml.Js.to_string,
        );
        ignore(call("complete", [|Js_of_ocaml.Js.Unsafe.inject(0)|]));
        check(bool, "waits for second store", false, done_^);
        ignore(call("complete", [|Js_of_ocaml.Js.Unsafe.inject(1)|]));
        check(bool, "reload after both commits", true, done_^);
      })
    ),
    test_case(
      "editor serialization excludes runtime credentials",
      `Quick,
      () => {
        let runtime = {
          ...AgentGlobals.init(),
          api_key: Some("credential-test-only"),
          remember_browser_key: true,
          remember_local_key: true,
          local_key_status: LocalSaved,
        };
        let restored =
          runtime
          |> AgentGlobals.Model.sexp_of_t
          |> AgentGlobals.Model.t_of_sexp;
        check(option(string), "sexp has no key", None, restored.api_key);
        check(
          bool,
          "browser preference not in settings",
          false,
          restored.remember_browser_key,
        );
        let json_restored =
          runtime
          |> AgentGlobals.Model.yojson_of_t
          |> AgentGlobals.Model.t_of_yojson;
        check(
          option(string),
          "JSON has no key",
          None,
          json_restored.api_key,
        );
        let action = AgentGlobals.Update.SetApiKey("credential-test-only");
        let redacted =
          action
          |> AgentGlobals.Update.sexp_of_action
          |> AgentGlobals.Update.action_of_sexp;
        check(
          bool,
          "credential actions cannot be exported or replayed",
          true,
          redacted == CredentialEvent,
        );
        let settings = {
          ...Settings.Model.init,
          agent_globals: runtime,
        };
        let restored =
          settings |> Settings.Model.sexp_of_t |> Settings.Model.t_of_sexp;
        check(
          option(string),
          "nested editor settings have no key",
          None,
          restored.agent_globals.api_key,
        );
      },
    ),
    test_case(
      "browser opt-out never restores an old settings key",
      `Quick,
      () => {
        let (restored, actions) =
          step(
            RestoreBrowserApiKey({
              key: None,
              remember: false,
              error: false,
            }),
            {
              ...AgentGlobals.init(),
              api_key: Some("legacy-key"),
            },
          );
        check(
          option(string),
          "old settings key discarded",
          None,
          restored.api_key,
        );
        check(
          bool,
          "continues local lookup",
          true,
          actions == [AgentGlobals.Update.LoadLocalApiKey],
        );
      },
    ),
    test_case(
      "browser opt-out preserves this session and recovers on failure",
      `Quick,
      () => {
        let original = {
          ...AgentGlobals.init(),
          api_key: Some("test-key"),
          remember_browser_key: true,
        };
        let (pending, actions) =
          step(SetRememberBrowserKey(false), original);
        check(
          option(string),
          "session retained",
          original.api_key,
          pending.api_key,
        );
        check(
          bool,
          "persistence scheduled",
          true,
          actions == [AgentGlobals.Update.PersistBrowserApiKey(true)],
        );
        let (failed, _) = step(BrowserKeySaved(false, true), pending);
        check(
          bool,
          "failed removal keeps preference",
          true,
          failed.remember_browser_key,
        );
        check(bool, "failure reported", true, failed.browser_key_error);
        let (success, _) = step(BrowserKeySaved(true, true), pending);
        check(
          bool,
          "successful removal opts out",
          false,
          success.remember_browser_key,
        );
        check(
          option(string),
          "session still retained",
          original.api_key,
          success.api_key,
        );
      },
    ),
    test_case(
      "OAuth credentials honor the chosen storage options",
      `Quick,
      () => {
        let (connected, actions) =
          step(
            OpenRouterConnected(
              Some({
                key: Some("oauth-test-key"),
                remember_browser: false,
                remember_local: true,
                error: None,
              }),
            ),
            {
              ...AgentGlobals.init(),
              local_key_status: LocalReady,
            },
          );
        check(
          bool,
          "browser remains session-only",
          false,
          connected.remember_browser_key,
        );
        check(bool, "local choice kept", true, connected.remember_local_key);
        check(
          bool,
          "normal key pipeline",
          true,
          actions == [AgentGlobals.Update.SetApiKey("oauth-test-key")],
        );
        let (failed, _) =
          step(
            OpenRouterConnected(
              Some({
                key: None,
                remember_browser: false,
                remember_local: false,
                error: Some("Cancelled"),
              }),
            ),
            {
              ...connected,
              api_key: Some("existing-key"),
            },
          );
        check(
          option(string),
          "failed connection keeps current key",
          Some("existing-key"),
          failed.api_key,
        );
      },
    ),
    test_case(
      "restore never writes a credential",
      `Quick,
      () => {
        let (restored, actions) =
          step(
            RestoreLocalApiKey(Some(Some("saved-test-key"))),
            AgentGlobals.init(),
          );
        check(
          option(string),
          "restores saved key",
          Some("saved-test-key"),
          restored.api_key,
        );
        check(
          bool,
          "existing opt-in shown",
          true,
          restored.remember_local_key,
        );
        check(
          bool,
          "remembered status",
          true,
          restored.local_key_status == LocalSaved,
        );
        check(
          bool,
          "only refreshes model catalog",
          true,
          actions
          == [AgentGlobals.Update.RefreshModels, FinishOpenRouterConnection],
        );
        let (existing, actions) =
          step(
            RestoreLocalApiKey(Some(Some("old-test-key"))),
            {
              ...AgentGlobals.init(),
              api_key: Some("browser-test-key"),
            },
          );
        check(
          option(string),
          "browser key wins",
          Some("browser-test-key"),
          existing.api_key,
        );
        check(
          bool,
          "different key not opted in",
          false,
          existing.remember_local_key,
        );
        check(
          bool,
          "different local key acknowledged",
          true,
          existing.local_key_status == LocalOtherKey,
        );
        check(
          bool,
          "does not overwrite local key",
          true,
          actions
          == [AgentGlobals.Update.RefreshModels, FinishOpenRouterConnection],
        );
      },
    ),
    test_case(
      "browser-only keys stay opted out across startup",
      `Quick,
      () => {
        let (restored, actions) =
          step(
            RestoreLocalApiKey(Some(None)),
            {
              ...AgentGlobals.init(),
              api_key: Some("browser-key"),
              remember_local_key: true,
            },
          );
        check(
          bool,
          "missing file resets old preference",
          false,
          restored.remember_local_key,
        );
        check(
          bool,
          "no auto-save",
          true,
          actions
          == [AgentGlobals.Update.RefreshModels, FinishOpenRouterConnection],
        );
        let (saved, actions) =
          step(SetApiKey("  replacement-key  "), restored);
        check(
          option(string),
          "browser key trimmed",
          Some("replacement-key"),
          saved.api_key,
        );
        check(
          bool,
          "saving without opt-in clears browser persistence",
          true,
          actions
          == [AgentGlobals.Update.PersistBrowserApiKey(false), RefreshModels],
        );
      },
    ),
    test_case(
      "enabling remembers current key or waits for first key",
      `Quick,
      () => {
        let ready = {
          ...AgentGlobals.init(),
          local_key_status: LocalReady,
        };
        let (opted_in, actions) = step(SetRememberLocalKey(true), ready);
        check(bool, "opted in", true, opted_in.remember_local_key);
        check(bool, "no empty-key write", true, actions == []);
        let (saved, actions) = step(SetApiKey("test-key"), opted_in);
        check(
          bool,
          "save schedules disk write",
          true,
          actions
          == [
               AgentGlobals.Update.SaveLocalApiKey,
               PersistBrowserApiKey(false),
               RefreshModels,
             ],
        );
        check(
          bool,
          "pending write serializes controls",
          true,
          saved.local_key_status == LocalBusy,
        );
        let (_, actions) =
          step(
            SetRememberLocalKey(true),
            {
              ...ready,
              api_key: Some("test-key"),
            },
          );
        check(
          bool,
          "existing key remembered immediately",
          true,
          actions == [AgentGlobals.Update.SaveLocalApiKey],
        );
      },
    ),
    test_case(
      "turning off removes only the local copy",
      `Quick,
      () => {
        let original = {
          ...AgentGlobals.init(),
          api_key: Some("test-key"),
          remember_local_key: true,
          local_key_status: LocalSaved,
        };
        let (pending, actions) = step(SetRememberLocalKey(false), original);
        check(
          bool,
          "deletion scheduled",
          true,
          actions == [AgentGlobals.Update.DeleteLocalApiKey],
        );
        let (success, _) = step(LocalKeyMemoryRemoved(true), pending);
        check(
          option(string),
          "keeps browser key",
          original.api_key,
          success.api_key,
        );
        check(bool, "opted out", false, success.remember_local_key);
        let (failed, _) = step(LocalKeyMemoryRemoved(false), pending);
        check(
          bool,
          "failed deletion restores check",
          true,
          failed.remember_local_key,
        );
        check(
          bool,
          "reports failure",
          true,
          failed.local_key_status == LocalError,
        );
        check(
          option(string),
          "failure retains browser key",
          original.api_key,
          failed.api_key,
        );
      },
    ),
    test_case(
      "pending writes block overlapping changes",
      `Quick,
      () => {
        let busy = {
          ...AgentGlobals.init(),
          api_key: Some("test-key"),
          remember_local_key: true,
          local_key_status: LocalBusy,
        };
        List.iter(
          action => {
            let (next, actions) = step(action, busy);
            check(bool, "model unchanged", true, next == busy);
            check(bool, "no overlapping operation", true, actions == []);
          },
          [
            SetRememberLocalKey(false),
            SetApiKey("new-key"),
            ForgetLocalApiKey,
          ],
        );
      },
    ),
    test_case(
      "forget clears only after deletion succeeds",
      `Quick,
      () => {
        let original = {
          ...AgentGlobals.init(),
          api_key: Some("test-key"),
          remember_local_key: true,
          local_key_status: LocalSaved,
        };
        let (failure, _) = step(LocalKeyForgotten(false), original);
        check(
          option(string),
          "failure retains key",
          original.api_key,
          failure.api_key,
        );
        let (success, _) = step(LocalKeyForgotten(true), original);
        check(
          option(string),
          "success clears browser key",
          None,
          success.api_key,
        );
        check(
          bool,
          "success resets opt-in",
          false,
          success.remember_local_key,
        );
      },
    ),
    test_case(
      "hosted builds keep browser storage",
      `Quick,
      () => {
        let original = {
          ...AgentGlobals.init(),
          api_key: Some("test-key"),
        };
        let (restored, actions) = step(RestoreLocalApiKey(None), original);
        check(
          bool,
          "local storage unavailable",
          true,
          restored.local_key_status == BrowserOnly,
        );
        check(
          bool,
          "catalog refreshed",
          true,
          actions
          == [AgentGlobals.Update.RefreshModels, FinishOpenRouterConnection],
        );
        let (next, actions) = step(SetRememberLocalKey(true), restored);
        check(
          bool,
          "cannot opt into absent endpoint",
          true,
          next == restored && actions == [],
        );
      },
    ),
    test_case(
      "old settings default to opt-out",
      `Quick,
      () => {
        let old = AgentGlobals.init() |> AgentGlobals.Model.sexp_of_t;
        let old =
          switch (old) {
          | Sexplib.Sexp.List(fields) =>
            Sexplib.Sexp.List(
              List.filter(
                field =>
                  switch (field) {
                  | Sexplib.Sexp.List([Atom("local_key_status"), ..._])
                  | Sexplib.Sexp.List([Atom("remember_local_key"), ..._]) =>
                    false
                  | _ => true
                  },
                fields,
              ),
            )
          | _ => old
          };
        let restored = AgentGlobals.Model.t_of_sexp(old);
        check(
          bool,
          "default status",
          true,
          restored.local_key_status == BrowserOnly,
        );
        check(bool, "default preference", false, restored.remember_local_key);
      },
    ),
  ],
);
