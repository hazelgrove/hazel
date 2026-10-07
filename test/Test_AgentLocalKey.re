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

let tests = (
  "Agent local key",
  [
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
          actions == [AgentGlobals.Update.RefreshModels],
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
          actions == [AgentGlobals.Update.RefreshModels],
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
          actions == [AgentGlobals.Update.RefreshModels],
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
          "saving without opt-in only refreshes models",
          true,
          actions == [AgentGlobals.Update.RefreshModels],
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
          actions == [AgentGlobals.Update.SaveLocalApiKey, RefreshModels],
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
          actions == [AgentGlobals.Update.RefreshModels],
        );
        let (next, actions) = step(SetRememberLocalKey(true), restored);
        check(
          bool,
          "cannot opt into absent endpoint",
          true,
          next == restored && actions == [],
        );
        let (forgotten, actions) = step(ForgetLocalApiKey, restored);
        check(
          option(string),
          "browser key can still be cleared",
          None,
          forgotten.api_key,
        );
        check(bool, "no local deletion", true, actions == []);
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
