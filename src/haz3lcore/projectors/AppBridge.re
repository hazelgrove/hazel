open Language;

/* Bridges the core-side HTML projector to the web-side AppStore.
 *
 * An app's state lives in the store (web), but the projector rendering it
 * lives in core, which can't depend on web. So web installs these refs at
 * startup (AppBridgeInstall). Defaults are inert, so CLI/test builds behave
 * as if no store existed. Same trick as Ascriptions.ctx_ref. */

/* Ensure the store holds an entry for `id` bound to this app value. Called
   from render, so the implementation must no-op when already current and
   schedule rather than mutate. */
let ensure_app: ref((Id.t, DHExp.t, option(string)) => unit) =
  ref((_, _, _) => ());

/* view_fn(model); None until built. */
let current_html: ref(Id.t => option(DHExp.t)) = ref(_ => None);

let dispatch: ref((Id.t, DHExp.t) => Ui_effect.t(unit)) =
  ref((_, _) => Ui_effect.Ignore);

/* Serialized model, when checkpointable; see AppStore.checkpoint. */
let checkpoint: ref(Id.t => option(string)) = ref(_ => None);

/* ProjectorView.ViewCache memoizes on its inputs, and the store isn't one it
   can see; this counter is how the store participates. */
let version: ref(int) = ref(0);

let bump = () => version := version^ + 1;
