open Language;

/* Bridges the core-side HTML projector to the web-side AppStore.
 *
 * An inline MVU app's state lives in the store (`Globals.Model.apps`, web),
 * but the projector that renders it lives in core, which cannot depend on
 * web. So the web layer installs these function refs once at startup
 * (AppBridgeInstall.install, called from Page); the defaults are inert, so
 * core-only builds (CLI, tests) behave as if no store existed.
 *
 * Precedent for the indirection: Ascriptions.ctx_ref, populated by
 * Evaluator at module load. */

/* (id, evaluated 4-tuple, checkpoint): ensure the store holds an entry for
 * `id` bound to this app value, creating or rebinding it as needed. Called
 * from render, so the installed implementation must no-op when the entry is
 * already current, and must schedule an action rather than mutate. The
 * checkpoint, if any, is a serialized model to restore on first build. */
let ensure_app: ref((Id.t, DHExp.t, option(string)) => unit) =
  ref((_, _, _) => ());

/* The entry's current view value, i.e. view_fn(model). None until built. */
let current_html: ref(Id.t => option(DHExp.t)) = ref(_ => None);

/* Route a msg to the entry's update_fn. */
let dispatch: ref((Id.t, DHExp.t) => Ui_effect.t(unit)) =
  ref((_, _) => Ui_effect.Ignore);

/* The entry's model, serialized, if it is checkpointable; see
 * AppStore.checkpoint. None when there's nothing safe to persist. */
let checkpoint: ref(Id.t => option(string)) = ref(_ => None);

/* Bumped by the web side whenever the store changes. Projector views are
 * memoized on their inputs (ProjectorView.ViewCache), and the store is an
 * input the cache can't see; this counter is how it participates. */
let version: ref(int) = ref(0);

let bump = () => version := version^ + 1;
