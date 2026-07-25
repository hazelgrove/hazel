open Haz3lcore;
open Language;

/* Installs the core-side bridge refs (see Haz3lcore.AppBridge). The HTML
 * projector renders inline MVU apps but lives in core, which can't see the
 * AppStore; the store's owner hands it these closures instead. Called from
 * Page.main_view, so they always read the current store and inject. */

/* Checkpoints handed over by the projector, consumed when the store builds
 * the entry. Kept out of the action payload so the existing app-view actions
 * (which AppViewPanel also sends) are untouched. */
let pending_checkpoints: Hashtbl.t(Id.t, string) = Hashtbl.create(4);

let take_checkpoint = (id: Id.t): option(string) => {
  let checkpoint = Hashtbl.find_opt(pending_checkpoints, id);
  Hashtbl.remove(pending_checkpoints, id);
  checkpoint;
};

/* Bring the store's entry for `id` in line with the app value the projector
 * just saw. Called at render time, so it must schedule rather than mutate,
 * and must do nothing at all once the entry is already bound to this value
 * (otherwise every render would schedule another rebind). */
let ensure_app =
    (~globals: Globals.t, id: Id.t, app: DHExp.t, checkpoint: option(string)) =>
  switch (MvuShape.detect_app_kind(app)) {
  | None => ()
  | Some(MvuShape.ElmApp(init_model, update_fn, view_fn, subs_fn)) =>
    let bound =
      switch (AppStore.lookup(id, globals.apps)) {
      | Some(entry) => entry.source_result === app
      | None => false
      };
    if (!bound) {
      switch (checkpoint) {
      | Some(c) => Hashtbl.replace(pending_checkpoints, id, c)
      | None => Hashtbl.remove(pending_checkpoints, id)
      };
      Bonsai.Effect.Expert.handle(
        globals.inject_global(
          InitAppView(id, app, init_model, update_fn, view_fn, subs_fn),
        ),
      );
    };
  };

let install = (~globals: Globals.t): unit => {
  AppBridge.ensure_app := ensure_app(~globals);
  AppBridge.current_html :=
    (
      id =>
        AppStore.lookup(id, globals.apps)
        |> Option.map((entry: AppStore.Entry.t) => entry.html)
    );
  AppBridge.dispatch :=
    ((id, msg) => globals.inject_global(AppViewMsg(id, msg)));
  AppBridge.checkpoint := (id => AppStore.checkpoint(id, globals.apps));
};
