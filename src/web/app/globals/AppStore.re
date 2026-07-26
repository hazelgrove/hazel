open Util;
open Language;
open IdTagged.FreshGrammar;

/* AppStore: id-keyed store of live MVU apps, keyed by the syntax id of the
 * app projector rendering them. This is the state-commit target:
 * dispatching a msg evaluates update(msg, model) and stores the new model.
 *
 * State/memo split: `model` is THE app state and is owned here. The
 * `update_fn`/`view_fn`/`subs_fn` closures are memos derived from the
 * program's eval result; they are rebuilt whenever the program re-evaluates
 * (see `init` rebind). `html` is the current view value (view_fn(model)).
 *
 * Ownership contract: `sub_handles` are live DOM/timer subscriptions owned
 * by the entry. Every operation that changes `model` reconciles them
 * (cleanup old handles, subscribe against the new model), and remove/gc
 * clean them up. Subscription lifecycle lives in the update path here,
 * never at render time. */

/* Live DOM/timer handles: opaque to (de)serialization. */
module Handles = {
  type t = list(Haz3lcore.SubManager.sub_handle);
  let pp = (fmt: Format.formatter, _: t) =>
    Format.fprintf(fmt, "<sub_handles>");
  let show = (_: t) => "<sub_handles>";
  let sexp_of_t = (_: t) => Sexplib.Sexp.Atom("<sub_handles>");
  let t_of_sexp = _: t => [];
  let yojson_of_t = (_: t): Yojson.Safe.t => `String("<sub_handles>");
  let t_of_yojson = _: t => [];
};

module Entry = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    source_result: DHExp.t, // program eval result this entry derives from
    model: DHExp.t, // THE state
    update_fn: DHExp.t, // memo: update: (msg, model) -> model' or (model', cmd)
    view_fn: DHExp.t, // memo: view: model -> Html
    subs_fn: option(DHExp.t), // memo: subscriptions: model -> Sub
    html: DHExp.t, // current view value: view_fn(model)
    sub_handles: Handles.t // owned lifecycle
  };
};

[@deriving (sexp, yojson)]
type t = Id.Map.t(Entry.t);

/* Id.Map.pp only supports empty maps; summarize instead. */
let pp = (fmt: Format.formatter, store: t): unit =>
  Format.fprintf(fmt, "<AppStore: %d apps>", Id.Map.cardinal(store));
let show = (store: t): string => Format.asprintf("%a", pp, store);

let empty: t = Id.Map.empty;

let lookup = (id: Id.t, store: t): option(Entry.t) =>
  Id.Map.find_opt(id, store);

// === Evaluation helpers ===

// Direct evaluation: MVU runtime values are already elaborated + evaluated
let eval = Haz3lcore.MvuShape.evaluate;
let safe_eval = Haz3lcore.MvuShape.safe_evaluate;

let mk_inject =
    (~schedule_msg: (Id.t, DHExp.t) => unit, id: Id.t, m: DHExp.t)
    : Ui_effect.t(unit) => {
  schedule_msg(id, m);
  Ui_effect.Ignore;
};

// Tear down old handles, then subscribe subs_fn(model). The model snapshot
// captured in the handler context stays fresh because every model change
// goes through an operation that re-subscribes.
let resubscribe =
    (
      ~schedule_msg: (Id.t, DHExp.t) => unit,
      id: Id.t,
      ~model: DHExp.t,
      ~subs_fn: option(DHExp.t),
      old_handles: list(Haz3lcore.SubManager.sub_handle),
    )
    : list(Haz3lcore.SubManager.sub_handle) => {
  Haz3lcore.SubManager.cleanup(old_handles);
  switch (subs_fn) {
  | None => []
  | Some(subs_fn) =>
    switch (safe_eval(Exp.ap(Forward, subs_fn, model))) {
    | Error(err) =>
      prerr_endline("AppStore: subscriptions eval error: " ++ err);
      [];
    | Ok(sub) =>
      let ctx: Haz3lcore.SubManager.context = {
        inject: mk_inject(~schedule_msg, id),
      };
      Haz3lcore.SubManager.subscribe(ctx, sub);
    }
  };
};

// Insert (or rebind) the app at `id`. Rebind: when the program re-evaluated
// and this id already has an entry, memos are re-derived from the incoming
// closures but the existing model is KEPT if the incoming view still accepts
// it (live-edit keeps app state); otherwise reset to the incoming init model.
let init =
    (
      ~schedule_msg: (Id.t, DHExp.t) => unit,
      id: Id.t,
      ~source_result: DHExp.t,
      ~init_model: DHExp.t,
      ~update_fn: DHExp.t,
      ~view_fn: DHExp.t,
      ~subs_fn: option(DHExp.t),
      ~checkpoint: option(string)=None,
      store: t,
    )
    : t => {
  let old = lookup(id, store);
  let (model, html) = {
    /* Building the entry from scratch: prefer a checkpointed model, but
       only if the current view still renders it — otherwise discard it
       silently and start from the app's own init model. */
    let restored = () =>
      Option.bind(checkpoint, Haz3lcore.MvuShape.restore_model(~view_fn));
    let fresh = () =>
      switch (restored()) {
      | Some(restored) => restored
      | None => (init_model, eval(Exp.ap(Forward, view_fn, init_model)))
      };
    switch (old) {
    | Some(old) =>
      switch (safe_eval(Exp.ap(Forward, view_fn, old.model))) {
      | Ok(html) => (old.model, html)
      | Error(_) => fresh()
      }
    | None => fresh()
    };
  };
  let old_handles =
    switch (old) {
    | Some(old) => old.sub_handles
    | None => []
    };
  let sub_handles =
    resubscribe(~schedule_msg, id, ~model, ~subs_fn, old_handles);
  Id.Map.add(
    id,
    Entry.{
      source_result,
      model,
      update_fn,
      view_fn,
      subs_fn,
      html,
      sub_handles,
    },
    store,
  );
};

// Route a msg through the entry's update_fn: eval update(msg, model), accept
// both `model'` and `(model', cmd)` results, run the cmd, re-derive html,
// and reconcile subscriptions.
let dispatch =
    (
      ~schedule_msg: (Id.t, DHExp.t) => unit,
      ~run_cmd: (Haz3lcore.CmdRunner.context, DHExp.t) => unit,
      id: Id.t,
      msg: DHExp.t,
      store: t,
    )
    : t =>
  switch (lookup(id, store)) {
  | None => store
  | Some(entry) =>
    try({
      let result =
        eval(
          Exp.ap(Forward, entry.update_fn, Exp.tuple([msg, entry.model])),
        )
        |> Haz3lcore.MvuShape.strip_wrappers;
      let (new_model, cmd) =
        switch (result.term) {
        | Tuple([m, c]) => (m, c)
        | _ =>
          Js_of_ocaml.Firebug.console##warn(
            Js_of_ocaml.Js.string(
              "AppStore.dispatch: update result is not a tuple, using fallback",
            ),
          );
          (result, Exp.constructor("CmdNone", None));
        };
      let html = eval(Exp.ap(Forward, entry.view_fn, new_model));
      // Run cmd (CmdRunner handles CmdNone as a no-op)
      let cmd_ctx: Haz3lcore.CmdRunner.context = {
        inject: mk_inject(~schedule_msg, id),
      };
      run_cmd(cmd_ctx, cmd);
      let sub_handles =
        resubscribe(
          ~schedule_msg,
          id,
          ~model=new_model,
          ~subs_fn=entry.subs_fn,
          entry.sub_handles,
        );
      Id.Map.add(
        id,
        Entry.{
          ...entry,
          model: new_model,
          html,
          sub_handles,
        },
        store,
      );
    }) {
    | exn =>
      Js_of_ocaml.Firebug.console##error(
        Js_of_ocaml.Js.string(
          "AppStore.dispatch EXCEPTION: " ++ Printexc.to_string(exn),
        ),
      );
      store;
    }
  };

/* The entry's model, serialized, unless it carries a captured environment
 * (see MvuShape.is_checkpointable). Not checkpointing is not an error: the
 * app just starts from `init` next time. */
let checkpoint = (id: Id.t, store: t): option(string) =>
  switch (lookup(id, store)) {
  | None => None
  | Some(entry) => Haz3lcore.MvuShape.serialize_model(entry.model)
  };

let remove = (id: Id.t, store: t): t =>
  switch (lookup(id, store)) {
  | None => store
  | Some(entry) =>
    Haz3lcore.SubManager.cleanup(entry.sub_handles);
    Id.Map.remove(id, store);
  };

// Drop every entry whose id fails the liveness predicate.
let gc = (live: Id.t => bool, store: t): t =>
  Id.Map.fold(
    (id, _entry, acc) => live(id) ? acc : remove(id, acc),
    store,
    store,
  );
