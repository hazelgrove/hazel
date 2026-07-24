open Util;
open Language;
open IdTagged.FreshGrammar;

/* AppStore: id-keyed store of live MVU apps (sidebar + future inline apps).
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
    update_fn: option(DHExp.t), // memo: Some = Elm mode, None = legacy
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

// Synthetic id for the sidebar app (deterministic UUID from string)
let sidebar_id: Id.t = Id.mk_str("app-view-sidebar");

let empty: t = Id.Map.empty;

let lookup = (id: Id.t, store: t): option(Entry.t) =>
  Id.Map.find_opt(id, store);

// === Evaluation helpers ===

// Direct evaluation: MVU runtime values are already elaborated + evaluated
let eval = Haz3lcore.MvuShape.evaluate;
let safe_eval = Haz3lcore.MvuShape.safe_evaluate;

// Full elaboration, for source-level values (legacy set_model path)
let eval_elab = (exp: DHExp.t): DHExp.t => {
  let (_info_map, elaborated) =
    Statics.mk(
      CoreSettings.on,
      Builtins.ctx_init(Some(Operators.Int)),
      exp,
    );
  fst(Evaluator.evaluate(~env=Builtins.env_init, elaborated));
};

let safe_eval_elab = (exp: DHExp.t): result(DHExp.t, string) =>
  try(Ok(eval_elab(exp))) {
  | exn => Error(Printexc.to_string(exn))
  };

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
      ~elab=false,
      id: Id.t,
      ~model: DHExp.t,
      ~update_fn: option(DHExp.t),
      ~subs_fn: option(DHExp.t),
      old_handles: list(Haz3lcore.SubManager.sub_handle),
    )
    : list(Haz3lcore.SubManager.sub_handle) => {
  Haz3lcore.SubManager.cleanup(old_handles);
  switch (subs_fn) {
  | None => []
  | Some(subs_fn) =>
    let subs_exp = Exp.ap(Forward, subs_fn, model);
    switch (elab ? safe_eval_elab(subs_exp) : safe_eval(subs_exp)) {
    | Error(err) =>
      prerr_endline("AppStore: subscriptions eval error: " ++ err);
      [];
    | Ok(sub) =>
      let ctx: Haz3lcore.SubManager.context = {
        model,
        inject: mk_inject(~schedule_msg, id),
        update_fn,
      };
      Haz3lcore.SubManager.subscribe(ctx, sub, () => model);
    };
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
      ~update_fn: option(DHExp.t),
      ~view_fn: DHExp.t,
      ~subs_fn: option(DHExp.t),
      store: t,
    )
    : t => {
  let old = lookup(id, store);
  let (model, html) = {
    let fresh = () => (
      init_model,
      eval(Exp.ap(Forward, view_fn, init_model)),
    );
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
    resubscribe(~schedule_msg, id, ~model, ~update_fn, ~subs_fn, old_handles);
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

// Legacy path (no update_fn routing): the injected value IS the new model.
let set_model =
    (
      ~schedule_msg: (Id.t, DHExp.t) => unit,
      id: Id.t,
      new_model: DHExp.t,
      store: t,
    )
    : t =>
  switch (lookup(id, store)) {
  | None => store // no app at this id yet
  | Some(entry) =>
    let html = eval_elab(Exp.ap(Forward, entry.view_fn, new_model));
    let sub_handles =
      resubscribe(
        ~schedule_msg,
        ~elab=true,
        id,
        ~model=new_model,
        ~update_fn=entry.update_fn,
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
  };

// Route a msg through the entry's update_fn: eval update(msg, model), accept
// both `model'` and `(model', cmd)` results, run the cmd, re-derive html,
// and reconcile subscriptions. Legacy entries (no update_fn) treat the msg
// as the new model (set_model semantics), so subscription handlers can use
// a single inject path.
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
    switch (entry.update_fn) {
    | None => set_model(~schedule_msg, id, msg, store)
    | Some(update_fn) =>
      try({
        let result =
          eval(Exp.ap(Forward, update_fn, Exp.tuple([msg, entry.model])))
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
        run_cmd(
          {
            model: new_model,
            inject: mk_inject(~schedule_msg, id),
            update_fn: entry.update_fn,
          },
          cmd,
        );
        let sub_handles =
          resubscribe(
            ~schedule_msg,
            id,
            ~model=new_model,
            ~update_fn=entry.update_fn,
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
    }
  };

let remove = (id: Id.t, store: t): t =>
  switch (lookup(id, store)) {
  | None => store
  | Some(entry) =>
    Haz3lcore.SubManager.cleanup(entry.sub_handles);
    Id.Map.remove(id, store);
  };

// Drop every entry whose id fails the liveness predicate. The synthetic
// sidebar entry is never gc'd (its id corresponds to no syntax).
let gc = (live: Id.t => bool, store: t): t =>
  Id.Map.fold(
    (id, _entry, acc) =>
      id == sidebar_id || live(id) ? acc : remove(id, acc),
    store,
    store,
  );
