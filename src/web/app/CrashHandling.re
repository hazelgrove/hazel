open Util;

type current_exception =
  | Update(string)
  | Calculate(string);

let last_exception: ref(option(exn)) = ref(None);
let current_exception: ref(option(current_exception)) = ref(None);

let set_last_exception = exn => {
  last_exception := Some(exn);
};

let clear_last_exception = () => {
  last_exception := None;
};

let set_current_exception = exn_type => {
  current_exception := Some(exn_type);
};

let clear_current_exception = () => {
  current_exception := None;
};

module Model = {
  [@deriving (sexp, yojson)]
  type state = Logged.Model.t;

  [@deriving (sexp, yojson)]
  type t = {model: state};

  let equal = (===);

  let load = () => {model: Logged.Model.load()};
};

module Update = {
  [@deriving (sexp, yojson)]
  type t = Logged.Update.t;

  let update =
      (
        ~import_log,
        ~get_log_and,
        ~schedule_action: t => unit,
        action: t,
        model: Model.t,
      )
      : Updated.t(Model.t) =>
    switch (action) {
    | Globals(ClearException) =>
      clear_last_exception();
      clear_current_exception();
      model |> Updated.return_quiet;
    | Globals(RethrowException) =>
      switch (last_exception^) {
      | None => model |> Updated.return_quiet
      | Some(exn) => raise(exn)
      }
    | _ when current_exception^ == None =>
      try({
        let updated =
          Logged.Update.update(
            ~import_log,
            ~get_log_and,
            ~schedule_action,
            action,
            model.model,
          );
        {
          ...updated,
          model: {
            model: updated.model,
          },
        };
      }) {
      | Haz3lcore.Action.Failure.Exception(t) =>
        Printf.printf(
          "ERROR: Action.Failure: %s\n",
          t |> Haz3lcore.Action.Failure.show,
        );
        model |> Updated.return_quiet;
      | Updated.InvalidAction =>
        print_endline("cannot perform action");
        model |> Updated.return_quiet;
      | exn =>
        set_last_exception(exn);
        let msg = Printexc.to_string(exn);
        print_endline("CrashHandling: Caught exception in update: " ++ msg);
        set_current_exception(Update(msg));
        model |> Updated.return_quiet;
      }
    | _ => model |> Updated.return_quiet
    };

  let calculate =
      (
        ~schedule_action: t => unit,
        ~is_edited: bool,
        ~dynamics,
        previous_model: Model.t,
        model: Model.t,
      )
      : Model.t =>
    try({
      model:
        model.model
        |> Logged.Update.calculate(~schedule_action, ~is_edited, ~dynamics),
    }) {
    | exn =>
      set_last_exception(exn);
      let msg = Printexc.to_string(exn);
      print_endline("CrashHandling: Caught exception in calculate: " ++ msg);
      set_current_exception(Calculate(msg));
      previous_model;
    };
};

module View = {
  open Virtual_dom.Vdom;
  open WebUtil.Node;

  let hsod_view =
      (
        ~title: string,
        ~msg: string,
        ~inject_backtrack: Ui_effect.t(unit),
        ~inject_rethrow: Ui_effect.t(unit),
      ) =>
    div(
      ~attrs=[Attr.class_("hsod-container")],
      [
        div(
          ~attrs=[Attr.class_("hsod")],
          [
            div(
              ~attrs=[Attr.class_("hsod-inner")],
              [
                div(
                  ~attrs=[Attr.class_("hsod-img")],
                  [
                    Node.img(
                      ~attrs=[
                        Attr.create("src", "img/dead-hazel.png"),
                        Attr.create("alt", "dead hazel"),
                      ],
                      (),
                    ),
                  ],
                ),
                div(
                  ~attrs=[Attr.class_("hsod-body")],
                  [
                    h1([Node.text(title)]),
                    pre([Node.text(msg)]),
                    div(
                      ~attrs=[Attr.class_("hsod-links")],
                      [
                        // button(
                        //   ~attrs=[
                        //     Attr.create("type", "button"),
                        //     Attr.class_("hsod-button"),
                        //     Attr.on_click(_ => {
                        //       let confirmed =
                        //         JsUtil.confirm(
                        //           "Are you SURE you want to reset Hazel to its initial state? You will lose any existing code that you have written!",
                        //         );
                        //       if (confirmed) {
                        //         JsUtil.clear_localstore();
                        //         Js_of_ocaml.Dom_html.window##.location##reload;
                        //       };
                        //       Virtual_dom.Vdom.Effect.Ignore;
                        //     }),
                        //   ],
                        //   [Node.text("Reset Hazel")],
                        // ),
                        a(
                          ~attrs=[
                            Attr.create(
                              "href",
                              "https://github.com/hazelgrove/hazel/issues/new",
                            ),
                            Attr.create("target", "_blank"),
                            Attr.class_("hsod-link"),
                          ],
                          [Node.text("Report this issue on GitHub")],
                        ),
                        button(
                          ~attrs=[
                            Attr.create("type", "button"),
                            Attr.classes([
                              "hsod-button",
                              "hsod-button-primary",
                            ]),
                            Attr.on_click(_ => inject_backtrack),
                          ],
                          [Node.text("Revert to previous state")],
                        ),
                        button(
                          ~attrs=[
                            Attr.create("type", "button"),
                            Attr.class_("hsod-button"),
                            Attr.on_click(_ => inject_rethrow),
                          ],
                          [Node.text("Rethrow exception")],
                        ),
                      ],
                    ),
                  ],
                ),
              ],
            ),
          ],
        ),
      ],
    );

  let view =
      (~get_log_and, ~inject: Update.t => Ui_effect.t(unit), model: Model.t) =>
    switch (current_exception^) {
    | None =>
      try(Logged.View.view(~get_log_and, ~inject, model.model)) {
      | exn =>
        set_last_exception(exn);
        let msg = Printexc.to_string(exn);
        set_current_exception(Update(msg));
        hsod_view(
          ~title="Exception during View",
          ~msg,
          ~inject_backtrack=inject(Globals(Undo)),
          ~inject_rethrow=inject(Globals(RethrowException)),
        );
      }
    | Some(Update(msg)) =>
      hsod_view(
        ~title="Exception during Update",
        ~msg,
        ~inject_backtrack=inject(Globals(ClearException)),
        ~inject_rethrow=inject(Globals(RethrowException)),
      )
    | Some(Calculate(msg)) =>
      hsod_view(
        ~title="Exception during Calculate",
        ~msg,
        ~inject_backtrack=inject(Globals(ClearException)),
        ~inject_rethrow=inject(Globals(RethrowException)),
      )
    };
};
