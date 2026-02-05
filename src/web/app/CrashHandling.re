open Util;

module Model = {
  [@deriving (sexp, yojson)]
  type state = Logged.Model.t;
  [@deriving (sexp, yojson)]
  type current_exception =
    | NoException
    | Update(string)
    | Calculate(string);

  [@deriving (sexp, yojson)]
  type t = {
    model: state,
    current_exception,
  };

  let equal = (===);

  let load = () => {
    model: Logged.Model.load(),
    current_exception: NoException,
  };
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
      {
        ...model,
        current_exception: NoException,
      }
      |> Updated.return_quiet
    | _ when model.current_exception == NoException =>
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
            current_exception: NoException,
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
        let msg = Printexc.to_string(exn);
        print_endline("CrashHandling: Caught exception in update: " ++ msg);
        Updated.return_quiet({
          ...model,
          current_exception: Update(msg),
        });
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
      current_exception: model.current_exception,
    }) {
    | exn =>
      let msg = Printexc.to_string(exn);
      print_endline("CrashHandling: Caught exception in calculate: " ++ msg);
      {
        ...previous_model,
        current_exception: Calculate(msg),
      };
    };
};

module View = {
  open Virtual_dom.Vdom;
  open WebUtil.Node;

  let hsod_view =
      (~title: string, ~msg: string, ~inject_backtrack: Ui_effect.t(unit)) =>
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
    switch (model.current_exception) {
    | NoException => Logged.View.view(~get_log_and, ~inject, model.model)
    | Update(msg) =>
      hsod_view(
        ~title="Exception during Update",
        ~msg,
        ~inject_backtrack=inject(Globals(ClearException)),
      )
    | Calculate(msg) =>
      hsod_view(
        ~title="Exception during Calculate",
        ~msg,
        ~inject_backtrack=inject(Globals(ClearException)),
      )
    | exception exn =>
      let msg = Printexc.to_string(exn);
      hsod_view(
        ~title="Exception during View",
        ~msg,
        ~inject_backtrack=inject(Globals(Undo)),
      );
    };
};
