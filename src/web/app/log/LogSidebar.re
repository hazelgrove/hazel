open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;
open Util;

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    messages: list(string),
    is_playing: bool,
    current_step: int,
    total_steps: int,
    show_details: bool,
  };

  let init = () => {
    messages: [],
    is_playing: false,
    current_step: 0,
    total_steps: 0,
    show_details: false,
  };
};

module Update = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | AddMessage(string)
    | ClearMessages
    | SetPlaying(bool)
    | SetSteps(int, int)
    | ToggleDetails;

  let update = (action: t, model: Model.t): Model.t =>
    switch (action) {
    | AddMessage(msg) => {
        ...model,
        messages: [msg, ...Util.ListUtil.take(100, model.messages)],
      }
    | ClearMessages => {
        ...model,
        messages: [],
      }
    | SetPlaying(playing) => {
        ...model,
        is_playing: playing,
      }
    | SetSteps(current, total) => {
        ...model,
        current_step: current,
        total_steps: total,
      }
    | ToggleDetails => {
        ...model,
        show_details: !model.show_details,
      }
    };
};

let button = (~attrs=[], ~tooltip="", ~onclick, ~disabled=false, children) => {
  let base_attrs = [
    Attr.class_("log-button"),
    Attr.on_pointerdown(onclick),
    disabled ? Attr.disabled : Attr.empty,
  ];
  let all_attrs =
    tooltip != "" ? [Attr.title(tooltip), ...base_attrs] : base_attrs;
  div(~attrs=all_attrs @ attrs, children);
};

let file_input = (~onchange, ~id, ~accept_extensions) => {
  label(
    ~attrs=[Attr.for_(id)],
    [
      Vdom_input_widgets.File_select.single(
        ~extra_attrs=[Attr.class_("file-select-button"), Attr.id(id)],
        ~accept=accept_extensions,
        ~on_input=onchange,
        (),
      ),
    ],
  );
};

let progress_bar = (~current, ~total) => {
  let percentage = total > 0 ? current * 100 / total : 0;
  div(
    ~attrs=[Attr.class_("log-progress")],
    [
      div(
        ~attrs=[
          Attr.class_("log-progress-bar"),
          Attr.style(
            Css_gen.create(
              ~field="width",
              ~value=Printf.sprintf("%d%%", percentage),
            ),
          ),
        ],
        [],
      ),
      div(
        ~attrs=[Attr.class_("log-progress-text")],
        [text(Printf.sprintf("%d / %d", current, total))],
      ),
    ],
  );
};

let message_item = (message: string) => {
  let timestamp = Printf.sprintf("%.0f", Js_of_ocaml.Js.date##now);
  div(
    ~attrs=[Attr.class_("log-message")],
    [
      span(
        ~attrs=[Attr.class_("log-timestamp")],
        [text("[" ++ timestamp ++ "]")],
      ),
      span(~attrs=[Attr.class_("log-content")], [text(message)]),
    ],
  );
};

let controls_section =
    (~inject: Globals.Action.t => Ui_effect.t(unit), ~model: Model.t) => {
  let play_pause_icon = model.is_playing ? "⏸️" : "▶️";
  let play_pause_tooltip =
    model.is_playing ? "Pause log replay" : "Start log replay";

  div(
    ~attrs=[Attr.class_("log-controls")],
    [
      div(
        ~attrs=[Attr.class_("log-control-row")],
        [
          file_input(
            ~onchange=
              file => {
                switch (file) {
                | None => Virtual_dom.Vdom.Effect.Ignore
                | Some(file) => inject(Log(InitImport(file)))
                }
              },
            ~id="log-import-input",
            ~accept_extensions=[
              `Extension("txt"),
              `Extension("log"),
              `Extension("json"),
            ],
          ),
          button(
            ~tooltip="Import log file",
            ~onclick=
              _ => {
                let elem = Util.JsUtil.get_elem_by_id("log-import-input");
                elem##click;
                Virtual_dom.Vdom.Effect.Ignore;
              },
            [text("Import Log")],
          ),
          button(
            ~tooltip="Export current log",
            ~onclick=_ => inject(Log(ToggleReplay)), // This should trigger export
            [text("Export Log")],
          ),
        ],
      ),
      div(
        ~attrs=[Attr.class_("log-control-row")],
        [
          button(
            ~tooltip=play_pause_tooltip,
            ~onclick=_ => inject(Log(ToggleReplay)),
            [text(play_pause_icon)],
          ),
          button(
            ~tooltip="Execute next log step",
            ~onclick=_ => inject(Log(NextLog)),
            [text("Next Step")],
          ),
          button(
            ~tooltip="Skip current log entry",
            ~onclick=_ => inject(Log(SkipLog)),
            [text("Skip")],
          ),
        ],
      ),
      progress_bar(~current=model.current_step, ~total=model.total_steps),
    ],
  );
};

let messages_section =
    (~model: Model.t, ~inject_log: Update.t => Ui_effect.t(unit)) => {
  div(
    ~attrs=[Attr.class_("log-messages")],
    [
      div(
        ~attrs=[Attr.class_("log-messages-header")],
        [
          span([text("Log Messages")]),
          button(
            ~attrs=[Attr.class_("log-clear-btn")],
            ~tooltip="Clear all messages",
            ~onclick=_ => inject_log(ClearMessages),
            [text("Clear")],
          ),
          button(
            ~attrs=[Attr.class_("log-details-btn")],
            ~tooltip="Toggle detailed view",
            ~onclick=_ => inject_log(ToggleDetails),
            [text(model.show_details ? "Hide Details" : "Show Details")],
          ),
        ],
      ),
      div(
        ~attrs=[
          Attr.class_("log-messages-content"),
          Attr.style(Css_gen.create(~field="max-height", ~value="20em")),
          Attr.style(Css_gen.create(~field="overflow", ~value="auto")),
        ],
        model.messages == []
          ? [
            div(
              ~attrs=[Attr.class_("log-empty")],
              [text("No log messages")],
            ),
          ]
          : List.map(message_item, model.messages),
      ),
    ],
  );
};

let debug_section =
    (~inject: Globals.Action.t => Ui_effect.t(unit), ~log_entries_count: int) => {
  div(
    ~attrs=[Attr.class_("log-debug")],
    [
      div(
        ~attrs=[Attr.class_("log-debug-header")],
        [text("Log Debug Info")],
      ),
      div(
        ~attrs=[Attr.class_("log-debug-content")],
        [
          div(
            ~attrs=[Attr.class_("log-debug-info")],
            [
              text(
                Printf.sprintf("Current log entries: %d", log_entries_count),
              ),
            ],
          ),
          div(
            ~attrs=[
              Attr.class_("log-button"),
              Attr.title("Clear all log entries"),
              Attr.on_pointerdown(_ => inject(Log(ClearLog))),
            ],
            [text("Clear Log")],
          ),
        ],
      ),
    ],
  );
};

let view =
    (
      ~inject: Globals.Action.t => Ui_effect.t(unit),
      ~inject_log: Update.t => Ui_effect.t(unit),
      ~model: Model.t,
      ~log_entries_count: int,
    ) => {
  div(
    ~attrs=[Attr.class_("log-sidebar")],
    [
      div(~attrs=[Attr.class_("log-header")], [text("Log Control Panel")]),
      controls_section(~inject, ~model),
      messages_section(~inject_log, ~model),
      debug_section(~inject, ~log_entries_count),
    ],
  );
};

// Helper functions for external use
let log_message = (message: string): unit => {
  // This would be called from History.re to add messages to the sidebar
  // For now, we'll use a simple print as a fallback, but in a full implementation
  // this would update the log sidebar model
  Printf.printf(
    "[LOG SIDEBAR] %s\n",
    message,
  );
};

let log_action = (action_name: string, details: option(string)): unit => {
  let message =
    switch (details) {
    | None => action_name
    | Some(d) => action_name ++ ": " ++ d
    };
  log_message(message);
};

let log_error = (error: string): unit => {
  log_message("ERROR: " ++ error);
};

let log_info = (info: string): unit => {
  log_message("INFO: " ++ info);
};

let log_step_update = (current: int, total: int): unit => {
  log_message(Printf.sprintf("Step %d of %d", current, total));
};
