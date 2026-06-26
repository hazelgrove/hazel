open Util;
open Virtual_dom.Vdom;
open Vdom_input_widgets;
open Haz3lcore;
open ProjectorBase;
open ProjectorViewBase;
let clss = Attr.classes;

module V: ProjectorView = {
  module L = CSVProjector.M;

  let focusable = Focusable.non;

  let file_select_button =
      (
        ~tooltip="",
        _,
        icon,
        on_input:
          option(Js_of_ocaml.Js.t(Js_of_ocaml.File.file)) =>
          Ui_effect.t(unit),
      ) => {
    Node.(
      WebUtil.(
        /* https://stackoverflow.com/questions/572768/styling-an-input-type-file-button */
        label([
          // ~attrs=[Attr.for_(id)],
          File_select.single(
            ~extra_attrs=[
              Attr.class_("file-select-button"),
              // Attr.id(id),
            ],
            ~accept=[`Extension("csv")],
            ~on_input,
            (),
          ),
          span(
            ~attrs=[clss(["icon"]), Attr.title(tooltip)],
            [text(icon)],
          ),
        ])
      )
    );
  };

  let view = ({model, info, local, parent, _}: View.args(L.model, L.action)) =>
    View.mk(
      Node.span(
        switch (model) {
        | CSVProjector.NoFile => [
            file_select_button(
              ~tooltip="Load CSV",
              "import-csv",
              "Load CSV",
              (file: option(Js_of_ocaml.Js.t(Js_of_ocaml.File.file))) => {
              switch (file) {
              | Some(file) =>
                JsUtil.read_file(
                  file,
                  content => {
                    let filename = file##.name |> Js_of_ocaml.Js.to_string;
                    let content = Option.value(~default="", content);
                    let csv_data =
                      CsvUtil.WithHeaders(
                        CsvUtil.parse_csv_with_headers(content),
                      );
                    Bonsai.Effect.Expert.handle(
                      Effect.Many([
                        local(
                          CSVProjector.SetFile({
                            filename,
                            content,
                          }),
                        ),
                        parent(SetSyntax(CSVProjector.put(info, csv_data))),
                      ]),
                    );
                  },
                );
                Virtual_dom.Vdom.Effect.Ignore;
              | _ => Virtual_dom.Vdom.Effect.Ignore
              }
            }),
          ]
        | CSVProjector.Pending(url) => [
            Node.span(
              ~attrs=[clss(["csv-loading"]), Attr.title("Loading " ++ url)],
              [Node.text("Loading…")],
            ),
          ]
        | CSVProjector.Failed({url, message}) => [
            Node.span(
              ~attrs=[
                clss(["csv-error"]),
                Attr.title(message ++ " (" ++ url ++ ")"),
              ],
              [Node.text("⚠ CSV")],
            ),
          ]
        | CSVProjector.FileLoaded({filename, content, with_headers}) => [
            Node.div(
              ~attrs=[Attr.class_("csv-loaded-container")],
              [
                Node.span(
                  ~attrs=[
                    Attr.on_click(_ => {
                      Effect.Many([
                        local(CSVProjector.Reset),
                        parent(SetSyntax(CSVProjector.reset_syntax(info))),
                      ])
                    }),
                    Attr.class_("reset-button"),
                    Attr.title("Reset projector"),
                  ],
                  [Node.text("⟲")],
                ),
                Node.span(
                  ~attrs=[Attr.class_("csv-loaded-filename")],
                  [Node.text(filename)],
                ),
                Node.div(
                  ~attrs=[
                    clss(
                      ["toggle-switch"] @ (with_headers ? ["active"] : []),
                    ),
                    Attr.on_click(_ => {
                      let csv_data =
                        if (with_headers) {
                          CsvUtil.WithoutHeaders(
                            CsvUtil.parse_csv_without_headers(content),
                          );
                        } else {
                          CsvUtil.WithHeaders(
                            CsvUtil.parse_csv_with_headers(content),
                          );
                        };
                      Effect.Many([
                        local(CSVProjector.ToggleHeaders),
                        parent(SetSyntax(CSVProjector.put(info, csv_data))),
                      ]);
                    }),
                    Attr.title("Toggle headers"),
                  ],
                  [
                    Node.div(
                      ~attrs=[clss(["toggle-knob"])],
                      [Node.text("H")],
                    ),
                  ],
                ),
              ],
            ),
          ]
        },
      ),
    );
};
