open Util;
open Virtual_dom.Vdom;
open Vdom_input_widgets;
open ProjectorBase;
let clss = Attr.classes;

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model =
    | NoFile // No file selected
    | FileLoaded({
        filename: string,
        content: string,
        with_headers: bool,
      }); // File loaded with header toggle
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action =
    | SetFile({
        filename: string,
        content: string,
      })
    | ToggleHeaders
    | Reset;

  let init = (a: Language.Any.t): option(model) => {
    switch (a) {
    | Exp({term: ListLit([]), _}) => Some(NoFile) // No file selected
    | _ => None
    };
  };
  let put = (info, rows: CsvUtil.csv_data): Base.segment => {
    let exp: Language.Exp.term =
      switch (rows) {
      | CsvUtil.WithHeaders(rows) =>
        ListLit(
          List.map(
            (row: list((string, string))) =>
              Language.IdTagged.FreshGrammar.Exp.(
                tuple(
                  List.map(
                    ((header: string, value: string)) =>
                      tup_label(
                        label(StringUtil.sanitize_for_label(header)),
                        string(
                          StringUtil.sanitize_for_string_expression(value),
                        ),
                      ),
                    row,
                  ),
                )
              ),
            rows,
          ),
        )
      | CsvUtil.WithoutHeaders(rows) =>
        ListLit(
          List.map(
            (row: list(string)) =>
              Language.IdTagged.FreshGrammar.Exp.(
                tuple(
                  List.map(
                    (value: string) =>
                      string(
                        StringUtil.sanitize_for_string_expression(value),
                      ),
                    row,
                  ),
                )
              ),
            rows,
          ),
        )
      };

    switch (
      info.utility.lift_syntax(
        ~inline=true,
        fun
        | Exp(any) =>
          Exp({
            ...any,
            term: exp,
          })
        | _any => failwith("csv: put: not string literal"),
        info.syntax,
      )
    ) {
    | Some(s) => s
    | None => failwith("csv: put: lift failed")
    };
  };

  let reset_syntax = (info: info): Base.segment => {
    put(info, CsvUtil.WithoutHeaders([]));
  };

  let focusable = Focusable.non;
  let dynamics = false;
  let placeholder = (m, _) =>
    switch (m) {
    | FileLoaded({filename, _}) =>
      ProjectorCore.Shape.inline(String.length(filename) + 6) // Account for reset button and toggle
    | NoFile => ProjectorCore.Shape.inline(13)
    };
  let update = (m: model, _, action: action) => {
    switch (action) {
    | SetFile({filename, content}) =>
      FileLoaded({
        filename,
        content,
        with_headers: true,
      })
    | ToggleHeaders =>
      switch (m) {
      | FileLoaded({filename, content, with_headers}) =>
        FileLoaded({
          filename,
          content,
          with_headers: !with_headers,
        })
      | _ => m
      }
    | Reset => NoFile
    };
  };

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
      Util.WebUtil.(
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

  let view = ({model, info, local, parent, _}: View.args(model, action)) =>
    View.mk(
      Node.span(
        switch (model) {
        | NoFile => [
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
                          SetFile({
                            filename,
                            content,
                          }),
                        ),
                        parent(SetSyntax(put(info, csv_data))),
                      ]),
                    );
                  },
                );
                Virtual_dom.Vdom.Effect.Ignore;
              | _ => Virtual_dom.Vdom.Effect.Ignore
              }
            }),
          ]
        | FileLoaded({filename, content, with_headers}) => [
            Node.div(
              ~attrs=[Attr.class_("csv-loaded-container")],
              [
                Node.span(
                  ~attrs=[
                    Attr.on_click(_ => {
                      Effect.Many([
                        local(Reset),
                        parent(SetSyntax(reset_syntax(info))),
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
                        local(ToggleHeaders),
                        parent(SetSyntax(put(info, csv_data))),
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
