open Util;
open Virtual_dom.Vdom;
open Vdom_input_widgets;
open ProjectorBase;
let clss = Attr.classes;

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model =
    | NoFile(bool) // Boolean to enable headers before file upload
    | FileSelected({filename: string}); // Filename after file is selected
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action =
    | SetFileName(string)
    | ToggleHeaders // Toggle whether the CSV has headers
    | Reset;

  let init = (a: Language.Any.t): option(model) => {
    switch (a) {
    | Exp({term: ListLit([]), _}) => Some(NoFile(false)) // No file selected, no headers
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
    | FileSelected({filename}) =>
      ProjectorCore.Shape.inline(String.length(filename) + 2)
    | NoFile(_) => ProjectorCore.Shape.inline(13)
    };
  let update = (m: model, _, action: action) => {
    switch (action) {
    | SetFileName(filename) => FileSelected({filename: filename})
    | ToggleHeaders =>
      switch (m) {
      | NoFile(has_headers) => NoFile(!has_headers)
      | FileSelected(_) => m
      }
    | Reset => NoFile(false)
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
        | NoFile(has_headers) => [
            Node.input(
              ~attrs=
                [
                  Attr.create("type", "checkbox"),
                  Attr.on_input((_, _) => local(ToggleHeaders)),
                  Attr.title("Parse CSV with headers"),
                ]
                @ (
                  switch (has_headers) {
                  | true => [Attr.checked]
                  | _ => []
                  }
                ),
              (),
            ),
            file_select_button(
              ~tooltip="Import CSV",
              "import-csv",
              model
              |> (
                m =>
                  switch (m) {
                  | NoFile(_) => "Upload CSV"
                  | FileSelected({filename}) => filename
                  }
              ),
              (file: option(Js_of_ocaml.Js.t(Js_of_ocaml.File.file))) => {
              switch (file) {
              | Some(file) =>
                JsUtil.read_file(
                  file,
                  content => {
                    let csv_data = Option.value(~default="", content);
                    let csv_data: CsvUtil.csv_data =
                      switch (model) {
                      | NoFile(true) =>
                        WithHeaders(CsvUtil.parse_csv_with_headers(csv_data))
                      | _ =>
                        WithoutHeaders(
                          CsvUtil.parse_csv_without_headers(csv_data),
                        )
                      };

                    Bonsai.Effect.Expert.handle(
                      Effect.Many([
                        parent(SetSyntax(put(info, csv_data))),
                        local(
                          SetFileName(
                            file##.name |> Js_of_ocaml.Js.to_string,
                          ),
                        ),
                      ]),
                    );
                  },
                );
                Virtual_dom.Vdom.Effect.Ignore;
              | _ => Virtual_dom.Vdom.Effect.Ignore
              }
            }),
          ]
        | FileSelected({filename}) => [
            Node.span(
              ~attrs=[
                Attr.on_click(_ => {
                  Effect.Many([
                    local(Reset),
                    parent(SetSyntax(reset_syntax(info))),
                  ])
                }),
                Attr.class_("cancel-button"),
              ],
              [Node.text("✘")],
            ),
            Node.text(filename),
          ]
        },
      ),
    );
};
