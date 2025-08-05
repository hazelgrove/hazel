open Util;
open Virtual_dom.Vdom;
open Vdom_input_widgets;
open ProjectorBase;
let clss = Attr.classes;

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = option(string); // Filename
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action =
    | SetFileName(string);

  let init = (_: Language.Any.t): option(model) => {
    Some
      (None); // TODO Make sure this is an actual empty hole
  };
  let put = (info, rows: list(list((string, string)))): Base.segment => {
    let exp: Language.Exp.term =
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
      );
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

  let focusable = Focusable.non;
  let dynamics = false;
  let placeholder = (m, _) =>
    switch (m) {
    | Some(name) => ProjectorCore.Shape.inline(String.length(name) + 2)
    | None => ProjectorCore.Shape.inline(10)
    };
  let update = (_: model, _, action: action) => {
    switch (action) {
    | SetFileName(name) => Some(name)
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
          div(
            ~attrs=[clss(["icon"]), Attr.title(tooltip)],
            [text(icon)],
          ),
        ])
      )
    );
  };

  let view =
      (
        model: model,
        info,
        ~local,
        ~parent: external_action => Ui_effect.t(unit),
        ~view_seg as _,
      ) =>
    View.mk(
      file_select_button(
        ~tooltip="Import CSV",
        "import-csv",
        model |> Option.value(~default="Upload CSV"),
        (file: option(Js_of_ocaml.Js.t(Js_of_ocaml.File.file))) => {
        switch (file) {
        | Some(file) =>
          JsUtil.read_file(
            file,
            content => {
              let csv_data = Option.value(~default="", content);
              let csv_data = CsvUtil.parse_csv(csv_data);

              Bonsai.Effect.Expert.handle(
                Effect.Many([
                  parent(SetSyntax(put(info, csv_data))),
                  local(
                    SetFileName(file##.name |> Js_of_ocaml.Js.to_string),
                  ),
                ]),
              );
            },
          );
          Virtual_dom.Vdom.Effect.Ignore;
        | _ => Virtual_dom.Vdom.Effect.Ignore
        }
      }),
    );
};
