open Haz3lcore;
open Util;
open OptUtil.Syntax;

open ProjectMode;

type event =
  | MakeActive(Selection.t);

let project_sidebar = (~globals, ~inject, model: Model.t) => {
  FileSystemView.view(
    ~globals,
    ~inject=a => inject(Update.FileSystemAction(a)),
    ~selected=None,
    Utils.current_project(model),
  );
};

let view =
    (
      ~globals,
      ~signal: event => 'a,
      ~inject: Update.t => 'a,
      ~selected: option(Selection.t),
      model: Model.t,
    ) => {
  let sidebar = project_sidebar(~globals, ~inject, model);
  let curr_project = Utils.current_project(model);
  let main_content =
    switch (FileSystem.Utils.current_file(curr_project.file_system)) {
    | None => []
    | Some(file) => [
        CellEditor.View.view(
          ~globals,
          ~signal=
            fun
            | MakeActive(selection) => signal(MakeActive(Cell(selection))),
          ~inject=a => inject(CellAction(a)),
          ~selected=
            switch (selected) {
            | Some(Selection.Cell(s)) => Some(s)
            | _ => None
            },
          ~locked=false,
          file.editor,
        ),
      ]
    };

  [
    Virtual_dom.Vdom.Node.div(
      ~attrs=[Virtual_dom.Vdom.Attr.id("project-mode")],
      [
        Virtual_dom.Vdom.Node.div(
          ~attrs=[Virtual_dom.Vdom.Attr.id("project-mode-sidebar")],
          sidebar,
        ),
        Virtual_dom.Vdom.Node.div(
          ~attrs=[Virtual_dom.Vdom.Attr.id("project-mode-main")],
          main_content,
        ),
      ],
    ),
  ];
};

let file_menu = (~globals as _, ~inject as _, _: Model.t) => {
  [];
};

let top_bar = (~globals as _, ~inject as _, _model: Model.t) => {
  [Virtual_dom.Vdom.Node.div([])];
};
