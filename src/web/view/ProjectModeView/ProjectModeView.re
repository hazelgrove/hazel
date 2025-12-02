open Util;

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

let top_bar = (~globals as _, ~inject: Update.t => 'a, model: Model.t) => {
  open Virtual_dom.Vdom;
  open Node;
  open Util.WebUtil;
  module W = Widgets;
  module I = Icons;

  let sorted_projects = Utils.sorted_projects(model);
  let current_id = model.current;

  let project_tabs =
    List.map(
      (project: Project.Model.t) => {
        let is_current = project.id == current_id;
        div(
          ~attrs=[
            clss([
              "project-tab",
              ...is_current ? ["project-tab-current"] : [],
            ]),
            Attr.on_click(_ =>
              inject(Update.ProjectMapAction(SwitchProject(project.id)))
            ),
          ],
          [
            div(~attrs=[clss(["project-tab-name"])], [text(project.name)]),
            div(
              ~attrs=[clss(["project-tab-actions"])],
              [
                div(
                  ~attrs=[clss(["project-tab-actions-icon"])],
                  [
                    W.button(~tooltip="Project actions", I.hamburger, _ =>
                      Effect.Ignore
                    ),
                  ],
                ),
                div(
                  ~attrs=[clss(["project-tab-actions-menu"])],
                  [
                    W.button(
                      ~tooltip="Rename",
                      I.rename,
                      _ => {
                        let name_opt =
                          JsUtil.prompt(
                            "Enter new project name:",
                            project.name,
                          );
                        switch (name_opt) {
                        | None => Effect.Ignore
                        | Some(n) =>
                          inject(
                            Update.ProjectMapAction(
                              RenameProject(project.id, n),
                            ),
                          )
                        };
                      },
                    ),
                    W.button(
                      ~tooltip="Delete",
                      I.delete,
                      _ => {
                        let confirmed =
                          JsUtil.confirm(
                            "Are you sure you want to delete this project? This will delete all files in the project.",
                          );
                        confirmed
                          ? inject(
                              Update.ProjectMapAction(
                                DeleteProject(project.id),
                              ),
                            )
                          : Effect.Ignore;
                      },
                    ),
                  ],
                ),
              ],
            ),
          ],
        );
      },
      sorted_projects,
    );

  let add_project_btn =
    div(
      ~attrs=[clss(["project-tab-add"])],
      [
        W.button(
          ~tooltip="Add new project",
          I.add_project,
          _ => {
            let name_opt =
              JsUtil.prompt("Enter new project name:", "NewProject");
            switch (name_opt) {
            | None => Effect.Ignore
            | Some(n) => inject(Update.ProjectMapAction(AddNewProject(n)))
            };
          },
        ),
      ],
    );

  [
    div(
      ~attrs=[Attr.id("project-tabs-container")],
      project_tabs @ [add_project_btn],
    ),
  ];
};
