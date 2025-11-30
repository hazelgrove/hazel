open Haz3lcore;
open Util;
open OptUtil.Syntax;
open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;
open Util.JsUtil;
open Js_of_ocaml;
module W = Widgets;
module I = Icons;

open FileSystem;

let path_equal = (p1: Model.path, p2: Model.path): bool =>
  List.length(p1) == List.length(p2) && List.for_all2((==), p1, p2);

let rec render_node =
        (
          ~inject: Update.t => 'a,
          ~project: Project.Model.t,
          ~depth: int,
          node_path: Model.path,
        )
        : list(Node.t) => {
  let model = project.file_system;
  let indent_px = 8 * depth;
  let is_current = path_equal(model.current, node_path);

  let base_classes =
    ["project-fs-item"] @ (is_current ? ["project-fs-item-current"] : []);

  let base_attrs = [clss(base_classes)];

  switch (Utils.find_opt(node_path, model)) {
  | None => []
  | Some(Model.File(file)) => [
      div(
        ~attrs=
          List.concat([
            base_attrs,
            [Attr.on_click(_ => inject(Update.SetCurrentFile(file.path)))],
          ]),
        [
          div(
            ~attrs=[clss(["project-fs-row"])],
            [
              div(
                ~attrs=[
                  clss(["project-fs-name"]),
                  Attr.style(Css_gen.padding_left(`Px(indent_px))),
                ],
                [text(file.name)],
              ),
              div(
                ~attrs=[clss(["project-fs-actions"])],
                [
                  div(
                    ~attrs=[clss(["project-fs-actions-icon"])],
                    [
                      W.button(~tooltip="File actions", I.hamburger, _ =>
                        Effect.Ignore
                      ),
                    ],
                  ),
                  div(
                    ~attrs=[clss(["project-fs-actions-menu"])],
                    [
                      W.button(
                        ~tooltip="Rename",
                        I.rename,
                        _ => {
                          let current_name =
                            switch (Utils.find_opt(node_path, model)) {
                            | Some(Model.File(f)) => f.name
                            | _ => ""
                            };
                          let name_opt =
                            JsUtil.prompt(
                              "Enter new file name (without .hz):",
                              current_name,
                            );
                          switch (name_opt) {
                          | None => Effect.Ignore
                          | Some(n) => inject(Update.Rename(node_path, n))
                          };
                        },
                      ),
                      W.button(
                        ~tooltip="Delete",
                        I.delete,
                        _ => {
                          let confirmed =
                            JsUtil.confirm(
                              "Are you sure you want to delete this file?",
                            );
                          confirmed
                            ? inject(Update.Delete(node_path))
                            : Effect.Ignore;
                        },
                      ),
                    ],
                  ),
                ],
              ),
            ],
          ),
        ],
      ),
    ]
  | Some(Model.Folder(folder)) =>
    // Root folder is represented as the project and is bold; other folders normal
    let is_root = folder.path == [""];
    let folder_label =
      div(
        ~attrs=base_attrs,
        [
          div(
            ~attrs=[clss(["project-fs-row"])],
            [
              div(
                ~attrs=[
                  clss(
                    [
                      "project-fs-name", /* make folder label behave like file label */
                      "project-fs-folder",
                      ...folder.expanded
                           ? ["project-fs-folder-expanded"] : [],
                    ]
                    @ (is_root ? ["project-fs-root-folder"] : []),
                  ),
                  Attr.style(Css_gen.padding_left(`Px(indent_px))),
                  Attr.on_click(_ =>
                    inject(Update.ToggleFolderExpansion(folder.path))
                  ),
                ],
                [
                  div(
                    ~attrs=[clss(["project-fs-disclosure"])],
                    [text(folder.expanded ? "▾" : "▸")],
                  ),
                  div([text(is_root ? project.name : folder.name)]),
                ],
              ),
              div(
                ~attrs=[clss(["project-fs-actions"])],
                [
                  div(
                    ~attrs=[clss(["project-fs-actions-icon"])],
                    [
                      W.button(~tooltip="Folder actions", I.hamburger, _ =>
                        Effect.Ignore
                      ),
                    ],
                  ),
                  div(
                    ~attrs=[clss(["project-fs-actions-menu"])],
                    {
                      let add_file_btn =
                        W.button(
                          ~tooltip="Add file",
                          I.add_file,
                          _ => {
                            let name_opt =
                              JsUtil.prompt(
                                "Enter new file name (without .hz):",
                                "NewFile",
                              );
                            switch (name_opt) {
                            | None => Effect.Ignore
                            | Some(n) =>
                              inject(Update.AddNewFile(folder.path, n))
                            };
                          },
                        );
                      let add_folder_btn =
                        W.button(
                          ~tooltip="Add folder",
                          I.add_folder,
                          _ => {
                            let name_opt =
                              JsUtil.prompt(
                                "Enter new folder name:",
                                "NewFolder",
                              );
                            switch (name_opt) {
                            | None => Effect.Ignore
                            | Some(n) =>
                              inject(Update.AddNewFolder(folder.path, n))
                            };
                          },
                        );
                      let rename_btns =
                        is_root
                          ? []
                          : [
                            W.button(
                              ~tooltip="Rename",
                              I.rename,
                              _ => {
                                let current_name = folder.name;
                                let name_opt =
                                  JsUtil.prompt(
                                    "Enter new folder name:",
                                    current_name,
                                  );
                                switch (name_opt) {
                                | None => Effect.Ignore
                                | Some(n) =>
                                  inject(Update.Rename(folder.path, n))
                                };
                              },
                            ),
                          ];
                      let delete_btns =
                        is_root
                          ? []
                          : [
                            W.button(
                              ~tooltip="Delete",
                              I.delete,
                              _ => {
                                let confirmed =
                                  JsUtil.confirm(
                                    "Are you sure you want to delete this folder and all its contents?",
                                  );
                                confirmed
                                  ? inject(Update.Delete(folder.path))
                                  : Effect.Ignore;
                              },
                            ),
                          ];
                      [add_file_btn, add_folder_btn]
                      @ rename_btns
                      @ delete_btns;
                    },
                  ),
                ],
              ),
            ],
          ),
        ],
      );

    let children_nodes =
      folder.expanded
        ? List.concat(
            List.map(
              (child_path: Model.path) =>
                render_node(~inject, ~project, ~depth=depth + 1, child_path),
              folder.children,
            ),
          )
        : [];

    [folder_label] @ children_nodes;
  };
};

let view =
    // File system tree view is a left-hand side sidebar for project navigation
    // It injects updates to the file system tree
    (
      ~globals as _,
      ~inject: Update.t => 'a,
      ~selected: option(Selection.t),
      project: Project.Model.t,
    ) => {
  /* For now we assume a single root folder at path [""] */
  let root_path: Model.path = [""];
  [
    div(
      ~attrs=[Attr.id("project-sidebar")],
      [
        div(
          ~attrs=[clss(["project-fs-container"])],
          render_node(~inject, ~project, ~depth=0, root_path),
        ),
      ],
    ),
  ];
};
