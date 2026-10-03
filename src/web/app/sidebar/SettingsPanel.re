open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;

/* The settings, in the sidebar: NutMenu's sections plus the config
   editors, each section foldable, with a filter over names and
   descriptions */

/* clicks act without taking focus from the editor */
let keep_focus = Attr.on_mousedown(_ => Effect.Prevent_default);

let colors_and_keys =
    (~editors_inject: Editors.Update.t => Effect.t(unit)): NutMenu.section => {
  s_name: "Colors and keys",
  s_rows:
    List.map(
      config_type => {
        let name = ConfigurationMode.Model.config_name_of_type(config_type);
        NutMenu.Action(
          "Edit " ++ String.lowercase_ascii(name) ++ {js|…|js},
          "Open the " ++ String.lowercase_ascii(name) ++ " as Hazel code",
          editors_inject(Editors.Update.ShowConfig(config_type)),
        );
      },
      ConfigurationMode.Model.all_of_config_type,
    ),
};

let sections = (~globals, ~editors_inject): list(NutMenu.section) =>
  NutMenu.[
    colors_and_keys(~editors_inject),
    code_display(~globals),
    editing(~globals),
    semantics(~globals),
    value_display(~globals),
    stepper(~globals),
    developer(~globals),
  ];

let row_text = (row: NutMenu.row): string =>
  switch (row) {
  | Toggle({name, tooltip, _}) =>
    name ++ " " ++ Option.value(tooltip, ~default="")
  | Choice(name, desc, choices) =>
    name
    ++ " "
    ++ desc
    ++ " "
    ++ String.concat(
         " ",
         List.map((c: NutMenu.choice) => c.c_label, choices),
       )
  | Action(label, desc, _) => label ++ " " ++ desc
  };

let matches = (query: string, text: string): bool => {
  let q = String.lowercase_ascii(query)
  and t = String.lowercase_ascii(text);
  let (n, m) = (String.length(q), String.length(t));
  let rec go = i => i + n <= m && (String.sub(t, i, n) == q || go(i + 1));
  n == 0 || go(0);
};

let desc = (text: string): Node.t =>
  div(~attrs=[clss(["settings-desc"])], [Node.text(text)]);

let row_view = (~globals: Globals.t, row: NutMenu.row): Node.t =>
  switch (row) {
  | Toggle({name, active, setting, tooltip}) =>
    Node.button(
      ~attrs=[
        Attr.create("type", "button"),
        clss(["settings-row", "settings-toggle"]),
        Attr.create("role", "switch"),
        Attr.create("aria-checked", string_of_bool(active)),
        keep_focus,
        Attr.on_click(_ => globals.inject_global(Set(setting))),
      ],
      [
        span(~attrs=[clss(["settings-name"])], [Node.text(name)]),
        div(
          ~attrs=[clss(["toggle-switch"] @ (active ? ["active"] : []))],
          [div(~attrs=[clss(["toggle-knob"])], [])],
        ),
      ]
      @ Option.to_list(Option.map(desc, tooltip)),
    )
  | Choice(name, description, choices) =>
    div(
      ~attrs=[clss(["settings-row", "settings-choice"])],
      [
        span(~attrs=[clss(["settings-name"])], [Node.text(name)]),
        div(
          ~attrs=[
            clss(["segmented-control"]),
            Attr.create("role", "group"),
            Attr.create("aria-label", name),
          ],
          List.map(
            (c: NutMenu.choice) =>
              Node.button(
                ~attrs=[
                  Attr.create("type", "button"),
                  clss(["segment"] @ (c.c_active ? ["active"] : [])),
                  Attr.create("aria-pressed", string_of_bool(c.c_active)),
                  Attr.title(c.c_tooltip),
                  keep_focus,
                  Attr.on_click(_ => c.c_set),
                ],
                [Node.text(c.c_label)],
              ),
            choices,
          ),
        ),
        desc(description),
      ],
    )
  | Action(label, description, effect) =>
    div(
      ~attrs=[clss(["settings-row", "settings-action"])],
      [
        Node.button(
          ~attrs=[
            Attr.create("type", "button"),
            clss(["settings-button"]),
            keep_focus,
            Attr.on_click(_ => effect),
          ],
          [Node.text(label)],
        ),
        desc(description),
      ],
    )
  };

let section_view =
    (~globals: Globals.t, ~query: string, sec: NutMenu.section)
    : option(Node.t) => {
  let rows = List.filter(r => matches(query, row_text(r)), sec.s_rows);
  let filtering = query != "";
  /* a filter opens every section with a match */
  let folded =
    !filtering
    && SidebarModel.Settings.is_settings_folded(
         sec.s_name,
         globals.settings.sidebar,
       );
  rows == []
    ? None
    : Some(
        div(
          ~attrs=[clss(["settings-section"] @ (folded ? ["folded"] : []))],
          [
            Node.button(
              ~attrs=[
                Attr.create("type", "button"),
                clss(["settings-section-header"]),
                Attr.create("aria-expanded", string_of_bool(!folded)),
                keep_focus,
                Attr.on_click(_ =>
                  filtering
                    ? Effect.Ignore
                    : globals.inject_global(
                        Set(Sidebar(ToggleSettingsFolded(sec.s_name))),
                      )
                ),
              ],
              [
                span(~attrs=[clss(["settings-chevron"])], []),
                span(
                  ~attrs=[clss(["settings-section-name"])],
                  [Node.text(sec.s_name)],
                ),
              ],
            ),
          ]
          @ (
            folded
              ? []
              : [
                div(
                  ~attrs=[clss(["settings-rows"])],
                  List.map(row_view(~globals), rows),
                ),
              ]
          ),
        ),
      );
};

let view =
    (
      ~globals: Globals.t,
      ~editors_inject: Editors.Update.t => Effect.t(unit),
    )
    : Node.t => {
  let query = String.trim(globals.settings_filter);
  let body =
    List.filter_map(
      section_view(~globals, ~query),
      sections(~globals, ~editors_inject),
    );
  div(
    ~attrs=[
      Attr.id("settings-panel"),
      /* the page pulls bubbled focus to its clipboard shim; keep it on
         the filter and the panel's buttons */
      Attr.on_focus(_ => Effect.Stop_propagation),
    ],
    [
      div(
        ~attrs=[clss(["settings-title-bar"])],
        [
          span(~attrs=[clss(["settings-title"])], [Node.text("Settings")]),
          input(
            ~attrs=[
              clss(["settings-filter"]),
              Attr.type_("search"),
              Attr.placeholder("Filter settings"),
              Attr.property(
                "autocomplete",
                Js_of_ocaml.Js.Unsafe.inject("off"),
              ),
              Attr.value(globals.settings_filter),
              Attr.on_input((_, v) =>
                globals.inject_global(SetSettingsFilter(v))
              ),
              /* its own keys (undo, copy, paste) stay in the field */
              Attr.on_keydown(_ => Effect.Stop_propagation),
              Attr.on_copy(_ => Effect.Stop_propagation),
              Attr.on_paste(_ => Effect.Stop_propagation),
              Attr.on_cut(_ => Effect.Stop_propagation),
            ],
            (),
          ),
        ],
      ),
      div(
        ~attrs=[clss(["settings-body"])],
        body == []
          ? [
            div(
              ~attrs=[clss(["settings-empty"])],
              [
                Node.text(
                  "No settings match " ++ {js|“|js} ++ query ++ {js|”|js},
                ),
              ],
            ),
          ]
          : body,
      ),
    ],
  );
};
