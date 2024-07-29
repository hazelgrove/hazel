open Virtual_dom.Vdom;
open Node;
open Widgets;

let option_view = (name, n) =>
  option(
    ~attrs=n == name ? [Attr.create("selected", "selected")] : [],
    [text(n)],
  );

let mode_menu = (~inject: Update.t => 'a, ~mode: Settings.mode) =>
  div(
    ~attrs=[Attr.class_("mode-name"), Attr.title("Toggle Mode")],
    [
      select(
        ~attrs=[
          Attr.on_change((_, name) =>
            inject(Set(Mode(Settings.mode_of_string(name))))
          ),
        ],
        List.map(
          option_view(Settings.show_mode(mode)),
          ["Scratch", "Documentation", "Exercises"],
        ),
      ),
    ],
  );

let slide_select = (~inject, ~cur_slide, ~num_slides) => {
  let next_ed = (cur_slide + 1) mod num_slides;
  let prev_ed = Util.IntUtil.modulo(cur_slide - 1, num_slides);
  [
    button(Icons.back, _ => inject(Update.SwitchScratchSlide(prev_ed))),
    text(Printf.sprintf("%d / %d", cur_slide + 1, num_slides)),
    button(Icons.forward, _ => inject(Update.SwitchScratchSlide(next_ed))),
  ];
};

let scratch_view = (~inject, ~cur_slide, ~slides) =>
  [text("/"), mode_menu(~inject, ~mode=Scratch), text("/")]
  @ slide_select(~inject, ~cur_slide, ~num_slides=List.length(slides));

let documentation_view = (~inject, ~name, ~editors) => {
  let editor_names = List.map(fst, editors);
  let rec find_prev_next: list(string) => (option(string), option(string)) =
    fun
    | []
    | [_] => (None, None)
    | [x, y] when name == x => (None, Some(y))
    | [x, y] when name == y => (Some(x), None)
    | [_, _] => (None, None)
    | [x, y, ..._] when name == x => (None, Some(y))
    | [x, y, z, ..._] when name == y => (Some(x), Some(z))
    | [_, ...ys] => find_prev_next(ys);
  let (prev, next) = find_prev_next(editor_names);
  let _prev =
    prev
    |> Option.map(s =>
         button(Icons.back, _ => inject(Update.SwitchDocumentationSlide(s)))
       )
    |> Option.value(
         ~default=
           button_d(
             Icons.back,
             inject(Update.SwitchDocumentationSlide("none")),
             ~disabled=true,
           ),
       );
  let _next =
    next
    |> Option.map(s =>
         button(Icons.forward, _ =>
           inject(Update.SwitchDocumentationSlide(s))
         )
       )
    |> Option.value(
         ~default=
           button_d(
             Icons.forward,
             inject(Update.SwitchDocumentationSlide("none")),
             ~disabled=true,
           ),
       );
  [
    text("/"),
    mode_menu(~inject, ~mode=Documentation),
    text("/"),
    select(
      ~attrs=[
        Attr.on_change((_, name) =>
          inject(Update.SwitchDocumentationSlide(name))
        ),
      ],
      List.map(option_view(name), editor_names),
    ),
  ];
};

let instructor_toggle = (~inject, ~instructor_mode) =>
  ExerciseSettings.show_instructor
    ? [
      toggle("🎓", ~tooltip="Toggle Instructor Mode", instructor_mode, _ =>
        inject(Update.Set(InstructorMode))
      ),
    ]
    : [];

let exercises_view = (~inject, ~cur_slide, ~specs, ~instructor_mode) => {
  [text("/"), mode_menu(~inject, ~mode=Exercises), text("/")]
  @ instructor_toggle(~inject, ~instructor_mode)
  @ [text("/")]
  @ slide_select(~inject, ~cur_slide, ~num_slides=List.length(specs));
};

let view =
    (
      ~inject: Update.t => 'a,
      ~editors: Editors.t,
      ~settings as {instructor_mode, _}: Settings.t,
    )
    : Node.t => {
  let contents =
    switch (editors) {
    | Scratch(cur_slide, slides) =>
      scratch_view(~inject, ~cur_slide, ~slides)
    | Documentation(name, editors) =>
      documentation_view(~inject, ~name, ~editors)
    | Exercises(cur_slide, specs, _) =>
      exercises_view(~cur_slide, ~specs, ~inject, ~instructor_mode)
    };
  div(~attrs=[Attr.id("editor-mode")], contents);
};
