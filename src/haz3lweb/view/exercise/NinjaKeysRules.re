open Js_of_ocaml;
open Util;

let pos = ref(Haz3lschool.ProofCore.Trees(0, Value));

let staged = ref(false);

let init = () =>
  ""
  |> Exercise.zipper_of_code(~root=Haz3lcore.Sort.Drv(Jdmt))
  |> Haz3lcore.Editor.init(~settings=Haz3lcore.CoreSettings.on, ~sort=Exp);

let map_model = (f, state: Exercise.state): Exercise.state => {
  ...state,
  model:
    switch (state.model) {
    | Proof(m) => Proof(f(m))
    | _ => raise(Failure("Expected Exercise.Proof"))
    },
};

let update_rule: Haz3lcore.Rule.t => UpdateAction.t =
  rule =>
    UpdateAction.MapExercise(
      map_model(
        Exercise.Proof.switch_rule(~pos=pos^, ~rule=Some(rule), ~init),
      ),
    );

/*
 Configuration of the rule choice palette using the https://github.com/ssleptsov/ninja-keys web component.
 */

let from_rule =
    (schedule_action: UpdateAction.t => unit, rule: Haz3lcore.Rule.t)
    : {
        .
        "handler": Js.readonly_prop(unit => unit),
        "id": Js.readonly_prop(string),
        "title": Js.readonly_prop(string),
        "section": Js.readonly_prop(Js.optdef(string)),
        "keywords": Js.readonly_prop(string),
      } => {
  [%js
   {
     val id = Haz3lcore.Rule.show(rule);
     val title = Haz3lcore.Rule.repr(rule);
     val section =
       Js.Optdef.option(
         Some(Haz3lcore.Rule.show_kind(Haz3lcore.Rule.of_kind(rule))),
       );
     val handler = () => update_rule(rule) |> schedule_action;
     val keywords = Haz3lcore.Rule.keywords(rule) |> String.concat(" ")
   }];
};

let options = (schedule_action: UpdateAction.t => unit) =>
  Array.of_list(List.map(from_rule(schedule_action), Haz3lcore.Rule.all));

let elem = () => JsUtil.get_elem_by_id("ninja-keys-rules");

let initialize = opts => Js.Unsafe.set(elem(), "data", Js.array(opts));

let open_command_palette = (): unit =>
  Js.Unsafe.meth_call(elem(), "open", [||]);
