open Js_of_ocaml;
open Util;
open Haz3lcore.Derivation;

let pos = ref(Haz3lschool.ProofCore.Trees(0, Value));

let init = () =>
  ""
  |> Exercise.zipper_of_code
  |> Haz3lcore.Editor.init(~settings=Haz3lcore.CoreSettings.on);

let map_model = (f, state: Exercise.state): Exercise.state => {
  ...state,
  model:
    switch (state.model) {
    | Proof(m) => Proof(f(m))
    | _ => raise(Failure("Expected Exercise.Proof"))
    },
};

let update_rule: Rule.t => UpdateAction.t =
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
    (schedule_action: UpdateAction.t => unit, rule: Rule.t)
    : {
        .
        "handler": Js.readonly_prop(unit => unit),
        "id": Js.readonly_prop(string),
        "title": Js.readonly_prop(string),
        "section": Js.readonly_prop(Js.optdef(string)),
      } => {
  [%js
   {
     val id = Rule.show(rule);
     val title = Rule.repr(rule);
     val section =
       Js.Optdef.option(Some(Rule.show_kind(Rule.of_kind(rule))));
     val handler = () => update_rule(rule) |> schedule_action
   }];
};

let options = (schedule_action: UpdateAction.t => unit) =>
  Array.of_list(List.map(from_rule(schedule_action), Rule.all));

let elem = () => JsUtil.get_elem_by_id("ninja-keys-rules");

let initialize = opts => Js.Unsafe.set(elem(), "data", Js.array(opts));

let open_command_palette = (): unit =>
  Js.Unsafe.meth_call(elem(), "open", [||]);
