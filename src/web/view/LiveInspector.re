open Virtual_dom.Vdom;
open Node;
open Util.Web;

let env_entry_view = (~font_metrics, (name: string, res: DHExp.t)): Node.t =>
  div(
    [clss(["environment-entry"])],
    [
      span([], [span([clss(["name"])], [text(name)]), text(":")]),
      Interface.dhcode_view(~font_metrics, ~width=20, res),
    ],
  );

let env_view = (~font_metrics, env): Node.t =>
  div(
    [clss(["environment"])],
    List.map(env_entry_view(~font_metrics), env),
  );

let result_view = (~font_metrics, res: DHExp.t): Node.t =>
  div(
    [clss(["live-instance-result"])],
    [Interface.dhcode_view(~font_metrics, ~width=1000, res)],
  );

let instance_view = (~font_metrics, (_, _, env, _res)): Node.t =>
  div(
    [clss(["live-instance"])],
    [env_view(~font_metrics, env) /*, result_view(~font_metrics, res)*/],
  );

let increment_instance = (~inject: Update.t => 'a, cur_idx, num_instances, _) => {
  let next_inst = (cur_idx + 1) mod num_instances;
  inject(Set(SelectedInstances(-1, next_inst)));
};

let decrement_instance = (~inject: Update.t => 'a, cur_idx, num_instances, _) => {
  let prev_inst = Util.IntUtil.modulo(cur_idx - 1, num_instances);
  inject(Set(SelectedInstances(-1, prev_inst)));
};

let instance_selector = (~inject, ~settings: Model.settings, num_instances) => {
  let cur_idx = Model.get_selected_instance(settings);
  let cur_idx = cur_idx >= num_instances ? 0 : cur_idx;
  let cur_instance = Printf.sprintf("%d / %d", cur_idx + 1, num_instances);
  div(
    [clss(["instance-selector"])],
    num_instances < 2
      ? []
      : [
        button(
          Icons.back,
          decrement_instance(~inject, cur_idx, num_instances),
        ),
        text(cur_instance),
        button(
          Icons.forward,
          increment_instance(~inject, cur_idx, num_instances),
        ),
      ],
  );
};

let inspector_view =
    (~inject as _, ~font_metrics, ~settings, instances): Node.t => {
  //HACK(andrew): remove duplicates due to multiple instances due to cases
  let cur_idx = Model.get_selected_instance(settings);
  let cur_idx = cur_idx >= List.length(instances) ? 0 : cur_idx;
  let instance_view =
    switch (instances) {
    | [] => []
    | _ => [instance_view(~font_metrics, List.nth(instances, cur_idx))]
    };
  div(
    [clss(["live-inspector"])],
    //[instance_selector(~inject, ~settings, List.length(instances))]
    [] @ instance_view,
    //@ List.map(instance_view(~font_metrics), instances),
  );
};

let view =
    (
      ~font_metrics,
      ~inject,
      ~settings: Model.settings,
      instances: option(list(TestMap.test_instance_report)),
    ) =>
  switch (instances) {
  | None => div([clss(["live-inspector"])], [text("No Dynamic Data")])
  | Some([]) => div([clss(["live-inspector"])], [text("No Traces")])
  | Some(instances) =>
    inspector_view(~inject, ~font_metrics, ~settings, instances)
  };
