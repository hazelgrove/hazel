open Virtual_dom.Vdom;
open Node;
open Util.Web;

//TODO: combine with below
let res_view = (~font_metrics: FontMetrics.t, eval_result): Node.t =>
  div(
    [Attr.classes(["result"])],
    [Interface.dhcode_view(~font_metrics, ~width=80, eval_result)],
  );

let result_view = (~font_metrics, res: DHExp.t): Node.t =>
  div(
    [clss(["live-instance-result"])],
    [Interface.dhcode_view(~font_metrics, ~width=1000, res)],
  );

let increment_instance = (~inject: Update.t => 'a, cur_idx, num_instances, _) => {
  let next_inst = (cur_idx + 1) mod num_instances;
  inject(Set(SelectedInstances(-1, next_inst)));
};

let decrement_instance = (~inject: Update.t => 'a, cur_idx, num_instances, _) => {
  let prev_inst = Util.IntUtil.modulo(cur_idx - 1, num_instances);
  inject(Set(SelectedInstances(-1, prev_inst)));
};

let get_cur_idx = (settings, num_instances) => {
  let cur_idx = Model.get_selected_instance(settings);
  cur_idx >= num_instances ? 0 : cur_idx;
};

let instance_selector = (~inject, ~settings: Model.settings, num_instances) => {
  let cur_idx = get_cur_idx(settings, num_instances);
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

let env_entry_view = (~font_metrics, (name: string, res: DHExp.t)): Node.t =>
  div(
    [clss(["environment-entry"])],
    [
      span([], [span([clss(["name"])], [text(name)]), text("≡")]),
      Interface.dhcode_view(~font_metrics, ~width=20, res),
    ],
  );

let env_view = (~font_metrics, env): Node.t =>
  div(
    [clss(["environment"])],
    List.map(env_entry_view(~font_metrics), env),
  );

let instances_view = (~inject as _, ~font_metrics, ~settings, instances) => {
  let num_instances = List.length(instances);
  let cur_idx = get_cur_idx(settings, num_instances);
  assert(cur_idx < num_instances);
  let (_, _, env, _) = List.nth(instances, cur_idx);
  List.length(env) == 0
    ? []
    : [
      div(
        [clss(["live-inspector"])],
        [div([clss(["live-instance"])], [env_view(~font_metrics, env)])],
      ),
    ];
};

let instance_environment_view =
    (
      ~font_metrics,
      ~inject,
      ~settings: Model.settings,
      instances: option(list(TestMap.test_instance_report)),
    ) =>
  switch (instances) {
  | _ when !settings.dynamics => []
  | None => [div([clss(["live-inspector"])], [text("No Dynamic Data")])]
  | Some([]) => [div([clss(["live-inspector"])], [text("No Traces")])]
  | Some(instances) =>
    instances_view(~inject, ~font_metrics, ~settings, instances)
  };

let instance_result_view =
    (
      ~settings: Model.settings,
      ~font_metrics,
      ~inject,
      //eval_result: option(DHExp.t),
      instances: option(Interface.instances),
    ) =>
  switch (instances) {
  | _ when !settings.dynamics => []
  | None
  | Some([]) => [] //[result_view(~font_metrics, eval_result)]
  | Some(instances) =>
    let num_instances = List.length(instances);
    let cur_idx = get_cur_idx(settings, num_instances);
    let (_, _, _, res) = List.nth(instances, cur_idx);
    [
      div(
        [clss(["live-bar"])],
        [
          instance_selector(~inject, ~settings, num_instances),
          result_view(~font_metrics, res),
        ],
      ),
    ];
  };
