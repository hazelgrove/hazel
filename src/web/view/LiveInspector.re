open Virtual_dom.Vdom;
open Node;
open Util.Web;

let env_entry_view = (~font_metrics, (name: string, res: DHExp.t)): Node.t =>
  div(
    [clss(["context-entry"])],
    [
      text(name),
      text("="),
      Interface.dhcode_view(~font_metrics, ~width=20, res),
    ],
  );

let instance_view = (~font_metrics, (res, _, env)): Node.t =>
  div(
    [clss(["context-entry"])],
    [
      //text(name),
      Interface.dhcode_view(~font_metrics, ~width=20, res),
    ]
    @ List.map(env_entry_view(~font_metrics), env),
  );

let ctxc = "context-entries";

let ctx_sorts_view = (~font_metrics, instances): Node.t => {
  div(
    [clss([ctxc, "exp"])],
    List.map(instance_view(~font_metrics), instances),
  );
};

let inspector_view = (~font_metrics, instances): Node.t => {
  let clss =
    clss([
      "live-inspector" /*@ (settings.context_inspector ? ["visible"] : []),*/
    ]);
  div([clss], [ctx_sorts_view(~font_metrics, instances)]);
};

let view =
    (
      ~font_metrics,
      ~settings as _: Model.settings,
      instances: option(list(TestMap.test_instance_report)),
    ) => {
  switch (instances) {
  | None
  | Some([]) => div([clss(["live-inspector"])], [text("No Dynamic Data")])
  | Some(instances) => inspector_view(~font_metrics, instances)
  };
};
