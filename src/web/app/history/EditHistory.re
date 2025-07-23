open Virtual_dom.Vdom;
open Node;

module View = {
  let history_view = model => {
    div(
      ~attrs=[Attr.id("edit-history")],
      List.map(item => div([text("Entry: " ++ item)]), model),
    );
  };
};
