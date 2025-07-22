open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;
open Haz3lcore;
open Language;

module View = {
  let history_view = (model: Page.Update.t) => {
    let str = Page.Update.sexp_of_t(model) |> Sexplib.Sexp.to_string;
    div([text("Current: " ++ str)]);
  };
};
