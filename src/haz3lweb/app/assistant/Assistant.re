open Virtual_dom.Vdom;
open Node;
open Util.Web;
open Util;
open Haz3lcore;

/*
 let human_button =
   Widgets.button_named(
     Icons.export,
     _ => inject(ExportModule),
     ~tooltip="Export Exercise Module",
   );

 let assistant_button =
   Widgets.button_named(
     Icons.export,
     _ => inject(ExportModule),
     ~tooltip="Export Exercise Module",
   );
 */

let view = (~globals: Globals.t, ~inject: 'a => Effect.t(unit)) => {
  div(
    ~attrs=[Attr.id("side-bar")],
    [
      div(
        ~attrs=[Attr.id("assistant")],
        [div(~attrs=[clss(["assistant-title"])], [text("Assistant")])],
      ),
    ],
  );
};
