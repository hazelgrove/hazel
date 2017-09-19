open Tyxml_js;

/* execution starts here */
let _ =
  Js_util.listen_to_t
    Dom_html.Event.domContentLoaded
    Dom_html.document
    (
      fun _ => {
        let (rs, rf) = React.S.create Model.empty;
        let parent = Js_util.forceGetElementById "container";
        Dom.appendChild parent (Chrome.view (rs, rf))
      }
    );
