open Tyxml_js;

Js_util.listen_to_t
  Dom_html.Event.domContentLoaded
  Dom_html.document
  (
    fun _ => {
      /* Create a reactive signal containing an empty model. */
      let (rs, rf) = React.S.create Model.empty;
      /* Construct the Chrome and append it to container div */
      let parent = Js_util.forceGetElementById "container";
      Dom.appendChild parent (Chrome.view (rs, rf))
    }
  );
