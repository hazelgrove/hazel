open Tyxml_js;

JSUtil.listen_to_t
  Dom_html.Event.domContentLoaded
  Dom_html.document
  (
    fun _ => {
      /* Create the reactive signals for an empty model and the do_action function that updates them. */
      let model = Model.new_model ();
      /* Construct the Chrome and append it to container div */
      let parent = JSUtil.forceGetElementById "container";
      let (chrome, set_cursor) = Chrome.view model;
      Dom.appendChild parent chrome;
      set_cursor ()
    }
  );
