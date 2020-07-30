module Exn = Base.Exn;
module Dom = Js_of_ocaml.Dom;
module Vdom = Virtual_dom.Vdom;

Logger.init_log();

let () = {
  // Handle exceptions via a Monitor
  // See <https://github.com/janestreet/incr_dom/blob/master/example/monitor>
  let monitor = Async_kernel.Monitor.create();
  let _ =
    Async_kernel.Monitor.detach_and_iter_errors(
      monitor,
      ~f=exn => {
        let message = Exn.to_string(Async_kernel.Monitor.extract_exn(exn));
        let root = JSUtil.force_get_elem_by_id("root");
        let error_box =
          Vdom.Node.to_dom(
            Vdom.Node.body(
              [Vdom.Attr.id("error-message")],
              [
                Vdom.Node.h3([], [Vdom.Node.text("ERROR!")]),
                Vdom.Node.text(message),
              ],
            ),
          );
        Dom.appendChild(root, error_box);
      },
    );

  Async_kernel.Async_kernel_scheduler.within(~monitor, ()
    // Start the main app.
    // See <https://github.com/janestreet/incr_dom/blob/master/src/start_app.mli>.
    =>
      Incr_dom.Start_app.start(
        (module Hazel),
        ~debug=false,
        ~bind_to_element_with_id="container",
        ~initial_model=Model.init(),
      )
    );
};
