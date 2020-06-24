module Js = Js_of_ocaml.Js;
module Exn = Base.Exn;

Logger.init_log();

let () = {
  // Handle exceptions via a Monitor
  // See <https://github.com/janestreet/incr_dom/blob/master/example/monitor>
  // <https://github.com/janestreet/async_js/blob/master/src/async_js0.ml>
  let monitor = Async_kernel.Monitor.create();
  let _ =
    Async_kernel.Monitor.detach_and_iter_errors(
      monitor,
      ~f=exn => {
        print_endline("There was some error that was successfully caught :D");
        let exn =
          switch (Async_kernel.Monitor.extract_exn(exn)) {
          | Js.Error(err) => `Js(err)
          | exn => `Exn(exn)
          };
        let message =
          switch (exn) {
          | `Js(err) =>
            let backtrace =
              switch (Js.Optdef.to_option(err##.stack)) {
              | Some(stack) => Js.to_string(stack)
              | None => "No backtrace found"
              };
            Js.to_string(err##.message) ++ backtrace;
          | `Exn(exn) => Exn.to_string(exn)
          };
        print_endline(message);
      },
    );
  /*~f=exn => {
      let exn = Async_kernel.Monitor.extract_exn(exn);
      let js_error = Async_js_jane.extract_js_error(exn);
      let backtrace =
        Option.value_map(
          js_error,
          ~f=
            js_error => {
              Option.value_map(
                Js.Optdef.to_option(js_error##.stack),
                ~f=Js.to_string,
                ~default="no backtrace found",
              )
            },
          ~default="no js error found",
        );
      ();
    },*/
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
