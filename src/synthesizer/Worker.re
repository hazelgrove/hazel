/* open Js_of_ocaml.Worker;
   open Types; */

/* Js.export("main", Main.main);
   Js.Unsafe.eval_string(
     {|
       onmessage = function(e) {
           postMessage(main(e.data));
       };
       |},
   ); */

Js_of_ocaml.Worker.set_onmessage((data: Types.to_worker) =>
  Js_of_ocaml.Worker.post_message(Main.main(data): Types.from_worker)
);
