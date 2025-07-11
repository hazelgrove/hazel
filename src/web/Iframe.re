open Js_of_ocaml;
open MessageTypes;

let send_to_parent = (message: Ojs.t) => {
  Js.Unsafe.fun_call(
    Js.Unsafe.js_expr("window.parent.postMessage"),
    [|Js.Unsafe.inject(message), Js.Unsafe.inject(Js.string("*"))|],
  );
};

let parse_incoming = (dataJs: Ojs.t): option(ParentToHazel.t) =>
  try(Some(ParentToHazel.t_of_js(dataJs))) {
  | _ => None
  };

let listen = (): unit => {
  let onMessage = (ev: Js.t(#Dom_html.event)) => {
    /* coerce raw JS `data` into our union type */
    let dataJs: Ojs.t = Js.Unsafe.get(ev, "data");

    let msg: ParentToHazel.t = ParentToHazel.t_of_js(dataJs);

    switch (msg) {
    | `U_s0_init(init) =>
      let text: string = Init.get_message(init);
      Firebug.console##log(Js.string("iframe got init: " ++ text));
    | `U_s1_ping(ping) =>
      let text: string = Ping.get_message(ping);
      Firebug.console##log(Js.string("iframe got ping: " ++ text));
      /* reply with Pong */
      let pongJs: Ojs.t =
        Pong.t_to_js(
          Pong.create(~t=`L_s2_pong, ~message="pong from iframe", ()),
        );
      send_to_parent(pongJs);
    | `U_s2_pong(pong) =>
      let text: string = Pong.get_message(pong);
      Firebug.console##log(Js.string("iframe got pong: " ++ text));
    };
    Js._false;
  };

  Js.Unsafe.fun_call(
    Js.Unsafe.js_expr("window.addEventListener"),
    [|
      Js.Unsafe.inject(Js.string("message")),
      Js.Unsafe.inject(onMessage),
      Js.Unsafe.inject(Js._false),
    |],
  );
};

let init_iframe = () => {
  print_endline("Initializing iframe stufffff...");
  let init_message =
    Init.t_to_js(
      Init.create(
        ~message="Hello I am hazel and I am inside of an iframe!",
        ~t=`L_s0_init,
        (),
      ),
    );

  let _ = send_to_parent(init_message);
  let _ = listen();
  ();
};
