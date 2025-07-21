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

    // switch (msg) {
    // | `U_s1_init(init) =>
    //   let text: string = Init.get_message(init);
    //   Firebug.console##log(Js.string("iframe got init: " ++ text));
    // | `U_s2_ping(ping) =>
    //   let text: string = Ping.get_message(ping);
    //   Firebug.console##log(Js.string("iframe got ping: " ++ text));
    //   /* reply with Pong */
    //   let pongJs: Ojs.t =
    //     Pong.t_to_js(
    //       Pong.create(~t=`L_s3_pong, ~message="pong from iframe", ()),
    //     );
    //   send_to_parent(pongJs);
    // | `U_s3_pong(pong) =>
    //   let text: string = Pong.get_message(pong);
    //   Firebug.console##log(Js.string("iframe got pong: " ++ text));
    // };

    //     ([ `U_s0_init of Init.t [@js "init"]
    //  | `U_s1_ping of Ping.t [@js "ping"]
    //  | `U_s2_pong of Pong.t [@js "pong"]
    //  | `U_s3_state of EditorState.t [@js "state"] ]

    switch (msg) {
    | `U_s0_init(init) =>
      let text: string = Init.get_message(init);
      Firebug.console##log(Js.string("iframe got init: " ++ text));
    | `U_s1_ping(ping) =>
      let text: string = Ping.get_message(ping);
      Firebug.console##log(Js.string("iframe got ping: " ++ text));
    | `U_s2_pong(pong) =>
      let text: string = Pong.get_message(pong);
      Firebug.console##log(Js.string("iframe got pong: " ++ text));
    | `U_s3_state(state) =>
      let state = EditorState.get_state(state);
      Firebug.console##log(
        "my name is iframe and I'm here to say you gave me this state",
      );
      Firebug.console##log(state);
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

// let send_delta = (delta: Delta.EditScript.t): unit => {
//   // to test, firebug console log
//   Firebug.console##log(Delta.EditScript.t_to_js(delta));
//   let message =
//     EditorDelta.t_to_js(EditorDelta.create(~t=`L_s0_delta, ~delta, ()));
//   send_to_parent(message);
// };

let send_state = (map: AutoSeg.Doc.t): unit => {
  let hd =
    Delta.HazelDoc.AnonymousInterface0.create(
      ~title="",
      ~tiles=map,
      ~root=Delta.TileId.t_to_js(Id.invalid),
      (),
    );
  let message =
    EditorState.t_to_js(EditorState.create(~t=`L_s3_state, ~state=hd, ()));
  send_to_parent(message);
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
