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

module RedundantCoverterIGuess = {
  let of_shape: Grout.shape => Delta.Shape.t =
    fun
    | Convex => `L_s3_Convex
    | Concave => `L_s2_Concave;

  let of_secondary_content:
    Secondary.secondary_content => Delta.SecondaryContent.t =
    fun
    | Whitespace(s) =>
      Delta.SecondaryContent.create(~t=`L_s12_Whitespace, ~content=s, ())
    | Comment(s) =>
      Delta.SecondaryContent.create(~t=`L_s1_Comment, ~content=s, ());

  let of_grout = (grout: Grout.t): Delta.Grout.t =>
    Delta.Grout.create(
      ~t=`L_s5_Grout,
      ~id=Id.to_string(grout.id),
      ~shape=of_shape(grout.shape),
      (),
    );

  let of_secondary = (secondary: Secondary.t): Delta.Secondary.t => {
    Delta.Secondary.create(
      ~t=`L_s8_Secondary,
      ~id=Id.to_string(secondary.id),
      ~content=of_secondary_content(secondary.content),
      (),
    );
  };

  let of_sort: Sort.t => Delta.Sort.t =
    fun
    | Exp => `L_s4_Exp
    | Pat => `L_s6_Pat
    | Typ => `L_s11_Typ
    | TPat => `L_s9_TPat
    | Rul => `L_s7_Rul
    | Any => `L_s0_Any;

  let of_nib_shape: Nib.Shape.t => Delta.NibShape.t =
    fun
    | Convex =>
      `U_s3_Convex(
        Delta.NibShape.AnonymousInterface1.create(~t=`L_s3_Convex, ()),
      )
    | Concave(n) =>
      `U_s2_Concave(
        Delta.NibShape.AnonymousInterface0.create(
          ~t=`L_s2_Concave,
          ~n=float_of_int(n), //wtf
          (),
        ),
      );

  let of_nib: Nib.t => Delta.Nib.t =
    fun
    | {shape, sort} =>
      Delta.Nib.create(~shape=of_nib_shape(shape), ~sort=of_sort(sort), ());

  let of_nibs: ((Nib.t, Nib.t)) => (Delta.Nib.t, Delta.Nib.t) =
    fun
    | (nib1, nib2) => (of_nib(nib1), of_nib(nib2));

  let of_mold = (mold: Mold.t): Delta.Mold.t =>
    Delta.Mold.create(
      ~out=of_sort(mold.out),
      ~in_=mold.in_ |> List.map(of_sort),
      ~nibs=mold.nibs |> of_nibs,
      (),
    );

  let of_tile = (tile: AutoSeg.Flat.tile): Delta.FlatTile.t =>
    Delta.FlatTile.create(
      ~t=`L_s10_Tile,
      ~id=Id.to_string(tile.id),
      ~label=tile.label,
      ~mold=tile.mold |> of_mold,
      ~shards=tile.shards |> List.map(float_of_int), //floats?? FLOATS????????????
      ~children=tile.children |> List.map(List.map(Id.to_string)),
      (),
    );

  let of_flat_piece = (x: AutoSeg.Flat.piece): Delta.FlatPiece.t => {
    switch (x) {
    | Grout(grout) => `U_s5_Grout(of_grout(grout))
    | Secondary(secondary) => `U_s8_Secondary(of_secondary(secondary))
    | Tile(tile) => `U_s10_Tile(of_tile(tile))
    };
  };

  let go = (map: AutoSeg.Doc.t): Ojs.t => {
    let entries =
      map
      |> Id.Map.to_list
      |> List.map(((x, y)) => (Id.to_string(x), of_flat_piece(y)));
    let map = Ts2ocaml.Map.create'''(~entries=Some(entries), ());
    let state =
      Delta.HazelDoc.AnonymousInterface2.create(~title="", ~map, ());
    EditorState.t_to_js(EditorState.create(~t=`L_s3_state, ~state, ()));
  };
};

let send_state = (map: AutoSeg.Doc.t): unit =>
  map |> RedundantCoverterIGuess.go |> send_to_parent;

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
