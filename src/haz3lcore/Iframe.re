open Js_of_ocaml;
open MessageTypes;

module RedundantCoverterIGuess = {
  let of_shape: Grout.shape => Delta.Shape.t =
    fun
    | Convex => `L_s3_Convex
    | Concave => `L_s2_Concave;

  let of_secondary_content:
    Language.Secondary.secondary_content => Delta.SecondaryContent.t =
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
          ~n, //wtf
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
      ~shards=tile.shards,
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

  let rec to_string: Delta.Shape.t => string =
    fun
    | `L_s3_Convex => "Convex"
    | `L_s2_Concave => "Concave"
  and to_shape: Delta.Shape.t => Grout.shape =
    fun
    | `L_s3_Convex => Convex
    | `L_s2_Concave => Concave
  and to_sort: Delta.Sort.t => Sort.t =
    fun
    | `L_s4_Exp => Exp
    | `L_s6_Pat => Pat
    | `L_s11_Typ => Typ
    | `L_s9_TPat => TPat
    | `L_s7_Rul => Rul
    | `L_s0_Any => Any
  and to_nib_shape: Delta.NibShape.t => Nib.Shape.t =
    fun
    | `U_s3_Convex(_) => Convex
    | `U_s2_Concave(concave) => {
        let n = Delta.NibShape.AnonymousInterface0.get_n(concave);
        Concave(n);
      }
  and to_nib: Delta.Nib.t => Nib.t =
    nib => {
      shape: to_nib_shape(Delta.Nib.get_shape(nib)),
      sort: to_sort(Delta.Nib.get_sort(nib)),
    }
  and to_nibs: ((Delta.Nib.t, Delta.Nib.t)) => (Nib.t, Nib.t) =
    fun
    | (nib1, nib2) => (to_nib(nib1), to_nib(nib2))
  and to_mold: Delta.Mold.t => Mold.t =
    mold => {
      out: to_sort(Delta.Mold.get_out(mold)),
      in_: Delta.Mold.get_in(mold) |> List.map(to_sort),
      nibs: to_nibs(Delta.Mold.get_nibs(mold)),
    }
  and to_tile: Delta.FlatTile.t => AutoSeg.Flat.tile =
    tile => {
      id: Id.of_string(Delta.FlatTile.get_id(tile)) |> Option.get,
      label: Delta.FlatTile.get_label(tile),
      mold: to_mold(Delta.FlatTile.get_mold(tile)),
      shards: Delta.FlatTile.get_shards(tile),
      children:
        Delta.FlatTile.get_children(tile)
        |> List.map(List.map(id => Id.of_string(id) |> Option.get)),
    }
  and to_secondary: Delta.Secondary.t => Secondary.t =
    secondary => {
      id: Id.of_string(Delta.Secondary.get_id(secondary)) |> Option.get,
      content: to_secondary_content(Delta.Secondary.get_content(secondary)),
    }
  and to_secondary_content:
    Delta.SecondaryContent.t => Language.Secondary.secondary_content =
    x => {
      switch (Delta.SecondaryContent.get_t(x)) {
      | `L_s12_Whitespace =>
        Whitespace(Delta.SecondaryContent.get_content(x))
      | `L_s1_Comment => Comment(Delta.SecondaryContent.get_content(x))
      };
    }
  and to_grout = (grout: Delta.Grout.t): Grout.t => {
    {
      id: Id.of_string(Delta.Grout.get_id(grout)) |> Option.get,
      shape: to_shape(Delta.Grout.get_shape(grout)),
    };
  }
  and to_flat_piece: Delta.FlatPiece.t => AutoSeg.Flat.piece =
    fun
    | `U_s5_Grout(grout) => Grout(to_grout(grout))
    | `U_s8_Secondary(secondary) => Secondary(to_secondary(secondary))
    | `U_s10_Tile(tile) => Tile(to_tile(tile));

  let id_from_piece = (piece: AutoSeg.Flat.piece): Id.t => {
    switch (piece) {
    | Tile(tile) => tile.id
    | Grout(grout) => grout.id
    | Secondary(secondary) => secondary.id
    };
  };

  let js_of_autoseg = (map: AutoSeg.Doc.t): Ojs.t => {
    let tiles =
      map |> Id.Map.to_list |> List.map(((_x, y)) => of_flat_piece(y));
    let state =
      Delta.HazelDoc.AnonymousInterface2.create(~title="", ~tiles, ());
    EditorState.t_to_js(EditorState.create(~t=`L_s3_state, ~state, ()));
  };

  let autoseg_of_hazeldoc = (doc: HazelDoc.t_0): AutoSeg.Doc.t =>
    doc
    |> Delta.HazelDoc.AnonymousInterface2.get_tiles
    |> List.map(to_flat_piece)
    |> List.map(piece => (id_from_piece(piece), piece))
    |> Id.Map.of_list;
};

let send_to_parent = (message: Ojs.t): unit => {
  Js.Unsafe.fun_call(
    Js.Unsafe.js_expr("window.parent.postMessage"),
    [|Js.Unsafe.inject(message), Js.Unsafe.inject(Js.string("*"))|],
  );
};

let listen = (schedule_action: Action.t => unit): unit => {
  let onMessage = (ev: Js.t(#Dom_html.event)) => {
    let dataJs: Ojs.t = Js.Unsafe.get(ev, "data");

    // check origin
    let from_self: bool =
      Js.Unsafe.get(ev, "source") |> Js.equals(Dom_html.window);

    let msg: option(ParentToHazel.t) =
      from_self ? None : Ojs.option_of_js(ParentToHazel.t_of_js, dataJs);

    switch (msg) {
    | Some(msg) =>
      switch (msg) {
      | `U_s0_init(init) =>
        let text: string = Init.get_message(init);
        Firebug.console##log(Js.string("iframe got init: " ++ text));
      | `U_s1_ping(ping) =>
        let text: string = Ping.get_message(ping);
        // send back pong
        let pongJs: Ojs.t =
          Pong.t_to_js(
            Pong.create(~t=`L_s2_pong, ~message="pong from iframe", ()),
          );
        Firebug.console##log(Js.string("iframe got ping: " ++ text));
        send_to_parent(pongJs);
      | `U_s2_pong(pong) =>
        let text: string = Pong.get_message(pong);
        Firebug.console##log(Js.string("iframe got pong: " ++ text));
      | `U_s3_state(state) =>
        let js_state = EditorState.get_state(state);
        let state = RedundantCoverterIGuess.autoseg_of_hazeldoc(js_state);
        let seg = AutoSeg.doc_to_seg(state);
        schedule_action(SyncReplace(seg));
        Firebug.console##log(
          "my name is iframe and I'm here to say you gave me this state",
        );
        Firebug.console##log(state);
      }
    | None => Firebug.console##log(Js.string("iframe got unknown message"))
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

let send_state = (map: AutoSeg.Doc.t): unit =>
  map |> RedundantCoverterIGuess.js_of_autoseg |> send_to_parent;

let init_iframe = schedule_action => {
  print_endline("Initializing iframe");
  let init_message =
    Init.t_to_js(
      Init.create(
        ~message="Hello I am hazel and I am inside of an iframe!",
        ~t=`L_s0_init,
        (),
      ),
    );
  send_to_parent(init_message);
  listen(schedule_action);
};
