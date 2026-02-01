open Js_of_ocaml;
open PatchworkMessages;
open Util;

/* Patchwork/iframe mode detection.
   When running inside Patchwork, we disable Hazel's localStorage persistence
   because Automerge handles persistence. This check is immediate (no waiting
   for postMessage handshake) so it can be used during initialization. */
let is_in_iframe = (): bool => {
  Js.Unsafe.global##.parent !== Js.Unsafe.global;
};

/* Remote caret state for collaborative cursor display */
type remote_caret = {
  user_id: string,
  user_name: option(string), /* Display name for label (None if not available) */
  color: string,
  piece_id: Id.t,
  shard_index: option(int), /* For tiles: which shard (needed for multi-shard tiles like let/in) */
  caret_offset: int,
  shape: option(Direction.t),
  side: option(Direction.t) /* Left = at left edge of piece, Right = at right edge (end of segment) */
};

let remote_carets: ref(Maps.StringMap.t(remote_caret)) =
  ref(Maps.StringMap.empty);

let get_remote_carets = (): list((string, remote_caret)) =>
  Maps.StringMap.bindings(remote_carets^);

module JsConvert = {
  let of_shape: Grout.shape => FlatDoc.Shape.t =
    fun
    | Convex => `L_s3_Convex
    | Concave => `L_s2_Concave;

  let of_secondary_content:
    Language.Secondary.secondary_content => FlatDoc.SecondaryContent.t =
    fun
    | Whitespace(s) =>
      FlatDoc.SecondaryContent.create(~t=`L_s13_Whitespace, ~content=s, ())
    | Comment(s) =>
      FlatDoc.SecondaryContent.create(~t=`L_s1_Comment, ~content=s, ());

  let of_grout = (grout: Grout.t): FlatDoc.Grout.t =>
    FlatDoc.Grout.create(
      ~t=`L_s5_Grout,
      ~id=Id.to_string(grout.id),
      ~shape=of_shape(grout.shape),
      (),
    );

  let of_secondary = (secondary: Secondary.t): FlatDoc.Secondary.t => {
    FlatDoc.Secondary.create(
      ~t=`L_s9_Secondary,
      ~id=Id.to_string(secondary.id),
      ~content=of_secondary_content(secondary.content),
      (),
    );
  };

  let of_sort: Sort.t => FlatDoc.Sort.t =
    fun
    | Exp => `L_s4_Exp
    | Pat => `L_s6_Pat
    | Typ => `L_s12_Typ
    | TPat => `L_s10_TPat
    | Rul => `L_s8_Rul
    | Any => `L_s0_Any;

  let of_nib_shape: Nib.Shape.t => FlatDoc.NibShape.t =
    fun
    | Convex =>
      `U_s3_Convex(
        FlatDoc.NibShape.AnonymousInterface1.create(~t=`L_s3_Convex, ()),
      )
    | Concave(n) =>
      `U_s2_Concave(
        FlatDoc.NibShape.AnonymousInterface0.create(
          ~t=`L_s2_Concave,
          ~n, //wtf
          (),
        ),
      );

  let of_nib: Nib.t => FlatDoc.Nib.t =
    fun
    | {shape, sort} =>
      FlatDoc.Nib.create(
        ~shape=of_nib_shape(shape),
        ~sort=of_sort(sort),
        (),
      );

  let of_nibs: ((Nib.t, Nib.t)) => (FlatDoc.Nib.t, FlatDoc.Nib.t) =
    fun
    | (nib1, nib2) => (of_nib(nib1), of_nib(nib2));

  let of_mold = (mold: Mold.t): FlatDoc.Mold.t =>
    FlatDoc.Mold.create(
      ~out=of_sort(mold.out),
      ~in_=mold.in_ |> List.map(of_sort),
      ~nibs=mold.nibs |> of_nibs,
      (),
    );

  let of_tile = (tile: FlatConvert.Flat.tile): FlatDoc.FlatTile.t =>
    FlatDoc.FlatTile.create(
      ~t=`L_s11_Tile,
      ~id=Id.to_string(tile.id),
      ~label=tile.label,
      ~mold=tile.mold |> of_mold,
      ~shards=tile.shards,
      ~children=tile.children |> List.map(List.map(Id.to_string)),
      (),
    );

  let of_projector =
      (proj: FlatConvert.Flat.projector): FlatDoc.FlatProjector.t =>
    FlatDoc.FlatProjector.create(
      ~t=`L_s7_Projector,
      ~id=Id.to_string(proj.id),
      ~kind=proj.kind,
      ~syntax=Id.to_string(proj.syntax),
      ~model=proj.model,
      (),
    );

  let of_flat_piece = (x: FlatConvert.Flat.piece): FlatDoc.FlatPiece.t => {
    switch (x) {
    | Grout(grout) => `U_s5_Grout(of_grout(grout))
    | Secondary(secondary) => `U_s9_Secondary(of_secondary(secondary))
    | Tile(tile) => `U_s11_Tile(of_tile(tile))
    | Projector(proj) => `U_s7_Projector(of_projector(proj))
    };
  };

  let rec to_string: FlatDoc.Shape.t => string =
    fun
    | `L_s3_Convex => "Convex"
    | `L_s2_Concave => "Concave"
  and to_shape: FlatDoc.Shape.t => Grout.shape =
    fun
    | `L_s3_Convex => Convex
    | `L_s2_Concave => Concave
  and to_sort: FlatDoc.Sort.t => Sort.t =
    fun
    | `L_s4_Exp => Exp
    | `L_s6_Pat => Pat
    | `L_s12_Typ => Typ
    | `L_s10_TPat => TPat
    | `L_s8_Rul => Rul
    | `L_s0_Any => Any
  and to_nib_shape: FlatDoc.NibShape.t => Nib.Shape.t =
    fun
    | `U_s3_Convex(_) => Convex
    | `U_s2_Concave(concave) => {
        let n = FlatDoc.NibShape.AnonymousInterface0.get_n(concave);
        Concave(n);
      }
  and to_nib: FlatDoc.Nib.t => Nib.t =
    nib => {
      shape: to_nib_shape(FlatDoc.Nib.get_shape(nib)),
      sort: to_sort(FlatDoc.Nib.get_sort(nib)),
    }
  and to_nibs: ((FlatDoc.Nib.t, FlatDoc.Nib.t)) => (Nib.t, Nib.t) =
    fun
    | (nib1, nib2) => (to_nib(nib1), to_nib(nib2))
  and to_mold: FlatDoc.Mold.t => Mold.t =
    mold => {
      out: to_sort(FlatDoc.Mold.get_out(mold)),
      in_: FlatDoc.Mold.get_in(mold) |> List.map(to_sort),
      nibs: to_nibs(FlatDoc.Mold.get_nibs(mold)),
    }
  and to_tile: FlatDoc.FlatTile.t => FlatConvert.Flat.tile =
    tile => {
      id: Id.of_string(FlatDoc.FlatTile.get_id(tile)) |> Option.get,
      label: FlatDoc.FlatTile.get_label(tile),
      mold: to_mold(FlatDoc.FlatTile.get_mold(tile)),
      shards: FlatDoc.FlatTile.get_shards(tile),
      children:
        FlatDoc.FlatTile.get_children(tile)
        |> List.map(List.map(id => Id.of_string(id) |> Option.get)),
    }
  and to_secondary: FlatDoc.Secondary.t => Secondary.t =
    secondary => {
      id: Id.of_string(FlatDoc.Secondary.get_id(secondary)) |> Option.get,
      content:
        to_secondary_content(FlatDoc.Secondary.get_content(secondary)),
    }
  and to_secondary_content:
    FlatDoc.SecondaryContent.t => Language.Secondary.secondary_content =
    x => {
      switch (FlatDoc.SecondaryContent.get_t(x)) {
      | `L_s13_Whitespace =>
        Whitespace(FlatDoc.SecondaryContent.get_content(x))
      | `L_s1_Comment => Comment(FlatDoc.SecondaryContent.get_content(x))
      };
    }
  and to_grout = (grout: FlatDoc.Grout.t): Grout.t => {
    {
      id: Id.of_string(FlatDoc.Grout.get_id(grout)) |> Option.get,
      shape: to_shape(FlatDoc.Grout.get_shape(grout)),
    };
  }
  and to_projector =
      (proj: FlatDoc.FlatProjector.t): FlatConvert.Flat.projector => {
    {
      id: Id.of_string(FlatDoc.FlatProjector.get_id(proj)) |> Option.get,
      kind: FlatDoc.FlatProjector.get_kind(proj),
      syntax:
        Id.of_string(FlatDoc.FlatProjector.get_syntax(proj)) |> Option.get,
      model: FlatDoc.FlatProjector.get_model(proj),
    };
  }
  and to_flat_piece: FlatDoc.FlatPiece.t => FlatConvert.Flat.piece =
    fun
    | `U_s5_Grout(grout) => Grout(to_grout(grout))
    | `U_s9_Secondary(secondary) => Secondary(to_secondary(secondary))
    | `U_s11_Tile(tile) => Tile(to_tile(tile))
    | `U_s7_Projector(proj) => Projector(to_projector(proj));

  let id_from_piece = (piece: FlatConvert.Flat.piece): Id.t => {
    switch (piece) {
    | Tile(tile) => tile.id
    | Grout(grout) => grout.id
    | Secondary(secondary) => secondary.id
    | Projector(proj) => proj.id
    };
  };

  let js_of_flatdoc = (map: FlatConvert.Doc.t): Ojs.t => {
    // Create empty JS object for pieces map
    let pieces_obj = Ojs.empty_obj();
    let pieces =
      FlatDoc.HazelDoc.AnonymousInterface2.Pieces4.t_of_js(pieces_obj);

    // Add each piece to the map using UUID as key
    map
    |> Id.Map.iter((id, piece) => {
         let id_str = Id.to_string(id);
         let js_piece = of_flat_piece(piece);
         FlatDoc.HazelDoc.AnonymousInterface2.Pieces4.set(
           pieces,
           id_str,
           js_piece,
         );
       });

    let state =
      FlatDoc.HazelDoc.AnonymousInterface2.create(~title="", ~pieces, ());
    EditorState.t_to_js(EditorState.create(~t=`L_s8_state, ~state, ()));
  };

  // Compute delta between old and new flat docs using structural equality
  type delta = {
    changed: Id.Map.t(FlatConvert.Flat.piece),
    added: Id.Map.t(FlatConvert.Flat.piece),
    deleted: list(Id.t),
  };

  let compute_delta =
      (old_doc: FlatConvert.Doc.t, new_doc: FlatConvert.Doc.t): delta => {
    let changed = ref(Id.Map.empty);
    let added = ref(Id.Map.empty);
    let deleted = ref([]);

    // Find changed and added pieces
    new_doc
    |> Id.Map.iter((id, new_piece) => {
         switch (Id.Map.find_opt(id, old_doc)) {
         | None =>
           // Piece is new
           added := Id.Map.add(id, new_piece, added^)
         | Some(old_piece) =>
           // Piece exists, check if changed using structural equality
           if (old_piece != new_piece) {
             changed := Id.Map.add(id, new_piece, changed^);
           }
         }
       });

    // Find deleted pieces
    old_doc
    |> Id.Map.iter((id, _) => {
         switch (Id.Map.find_opt(id, new_doc)) {
         | None => deleted := [id, ...deleted^]
         | Some(_) => ()
         }
       });

    {
      changed: changed^,
      added: added^,
      deleted: deleted^,
    };
  };

  let js_of_delta = (delta: delta): Ojs.t => {
    // Create JS objects for changed and added maps
    let changed_obj = Ojs.empty_obj();
    let changed_pieces =
      FlatDoc.HazelDoc.AnonymousInterface2.Pieces4.t_of_js(changed_obj);

    delta.changed
    |> Id.Map.iter((id, piece) => {
         let id_str = Id.to_string(id);
         let js_piece = of_flat_piece(piece);
         FlatDoc.HazelDoc.AnonymousInterface2.Pieces4.set(
           changed_pieces,
           id_str,
           js_piece,
         );
       });

    let added_obj = Ojs.empty_obj();
    let added_pieces =
      FlatDoc.HazelDoc.AnonymousInterface2.Pieces4.t_of_js(added_obj);

    delta.added
    |> Id.Map.iter((id, piece) => {
         let id_str = Id.to_string(id);
         let js_piece = of_flat_piece(piece);
         FlatDoc.HazelDoc.AnonymousInterface2.Pieces4.set(
           added_pieces,
           id_str,
           js_piece,
         );
       });

    // Create JS array for deleted IDs
    let deleted_array =
      delta.deleted
      |> List.map(id => Id.to_string(id) |> Js.string)
      |> Array.of_list
      |> Js.array;

    // Create delta message object using Js.Unsafe
    let obj = Js.Unsafe.obj([||]);
    Js.Unsafe.set(obj, "t", Js.string("delta"));
    Js.Unsafe.set(obj, "changed", changed_obj);
    Js.Unsafe.set(obj, "added", added_obj);
    Js.Unsafe.set(obj, "deleted", deleted_array);

    // Convert to Ojs.t - Obj.magic is safe here as we're just changing the type tag
    (Obj.magic(obj): Ojs.t);
  };

  let flatdoc_of_hazeldoc = (doc: HazelDoc.t_0): FlatConvert.Doc.t => {
    let pieces_map = FlatDoc.HazelDoc.AnonymousInterface2.get_pieces(doc);
    let js_obj =
      FlatDoc.HazelDoc.AnonymousInterface2.Pieces4.t_to_js(pieces_map);

    // Get keys from JS object using Object.keys()
    let keys_js_array = Js.Unsafe.global##.Object##keys(js_obj);
    let keys = Js.to_array(keys_js_array) |> Array.map(Js.to_string);

    // Convert JS object to list of (key, value) pairs, then to OCaml Map
    keys
    |> Array.to_list
    |> List.map(key => {
         let js_piece =
           FlatDoc.HazelDoc.AnonymousInterface2.Pieces4.get(pieces_map, key);
         let piece = to_flat_piece(js_piece);
         let id = id_from_piece(piece);
         (id, piece);
       })
    |> Id.Map.of_list;
  };
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

    /* Wrap in try/catch to gracefully handle unknown message types.
       Other scripts (browser extensions, Patchwork internals) may send
       postMessages that don't match our protocol. */
    let msg: option(ParentToHazel.t) =
      if (from_self) {
        None;
      } else {
        try(Some(ParentToHazel.t_of_js(dataJs))) {
        | _ => None
        };
      };

    switch (msg) {
    | Some(msg) =>
      switch (msg) {
      | `U_s3_ping(_ping) =>
        let pongJs: Ojs.t =
          Pong.t_to_js(
            Pong.create(~t=`L_s4_pong, ~message="pong from iframe", ()),
          );
        send_to_parent(pongJs);
      | `U_s4_pong(_pong) => ()
      | `U_s5_remote_caret(rc) =>
        let user_id = RemoteCaret.get_userId(rc);
        let user_name = RemoteCaret.get_userName(rc);
        let color = RemoteCaret.get_color(rc);
        let piece_id_str = RemoteCaret.get_pieceId(rc);
        let shard_index = RemoteCaret.get_shardIdx(rc);
        let caret_offset = RemoteCaret.get_caretOffset(rc);
        let shape =
          switch (RemoteCaret.get_shape(rc)) {
          | Some(`L_s2_left) => Some(Direction.Left)
          | Some(`L_s7_right) => Some(Direction.Right)
          | None => None
          };
        let side =
          switch (RemoteCaret.get_side(rc)) {
          | Some(`L_s2_left) => Some(Direction.Left)
          | Some(`L_s7_right) => Some(Direction.Right)
          | None => None
          };
        // Firebug.console##log(
        //   Js.string(
        //     "[CARET] iframe received remote-caret: user="
        //     ++ user_id
        //     ++ " piece="
        //     ++ piece_id_str
        //     ++ " shard="
        //     ++ (
        //       switch (shard_index) {
        //       | Some(i) => string_of_int(i)
        //       | None => "None"
        //       }
        //     )
        //     ++ " offset="
        //     ++ string_of_int(caret_offset),
        //   ),
        // );
        switch (Id.of_string(piece_id_str)) {
        | Some(piece_id) =>
          let caret = {
            user_id,
            user_name,
            color,
            piece_id,
            shard_index,
            caret_offset,
            shape,
            side,
          };
          remote_carets := Maps.StringMap.add(user_id, caret, remote_carets^);
          schedule_action(UpdateRemoteCarets);
        | None =>
          // Firebug.console##log(
          //   Js.string(
          //     "[CARET] Invalid piece_id in remote-caret: " ++ piece_id_str,
          //   ),
          // )
          ()
        };
      | `U_s6_remote_caret_remove(rcr) =>
        let user_id = RemoteCaretRemove.get_userId(rcr);
        // Firebug.console##log(
        //   Js.string(
        //     "[CARET] iframe received remote-caret-remove: user=" ++ user_id,
        //   ),
        // );
        remote_carets := Maps.StringMap.remove(user_id, remote_carets^);
        schedule_action(UpdateRemoteCarets);
      | `U_s8_state(state) =>
        let receive_log = PerfLog.start("receive_state_total");

        let js_state = EditorState.get_state(state);

        let delta_doc =
          PerfLog.measure("flatdoc_of_hazeldoc", () =>
            JsConvert.flatdoc_of_hazeldoc(js_state)
          );

        let num_entries = FlatConvert.Doc.cardinal(delta_doc);
        PerfLog.log(
          "Received delta with " ++ string_of_int(num_entries) ++ " pieces",
        );

        PerfLog.end_(receive_log);

        schedule_action(SyncReplace(delta_doc));
      }
    | None => ()
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

let send_state =
    (old_doc: FlatConvert.Doc.t, new_doc: FlatConvert.Doc.t): unit => {
  // Compute delta using structural equality
  let delta =
    PerfLog.measure("compute_delta", () =>
      JsConvert.compute_delta(old_doc, new_doc)
    );

  let num_changed = Id.Map.cardinal(delta.changed);
  let num_added = Id.Map.cardinal(delta.added);
  let num_deleted = List.length(delta.deleted);
  PerfLog.log(
    "Delta: "
    ++ string_of_int(num_changed)
    ++ " changed, "
    ++ string_of_int(num_added)
    ++ " added, "
    ++ string_of_int(num_deleted)
    ++ " deleted",
  );

  // Combine changed and added pieces into a single map for sending
  // (deletions work implicitly via parent's children array changes)
  let affected_pieces =
    Id.Map.union((_, _, b) => Some(b), delta.changed, delta.added);

  // Convert to JS using existing state format: { t: "state", state: { title, pieces } }
  let js_obj =
    PerfLog.measure("js_of_state", () =>
      JsConvert.js_of_flatdoc(affected_pieces)
    );

  // Measure payload size
  let json_str = Js.Unsafe.global##.JSON##stringify(js_obj);
  let payload_size = json_str##.length;
  let size_kb = float_of_int(payload_size) /. 1024.0;
  let size_kb_str = Js.number_of_float(size_kb)##toFixed(2) |> Js.to_string;
  PerfLog.log(
    "Payload size: "
    ++ string_of_int(payload_size)
    ++ " bytes ("
    ++ size_kb_str
    ++ " KB)",
  );

  PerfLog.measure("postMessage_send", () => send_to_parent(js_obj));
};

let init_iframe = schedule_action => {
  print_endline("Initializing iframe");
  let init_message =
    Init.t_to_js(
      Init.create(
        ~message="Hello I am hazel and I am inside of an iframe!",
        ~t=`L_s1_init,
        (),
      ),
    );
  send_to_parent(init_message);
  listen(schedule_action);
};

/* Send caret position to parent for collaborative cursor display.
   shard_index: For tiles, which shard (needed for multi-shard tiles like let/in)
   caret_offset: 0 = Outer, n = Inner(n-1)
   shape: caret shape at piece boundaries (None when inside a piece)
   side: which edge of the piece the caret is on (Left = left edge, Right = right edge at end of segment) */
let send_caret =
    (
      piece_id: Id.t,
      shard_index: option(int),
      caret_offset: int,
      shape: option(Direction.t),
      side: option(Direction.t),
    )
    : unit => {
  // Firebug.console##log(
  //   Js.string(
  //     "[CARET] iframe sending caret: piece="
  //     ++ Id.to_string(piece_id)
  //     ++ " shard="
  //     ++ (
  //       switch (shard_index) {
  //       | Some(i) => string_of_int(i)
  //       | None => "None"
  //       }
  //     )
  //     ++ " offset="
  //     ++ string_of_int(caret_offset),
  //   ),
  // );
  let shape_js =
    switch (shape) {
    | Some(Left) => Some(`L_s2_left)
    | Some(Right) => Some(`L_s7_right)
    | None => None
    };
  let side_js =
    switch (side) {
    | Some(Left) => Some(`L_s2_left)
    | Some(Right) => Some(`L_s7_right)
    | None => None
    };
  let caret_message =
    CaretUpdate.t_to_js(
      CaretUpdate.create(
        ~t=`L_s0_caret,
        ~pieceId=Id.to_string(piece_id),
        ~shardIdx=?shard_index,
        ~caretOffset=caret_offset,
        ~shape=?shape_js,
        ~side=?side_js,
        (),
      ),
    );
  send_to_parent(caret_message);
};
