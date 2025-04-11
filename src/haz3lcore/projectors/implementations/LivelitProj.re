open Util;
open Virtual_dom.Vdom;
open ProjectorBase;
open Node;

let of_id = (id: Id.t) =>
  "id" ++ (id |> Id.to_string |> String.sub(_, 0, 8));

let rec replace_tile_labels =
        (target: Base.tile, source: Base.tile): Base.tile =>
  if (List.length(target.children) != List.length(source.children)) {
    print_endline(
      "Warning - replace_tile_labels: Tile children have different lengths. Aborting!",
    );
    print_endline("Target: " ++ (target |> Tile.show));
    print_endline("Source: " ++ (source |> Tile.show));
    target;
  } else {
    {
      ...target,
      children:
        List.map2(
          (t, s) => {replace_segment_labels(t, s)},
          target.children,
          source.children,
        ),
      label: source.label,
    };
  }
and replace_segment_labels =
    (target: Base.segment, source: Base.segment): Base.segment =>
  // Check if both are lists of the same length
  if (List.length(target) != List.length(source)) {
    print_endline(
      "Warning - replace_segment_labels: Segments have different lengths",
    );
    print_endline("Target: " ++ (target |> Segment.show));
    print_endline("Source: " ++ (source |> Segment.show));
    target;
  } else {
    // Process the pair
    List.map2(
      (a, b) => {
        switch (a, b) {
        | (Base.Tile(t1), Base.Tile(t2)) =>
          Base.Tile(replace_tile_labels(t1, t2))
        | _ => a
        }
      },
      target,
      source,
    );
  };

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = unit;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = unit;

  let get_model = (info: info) =>
    switch (info.statics) {
    | Some(
        InfoExp({
          term: {term: Ap(_dir, {term: LivelitName(llname), _}, model), _},
          _,
        }),
      ) =>
      Some((llname, model))
    | _ =>
      print_endline("Warning - LivelitProj.get: Not an InfoExp term");
      None;
    };

  let init = (_any: Term.Any.t) => Some();
  let can_focus = false;
  let placeholder = (_model, info) => {
    switch (get_model(info), info.statics) {
    | (Some((llname, _)), Some(InfoExp(exp))) =>
      /* Get the livelit size */
      switch (Ctx.lookup_livelit(exp.ctx, llname)) {
      | Some(ll) => ll.size
      | None =>
        /* Default size */
        ProjectorCore.Shape.inline(32)
      }
    | _ =>
      /* Default size */
      ProjectorCore.Shape.inline(32)
    };
  };

  let put =
      (info: info, segment: Segment.t, exp: TermBase.Exp.t): Base.segment => {
    print_endline("LivelitProj.put: segment: " ++ (segment |> Segment.show));
    print_endline("LivelitProj.put: exp: " ++ (exp |> TermBase.Exp.show));
    switch (
      info.utility.lift_syntax(
        fun
        | Exp(t) =>
          Exp({
            ...t,
            term: exp.term,
          })
        | _ => failwith("Livelit: Put: did not match expected model"),
        segment,
      )
    ) {
    | Some(s) => s
    | None => failwith("LivelitProj: Put: lift failed")
    };
  };
  let replace_model =
      (info: info, segment: Base.segment, term: TermBase.Exp.t) =>
    switch (segment) {
    | [name, Tile(old_model)] => [
        name,
        put(info, List.hd(old_model.children), term)
        |> Segment.unparenthesize
        |> Segment.parenthesize,
      ]
    | _ =>
      print_endline(
        "Warning - LivelitProj.replace_model: Livelit segment didn't match expected pattern",
      );
      segment;
    };

  let update = (_model, _info, action) =>
    switch (action) {
    | _ => print_endline("Warning - LivelitProj.update: No action")
    };

  let focus_pointer = (id: Id.t) => {
    JsUtil.get_elem_by_id(Id.cls(id))##focus;
  };

  let focusable =
    Focusable.{
      pointer: Some(focus_pointer),
      keyboard: None,
    };

  let dynamics = false;

  let view =
      (
        _,
        info,
        ~local as _,
        ~parent: ProjectorBase.external_action => Ui_effect.t(unit),
        ~view_seg as _,
      ) => {
    let ctx =
      switch (info.statics) {
      | Some(InfoExp(exp)) => exp.ctx
      | _ => Ctx.empty
      };

    let node =
      switch (get_model(info)) {
      | Some((ll_name, model)) =>
        let ll = Ctx.lookup_livelit(ctx, ll_name);

        switch (ll) {
        | Some(ll) =>
          let action_callback = (action: LivelitCtx.action_exp) => {
            let new_model = ll.update(action, model);
            // let new_model_seg =
            //   info.utility.term_to_seg(Exp(new_model))
            //   |> Segment.unparenthesize
            //   |> Segment.parenthesize;
            // parent(SetSyntax(put(info, new_model)));
            parent(SetSyntax(replace_model(info, info.syntax, new_model)));
          };

          let list_contents = ll.view(model, action_callback);
          Node.div(
            ~attrs=[Attr.class_(ll_name), Attr.id(Id.cls(info.id))],
            [list_contents],
          );
        | None =>
          print_endline("Warning - LivelitProj.view: not found in context");
          Node.text("No livelit found");
        };
      | None =>
        print_endline("Warning - LivelitProj.view: get is empty");
        Node.text("No livelit found");
      };

    View.mk(node);
  };
};
