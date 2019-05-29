open Tyxml_js;
open SemanticsCommon;
include EditorBoxTypes;
module Dom_html = Js_of_ocaml.Dom_html;
module Dom = Js_of_ocaml.Dom;
module Js = Js_of_ocaml.Js;

let string_insert = (s1, offset, s2) => {
  let prefix = String.sub(s1, 0, offset);
  let length = String.length(s1);
  let suffix = String.sub(s1, offset, length - offset);
  prefix ++ s2 ++ suffix;
};

let string_backspace = (s, offset, ctrlKey) => {
  let prefix = ctrlKey ? "" : String.sub(s, 0, offset - 1);
  let length = String.length(s);
  let suffix = String.sub(s, offset, length - offset);
  let offset' = ctrlKey ? 0 : offset - 1;
  (prefix ++ suffix, offset');
};

let string_delete = (s, offset, ctrlKey) => {
  let prefix = String.sub(s, 0, offset);
  let length = String.length(s);
  let suffix = ctrlKey ? "" : String.sub(s, offset + 1, length - offset - 1);
  (prefix ++ suffix, offset);
};

let side_of_str_offset = (s, offset) =>
  if (offset == 0) {
    Before;
  } else if (offset == String.length(s)) {
    After;
  } else {
    In(offset);
  };

exception InvalidExpression;
let mk =
    (
      mk_editor_box,
      prefix,
      rev_path,
      rev_paths,
      model: Model.t,
      block: UHExp.block,
    )
    : t => {
  let cursor_info_rs = model.cursor_info_rs;
  let do_action = model.do_action;
  let palette_stuff =
    View.{
      palette_view_ctx: Palettes.initial_palette_view_ctx,
      mk_editor_box,
      do_action,
    };

  /* TODO figure out width stuff */
  let width = 120;
  let doc = View.of_hblock(palette_stuff, prefix, rev_path, block);
  let sdoc = Pretty.PP.sdoc_of_doc(width, doc, rev_paths);
  let view = Pretty.HTML_Of_SDoc.html_of_sdoc(sdoc, rev_paths);
  let pp_view =
    Html5.div(
      ~a=
        Html5.[
          a_contenteditable(true),
          a_onkeydown(evt =>
            switch (JSUtil.is_single_key(evt)) {
            | Some(single_key) =>
              Dom.preventDefault(evt);
              let cursor_info = React.S.value(cursor_info_rs);
              switch (cursor_info.sort) {
              | CursorInfo.IsLine(UHExp.EmptyLine)
              | CursorInfo.IsLine(UHExp.ExpLine(UHExp.EmptyHole(_)))
              | CursorInfo.IsExpr(UHExp.EmptyHole(_))
              | CursorInfo.IsPat(UHPat.EmptyHole(_))
              | CursorInfo.IsPat(UHPat.Pat(_, UHPat.Var(_, "")))
              | CursorInfo.IsType(UHTyp.Hole)
              | CursorInfo.IsTPat(TPat.Hole(_)) =>
                let shape =
                  switch (single_key) {
                  | JSUtil.Number(n) => Action.SNumLit(n, After)
                  | JSUtil.Letter(x) => Action.SVar(x, After)
                  | JSUtil.Underscore => Action.SWild
                  };
                Dom.preventDefault(evt);
                Dom_html.stopPropagation(evt);
                do_action(Action.Construct(shape));
                false;
              | CursorInfo.IsExpr(UHExp.Tm(_, UHExp.NumLit(_)))
              | CursorInfo.IsExpr(UHExp.Tm(_, UHExp.BoolLit(_)))
              | CursorInfo.IsExpr(UHExp.Tm(_, UHExp.Var(_, _)))
              | CursorInfo.IsPat(UHPat.Pat(_, UHPat.Var(_)))
              | CursorInfo.IsTPat(TPat.Var(_))
              | CursorInfo.IsType(UHTyp.TVar(_, _))
              | CursorInfo.IsPat(UHPat.Pat(_, UHPat.NumLit(_)))
              | CursorInfo.IsPat(UHPat.Pat(_, UHPat.BoolLit(_))) =>
                let selection = Dom_html.window##getSelection;
                let anchorNode = selection##.anchorNode;
                let nodeValue =
                  Js.to_string(
                    Js.Opt.get(anchorNode##.nodeValue, () => assert(false)),
                  );
                let anchorOffset = selection##.anchorOffset;
                let key_string = JSUtil.single_key_string(single_key);
                let newNodeValue =
                  string_insert(nodeValue, anchorOffset, key_string);
                switch (int_of_string_opt(newNodeValue)) {
                | Some(new_n) =>
                  let new_side =
                    side_of_str_offset(newNodeValue, anchorOffset + 1);
                  do_action(
                    Action.Construct(Action.SNumLit(new_n, new_side)),
                  );
                | None =>
                  Var.is_valid(newNodeValue)
                    ? {
                      let new_side =
                        side_of_str_offset(newNodeValue, anchorOffset + 1);
                      do_action(
                        Action.Construct(
                          Action.SVar(newNodeValue, new_side),
                        ),
                      );
                    }
                    : ()
                };
                Dom_html.stopPropagation(evt);
                false;
              | CursorInfo.IsLine(_)
              | CursorInfo.IsExpr(_)
              | CursorInfo.IsPat(_)
              | CursorInfo.IsType(_)
              | CursorInfo.IsBlock(_) => true
              };
            | None =>
              let is_backspace =
                JSUtil.KeyCombo.matches(JSUtil.KeyCombos.backspace, evt);
              let is_del =
                JSUtil.KeyCombo.matches(JSUtil.KeyCombos.delete, evt);
              if (is_backspace || is_del) {
                let cursor_info = React.S.value(cursor_info_rs);
                switch (cursor_info.sort) {
                | CursorInfo.IsExpr(UHExp.Tm(_, UHExp.NumLit(_)))
                | CursorInfo.IsExpr(UHExp.Tm(_, UHExp.BoolLit(_)))
                | CursorInfo.IsExpr(UHExp.Tm(_, UHExp.Var(_, _)))
                | CursorInfo.IsTPat(TPat.Var(_))
                | CursorInfo.IsPat(UHPat.Pat(_, UHPat.NumLit(_)))
                | CursorInfo.IsPat(UHPat.Pat(_, UHPat.BoolLit(_)))
                | CursorInfo.IsPat(UHPat.Pat(_, UHPat.Var(_))) =>
                  let side = cursor_info.side;
                  let is_Before =
                    switch (side) {
                    | Before => true
                    | _ => false
                    };
                  let is_After =
                    switch (side) {
                    | After => true
                    | _ => false
                    };
                  if (is_backspace && is_Before || is_del && is_After) {
                    Dom.preventDefault(evt);
                    false;
                  } else {
                    let selection = Dom_html.window##getSelection;
                    Dom_html.stopPropagation(evt);
                    Dom.preventDefault(evt);
                    let anchorNode = selection##.anchorNode;
                    let anchorOffset = selection##.anchorOffset;
                    let nodeValue =
                      Js.to_string(
                        Js.Opt.get(anchorNode##.nodeValue, () =>
                          assert(false)
                        ),
                      );
                    let ctrlKey = Js.to_bool(evt##.ctrlKey);
                    let (nodeValue', anchorOffset') =
                      is_backspace
                        ? string_backspace(nodeValue, anchorOffset, ctrlKey)
                        : string_delete(nodeValue, anchorOffset, ctrlKey);
                    if (String.equal(nodeValue', "")) {
                      if (is_Before) {
                        do_action(Action.Delete);
                      } else {
                        do_action(Action.Backspace);
                      };
                    } else {
                      switch (int_of_string_opt(nodeValue')) {
                      | Some(new_n) =>
                        let new_side =
                          side_of_str_offset(nodeValue', anchorOffset');
                        do_action(
                          Action.Construct(Action.SNumLit(new_n, new_side)),
                        );
                      | None =>
                        Var.is_valid(nodeValue')
                          ? {
                            let new_side =
                              side_of_str_offset(nodeValue', anchorOffset');
                            do_action(
                              Action.Construct(
                                Action.SVar(nodeValue', new_side),
                              ),
                            );
                          }
                          : ()
                      };
                    };
                    false;
                  };
                | _ =>
                  Dom.preventDefault(evt);
                  false;
                };
              } else {
                true;
              };
            }
          ),
          a_onkeypress(evt =>
            JSUtil.is_movement_key(evt)
              ? true
              : {
                Dom.preventDefault(evt);
                true;
              }
          ),
          a_ondrop(evt => {
            Dom.preventDefault(evt);
            false;
          }),
        ],
      [view],
    );

  let pp_view_dom = Tyxml_js.To_dom.of_div(pp_view);
  let preventDefault_handler = evt => {
    Dom.preventDefault(evt);
    ();
  };
  let _ =
    JSUtil.listen_to_t(
      Dom.Event.make("paste"),
      pp_view_dom,
      preventDefault_handler,
    );

  let _ =
    JSUtil.listen_to_t(
      Dom.Event.make("cut"),
      pp_view_dom,
      preventDefault_handler,
    );

  {block, pp_view, pp_view_dom, rev_paths};
  /* TODO whatever calls this should wrap it in a div of class "ModelExp"
     Html5.(div(~a=[a_class(["ModelExp"])], [pp_view]));
     */
};
