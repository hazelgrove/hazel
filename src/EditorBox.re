open Tyxml_js;
open SemanticsCore;
include EditorBoxTypes;

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
    (mk_editor_box, prefix, rev_path, rev_paths, model: Model.t, e: UHExp.t)
    : t => {
  let cursor_info_rs = model.cursor_info_rs;
  let do_action = model.do_action;
  let kc = JSUtil.KeyCombo.key;
  let palette_stuff =
    View.{
      palette_view_ctx: Palettes.initial_palette_view_ctx,
      mk_editor_box,
      do_action,
    };

  /* TODO figure out width stuff */
  let width = 80;
  let doc = View.of_hexp(palette_stuff, prefix, rev_path, e);
  let sdoc = Pretty.PP.sdoc_of_doc(width, doc, rev_paths);
  let view = Pretty.HTML_Of_SDoc.html_of_sdoc(sdoc, rev_paths);
  let pp_view =
    Html5.div(
      ~a=
        Html5.[
          a_contenteditable(true),
          a_onkeypress(evt => {
            let charCode = Js.Optdef.get(evt##.charCode, () => assert(false));

            let key =
              Js.to_string(Js.Optdef.get(evt##.key, () => assert(false)));

            switch (int_of_string_opt(key)) {
            | Some(n) =>
              let cursor_info = React.S.value(cursor_info_rs);
              switch (cursor_info.form) {
              | ZExp.IsHole(_) =>
                Dom.preventDefault(evt);
                do_action(Action.Construct(Action.SLit(n, After)));
                true;
              | ZExp.IsNumLit =>
                let selection = Dom_html.window##getSelection;
                let anchorNode = selection##.anchorNode;
                let nodeValue =
                  Js.to_string(
                    Js.Opt.get(anchorNode##.nodeValue, () => assert(false)),
                  );
                let anchorOffset = selection##.anchorOffset;
                let newNodeValue =
                  string_insert(nodeValue, anchorOffset, key);
                Dom.preventDefault(evt);
                switch (int_of_string_opt(newNodeValue)) {
                | Some(new_n) =>
                  let new_side =
                    side_of_str_offset(newNodeValue, anchorOffset + 1);
                  do_action(Action.Construct(Action.SLit(new_n, new_side)));
                | None => ()
                };
                true;
              | _ =>
                Dom.preventDefault(evt);
                true;
              };
            | None =>
              if (charCode != 0
                  || String.equal(key, "Enter")
                  || String.equal(key, "Tab")) {
                Dom.preventDefault(evt);
                true;
              } else {
                true;
              }
            };
          }),
          a_onkeydown(evt => {
            /* TODO we should use a more general matches function */
            let key = JSUtil.get_key(evt);
            let is_backspace = key == kc(JSUtil.KeyCombos.backspace);
            let is_del = key == kc(JSUtil.KeyCombos.del);
            if (is_backspace || is_del) {
              let cursor_info = React.S.value(cursor_info_rs);
              switch (cursor_info.form) {
              | ZExp.IsNumLit =>
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
                      Js.Opt.get(anchorNode##.nodeValue, () => assert(false)),
                    );
                  let ctrlKey = Js.to_bool(evt##.ctrlKey);
                  let (nodeValue', anchorOffset') =
                    is_backspace ?
                      string_backspace(nodeValue, anchorOffset, ctrlKey) :
                      string_delete(nodeValue, anchorOffset, ctrlKey);
                  if (String.equal(nodeValue', "")) {
                    if (is_Before) {
                      do_action(Action.Delete);
                    } else {
                      do_action(Action.Backspace);
                    };
                  } else {
                    let n = int_of_string(nodeValue');
                    let side = side_of_str_offset(nodeValue', anchorOffset');
                    do_action(Action.Construct(Action.SLit(n, side)));
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
          }),
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

  {e, pp_view, pp_view_dom, rev_paths};
  /* TODO whatever calls this should wrap it in a div of class "ModelExp"
     Html5.(div(~a=[a_class(["ModelExp"])], [pp_view]));
     */
};
