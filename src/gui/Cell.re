module Vdom = Virtual_dom.Vdom;
module Dom_html = Js_of_ocaml.Dom_html;
module Js = Js_of_ocaml.Js;
module KC = JSUtil.KeyCombo;
module KCs = JSUtil.KeyCombos;
open GeneralUtil;

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

let kc_actions: Hashtbl.t(KC.t, Action.t) =
  Hashtbl.of_seq(
    [
      (KCs.backspace, Action.Backspace),
      (KCs.delete, Action.Delete),
      (KCs.shift_tab, Action.MoveToPrevHole),
      (KCs.tab, Action.MoveToNextHole),
      (KCs.key_N, Action.Construct(SNum)),
      (KCs.key_B, Action.Construct(SBool)),
      (KCs.gt, Action.Construct(SOp(SArrow))),
      (KCs.vbar, Action.Construct(SOp(SVBar))),
      (KCs.key_L, Action.Construct(SList)),
      (KCs.left_parens, Action.Construct(SParenthesized)),
      (KCs.colon, Action.Construct(SAsc)),
      (KCs.equals, Action.Construct(SLet)),
      (KCs.enter, Action.Construct(SLine)),
      (KCs.backslash, Action.Construct(SLam)),
      (KCs.plus, Action.Construct(SOp(SPlus))),
      (KCs.asterisk, Action.Construct(SOp(STimes))),
      (KCs.lt, Action.Construct(SOp(SLessThan))),
      (KCs.space, Action.Construct(SOp(SSpace))),
      (KCs.comma, Action.Construct(SOp(SComma))),
      (KCs.left_bracket, Action.Construct(SListNil)),
      (KCs.semicolon, Action.Construct(SOp(SCons))),
      (KCs.alt_L, Action.Construct(SInj(L))),
      (KCs.alt_R, Action.Construct(SInj(R))),
      (KCs.alt_C, Action.Construct(SCase)),
    ]
    |> List.to_seq,
  );

let kc_of_evt = (evt): option(KC.t) => {
  let matches = kc => KC.matches(kc, evt);
  if (matches(KCs.backspace)) {
    Some(KCs.backspace);
  } else if (matches(KCs.delete)) {
    Some(KCs.delete);
  } else if (matches(KCs.shift_tab)) {
    Some(KCs.shift_tab);
  } else if (matches(KCs.tab)) {
    Some(KCs.tab);
  } else if (matches(KCs.key_N)) {
    Some(KCs.key_N);
  } else if (matches(KCs.key_B)) {
    Some(KCs.key_B);
  } else if (matches(KCs.gt)) {
    Some(KCs.gt);
  } else if (matches(KCs.vbar)) {
    Some(KCs.vbar);
  } else if (matches(KCs.key_L)) {
    Some(KCs.key_L);
  } else if (matches(KCs.left_parens)) {
    Some(KCs.left_parens);
  } else if (matches(KCs.colon)) {
    Some(KCs.colon);
  } else if (matches(KCs.equals)) {
    Some(KCs.equals);
  } else if (matches(KCs.enter)) {
    Some(KCs.enter);
  } else if (matches(KCs.backslash)) {
    Some(KCs.backslash);
  } else if (matches(KCs.plus)) {
    Some(KCs.plus);
  } else if (matches(KCs.asterisk)) {
    Some(KCs.asterisk);
  } else if (matches(KCs.lt)) {
    Some(KCs.lt);
  } else if (matches(KCs.space)) {
    Some(KCs.space);
  } else if (matches(KCs.comma)) {
    Some(KCs.comma);
  } else if (matches(KCs.left_bracket)) {
    Some(KCs.left_bracket);
  } else if (matches(KCs.semicolon)) {
    Some(KCs.semicolon);
  } else if (matches(KCs.alt_L)) {
    Some(KCs.alt_L);
  } else if (matches(KCs.alt_R)) {
    Some(KCs.alt_R);
  } else if (matches(KCs.alt_C)) {
    Some(KCs.alt_C);
  } else {
    None;
  };
};

let view =
    (~inject: Update.Action.t => Vdom.Event.t, model: Model.t): Vdom.Node.t => {
  Vdom.(
    Node.div(
      [
        Attr.id("cell"),
        Attr.create("contenteditable", "true"),
        Attr.on("drop", _ => Event.Prevent_default),
        Attr.on_keypress(evt =>
          JSUtil.is_movement_key(evt)
            ? Event.Many([]) : Event.Prevent_default
        ),
        Attr.on_keydown(evt => {
          let prevent_stop = [Event.Stop_propagation, Event.Prevent_default];
          let is_backspace =
            JSUtil.KeyCombo.matches(JSUtil.KeyCombos.backspace, evt);
          let is_del = JSUtil.KeyCombo.matches(JSUtil.KeyCombos.delete, evt);
          switch (
            JSUtil.is_single_key(evt),
            is_backspace || is_del,
            kc_of_evt(evt),
          ) {
          | (None, false, None) => Event.Ignore
          | (Some(single_key), _, _) =>
            switch (model.cursor_info.sort) {
            | IsLine(EmptyLine)
            | IsLine(ExpLine(EmptyHole(_)))
            | IsExpr(EmptyHole(_))
            | IsPat(EmptyHole(_)) =>
              let shape =
                switch (single_key) {
                | Number(n) => Action.SNumLit(n, OnText(num_digits(n)))
                | Letter(x) => Action.SVar(x, OnText(Var.length(x)))
                | Underscore => Action.SWild
                };
              Event.Many([
                inject(Update.Action.EditAction(Construct(shape))),
                Event.Stop_propagation,
                Event.Prevent_default,
              ]);
            | IsExpr(NumLit(_, _))
            | IsExpr(BoolLit(_, _))
            | IsExpr(Var(_, _, _))
            | IsPat(Var(_, _, _))
            | IsPat(NumLit(_, _))
            | IsPat(BoolLit(_, _)) =>
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
              let update =
                switch (int_of_string_opt(newNodeValue)) {
                | Some(new_n) =>
                  inject(
                    Update.Action.EditAction(
                      Action.Construct(
                        Action.SNumLit(new_n, OnText(anchorOffset + 1)),
                      ),
                    ),
                  )
                | None =>
                  Var.is_valid(newNodeValue)
                    ? inject(
                        Update.Action.EditAction(
                          Action.Construct(
                            Action.SVar(
                              newNodeValue,
                              OnText(anchorOffset + 1),
                            ),
                          ),
                        ),
                      )
                    : inject(Update.Action.InvalidVar(newNodeValue))
                };
              Event.Many([
                update,
                Event.Stop_propagation,
                Event.Prevent_default,
              ]);
            | IsLine(_)
            | IsExpr(_)
            | IsPat(_)
            | IsType
            | IsBlock(_) => Event.Ignore
            }
          | (_, true, _) =>
            let cursor_info = model.cursor_info;
            switch (cursor_info.sort) {
            | IsExpr(NumLit(_, _))
            | IsExpr(BoolLit(_, _))
            | IsExpr(Var(_, _, _))
            | IsPat(NumLit(_, _))
            | IsPat(BoolLit(_, _))
            | IsPat(Var(_, _, _)) =>
              let is_Before =
                switch (cursor_info.side) {
                | OnText(0) => true
                | _ => false
                };
              let is_After =
                switch (cursor_info.side) {
                | OnText(j) =>
                  switch (cursor_info.sort) {
                  | IsExpr(NumLit(_, n))
                  | IsPat(NumLit(_, n)) => j == num_digits(n)
                  | IsExpr(BoolLit(_, b))
                  | IsPat(BoolLit(_, b)) => j == (b ? 4 : 5)
                  | IsExpr(Var(_, _, x))
                  | IsPat(Var(_, _, x)) => j == Var.length(x)
                  | _ => false
                  }
                | _ => false
                };
              if (is_backspace && is_Before || is_del && is_After) {
                Event.Prevent_default;
              } else {
                let selection = Dom_html.window##getSelection;
                let anchorNode = selection##.anchorNode;
                let anchorOffset = selection##.anchorOffset;
                let nodeValue =
                  Js.to_string(
                    Js.Opt.get(anchorNode##.nodeValue, () => assert(false)),
                  );
                let ctrlKey = Js.to_bool(evt##.ctrlKey);
                let (nodeValue', anchorOffset') =
                  is_backspace
                    ? string_backspace(nodeValue, anchorOffset, ctrlKey)
                    : string_delete(nodeValue, anchorOffset, ctrlKey);
                let prevent_stop = [
                  Event.Stop_propagation,
                  Event.Prevent_default,
                ];
                if (String.equal(nodeValue', "")) {
                  if (is_Before) {
                    Event.Many([
                      inject(Update.Action.EditAction(Delete)),
                      ...prevent_stop,
                    ]);
                  } else {
                    Event.Many([
                      inject(Update.Action.EditAction(Backspace)),
                      ...prevent_stop,
                    ]);
                  };
                } else {
                  switch (int_of_string_opt(nodeValue')) {
                  | Some(new_n) =>
                    Event.Many([
                      inject(
                        Update.Action.EditAction(
                          Construct(SNumLit(new_n, OnText(anchorOffset'))),
                        ),
                      ),
                      ...prevent_stop,
                    ])
                  | None =>
                    Var.is_valid(nodeValue')
                      ? Event.Many([
                          inject(
                            Update.Action.EditAction(
                              Construct(
                                SVar(nodeValue', OnText(anchorOffset')),
                              ),
                            ),
                          ),
                          ...prevent_stop,
                        ])
                      : Event.Many(prevent_stop)
                  };
                };
              };
            | _ => Event.Prevent_default
            };
          | (_, _, Some(kc)) =>
            Event.Many([
              inject(
                Update.Action.EditAction(Hashtbl.find(kc_actions, kc)),
              ),
              ...prevent_stop,
            ])
          };
        }),
      ],
      [Code.view_of_zblock(~inject, model)],
    )
  );
};
