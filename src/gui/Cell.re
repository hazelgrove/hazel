let view =
    (~inject: Update.Action.t => Vdom.Event.t, model: MyModel.t): Vdom.Node.t => {
  Vdom.(
    Node.div(
      [
        Attr.create("contenteditable", "true"),
        Attr.on_keydown(evt =>
          switch (JSUtil.is_single_key(evt)) {
          | Some(single_key) =>
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
                  let new_side =
                    side_of_str_offset(newNodeValue, anchorOffset + 1);
                  inject(
                    Update.Action.EditAction(
                      Action.Construct(Action.SNumLit(new_n, new_side)),
                    ),
                  );
                | None =>
                  Var.is_valid(newNodeValue)
                    ? {
                      let new_side =
                        side_of_str_offset(newNodeValue, anchorOffset + 1);
                      inject(
                        Update.Action.EditAction(
                          Action.Construct(
                            Action.SVar(newNodeValue, new_side),
                          ),
                        ),
                      );
                    }
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
            | IsBlock(_) => Event.Many([])
            }
          | None =>
            let is_backspace =
              JSUtil.KeyCombo.matches(JSUtil.KeyCombos.backspace, evt);
            let is_del =
              JSUtil.KeyCombo.matches(JSUtil.KeyCombos.delete, evt);
            if (is_backspace || is_del) {
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
                  | Text(0) => true
                  | _ => false
                  };
                let is_After =
                  switch (cursor_info.side) {
                  | Text(j) =>
                    switch (cursor_info.sort) {
                    | IsExpr(NumLit(_, n))
                    | IsPat(NumLit(_, n)) => j == num_digits(n)
                    | IsExpr(BoolLit(_, b))
                    | IsPat(BoolLit(_, b)) => j == (b ? 4 : 5)
                    | IsExpr(Var(_, _, x))
                    | IsPat(Var(_, _, x)) => j == Var.length(x)
                    | _ => false
                    }
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
                      let new_side =
                        side_of_str_offset(nodeValue', anchorOffset');
                      Event.Many([
                        inject(
                          Update.Action.EditAction(
                            Construct(SNumLit(new_n, new_side)),
                          ),
                        ),
                        ...prevent_stop,
                      ]);
                    | None =>
                      Var.is_valid(nodeValue')
                        ? {
                          let new_side =
                            side_of_str_offset(nodeValue', anchorOffset');
                          Event.Many([
                            inject(
                              Update.Action.EditAction(
                                Construct(SVar(nodeValue', new_side)),
                              ),
                            ),
                            ...prevent_stop,
                          ]);
                        }
                        : prevent_stop
                    };
                  };
                };
              | _ => Event.Prevent_default
              };
            } else {
              Event.Many([]);
            };
          }
        ),
        a_onkeypress(evt =>
          JSUtil.is_movement_key(evt)
            ? Event.Many([]) : Event.Prevent_default
        ),
        a_ondrop(evt => Event.Prevent_default),
      ],
      [Code.view_of_zblock(~inject, model)],
    )
  );
};
