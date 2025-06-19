open Util;
open Calc.Syntax;
open ProjectorBase;
open OptUtil.Syntax;
open Language;

[@deriving (show({with_path: false}), sexp, yojson)]
type model('ed) = ('ed, 'ed);

[@deriving (show({with_path: false}), sexp, yojson)]
type action('ed_a) =
  | Left('ed_a)
  | Right('ed_a);

[@deriving (show({with_path: false}), sexp, yojson)]
type focus('ed_f) =
  | Left('ed_f)
  | Right('ed_f);

module M =
       (Editor: ProjectorInterface.EDITOR)

         : (
           ProjectorInterface.PROJECTOR with
             type model' = model(Editor.model) and
             type action' = action(Editor.action) and
             type focus' = focus(Editor.focus) and
             type editor_model = Editor.model
       ) => {
  type editor_model = Editor.model;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type model' = model(Editor.model);
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action' = action(Editor.action);
  [@deriving (show({with_path: false}), sexp, yojson)]
  type focus' = focus(Editor.focus);

  // MODEL

  let mk = (_any, ed) => {
    let* ed = ed();
    Some((ed, Editor.Model.copy(ed)));
  };

  let dynamics = false;

  // UPDATE

  let update =
      (
        ~common,
        ~sort as _,
        _info,
        (left: editor_model, right: editor_model),
        action: action(Editor.action),
      ) => {
    switch (action) {
    | Left(ed_ac) =>
      let l_ed = Editor.Update.update(~common, ed_ac, left);
      (l_ed, right);
    | Right(ed_ac) =>
      let r_ed = Editor.Update.update(~common, ed_ac, right);
      (left, r_ed);
    };
  };

  let mk_term = (~sort: Language.Sort.t, ~prev, (ed1, ed2)) => {
    let inner_sort =
      switch (sort) {
      | Exp
      | Pat
      | Typ => sort
      | Any
      | TPat
      | Rul => Exp
      };
    let (ed1', t1) = Editor.Update.make_term(~sort=inner_sort, ed1);
    let (ed2', t2) = Editor.Update.make_term(~sort=inner_sort, ed2);
    let term' =
      prev
      |> {
        let.calc t1 = t1
        and.calc t2 = t2;
        switch (sort) {
        | Exp => (
            Exp(
              Exp.fresh(
                Tuple([
                  t1 |> Any.is_exp |> Option.get,
                  t2 |> Any.is_exp |> Option.get,
                ]),
              ),
            ): Any.t
          )
        | Pat =>
          Pat(
            Pat.fresh(
              Tuple([
                t1 |> Any.is_pat |> Option.get,
                t2 |> Any.is_pat |> Option.get,
              ]),
            ),
          )
        | Typ =>
          Typ(
            Typ.fresh(
              Prod([
                t1 |> Any.is_typ |> Option.get,
                t2 |> Any.is_typ |> Option.get,
              ]),
            ),
          )
        | Any
        | TPat
        | Rul =>
          Exp(
            Exp.fresh(
              Tuple([
                t1 |> Any.is_exp |> Option.get,
                t2 |> Any.is_exp |> Option.get,
              ]),
            ),
          )
        };
      };
    ((ed1', ed2'), term');
  };

  let calculate = (~common, (left: editor_model, right: editor_model)) => (
    Editor.Update.calculate(~common, left),
    Editor.Update.calculate(~common, right),
  );

  // FOCUS

  let get_cursor_info =
      (
        ~common,
        ~inject: action' => Ui_effect.t(unit),
        ~read_only,
        (ed1, ed2),
        focus,
      ) =>
    switch (focus) {
    | Left(ed_f) =>
      Editor.Focus.get_cursor_info(
        ~common,
        ~inject=x => inject(Left(x)),
        ~read_only,
        ed1,
        ed_f,
      )
    | Right(ed_f) =>
      Editor.Focus.get_cursor_info(
        ~common,
        ~inject=x => inject(Right(x)),
        ~read_only,
        ed2,
        ed_f,
      )
    };

  // VIEW

  let view =
      (
        ~common,
        ~local: action' => Ui_effect.t(unit),
        ~parent,
        ~focus: focus' => Ui_effect.t(unit),
        ~focussed,
        (ed1, ed2),
        _info,
      ) =>
    View.{
      inline:
        WebUtil.div_c(
          "main",
          [
            WebUtil.div_c("pair-proj-parens", [WebUtil.Node.text("(")]),
            Editor.View.view_editable(
              ~common,
              ~inject=a => local(Left(a)),
              ~focus=f => focus(Left(f)),
              ~focussed=
                switch (focussed) {
                | Some(Left(f)) => Some(f)
                | _ => None
                },
              ~escape=
                fun
                | Direction.Left => parent(ProjectorInterface.Escape(Left))
                | Direction.Right =>
                  Editor.Focus.enter(
                    ~inject=a => local(Right(a)),
                    ~focus=f => focus(Right(f)),
                    Direction.Left,
                    ed2,
                  ),
              ~sort=Exp,
              ed1,
            ),
            WebUtil.div_c("pair-proj-parens", [WebUtil.Node.text(",")]),
            Editor.View.view_editable(
              ~common,
              ~inject=a => local(Right(a)),
              ~focus=f => focus(Right(f)),
              ~escape=
                fun
                | Direction.Left =>
                  Editor.Focus.enter(
                    ~inject=a => local(Left(a)),
                    ~focus=f => focus(Left(f)),
                    Direction.Right,
                    ed1,
                  )
                | Direction.Right => parent(Escape(Right)),
              ~focussed=
                switch (focussed) {
                | Some(Right(f)) => Some(f)
                | _ => None
                },
              ~sort=Exp,
              ed2,
            ),
            WebUtil.div_c("pair-proj-parens", [WebUtil.Node.text(")")]),
          ],
        ),
      offside: None,
      overlay: None,
      enter_left:
        Some(
          Editor.Focus.enter(
            ~inject=a => local(Left(a)),
            ~focus=f => focus(Left(f)),
            Direction.Left,
            ed1,
          ),
        ),
      enter_right:
        Some(
          Editor.Focus.enter(
            ~inject=a => local(Right(a)),
            ~focus=f => focus(Right(f)),
            Direction.Right,
            ed2,
          ),
        ),
    };

  let placeholder = ((ed1, ed2), _info) => {
    let ed1_size = Editor.View.get_dimensions(ed1);
    let ed2_size = Editor.View.get_dimensions(ed2);
    ProjectorShape.{
      horizontal: ed1_size.row + ed2_size.row + 6,
      vertical: ProjectorShape.Block(max(ed1_size.col, ed2_size.col)),
    };
  };
};
