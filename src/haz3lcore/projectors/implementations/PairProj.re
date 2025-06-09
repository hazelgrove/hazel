open Util;
open Calc.Syntax;
open ProjectorBase;
open OptUtil.Syntax;

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

let methods:
  methods(model('ed_m), action('ed_a), focus('ed_f), 'ed_m, 'ed_a, 'ed_f) = {
  init: (~copy_ed, _any, ed) => {
    let+ ed = ed();
    (ed, copy_ed(ed));
  },
  dynamics: false,
  view:
    (
      ~common,
      ~ed_str as _,
      ~view_ed as _,
      ~view_editable,
      ~enter_ed,
      ~mk_ed as _,
      ~mk_term_ed as _,
      ~calculate_ed as _,
      ~local,
      ~parent,
      ~focus,
      ~focussed,
      (ed1, ed2),
      _info,
    ) =>
    View.{
      inline:
        Web.div_c(
          "main",
          [
            Web.div_c("pair-proj-parens", [Web.Node.text("(")]),
            view_editable(
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
                | Direction.Left => parent(Escape(Left))
                | Direction.Right =>
                  enter_ed(
                    ~inject=a => local(Right(a)),
                    ~focus=f => focus(Right(f)),
                    Direction.Left,
                    ed2,
                  ),
              ~sort=Exp,
              ed1,
            ),
            Web.div_c("pair-proj-parens", [Web.Node.text(",")]),
            view_editable(
              ~common,
              ~inject=a => local(Right(a)),
              ~focus=f => focus(Right(f)),
              ~escape=
                fun
                | Direction.Left =>
                  enter_ed(
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
            Web.div_c("pair-proj-parens", [Web.Node.text(")")]),
          ],
        ),
      offside: None,
      overlay: None,
      enter_left:
        Some(
          enter_ed(
            ~inject=a => local(Left(a)),
            ~focus=f => focus(Left(f)),
            Direction.Left,
            ed1,
          ),
        ),
      enter_right:
        Some(
          enter_ed(
            ~inject=a => local(Right(a)),
            ~focus=f => focus(Right(f)),
            Direction.Right,
            ed2,
          ),
        ),
    },
  placeholder: (~ed_size, (ed1, ed2), _info) => {
    let ed1_size = ed_size(ed1);
    let ed2_size = ed_size(ed2);
    ProjectorShape.{
      horizontal: ed1_size.row + ed2_size.row + 6,
      vertical: ProjectorShape.Block(max(ed1_size.col, ed2_size.col)),
    };
  },
  update:
    (~update_ed, ~common, ~sort as _, _info, (left: 'ed, right: 'ed), action) => {
    switch (action) {
    | Left(ed_ac) =>
      let l_ed = update_ed(~common, ed_ac, left);
      (l_ed, right);
    | Right(ed_ac) =>
      let r_ed = update_ed(~common, ed_ac, right);
      (left, r_ed);
    };
  },
  mk_term: (~mk_term_ed, ~sort, ~prev, (ed1, ed2)) => {
    let inner_sort =
      switch (sort) {
      | Exp
      | Pat
      | Typ => sort
      | Any
      | TPat
      | Rul => Exp
      };
    let (ed1', t1) = mk_term_ed(~sort=inner_sort, ed1);
    let (ed2', t2) = mk_term_ed(~sort=inner_sort, ed2);
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
  },
  calculate: (~calculate_ed, ~common, (left: 'ed, right: 'ed)) => (
    calculate_ed(~common, left),
    calculate_ed(~common, right),
  ),
  get_cursor_info:
    (
      ~get_cursor_info_ed,
      ~common,
      ~inject: action('a) => Ui_effect.t(unit),
      ~read_only,
      (ed1, ed2),
      focus,
    ) =>
    switch (focus) {
    | Left(ed_f) =>
      get_cursor_info_ed(
        ~common,
        ~inject=x => inject(Left(x)),
        ~read_only,
        ed1,
        ed_f,
      )
    | Right(ed_f) =>
      get_cursor_info_ed(
        ~common,
        ~inject=x => inject(Right(x)),
        ~read_only,
        ed2,
        ed_f,
      )
    },
  sexp_of_model,
  model_of_sexp,
  yojson_of_model,
  model_of_yojson,
  sexp_of_action,
  action_of_sexp,
  yojson_of_action,
  action_of_yojson,
  sexp_of_focus,
  focus_of_sexp,
  yojson_of_focus,
  focus_of_yojson,
};
