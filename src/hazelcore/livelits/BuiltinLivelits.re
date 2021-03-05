open Sexplib.Std;

module type LIVELIT_CORE = {
  let name: LivelitName.t;
  let expansion_ty: HTyp.t;
  let param_tys: list((Var.t, HTyp.t));
  [@deriving sexp]
  type model;
  [@deriving sexp]
  type action;

  let init_model: SpliceGenCmd.t(model);
  let update: (model, action) => SpliceGenCmd.t(model);
  let expand: model => UHExp.t;
};

let _to_uhvar = id => UHExp.var(SpliceInfo.var_of_splice_name(id));

let wrap_lambda_dummy = u => UHExp.wrap_lambda(u, "_");

module PairLivelitCore = {
  let name = "$pair";
  let expansion_ty = HTyp.(Prod([Hole, Hole]));
  let param_tys = [];

  [@deriving sexp]
  type model = (int, int);
  [@deriving sexp]
  type action = unit;

  let init_model =
    SpliceGenCmd.bind(SpliceGenCmd.new_splice(HTyp.Hole), leftID =>
      SpliceGenCmd.bind(SpliceGenCmd.new_splice(HTyp.Hole), rightID =>
        SpliceGenCmd.return((leftID, rightID))
      )
    );

  let update = (m, _) => SpliceGenCmd.return(m);

  let expand = ((leftID, rightID)) => {
    let pair_seq =
      Seq.mk(
        _to_uhvar(leftID),
        [(Operators_Exp.Comma, _to_uhvar(rightID))],
      );
    wrap_lambda_dummy(UHExp.Block.wrap'(UHExp.mk_OpSeq(pair_seq)));
  };
};

module MatrixLivelitCore = {
  let name = "$matrix";
  let expansion_ty = HTyp.(List(List(Int)));
  let param_tys = [];

  // assume nonzero height and width
  [@deriving sexp]
  type model = (SpliceName.t, list(list(SpliceName.t)));
  [@deriving sexp]
  type dim =
    | Row
    | Col;
  [@deriving sexp]
  type action =
    | Select(SpliceName.t)
    | Add(dim)
    | Del(dim, int);

  let init_height = 2;
  let init_width = 2;

  let get_height = (m: list(list(SpliceName.t))): int => List.length(m);
  let get_width = (m: list(list(SpliceName.t))): int =>
    List.length(List.hd(m));

  let init_model =
    SpliceGenCmd.(
      MonadsUtil.bind_count(
        init_height,
        bind(
          MonadsUtil.bind_count(
            init_width,
            bind(new_splice(HTyp.Int)),
            return,
          ),
        ),
        grid =>
        return((grid |> List.hd |> List.hd, grid))
      )
    );

  let update = ((selected, m)) =>
    fun
    | Select(to_select) => {
        let to_select_in_grid =
          !(m |> List.for_all(List.for_all(s => s != to_select)));
        if (!to_select_in_grid) {
          print_endline(
            Printf.sprintf(
              "Attempt to select splice name %d, which is not in the matrix",
              to_select,
            ),
          );
        };
        SpliceGenCmd.return((to_select_in_grid ? to_select : selected, m));
      }
    | Add(Row) =>
      SpliceGenCmd.(
        MonadsUtil.bind_count(
          get_width(m), bind(new_splice(HTyp.Int)), new_row =>
          return((selected, m @ [new_row]))
        )
      )
    | Add(Col) =>
      SpliceGenCmd.(
        MonadsUtil.bind_count(
          get_height(m), bind(new_splice(HTyp.Int)), new_col =>
          return((selected, List.map2((c, r) => r @ [c], new_col, m)))
        )
      )
    | Del(dim, i) => {
        let drop = (to_drop, ret) => {
          let selected_in_to_drop =
            !(to_drop |> List.for_all(s => s != selected));
          let to_select =
            selected_in_to_drop ? ret |> List.hd |> List.hd : selected;
          SpliceGenCmd.(
            MonadsUtil.bind_list(
              to_drop,
              d => bind(drop_splice(d)),
              _ => return((to_select, ret)),
            )
          );
        };
        switch (dim) {
        | Row =>
          if (get_height(m) <= 1) {
            SpliceGenCmd.return((selected, m));
          } else {
            let (before, to_drop, after) = ListUtil.split_nth(i, m);
            drop(to_drop, before @ after);
          }
        | Col =>
          if (get_width(m) <= 1) {
            SpliceGenCmd.return((selected, m));
          } else {
            let (before, to_drop, after) =
              m |> List.map(r => ListUtil.split_nth(i, r)) |> ListUtil.split3;
            drop(to_drop, List.map2((b, a) => b @ a, before, after));
          }
        };
      };

  let expand = ((_, m)) => {
    let to_uhexp_list =
      fun
      | [] => UHExp.(Block.wrap(ListNil(NotInHole)))
      | [fst, ...rest] => {
          let rest' =
            (rest |> List.map(item => (Operators_Exp.Cons, item)))
            @ [(Operators_Exp.Cons, UHExp.ListNil(NotInHole))];
          let seq = Seq.mk(fst, rest');
          UHExp.Block.wrap'(UHExp.mk_OpSeq(seq));
        };
    let m' =
      m
      |> List.map(r =>
           r
           |> List.map(_to_uhvar)
           |> to_uhexp_list
           |> (q => UHExp.Parenthesized(q))
         );
    wrap_lambda_dummy(to_uhexp_list(m'));
  };
};

module GradeCutoffLivelitCore = {
  let name = "$grade_cutoffs";
  let expansion_ty = HTyp.(Prod([Float, Float, Float, Float]));
  let param_tys = [("data", HTyp.(List(Prod([String, Float]))))];

  [@deriving sexp]
  type letter_grade =
    | A
    | B
    | C
    | D;

  [@deriving sexp]
  type model = {
    a: float,
    b: float,
    c: float,
    d: float,
    selecting: option(letter_grade),
  };

  [@deriving sexp]
  type action =
    | UpdateCutoff(letter_grade, float)
    | StartSelecting(letter_grade)
    | StopSelecting;

  let init_model =
    SpliceGenCmd.return({a: 90., b: 80., c: 70., d: 60., selecting: None});

  let is_valid_percentage = (p: float) => 0. <= p && p <= 100.;

  let rec update_a = (new_a, model): model =>
    if (!is_valid_percentage(new_a)) {
      model;
    } else {
      let updated = {...model, a: new_a};
      if (updated.a < updated.b) {
        update_b(updated.a -. 1., updated);
      } else {
        updated;
      };
    }
  and update_b = (new_b, model): model =>
    if (!is_valid_percentage(new_b)) {
      model;
    } else {
      let updated = {...model, b: new_b};
      if (updated.a <= updated.b) {
        update_a(updated.b +. 1., updated);
      } else if (updated.b <= updated.c) {
        update_c(updated.b -. 1., updated);
      } else {
        updated;
      };
    }
  and update_c = (new_c, model): model =>
    if (!is_valid_percentage(new_c)) {
      model;
    } else {
      let updated = {...model, c: new_c};
      if (updated.b <= updated.c) {
        update_b(updated.c +. 1., updated);
      } else if (updated.c <= updated.d) {
        update_d(updated.c -. 1., updated);
      } else {
        updated;
      };
    }
  and update_d = (new_d, model): model =>
    if (!is_valid_percentage(new_d)) {
      model;
    } else {
      let updated = {...model, d: new_d};
      if (updated.c <= updated.d) {
        update_c(updated.c +. 1., updated);
      } else {
        updated;
      };
    };

  let update = (model, action) =>
    SpliceGenCmd.return(
      switch (action) {
      | StopSelecting => {...model, selecting: None}
      | StartSelecting(letter) => {...model, selecting: Some(letter)}
      | UpdateCutoff(letter, new_cutoff) =>
        let update =
          switch (letter) {
          | A => update_a(new_cutoff)
          | B => update_b(new_cutoff)
          | C => update_c(new_cutoff)
          | D => update_d(new_cutoff)
          };
        update(model);
      },
    );

  let expand = ({a, b, c, d, selecting: _}) => {
    let tupl_seq =
      UHExp.(
        Seq.mk(
          floatlit'(a),
          [
            (Operators_Exp.Comma, floatlit'(b)),
            (Operators_Exp.Comma, floatlit'(c)),
            (Operators_Exp.Comma, floatlit'(d)),
          ],
        )
      );
    wrap_lambda_dummy(UHExp.Block.wrap'(UHExp.mk_OpSeq(tupl_seq)));
  };
};

module GrayscaleLivelitCore = {
  let name = "$basic_adjustments";
  let expansion_ty = HTyp.Int;
  let param_tys = [("url", HTyp.String)];

  [@deriving sexp]
  type model = {
    brightness: SpliceName.t,
    grayscale: SpliceName.t,
  };
  [@deriving sexp]
  type action = unit;

  let init_model =
    SpliceGenCmd.(
      bind(
        new_splice(
          ~init_uhexp_gen=
            u_gen => (UHExp.(Block.wrap(intlit'(100))), u_gen),
          HTyp.Int,
        ),
        brightness =>
        bind(
          new_splice(
            ~init_uhexp_gen=
              u_gen => (UHExp.(Block.wrap(intlit'(100))), u_gen),
            HTyp.Int,
          ),
          grayscale =>
          return({brightness, grayscale})
        )
      )
    );

  let update = (model, _) => SpliceGenCmd.return(model);

  let expand = _ => wrap_lambda_dummy(UHExp.Block.wrap(UHExp.intlit'(0)));
};

module ColorLivelitCore = {
  let name = "$color";
  let expansion_ty = HTyp.Prod([Int, Int, Int, Int]);
  let param_tys = [];

  [@deriving sexp]
  type model = {
    rgb: (SpliceName.t, SpliceName.t, SpliceName.t),
    a: SpliceName.t,
    selecting_sat_val: bool,
  };
  let init_model = {
    let (rval, gval, bval) = (255, 0, 0);
    SpliceGenCmd.(
      bind(
        new_splice(
          ~init_uhexp_gen=
            u_gen => (UHExp.(Block.wrap(intlit'(rval))), u_gen),
          HTyp.Int,
        ),
        r =>
        bind(
          new_splice(
            ~init_uhexp_gen=
              u_gen => (UHExp.(Block.wrap(intlit'(gval))), u_gen),
            HTyp.Int,
          ),
          g =>
          bind(
            new_splice(
              ~init_uhexp_gen=
                u_gen => (UHExp.(Block.wrap(intlit'(bval))), u_gen),
              HTyp.Int,
            ),
            b =>
            bind(
              new_splice(
                ~init_uhexp_gen=
                  u_gen => (UHExp.(Block.wrap(intlit'(100))), u_gen),
                HTyp.Int,
              ),
              a =>
              return({rgb: (r, g, b), a, selecting_sat_val: false})
            )
          )
        )
      )
    );
  };

  [@deriving sexp]
  type action =
    | StartSelectingSatVal
    | StopSelectingSatVal
    | SelectRGB((int, int, int));

  let update = (m, action) =>
    switch (action) {
    | StartSelectingSatVal =>
      SpliceGenCmd.return({...m, selecting_sat_val: true})
    | StopSelectingSatVal =>
      SpliceGenCmd.return({...m, selecting_sat_val: false})
    | SelectRGB((rval, gval, bval)) =>
      let (r, g, b) = m.rgb;
      SpliceGenCmd.(
        bind(
          map_splice(r, (_, u_gen) =>
            ((HTyp.Int, UHExp.(Block.wrap(intlit'(rval)))), u_gen)
          ),
          _ =>
          bind(
            map_splice(g, (_, u_gen) =>
              ((HTyp.Int, UHExp.(Block.wrap(intlit'(gval)))), u_gen)
            ),
            _ =>
            bind(
              map_splice(b, (_, u_gen) =>
                ((HTyp.Int, UHExp.(Block.wrap(intlit'(bval)))), u_gen)
              ),
              _ =>
              return(m)
            )
          )
        )
      );
    };

  let expand = ({rgb: (r, g, b), a, _}) => {
    let four_tuple =
      Seq.mk(
        _to_uhvar(r),
        [
          (Operators_Exp.Comma, _to_uhvar(g)),
          (Operators_Exp.Comma, _to_uhvar(b)),
          (Operators_Exp.Comma, _to_uhvar(a)),
        ],
      );
    wrap_lambda_dummy(UHExp.Block.wrap'(UHExp.mk_OpSeq(four_tuple)));
  };
};

/*
 module GradientLivelitCore = {
   let name = "$gradient";
   let expansion_ty = ColorLivelitCore.expansion_ty;
   let param_tys = [];


   let (color_livelit_ctx, _) = // WTF
      LivelitContexts.extend(
        LivelitContexts.empty,
        ColorLivelitAdapter.contexts_entry,
      );
   let color_ctx = (VarCtx.empty, color_livelit_ctx);

   [@deriving sexp]
   type model = {
     lcolor: SpliceName.t,
     rcolor: SpliceName.t,
     slider_value: int,
   };
   [@deriving sexp]
   type action =
     | Slide(int);

   let slider_min = 0;
   let slider_max = 100;
   let init_slider_value = 50;
   let init_model = {
     let init_uhexp_gen = u_gen => {
       let (u, u_gen) = MetaVarGen.next_livelit(u_gen);
       let (e, _, u_gen) =
         Statics_Exp.syn_fix_holes( // WTF
           color_ctx,
           u_gen,
           UHExp.Block.wrap(FreeLivelit(u, "$color")),
         );
       (e, u_gen);
     };
     SpliceGenCmd.(
       bind(new_splice(~init_uhexp_gen, ColorLivelitCore.expansion_ty), lcolor =>
         bind(
           new_splice(~init_uhexp_gen, ColorLivelitCore.expansion_ty), rcolor =>
           return({lcolor, rcolor, slider_value: init_slider_value})
         )
       )
     );
   };

   let update = (model, a) =>
     switch (a) {
     | Slide(n) => SpliceGenCmd.return({...model, slider_value: n})
     };

   let expand = ({lcolor, rcolor, slider_value}) => {
     let typ_opseq = (hd, tl) => UHTyp.mk_OpSeq(Seq.mk(hd, tl));
     let pat_opseq = (hd, tl) => UHPat.mk_OpSeq(Seq.mk(hd, tl));
     let exp_opseq = (hd, tl) => UHExp.mk_OpSeq(Seq.mk(hd, tl));
     let typ_triple = (x1, x2, x3) =>
       typ_opseq(x1, [(Operators_Typ.Prod, x2), (Operators_Typ.Prod, x3)]);
     let pat_triple = (x1, x2, x3) =>
       UHPat.(pat_opseq(var(x1), [(Comma, var(x2)), (Comma, var(x3))]));
     let scalar =
       UHExp.floatlit'(
         float_of_int(slider_value) /. float_of_int(slider_max),
       );
     let interpolate_vars = (x1, x2) =>
       UHExp.(
         Parenthesized(
           Block.wrap'(
             exp_opseq(
               var(x1),
               [
                 (FPlus, scalar),
                 (
                   FTimes,
                   Parenthesized(
                     Block.wrap'(exp_opseq(var(x2), [(FMinus, var(x1))])),
                   ),
                 ),
               ],
             ),
           ),
         )
       );
     let interpolate =
       UHExp.lam(
         UHPat.(
           pat_opseq(
             Parenthesized(pat_triple("r1", "g1", "b1")),
             [(Comma, Parenthesized(pat_triple("r2", "g2", "b2")))],
           )
         ),
         ~ann=
           UHTyp.(
             typ_opseq(
               Parenthesized(typ_triple(Float, Float, Float)),
               [
                 (
                   Operators_Typ.Prod,
                   Parenthesized(typ_triple(Float, Float, Float)),
                 ),
               ],
             )
           ),
         UHExp.(
           Block.wrap'(
             exp_opseq(
               interpolate_vars("r1", "r2"),
               [
                 (Comma, interpolate_vars("g1", "g2")),
                 (Comma, interpolate_vars("b1", "b2")),
               ],
             ),
           )
         ),
       );
     UHExp.(
       Block.wrap'(
         exp_opseq(
           interpolate,
           [
             (
               Space,
               Parenthesized(
                 Block.wrap'(
                   exp_opseq(
                     _to_uhvar(lcolor),
                     [(Comma, _to_uhvar(rcolor))],
                   ),
                 ),
               ),
             ),
           ],
         ),
       )
     );
   };
 };
 */

module CheckboxLivelitCore = {
  let name = "$checkbox";
  let expansion_ty = HTyp.Bool;
  let param_tys = [];

  [@deriving sexp]
  type model = bool;
  [@deriving sexp]
  type action =
    | Toggle;

  let init_model = SpliceGenCmd.return(false);
  let update = (m, Toggle) => SpliceGenCmd.return(!m);

  let expand = m =>
    wrap_lambda_dummy(UHExp.Block.wrap(UHExp.BoolLit(NotInHole, m)));
};

module SliderLivelitMinSpliceCore = {
  let name = "$slidep";
  let expansion_ty = HTyp.Int;
  let param_tys = [];

  [@deriving sexp]
  type model = (SpliceName.t, int);
  [@deriving sexp]
  type action = int;

  let init_model = {
    SpliceGenCmd.(
      bind(
        new_splice(
          ~init_uhexp_gen=
            u_gen => (UHExp.(Block.wrap(intlit'(100))), u_gen),
          HTyp.Int,
        ),
        endpoint_splice_number =>
        return((endpoint_splice_number, 50))
      )
    );
  };

  let update = ((endpoint_splice_number, _), action) => {
    SpliceGenCmd.(return((endpoint_splice_number, action)));
  };
  let expand = ((_, n)) =>
    wrap_lambda_dummy(UHExp.Block.wrap(UHExp.intlit'(n)));
};

module SliderLivelitMinCore = {
  let name = "$slidem";
  let expansion_ty = HTyp.Int;
  let param_tys = [];

  [@deriving sexp]
  type model = int;
  [@deriving sexp]
  type action = int;

  let init_model = SpliceGenCmd.return(0);
  let update = (_, n) => SpliceGenCmd.return(n);
  let expand = n => wrap_lambda_dummy(UHExp.Block.wrap(UHExp.intlit'(n)));
};

module SliderLivelitCore = {
  let name = "$slider";
  let expansion_ty = HTyp.Int;
  let param_tys = [("min", HTyp.Int), ("max", HTyp.Int)];

  [@deriving sexp]
  type endpoint =
    | Min
    | Max;

  [@deriving sexp]
  type model = option(int);

  [@deriving sexp]
  type action =
    | InvalidParams
    | Slide(int);

  let init_model = SpliceGenCmd.return(Some(0));

  let update = (_, a) =>
    switch (a) {
    | InvalidParams => SpliceGenCmd.return(None)
    | Slide(n) => SpliceGenCmd.return(Some(n))
    };

  let expand =
    fun
    | None => wrap_lambda_dummy(UHExp.Block.wrap(UHExp.intlit'(0)))
    | Some(n) => wrap_lambda_dummy(UHExp.Block.wrap(UHExp.intlit'(n)));
};

module SliderLivelitFloatCore = {
  let name = "$fslider";
  let expansion_ty = HTyp.Float;
  let param_tys = [("min", HTyp.Float), ("max", HTyp.Float)];

  [@deriving sexp]
  type endpoint =
    | Min
    | Max;

  [@deriving sexp]
  type model = option(float);

  [@deriving sexp]
  type action =
    | InvalidParams
    | Slide(float);

  let init_model = SpliceGenCmd.return(Some(0.));

  let update = (_, a) =>
    switch (a) {
    | InvalidParams => SpliceGenCmd.return(None)
    | Slide(f) => SpliceGenCmd.return(Some(f))
    };

  let expand =
    fun
    | None => wrap_lambda_dummy(UHExp.Block.wrap(UHExp.floatlit'(0.)))
    | Some(f) => wrap_lambda_dummy(UHExp.Block.wrap(UHExp.floatlit'(f)));
};

module DataFrameLivelitCore = {
  let name = "$data_frame";
  let expansion_ty =
    HTyp.(Prod([List(String), List(Prod([String, List(Float)]))]));
  let param_tys = [];

  [@deriving sexp]
  type row = {
    header: SpliceName.t,
    cells: list(SpliceName.t),
  };
  // assume nonzero height and width
  [@deriving sexp]
  type model = {
    selected: SpliceName.t,
    col_headers: list(SpliceName.t),
    rows: list(row),
  };

  [@deriving sexp]
  type dim =
    | Row
    | Col;
  [@deriving sexp]
  type action =
    | Select(SpliceName.t)
    | Add(dim)
    | Del(dim, int);

  let init_height = 3;
  let init_width = 5;

  let get_height = (m: model): int => List.length(m.rows);
  let get_width = (m: model): int => List.length(m.col_headers);

  let init_model =
    SpliceGenCmd.(
      MonadsUtil.bind_count(
        init_height,
        bind(
          MonadsUtil.bind_count(
            init_width, bind(new_splice(HTyp.Float)), row_cells =>
            bind(new_splice(HTyp.String), row_header =>
              return({header: row_header, cells: row_cells})
            )
          ),
        ),
        rows =>
        MonadsUtil.bind_count(
          init_width, bind(new_splice(HTyp.String)), col_headers =>
          return({selected: List.hd(col_headers), col_headers, rows})
        )
      )
    );

  let update = (m, u): SpliceGenCmd.t(model) =>
    switch (u) {
    | Select(splice) => SpliceGenCmd.return({...m, selected: splice})
    | Add(Row) =>
      SpliceGenCmd.(
        MonadsUtil.bind_count(
          get_width(m), bind(new_splice(HTyp.Float)), new_cells =>
          bind(new_splice(HTyp.String), new_header =>
            return({
              ...m,
              selected: new_header,
              rows: m.rows @ [{header: new_header, cells: new_cells}],
            })
          )
        )
      )
    | Add(Col) =>
      SpliceGenCmd.(
        MonadsUtil.bind_count(
          get_height(m), bind(new_splice(HTyp.Float)), new_cells =>
          bind(new_splice(HTyp.String), new_header =>
            return({
              selected: new_header,
              col_headers: m.col_headers @ [new_header],
              rows:
                List.map2(
                  (r, c) => {...r, cells: r.cells @ [c]},
                  m.rows,
                  new_cells,
                ),
            })
          )
        )
      )
    | Del(Row, i) =>
      switch (ListUtil.split_nth_opt(i, m.rows)) {
      | None
      | Some(([], _, [])) => SpliceGenCmd.return(m)
      | Some((prefix, row_to_drop, suffix)) =>
        let row_to_drop = [row_to_drop.header, ...row_to_drop.cells];
        let selected =
          List.exists((==)(m.selected), row_to_drop)
            ? switch (ListUtil.split_first_opt(suffix)) {
              | Some((hd, _)) => hd.header
              | None =>
                let (_, last) = ListUtil.split_last(prefix);
                last.header;
              }
            : m.selected;
        SpliceGenCmd.(
          MonadsUtil.bind_list(
            row_to_drop,
            splice => bind(drop_splice(splice)),
            _ => return({...m, selected, rows: prefix @ suffix}),
          )
        );
      }
    | Del(Col, i) =>
      if (get_width(m) <= 1) {
        SpliceGenCmd.return(m);
      } else {
        let (hdr_prefix, hdr_to_drop, hdr_suffix) =
          ListUtil.split_nth(i, m.col_headers);
        let (prefixes, cells_to_drop, suffixes) =
          m.rows
          |> List.map(row => row.cells)
          |> List.map(ListUtil.split_nth(i))
          |> ListUtil.split3;
        let col_to_drop = [hdr_to_drop, ...cells_to_drop];
        let selected =
          List.exists((==)(m.selected), col_to_drop)
            ? {
              let col =
                switch (ListUtil.split_first_opt(suffixes)) {
                | Some((hd, _)) => hd
                | None =>
                  let (_, last) = ListUtil.split_last(prefixes);
                  last;
                };
              List.hd(col);
            }
            : m.selected;
        let col_headers = hdr_prefix @ hdr_suffix;
        let rows = {
          let row_hdrs = List.map(row => row.header, m.rows);
          List.combine(row_hdrs, List.combine(prefixes, suffixes))
          |> List.map(((header, (prefix, suffix))) =>
               {header, cells: prefix @ suffix}
             );
        };
        SpliceGenCmd.(
          MonadsUtil.bind_list(
            col_to_drop,
            splice => bind(drop_splice(splice)),
            _ => return({selected, col_headers, rows}),
          )
        );
      }
    };

  let expand = m => {
    let to_uhexp_list =
      fun
      | [] => Seq.wrap(UHExp.listnil())
      | [fst, ...rest] => {
          let rest' =
            (rest |> List.map(item => (Operators_Exp.Cons, item)))
            @ [(Operators_Exp.Cons, UHExp.listnil())];
          Seq.mk(fst, rest');
        };
    let col_headers = m.col_headers |> List.map(_to_uhvar) |> to_uhexp_list;
    let rows =
      m.rows
      |> List.map(row => {
           let header = _to_uhvar(row.header);
           let cells = row.cells |> List.map(_to_uhvar) |> to_uhexp_list;
           UHExp.(
             Parenthesized([
               ExpLine(
                 UHExp.mk_OpSeq(
                   Seq.S(header, A(Operators_Exp.Comma, cells)),
                 ),
               ),
             ])
           );
         })
      |> to_uhexp_list;
    wrap_lambda_dummy(
      UHExp.[
        ExpLine(
          mk_OpSeq(Seq.seq_op_seq(col_headers, Operators_Exp.Comma, rows)),
        ),
      ],
    );
  };
};

module LivelitCoreAdapter = (L: LIVELIT_CORE) => {
  let serialize_monad = model => SpliceGenCmd.return(L.sexp_of_model(model));

  /* generate livelit definition for Semantics */
  let livelit_defn =
    LivelitDefinition.{
      name: L.name,
      expansion_ty: L.expansion_ty,
      captures_ty: HTyp.Prod([]),
      action_ty: HTyp.Hole,
      model_ty: HTyp.Hole,
      param_tys: L.param_tys,
      init_model: SpliceGenCmd.bind(L.init_model, serialize_monad),
      update: (serialized_model, serialized_action) =>
        SpliceGenCmd.bind(
          L.update(
            L.model_of_sexp(serialized_model),
            L.action_of_sexp(serialized_action),
          ),
          serialize_monad,
        ),
      expand: serialized_model =>
        Success(L.expand(L.model_of_sexp(serialized_model))),
    };

  let contexts_entry = (L.name, livelit_defn);
};

module LivelitCoreContexts = {
  //type t('a) = LivelitCtx.t('a);
  let empty = []; //LivelitCtx.empty;
  let extend = (livelit_ctx, (name, def: LivelitDefinition.t)) => {
    if (!LivelitName.is_valid(name)) {
      failwith("Invalid livelit name " ++ name);
    };
    if (name != def.name) {
      failwith(
        "Livelit name " ++ name ++ " differs from def name " ++ def.name,
      );
    };
    let (param_names, _) = List.split(def.param_tys);
    let rec contains_dupl =
      fun
      | [] => false
      | [hd, ...tl] => List.mem(hd, tl) || contains_dupl(tl);
    if (contains_dupl(param_names)) {
      failwith(
        "Parameter names for livelit "
        ++ name
        ++ " must be unique: "
        ++ String.concat(", ", param_names),
      );
    };
    VarMap.extend(livelit_ctx, (name, (def, [])));
  };
};

module ColorLivelitCoreAdapter = LivelitCoreAdapter(ColorLivelitCore);
module CheckboxLivelitCoreAdapter = LivelitCoreAdapter(CheckboxLivelitCore);
module PairLivelitCoreAdapter = LivelitCoreAdapter(PairLivelitCore);
module SliderLivelitCoreAdapter = LivelitCoreAdapter(SliderLivelitCore);
module SliderLivelitFloatCoreAdapter =
  LivelitCoreAdapter(SliderLivelitFloatCore);
module SliderLivelitMinCoreAdapter = LivelitCoreAdapter(SliderLivelitMinCore);
module SliderLivelitMinSpliceCoreAdapter =
  LivelitCoreAdapter(SliderLivelitMinSpliceCore);
module MatrixLivelitCoreAdapter = LivelitCoreAdapter(MatrixLivelitCore);
module GradeCutoffLivelitCoreAdapter =
  LivelitCoreAdapter(GradeCutoffLivelitCore);
module DataFrameLivelitCoreAdapter = LivelitCoreAdapter(DataFrameLivelitCore);
module GrayscaleLivelitCoreAdapter = LivelitCoreAdapter(GrayscaleLivelitCore);
//module GradientLivelitCoreAdapter = LivelitCoreAdapter(GradientLivelitCore);

let ctx =
  List.fold_left(
    LivelitCoreContexts.extend,
    LivelitCoreContexts.empty,
    [
      GradeCutoffLivelitCoreAdapter.contexts_entry,
      DataFrameLivelitCoreAdapter.contexts_entry,
      GrayscaleLivelitCoreAdapter.contexts_entry,
      ColorLivelitCoreAdapter.contexts_entry,
      SliderLivelitFloatCoreAdapter.contexts_entry,
      SliderLivelitCoreAdapter.contexts_entry,
      // MatrixLivelitCoreAdapter.contexts_entry,
      // PairLivelitCoreAdapter.contexts_entry,
      // CheckboxLivelitCoreAdapter.contexts_entry,
      // SliderLivelitMinCoreAdapter.contexts_entry,
      // SliderLivelitMinSpliceCoreAdapter.contexts_entry,
      //GradientLivelitCoreAdapter.contexts_entry,
    ],
  );
