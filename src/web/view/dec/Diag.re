open DecUtil;
open SvgUtil.Path;
open Sexplib.Std;

//TODO(?): deprecate this module

[@deriving (show({with_path: false}), sexp, yojson)]
type tip_shape = (Core.Nib.t, int);

// top right to bottom left
let tr_bl =
    (
      ~scale as s=1.,
      ~hemi: [ | `North | `South],
      ~with_child_border=false,
      ~stretch_x=0.,
      ~stretch_y=0.,
      (),
    ) =>
  SvgUtil.Path.(
    {
      let (diag, junction) =
        with_child_border
          ? (
            L_({dx: Float.neg(short_tip_width), dy: short_tip_height}),
            H_({dx: Float.neg(0.5 -. short_tip_width)}),
          )
          : (
            L_({dx: Float.neg(tip_width), dy: 0.5 +. stretch_y}),
            H_({dx: Float.neg(stretch_x)}),
          );
      let path =
        switch (hemi) {
        | `North => [junction, diag]
        | `South => [diag, junction]
        };
      scale(s, path);
    }
  );
// bottom left to top right
let bl_tr =
    (
      ~hemi: [ | `North | `South],
      ~with_child_border=false,
      ~stretch_x=0.,
      ~stretch_y=0.,
      (),
    ) =>
  SvgUtil.Path.reverse(
    tr_bl(~hemi, ~with_child_border, ~stretch_x, ~stretch_y, ()),
  );

// top left to bottom right
let tl_br =
    (
      ~hemi: [ | `North | `South],
      ~with_child_border=false,
      ~stretch_x=0.,
      ~stretch_y=0.,
      (),
    ) =>
  SvgUtil.Path.(
    {
      let (diag, junction) =
        with_child_border
          ? (
            L_({dx: short_tip_width, dy: short_tip_height}),
            H_({dx: 0.5 -. short_tip_width}),
          )
          : (L_({dx: tip_width, dy: 0.5 +. stretch_y}), H_({dx: stretch_x}));
      switch (hemi) {
      | `North => [junction, diag]
      | `South => [diag, junction]
      };
    }
  );
// bottom right to top left
let br_tl =
    (
      ~hemi: [ | `North | `South],
      ~with_child_border=false,
      ~stretch_x=0.,
      ~stretch_y=0.,
      (),
    ) =>
  SvgUtil.Path.reverse(
    tl_br(~hemi, ~with_child_border, ~stretch_x, ~stretch_y, ()),
  );

let left_tip_path =
    (~scale_x as s_x=1., ~scale_y as s_y=1., tip: tip_shape): SvgUtil.Path.t => {
  let path =
    switch (tip) {
    | ({shape: Convex, _}, _) =>
      br_tl(~hemi=`South, ()) @ bl_tr(~hemi=`North, ())
    | ({shape: Concave(_a), _}, n) =>
      let jag = [
        L_({dx: -. jagged_edge_w, dy: -. jagged_edge_h}),
        L_({dx: jagged_edge_w, dy: -. jagged_edge_h}),
        L_({dx: -. jagged_edge_w, dy: -. jagged_edge_h}),
      ];
      let bottom_half =
        n == 0
          ? [H_({dx: Float.neg(tip_width)}), ...bl_tr(~hemi=`South, ())]
          : List.concat([
              [H_({dx: -. (extra_tail +. 0.5)})],
              jag,
              [H_({dx: jagged_edge_w +. extra_tail})],
              bl_tr(~hemi=`South, ~with_child_border=true, ()),
            ]);
      let top_half =
        n == 0 || n == 1
          ? br_tl(~hemi=`North, ()) @ [H_({dx: tip_width})]
          : List.concat([
              br_tl(~hemi=`North, ~with_child_border=true, ()),
              [H_({dx: -. (jagged_edge_w +. extra_tail)})],
              jag,
              [H_({dx: extra_tail +. 0.5})],
            ]);
      bottom_half @ top_half;
    };
  scale_x(s_x, scale_y(s_y, path));
};
let right_tip_path =
    (~scale_x as s_x=1., ~scale_y as s_y=1., tip: tip_shape): SvgUtil.Path.t => {
  let path =
    switch (tip) {
    | ({shape: Convex, _}, _) =>
      tl_br(~hemi=`North, ()) @ tr_bl(~hemi=`South, ())
    | ({shape: Concave(_a), _}, n) =>
      open SvgUtil.Path;
      let jag = [
        L_({dx: jagged_edge_w, dy: jagged_edge_h}),
        L_({dx: -. jagged_edge_w, dy: jagged_edge_h}),
        L_({dx: jagged_edge_w, dy: jagged_edge_h}),
      ];
      let top_half =
        n == 0 || n == 1
          ? [H_({dx: tip_width}), ...tr_bl(~hemi=`North, ())]
          : List.concat([
              [H_({dx: 0.5 +. extra_tail})],
              jag,
              [H_({dx: -. (extra_tail +. jagged_edge_w)})],
              tr_bl(~hemi=`North, ~with_child_border=true, ()),
            ]);
      let bottom_half =
        n == 0
          ? tl_br(~hemi=`South, ()) @ [H_({dx: Float.neg(tip_width)})]
          : List.concat([
              tl_br(~with_child_border=true, ~hemi=`South, ()),
              [H_({dx: extra_tail})],
              jag,
              [H_({dx: Float.neg(jagged_edge_w +. extra_tail +. 0.5)})],
            ]);
      top_half @ bottom_half;
    };
  scale_x(s_x, scale_y(s_y, path));
};
