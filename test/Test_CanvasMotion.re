open Alcotest;
module A = Web.CanvasAvatar;
let camera_independent_motion = () => {
  let position = (~camera, ~zoom, ~world) => {
    let (ox, oy) = camera
    and (x, y) = world;
    A.board_position(
      ~origin=camera,
      ~zoom,
      (ox +. x *. zoom, oy +. y *. zoom),
    );
  };
  let world = (320., 180.);
  List.iter(
    ((camera, zoom)) =>
      check(
        pair(float(0.00001), float(0.00001)),
        "camera pan and zoom preserve the resting body position",
        world,
        position(~camera, ~zoom, ~world),
      ),
    [
      ((200., 100.), 1.),
      (((-480.), 240.), 0.4),
      (((-800.), (-1200.)), 2.5),
      ((13.5, (-97.2)), 1.374),
    ],
  );
  let (x0, y0) = position(~camera=(200., 100.), ~zoom=0.4, ~world);
  let (x1, y1) =
    position(~camera=((-800.), (-1200.)), ~zoom=2.5, ~world=(365., 158.));
  check(
    pair(float(0.00001), float(0.00001)),
    "real travel survives simultaneous camera motion",
    (45., (-22.)),
    (x1 -. x0, y1 -. y0),
  );
};
let nested_hull_removal = () => {
  let ids = [
    "hullc-n-m-inner-T",
    "hullc-n-m-inner-T--vm",
    "hullc-n-m-inner-T--vm-inner",
    "hullc-n-m-inner-Tree",
    "hullc-n-m-inner-Tree--vm",
    "hullc-n-m-inner-T-child",
    "hullc-l-m-inner-T",
  ];
  check(
    list(string),
    "removing a member includes each ancestor copy and preserves similarly named siblings",
    [
      "hullc-n-m-inner-T",
      "hullc-n-m-inner-T--vm",
      "hullc-n-m-inner-T--vm-inner",
    ],
    Web.CanvasEnact.owned_hull_ids(~ids, ~prefix="hullc-n-", "m.inner.T"),
  );
};
let tests = (
  "Canvas motion",
  [
    test_case(
      "overview fits tall and wide graphs below follow's floor",
      `Quick,
      () => {
        List.iter(
          ((w, h)) => {
            let z =
              Web.CanvasZoom.fit(~width=w, ~height=h, ~aw=638., ~ah=503.);
            check(
              bool,
              "full extent fits with screen-space padding",
              true,
              w *. z <= 606. && h *. z <= 471.,
            );
            check(
              bool,
              "overview is below the old zoom floor",
              true,
              z < 0.4,
            );
            check(
              float(0.00001),
              "persisting the fitted zoom preserves it",
              z,
              Web.CanvasZoom.clamp(z),
            );
            check(
              bool,
              "a small pinch stays near the fitted scale",
              true,
              Web.CanvasZoom.clamp(z *. 1.01) < z *. 1.02,
            );
          },
          [(600., 3000.), (5000., 400.), (9000., 9000.)],
        );
        check(
          float(0.00001),
          "small graphs keep the magnification ceiling",
          2.5,
          Web.CanvasZoom.fit(~width=1., ~height=1., ~aw=638., ~ah=503.),
        );
      },
    ),
    test_case("nested hull removal ownership", `Quick, nested_hull_removal),
    test_case(
      "body motion excludes camera pan and zoom",
      `Quick,
      camera_independent_motion,
    ),
  ],
);
