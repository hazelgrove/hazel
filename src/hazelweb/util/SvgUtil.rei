module Point: {
  [@deriving (sexp, show)]
  type t = {
    x: float,
    y: float,
  };
};

module Rect: {
  [@deriving (sexp, show)]
  type t = {
    min: Point.t,
    width: float,
    height: float,
  };
};

module Path: {
  /**
   * SVG <path> element
   * https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Paths
   */
  type t = list(cmd)
  /**
   * Path commands. Constructors with underscore
   * suffix correspond to lower-case variants.
   */
  and cmd =
    | M(Point.t)
    | M_({
        dx: float,
        dy: float,
      })
    | H_({dx: float})
    | V_({dy: float})
    | A_({
        rx: float,
        ry: float,
        x_axis_rotation: float,
        large_arc_flag: bool,
        sweep_flag: bool,
        dx: float,
        dy: float,
      });

  let view:
    (~attrs: list(Virtual_dom.Vdom.Attr.t), t) => Virtual_dom.Vdom.Node.t;
};

/**
 * An orthogonal polygon is a polygon whose edges intersect at right angles
 */
module OrthogonalPolygon: {
  type t = Path.t;

  /**
   * `mk(~corner_radii, rects)` returns a path tracing the
   * contour of the union of rectangles in `rects`, rounding
   * corners according to `corner_radii`.
   *
   * Expects `rects` to be nonempty.
   *
   * Expects union of rectangles to form a single contour, otherwise
   * result is unspecified. (This is not a fundamental limitation of
   * the underlying algorithm, just an incidental one of the current
   * implementation.)
   */
  let mk: (~corner_radii: (float, float), list(Rect.t)) => t;
};
