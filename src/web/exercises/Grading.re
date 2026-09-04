open Util_web;
open Virtual_dom.Vdom;
open Node;

/* Generic grading primitives used by all kinds of exercises.

   Code-exercise-specific grading logic (test validation, mutation testing,
   syntax checks, impl grading, etc.) lives in [CodeGrading.re]. Tutorial
   grading lives in [TutorialGrading.re]. Each kind is free to plug its own
   reports into the common [score] / [percentage] currency defined here. */

[@deriving (show({with_path: false}), sexp, yojson)]
type percentage = float;
[@deriving (show({with_path: false}), sexp, yojson)]
type points = float;
[@deriving (show({with_path: false}), sexp, yojson)]
type score = (points, points);

let score_of_percent = (percent, max_points) => {
  let max_points = float_of_int(max_points);
  (percent *. max_points, max_points);
};

let score_view = ((earned: points, max: points)) => {
  div(
    ~attrs=[
      Attr.classes([
        "test-percent",
        Float.equal(earned, max) ? "all-pass" : "some-fail",
      ]),
    ],
    [text(Printf.sprintf("%.1f / %.1f pts", earned, max))],
  );
};

/* Shown while evaluation is still streaming so partial test maps are not
   presented as an authoritative grade. */
let pending_score_view = () => {
  div(~attrs=[Attr.classes(["test-percent", "pending"])], [text("…")]);
};

let percentage_view = (p: percentage) => {
  div(
    ~attrs=[
      Attr.classes([
        "test-percent",
        Float.equal(p, 1.) ? "all-pass" : "some-fail",
      ]),
    ],
    [text(Printf.sprintf("%.0f%%", 100. *. p))],
  );
};
