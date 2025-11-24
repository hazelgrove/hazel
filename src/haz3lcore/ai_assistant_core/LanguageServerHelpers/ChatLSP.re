open Language;

let get_sketch_and_error_ctx =
    (zipper: Zipper.t, info_map: Statics.Map.t): list(string) => {
  let sketch_seg = Dump.to_segment(zipper);
  let errors = ErrorPrint.all(info_map);
  let static_error_arr =
    switch (errors) {
    | [] => ["No static errors found"]
    | _ => errors
    };
  let ctx =
    [
      "PROGRAM SKETCH: ```"
      ++ ErrorPrint.Print.seg(~holes="?", sketch_seg)
      ++ "```",
    ]
    @ ["STATIC ERRORS: "]
    @ static_error_arr;
  ctx;
};
