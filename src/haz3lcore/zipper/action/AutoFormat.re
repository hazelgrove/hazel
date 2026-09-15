/* TODO(andrew): once the pretty printer is fixed (it now incorporates
 * canonical indentation via Indentation.level_map, but has remaining
 * issues), rip out the Format Action.t and this module in favor of an
 * indentation-incorporating PrettyPrint as the single reformat action.
 * Related cleanup: the Format/PrettyPrint menu + keybinding split. */
let segment = (seg: Segment.t): Segment.t => {
  let indent_map = Indentation.level_map(seg);
  Indentation.fix_indentation_in_segment(indent_map, seg);
};

let zipper = (z: Zipper.t): Zipper.t => {
  let full_seg = Zipper.unselect_and_zip(z);
  let indent_map = Indentation.level_map(full_seg);
  ZipperBase.MapSegment.go(
    Indentation.fix_indentation_in_segment(indent_map),
    z,
  );
};
