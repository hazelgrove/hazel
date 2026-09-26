/* Where a projector instance draws its primary UI. Inline means
 * in-place in the code; Sidebar means docked in the projector panel,
 * leaving a compact chip at the code site. Lives in the language
 * library (like ProjectorKind) so Token can spell the placement
 * suffix of an invoke token; Haz3lcore.ProjectorCore.Placement is
 * this module. */
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t =
  | Inline
  | Sidebar;

let toggle: t => t =
  fun
  | Inline => Sidebar
  | Sidebar => Inline;

let is_sidebar: t => bool =
  fun
  | Inline => false
  | Sidebar => true;
