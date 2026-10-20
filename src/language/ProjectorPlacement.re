/* Where a projector's UI sits: at its code site, or docked in the sidebar.
   Defined here, beside ProjectorKind, so that Token can read and write the
   placement suffix of an invoke token (`^^slider_sidebar`);
   ProjectorCore.Placement is this module. */
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
