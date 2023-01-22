[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Whitespace // Not really an error
  | MalformedGrout // Should never happen
  | UnrecognizedTerm // Reminder to add term to MakeTerm
  | IncompleteTile; // Remove in future

let show: t => string =
  fun
  | Whitespace => "Whitespace"
  | MalformedGrout => "Malformed Grout"
  | UnrecognizedTerm => "Unrecognized Term"
  | IncompleteTile => "Incomplete Tile";
