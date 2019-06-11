let space_separated: list(string) => string = String.concat(" ");

let props = (props: list(string)): string => space_separated(props);

let px = (length: int): string => string_of_int(length) ++ "px";
let fr = (length: int): string => string_of_int(length) ++ "fr";

let display = (display_type: string): string =>
  "display: " ++ display_type ++ ";";

let grid_template_columns = (widths: list(string)): string =>
  "grid-template-columns: " ++ space_separated(widths) ++ ";";

let grid_template_rows = (heights: list(string)): string =>
  "grid-template-rows: " ++ space_separated(heights) ++ ";";

let grid_template_areas = (row_templates: list(string)): string =>
  "grid-template-areas: "
  ++ space_separated(row_templates |> List.map(s => "\"" ++ s ++ "\""))
  ++ ";";

let grid_area = (name: string): string => "grid-area: " ++ name ++ ";";
