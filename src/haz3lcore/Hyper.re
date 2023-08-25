let export_id =
  "10000137-0000-0000-0000-000000000000" |> Uuidm.of_string |> Option.get;

let export_str = "EXPORT";
let is_export = (==)(export_str);
