type csv_data =
  | WithHeaders(list(list((string, string))))
  | WithoutHeaders(list(list(string)));

let parse_csv_with_headers = (csv: string) => {
  let data = Csv.of_string(~fix=true, ~has_header=true, csv);

  let header = Csv.Rows.header(data);
  let rows =
    data
    |> Csv.input_all
    |> List.filter(row => !List.is_empty(row) && row != [""]);
  Csv.associate(header, rows);
};

let parse_csv_without_headers = (csv: string) => {
  let data = Csv.of_string(~fix=true, ~has_header=false, csv);

  data
  |> Csv.input_all
  |> List.filter(row => !List.is_empty(row) && row != [""]);
};
