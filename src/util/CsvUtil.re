let parse_csv = (csv: string) => {
  let data = Csv.of_string(~fix=true, ~has_header=true, csv);

  let header = Csv.Rows.header(data);
  let rows =
    data
    |> Csv.input_all
    |> List.filter(row => !List.is_empty(row) && row != [""]);
  Csv.associate(header, rows);
};
