type csv_data =
  | WithHeaders(list(list((string, string))))
  | WithoutHeaders(list(list(string)));

/* Strip a leading UTF-8 byte-order mark (EF BB BF). Files exported from some
   tools (and some HTTP servers) prefix it to the first header; left in place
   it becomes part of that column's label, so `data.`name`` projection
   silently fails to match. Applied centrally so both the web and CLI
   ingestion paths benefit. */
let strip_bom = (s: string): string =>
  String.length(s) >= 3
  && Char.code(s.[0]) == 0xEF
  && Char.code(s.[1]) == 0xBB
  && Char.code(s.[2]) == 0xBF
    ? String.sub(s, 3, String.length(s) - 3) : s;

let parse_csv_with_headers = (csv: string) => {
  let data = Csv.of_string(~fix=true, ~has_header=true, strip_bom(csv));

  let header = Csv.Rows.header(data);
  let rows =
    data
    |> Csv.input_all
    |> List.filter(row => !List.is_empty(row) && row != [""]);
  Csv.associate(header, rows);
};

let parse_csv_without_headers = (csv: string) => {
  let data = Csv.of_string(~fix=true, ~has_header=false, strip_bom(csv));

  data
  |> Csv.input_all
  |> List.filter(row => !List.is_empty(row) && row != [""]);
};
