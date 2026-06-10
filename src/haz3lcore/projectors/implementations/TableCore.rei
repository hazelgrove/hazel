open Language;

let max_column_length: int;

/* --- Table Parsing --- */

type table_data = (list(option(string)), list(list(Exp.t)));
let parse_table: Exp.t => option(table_data);
