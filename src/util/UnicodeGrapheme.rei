/* Lightweight grapheme helpers shared by text editing and measurement. */

let length: string => int;
let remove_nth: (string, int) => string;
let insert_nth: (string, int, string) => string;
let split_nth: (string, int) => (string, string);
let remove_last: string => string;
let remove_first: string => string;
let append: (string, string) => string;
let to_array: string => array(string);
let of_list: list(string) => string;
