type t = int;

let string_to_id = Hashtbl.create(100);
let id_to_string = Hashtbl.create(100);

let fresh_id_counter = ref(0);
let fresh_id = (): t => {
  decr(fresh_id_counter);
  fresh_id_counter^;
};

let of_string = (str: string): t =>
  switch (Hashtbl.find_opt(string_to_id, str)) {
  | Some(id) => id
  | None =>
    let id = fresh_id();
    Hashtbl.add(string_to_id, str, id);
    Hashtbl.add(id_to_string, id, str);
    id;
  };

let to_string = (id: t): string =>
  switch (Hashtbl.find_opt(id_to_string, id)) {
  | Some(str) => str
  | None when id >= 0 => string_of_int(id)
  | _ => failwith("Negative ids are reserved for use by fresh_id.")
  };
