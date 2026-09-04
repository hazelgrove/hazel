include Bigint;
include Ppx_yojson_conv_lib.Yojson_conv.Primitives;

let t_of_yojson = (json): t => {
  let str = string_of_yojson(json);
  switch (of_string_opt(str)) {
  | Some(int) => int
  | None => raise(Failure("Invalid JSON for BigInt"))
  };
};

let yojson_of_t = (int): Yojson.Safe.t => {
  yojson_of_string(to_string(int));
};
