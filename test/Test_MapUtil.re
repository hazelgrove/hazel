open Alcotest;
open Util;

let m = Maps.StringMap.of_alist_exn([("a", 1), ("b", 2)]);
let sexp = Sexplib.Sexp.of_string("((a 1) (b 2))");
let json = Yojson.Safe.from_string({|[["a",1],["b",2]]|});

let tests = (
  "MapUtil",
  [
    test_case("sexp_of_t writes (key value) pairs", `Quick, () =>
      check(
        string,
        "sexp",
        Sexplib.Sexp.to_string(sexp),
        Sexplib.Sexp.to_string(Maps.StringMap.sexp_of_t(sexp_of_int, m)),
      )
    ),
    test_case("t_of_sexp reads (key value) pairs", `Quick, () =>
      check(
        list(pair(string, int)),
        "bindings",
        [("a", 1), ("b", 2)],
        Maps.StringMap.to_alist(Maps.StringMap.t_of_sexp(int_of_sexp, sexp)),
      )
    ),
    test_case("yojson_of_t writes [key, value] pairs", `Quick, () =>
      check(
        string,
        "json",
        Yojson.Safe.to_string(json),
        Yojson.Safe.to_string(
          Maps.StringMap.yojson_of_t(
            Ppx_yojson_conv_lib.Yojson_conv.yojson_of_int,
            m,
          ),
        ),
      )
    ),
    test_case(
      "t_of_yojson keeps the last binding of a repeated key", `Quick, () =>
      check(
        list(pair(string, int)),
        "bindings",
        [("a", 3), ("b", 2)],
        Maps.StringMap.to_alist(
          Maps.StringMap.t_of_yojson(
            Ppx_yojson_conv_lib.Yojson_conv.int_of_yojson,
            Yojson.Safe.from_string({|[["a",1],["b",2],["a",3]]|}),
          ),
        ),
      )
    ),
    test_case("Marshal round-trips a map", `Quick, () =>
      check(
        list(pair(string, int)),
        "bindings",
        [("a", 1), ("b", 2)],
        Maps.StringMap.to_alist(
          Stdlib.Marshal.from_string(Stdlib.Marshal.to_string(m, []), 0),
        ),
      )
    ),
  ],
);
