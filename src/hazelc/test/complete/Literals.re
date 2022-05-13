let test = Case.test;

let%test "bool true" = test((Str("true"), Pass("true")));
let%test "bool false" = test((Str("false"), Pass("false")));

let%test "int 0" = test((Str("0"), Pass("0")));
let%test "int 5" = test((Str("5"), Pass("5")));

let%test "unit" = test((DH(Triv), Pass("void")));

let%test "list nil" = test((Str("[]"), Pass("[]")));
let%test "list cons" = test((Str("true::[]"), Pass("[true]")));
