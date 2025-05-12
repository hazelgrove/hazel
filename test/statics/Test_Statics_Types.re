open Test_Statics_Prelude;

let tests = [
  skip_known_bug(
    "Typ.weak_head_normalize infinite recursion", // https://github.com/hazelgrove/hazel/issues/1621
    "type y = y in type ? = y in ?",
  ),
  skip_known_bug(
    "Coverage.all_ctrs_of_typ infinite recursion", // https://github.com/hazelgrove/hazel/issues/1624
    "fun ((()): ((rec x -> (rec y -> x)))) -> []",
  ),
  skip_known_bug(
    "all_ctrs_of_type called with a non-normalized type", // https://github.com/hazelgrove/hazel/issues/1626
    {|fun (?: (Float((+ A(Bool))))) -> ""|},
  ),
  skip_known_bug(
    "Type join of ap", // https://github.com/hazelgrove/hazel/issues/1459
    "type x = Int(Float) in let y : x =  1",
  ),
];
