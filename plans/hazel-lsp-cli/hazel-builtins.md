# Hazel Built-in Functions

This file lists all built-in functions available in Hazel. These serve as Hazel's standard library.

**Important notes:**

- No namespaces: Unlike OCaml (`List.map`), Hazel uses flat names (`map`)
- Reversed argument order for many list functions: `map(list, fn)` not `map(fn, list)`
- String functions use `string_` prefix: `string_length`, `string_concat`, etc.
- Type holes `?` indicate polymorphic parameters (type constraints not currently available)

See [hazel-primer.md](./hazel-primer.md) for usage examples.

---

## Complete List

```
abs:Int -> Int
abs_float:Float -> Float
acos:Float -> Float
all:([?], ? -> Bool) -> Bool
all2:([?], [?], (?, ?) -> Bool) -> Bool
any:([?], ? -> Bool) -> Bool
any2:([?], [?], (?, ?) -> Bool) -> Bool
append:([?], [?]) -> [?]
asin:Float -> Float
assoc:([(?, ?)], ?) -> ?
assoc_opt:([(?, ?)], ?) -> ?
atan:Float -> Float
bool_and:(Bool, Bool) -> Bool
bool_of_float:Float -> Bool
bool_of_int:Int -> Bool
bool_of_nat:Nat -> Bool
bool_of_sint:SInt -> Bool
bool_of_string:String -> Bool
bool_or:(Bool, Bool) -> Bool
ceil:Float -> Float
combine:([?], [?]) -> [(?, ?)]
concat:[[?]] -> [?]
concat_map:([?], ? -> [?]) -> [?]
cons:(?, [?]) -> [?]
contains:([?], ? -> Bool) -> Bool
cos:Float -> Float
drop:([?], Int) -> [?]
drop_while:([?], ? -> Bool) -> [?]
enumerate:[?] -> [(Int, ?)]
epsilon_float:Float
exists:([?], ? -> Bool) -> Bool
exists2:([?], [?], (?, ?) -> Bool) -> Bool
exp:Float -> Float
filter:([?], ? -> Bool) -> [?]
filter_map:([?], ? -> ?) -> [?]
filteri:([?], (Int, ?) -> Bool) -> [?]
find:([?], ? -> Bool) -> ?
find_index:([?], ? -> Bool) -> ?
find_map:([?], ? -> ?) -> ?
find_mapi:([?], (Int, ?) -> ?) -> ?
find_opt:([?], ? -> Bool) -> ?
flat_map:([?], ? -> [?]) -> [?]
flatten:[[?]] -> [?]
float_divide:(Float, Float) -> Float
float_eq:(Float, Float) -> Bool
float_gt:(Float, Float) -> Bool
float_gte:(Float, Float) -> Bool
float_lt:(Float, Float) -> Bool
float_lte:(Float, Float) -> Bool
float_minus:(Float, Float) -> Float
float_mod:(Float, Float) -> Float
float_neq:(Float, Float) -> Bool
float_of_bool:Bool -> Float
float_of_int:Int -> Float
float_of_nat:Nat -> Float
float_of_sint:SInt -> Float
float_of_string:String -> Float
float_plus:(Float, Float) -> Float
float_power:(Float, Float) -> Float
float_times:(Float, Float) -> Float
floor:Float -> Float
fold_left:([?], (?, ?) -> ?, ?) -> ?
fold_left2:([?], [?], (?, ?, ?) -> ?, ?) -> ?
fold_right:([?], (?, ?) -> ?, ?) -> ?
fold_right2:([?], [?], (?, ?, ?) -> ?, ?) -> ?
for_all:([?], ? -> Bool) -> Bool
for_all2:([?], [?], (?, ?) -> Bool) -> Bool
from_lvs:[(label=String, value=?)] -> ?
fst:(?, ?) -> ?
group_by_label:([?], ?) -> ?
hd:[?] -> ?
hd_opt:[?] -> ?
infinity:Float
init:(Int, Int -> ?) -> [?]
int_divide:(Int, Int) -> Int
int_gt:(Int, Int) -> Bool
int_gte:(Int, Int) -> Bool
int_lt:(Int, Int) -> Bool
int_lte:(Int, Int) -> Bool
int_minus:(Int, Int) -> Int
int_mod:(Int, Int) -> Int
int_of_bool:Bool -> Int
int_of_float:Float -> Int
int_of_nat:Nat -> Int
int_of_sint:SInt -> Int
int_of_string:String -> Int
int_plus:(Int, Int) -> Int
int_power:(Int, Int) -> Int
int_times:(Int, Int) -> Int
intersperse:([?], ?) -> [?]
is_empty:[?] -> Bool
is_finite:Float -> Bool
is_infinite:Float -> Bool
is_nan:Float -> Bool
length:[?] -> Int
log:Float -> Float
log10:Float -> Float
map:([?], ? -> ?) -> [?]
map2:([?], [?], (?, ?) -> ?) -> [?]
mapi:([?], (Int, ?) -> ?) -> [?]
max_sint:SInt
mem:([?], ?) -> Bool
mem_assoc:([(?, ?)], ?) -> Bool
min_sint:SInt
monus:(Nat, Nat) -> Nat
nan:Float
nat_divide:(Nat, Nat) -> Nat
nat_gt:(Nat, Nat) -> Bool
nat_gte:(Nat, Nat) -> Bool
nat_lt:(Nat, Nat) -> Bool
nat_lte:(Nat, Nat) -> Bool
nat_mod:(Nat, Nat) -> Nat
nat_of_bool:Bool -> Nat
nat_of_float:Float -> Nat
nat_of_int:Int -> Nat
nat_of_sint:SInt -> Nat
nat_of_string:String -> Nat
nat_plus:(Nat, Nat) -> Nat
nat_power:(Nat, Nat) -> Nat
nat_times:(Nat, Nat) -> Nat
neg_infinity:Float
nth:([?], Int) -> ?
nth_opt:([?], Int) -> ?
omit_all_labels:? -> ?
omit_labels:? -> ?
option_bind:(+ None + Some(?), ? -> ?) -> ?
option_map:(+ None + Some(?), ? -> ?) -> ?
option_to_list:+ None + Some(?) -> [?]
partition:([?], ? -> Bool) -> ([?], [?])
partition_map:([?], ? -> ?) -> ([?], [?])
pi:Float
project_labels:? -> ?
range:(Int, Int) -> [Int]
remove_assoc:([(?, ?)], ?) -> [(?, ?)]
rev:[?] -> [?]
rev_append:([?], [?]) -> [?]
reverse:[?] -> [?]
select_labels:? -> ?
sin:Float -> Float
sint_divide:(SInt, SInt) -> SInt
sint_gt:(SInt, SInt) -> Bool
sint_gte:(SInt, SInt) -> Bool
sint_lt:(SInt, SInt) -> Bool
sint_lte:(SInt, SInt) -> Bool
sint_minus:(SInt, SInt) -> SInt
sint_mod:(SInt, SInt) -> SInt
sint_of_bool:Bool -> SInt
sint_of_float:Float -> SInt
sint_of_int:Int -> SInt
sint_of_nat:Nat -> SInt
sint_of_string:String -> SInt
sint_plus:(SInt, SInt) -> SInt
sint_power:(SInt, SInt) -> SInt
sint_times:(SInt, SInt) -> SInt
slice:(Int, Int, [?]) -> [?]
snd:(?, ?) -> ?
sort:((?, ?) -> + Lt + Eq+ Gt, [?]) -> [?]
split:[(?, ?)] -> ([?], [?])
sqrt:Float -> Float
string_capitalize:String -> String
string_compare:(String, String) -> + Lt + Eq+ Gt
string_concat:(String, String) -> String
string_eq:(String, String) -> Bool
string_escaped:String -> String
string_join:(String, [String]) -> String
string_length:String -> Int
string_lowercase:String -> String
string_match:(String, String) -> Bool
string_of_bool:Bool -> String
string_of_float:Float -> String
string_of_int:Int -> String
string_of_nat:Nat -> String
string_of_sint:SInt -> String
string_replace:(String, String, String) -> String
string_search:(String, String, Int) -> Int
string_split:(String, String) -> [String]
string_sub:(String, Int, Int) -> String
string_trim:String -> String
string_uncapitalize:String -> String
string_unescaped:String -> String
string_uppercase:String -> String
take:([?], Int) -> [?]
take_while:([?], ? -> Bool) -> [?]
tan:Float -> Float
tl:[?] -> [?]
tl_opt:[?] -> ?
to_lvs:? -> [(label=String, value=?)]
unzip:[(?, ?)] -> ([?], [?])
zip:([?], [?]) -> [(?, ?)]
```
