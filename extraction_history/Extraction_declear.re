/* This is a decleration of the inner types used in the extraction
   most base but useless code can be directly re-used in the old version
       - Note that opseq had been rewrite to a top-level expression, need to re-write indeed.

   TODO: things in order need to be done
       1. declear the inner type used and functions
       2. declear the vocabulary_set functions
       3. include the error type and pass rules*/

type pass_t =
  | HOLE //hole indicates there's an incomplete hole, like conflict, will throw hole error
  | Bool
  | Number
  | Unit
  | List(pass_t)
  | ARROW(pass_t, pass_t) //ARROW(a, b) = a -> b
  | SUM(pass_t, pass_t)
  | PROD(pass_t, pass_t)
  //| Pair(pass_t, pass_t)
  | EMPTY //as None, or not a Type
  // I think EMPTY should never appear since everything have a type
  | UNK // dependency type like list(a), or can trans to whatever
  // Strings should be UNK type
  // Or let expression should have UNK, meaning "not applied"
  | CONFLICT; //it's an error, only errors like gradual types

//BASE CASE = (Some(""), UNK)

// the return type of most function
// (extracted string, the passing type)
// FIXME: if pass_t is CONFLICT, EMPTY, HOLE... string may not exactly be None
//        need a further check maybe at last
type extract_t = (option(string), pass_t);

// the variable environment
// from top level to down levels, each level insert new ones
// (name, type), if local bind, override as rules
type variable_set_t = list((string, pass_t));
