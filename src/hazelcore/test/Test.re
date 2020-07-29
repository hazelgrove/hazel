let%test "inline_test" = 12 == 12;

// Statics_Exp.syn(ctx, e) != None && Statics_Exp.syn_fix_holes(ctx, u_gen, e) = (e', _, changed) ==> e === e' && changed == false
// Statics_Exp.syn(ctx, e) == None && Statics_Exp.syn_fix_holes(ctx, u_gen, e) = (e', _, changed) ==> e !== e' && changed == true
// Statics_Exp.ana(ctx, e, ty) != None && Statics_Exp.ana_fix_holes(ctx, u_gen, e, ty) = (e', changed) ==> e === e' && changed == false

// enumerate over all program term constructors
// for each program term getting fixed, we need to test following scenarios:
// - no children changed, term itself didn't change
//   - produce a function of of type ty1 -> ty2, produce an argument of type ty1
// - some child has changed, term itself didn't change
//   - produce a function of type ty1 -> ty2, produce an argument of type ≠ ty1
//   - produce a term not of function type
// - no child has changed, term itself changed
//   - synthesizes a type correctly but started in a hole
//   - started not in hole, synthesizes inconsistent type
//   - 1 2 (nothing in hole) ==> _1_ 2
// - both children and term itself have changed
// - syn vs ana position
// - add vs remove holes

// additional concerns:
// - space cost (but probably should be fine since number of children is small)
// - let contributors know of possible bugs and to keep an eye out (and to prevent wasted time)