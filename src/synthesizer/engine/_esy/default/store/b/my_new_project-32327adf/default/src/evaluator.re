
open Types;

// Takes an expression and returns a corresponding result by
// Evaluating the expression until a hole is in applciation position
// This process is completely deterministic

// Take in an environment E and a expression e, and change the expression to
// a result. Evaluator.eval(E, e) 
// e = () => r = ()
// e = (\x.x) () => r = ()
// e = (\x.??) () => (E, x -> ()) ??
// e = 6 + 11 => 17
// e = 6 + ?? => 6 + [E] ??

let rec eval = (_env:environment, e:exp):res => {
    switch (e) {
        | Hole(x) => Rhole(x, _env)
        | Var(x) => Tools.lookup(x, _env)
        | Function(id, exp) => Rfunc(id, exp, _env)
        | Application(e1, e2) => {
            switch (e1) {
                | Function(id, exp) => eval([(id, eval(_env, e2)), ..._env], exp)
                | _ => Rapp(eval(_env, e1), eval(_env, e2))//This line seems fishy to me.
            }
        }
        | Unit => Runit 
        | Pair(e1, e2) => Rpair(eval(_env, e1), eval(_env, e2))
        | Fst(e1) => Rfst(eval(_env, e1))
        | Snd(e1) => Rsnd(eval(_env, e1))
        | Int(x) => Rint(x)
        | Float(f) => Rfloat(f)
        | Bool(b) => Rbool(b)
        | Cons(e1, e2) => Rcons(eval(_env, e1), eval(_env, e2))
        | Nil => Rnil 
    }
};

