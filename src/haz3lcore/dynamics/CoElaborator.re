open Term.UExp
let dhexp_to_uexp = (dhexp: DHExp.t) => {
    switch(dhexp) {
        | IntLit(int) => Int(int)
        | FloatLit(float) => Float(float)
        | BoolLit(boo) => Bool(boo)
        | Triv => Triv
        | _ => failwith("NYI")
        
    }
}
