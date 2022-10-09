open Term.UExp
let dhexp_to_uexp = (dhexp: DHExp.t, id_gen: Id.t) : (Term.UExp.t, Id.t) => {
    let (id, id_gen) = IdGen.fresh(id_gen);
    switch(dhexp) {
        | IntLit(int) => {
            ({ids: [id], term: Int(int)}, id_gen);
        }
        | FloatLit(float) => {
            ({ids: [id], term: Float(float)}, id_gen);
        }
        | BoolLit(bool) => {
            ({ids: [id], term: Bool(bool)}, id_gen);            
        }
        | Triv => ({ids: [id], term: Triv}, id_gen);
        | _ => failwith("NYI")
    }
}


