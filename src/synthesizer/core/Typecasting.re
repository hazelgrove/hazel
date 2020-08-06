open Types;

//----------------------------------------------------------------------
//                     Typecasting Functions
//----------------------------------------------------------------------

let rec intToNum = i =>
  if (i <= 0) {
    Ctor(0, Num, Unit);
  } else {
    Ctor(1, Num, intToNum(i - 1));
  };

let rec valToExp = (v: value): exp => {
  switch (v) {
  | Vunit => Unit
  | Vint(x) => Int(x)
  | Vbool(x) => Bool(x)
  | Vpair(v1, v2) => Pair(valToExp(v1), valToExp(v2))
  | Vctor(id, adt, v') => Ctor(id, adt, valToExp(v'))
  };
};

let rec valToRes = (v: value): res => {
  switch (v) {
  | Vunit => Runit
  | Vint(x) => Rint(x)
  | Vbool(x) => Rbool(x)
  | Vpair(v1, v2) => Rpair(valToRes(v1), valToRes(v2))
  | Vctor(id, adt, v') => Rctor(id, adt, valToRes(v'))
  };
};

let rec exToExp = (ex: example): option(exp) => {
  switch (ex) {
  | Epair(ex1, ex2) =>
    switch (exToExp(ex1), exToExp(ex2)) {
    | (Some(x), Some(y)) => Some(Pair(x, y))
    | _ => None
    }
  | Eunit => Some(Unit)
  | Ector(id, adt, ex1) =>
    switch (exToExp(ex1)) {
    | None => None
    | Some(exp) => Some(Ctor(id, adt, exp))
    }
  | _ => None
  };
};

let rec expToEx = (e: exp): option(example) => {
  switch (e) {
  | Int(x) => Some(Eint(x))
  | Bool(b) => Some(Ebool(b))
  | Pair(e1, e2) =>
    switch (expToEx(e1), expToEx(e2)) {
    | (Some(ex1), Some(ex2)) => Some(Epair(ex1, ex2))
    | _ => None
    }
  | _ => None
  };
};

let rec resToVal = (res: res): option(value) => {
  switch (res) {
  | Rint(x) => Some(Vint(x))
  | Rbool(x) => Some(Vbool(x))
  | Runit => Some(Vunit)
  | Rpair(r1, r2) =>
    switch (resToVal(r1), resToVal(r2)) {
    | (Some(x), Some(y)) => Some(Vpair(x, y))
    | _ => None
    }
  | Rapp(r1, r2) =>
    switch (r1) {
    | Rfunc(name, id, _, e, env) =>
      Eval.eval([(name, r1), (id, r2), ...env], e) |> resToVal
    | _ => None
    }
  | Rctor(id, adt, r) =>
    switch (resToVal(r)) {
    | None => None
    | Some(v) => Some(Vctor(id, adt, v))
    }
  | _ => None
  };
}

// is res -> val possible?
and castable = (res: res): bool =>
  switch (resToVal(res)) {
  | Some(_) => true
  | None => false
  };
