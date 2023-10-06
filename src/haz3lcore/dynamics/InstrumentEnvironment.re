type t = list(Instrument.t);

let equal = (t1, t2) => {
  List.equal(Instrument.equal, t1, t2);
};
