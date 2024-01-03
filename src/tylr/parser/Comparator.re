let lt = (l: Bound.t(Molded.Label.t), r: Molded.Label.t) => {
  let (l, r) = Molded.Sym.(Bound.map(t, l), t(r));
  l |> walk(R) |> Set.neq(r);
};

let gt = (l: Molded.Label.t, r: Bound.t(Molded.Label.t)) => {
  let (l, r) = Molded.Sym.(t(l), Bound.map(t, r));
  r |> walk(L) |> Set.neq(l);
};

// todo: tidy up from parameter
let eq = (~from=Dir.L, l: Molded.Label.t, r: Molded.Label.t) => {
  let (l, r) = Molded.Sym.(t(l), t(r));
  let (m_from, m_onto) = Dir.choose(from, l, r);
  Bound.Node(m_from) |> walk(Dir.toggle(from)) |> Set.eq(m_onto);
};
