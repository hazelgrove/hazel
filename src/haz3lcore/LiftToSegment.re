open Language;

let lift_segment =
    (
      ~settings: ExpToSegment.Settings.t,
      transformation: Exp.t => Exp.t,
      segment: Segment.t,
    )
    : Segment.t => {
  let MakeTerm.{term, term_data, _} = MakeTerm.go(segment);

  let transformed = transformation(term);
  ExpToSegment.exp_to_segment(
    ~override=id => TermData.segment(id, term_data),
    ~settings,
    transformed,
  );
};
