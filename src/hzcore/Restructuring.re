type t = {
  backpack: (Segment.t, Segment.frame),
  rest_of_subj: ListFrame.t(Either.t(Piece.t, Segment.t)),
};