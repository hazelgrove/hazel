type t = {
  current_shard: Shard.t,
  rest_of_tile: AltListFrame.aframe(Shard.t, Term.t),
  rest_of_subj: Segment.frame,
};