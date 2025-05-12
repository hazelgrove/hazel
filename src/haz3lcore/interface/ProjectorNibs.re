/* Projectors currently are all convex */
let nibs = (_: Base.projector('p)): Nibs.shapes =>
  Nib.Shape.(Convex, Convex);
