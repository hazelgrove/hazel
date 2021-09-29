let assemble_tile: AltList.t(Shard.t, Term.t) => Tile.t;

/**
 * Ensures tiles and shards of the input frame are
 * sequentially connected (todo: define).
 * This involves
 * - upgrading ambiguously shaped singleton shards
 *   into tiles of known shape
 * - possibly changing ambiguous tile shapes
 * - insert/removing holes as needed
 * as determined by which choices minimize the number
 * of holes in the result.
 *
 * todo: optimize by taking ListFrame.t(Segment.t)
 */
let fix_tips: Segment.frame => Segment.frame;
