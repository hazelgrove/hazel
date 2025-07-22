import type { HazelDoc as InternalHazelDoc, FlatPiece } from "./delta";

export function generateHazelDoc(doc: InternalHazelDoc): HazelDoc {
  const pieceMap = new Map<string, FlatPiece>();
  doc.tiles.forEach((piece) => {
    pieceMap.set(piece.id, piece);
  });

  return {
    title: doc.title,
    pieceMap,
  };
}

export function exportHazelDoc(doc: HazelDoc): InternalHazelDoc {
  const tiles: FlatPiece[] = Array.from(doc.pieceMap.values());
  return {
    title: doc.title,
    tiles,
  };
}

export type HazelDoc = {
    title: string;
    pieceMap: Map<string, FlatPiece>;
}