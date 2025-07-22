import type { HazelDoc as __HazelDoc, FlatPiece } from "./delta";

export function generateHazelDoc(doc: __HazelDoc): HazelDoc {
  const pieceMap = new Map<string, FlatPiece>();
  doc.tiles.forEach((piece) => {
    pieceMap.set(piece.id, piece);
  });

  return {
    title: doc.title,
    pieceMap,
  };
}

export function exportHazelDoc(doc: HazelDoc): __HazelDoc {
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

export type { HazelToParent, ParentToHazel } from "../components/HazelEmbed";