type UUID = string;

type Sort = "Exp" | "Pat" | "Typ" | "TPat" | "Rul" | "Any";
type Shape = "Convex" | "Concave";
type NibShape = {t:"Convex"} | {t:"Concave", n:number};

interface Nib {
  shape: NibShape;
  sort: Sort;
}

interface Mold {
  out: Sort;
  in: Sort[];
  nibs: [Nib, Nib];
}

interface Grout {
  readonly t: "Grout";
  readonly id: UUID;
  readonly shape: Shape;
}

interface SecondaryContent {
  readonly t: "Whitespace" | "Comment";
  readonly content: string;
}
interface Secondary {
  readonly t: "Secondary";
  readonly id: UUID;
  readonly content: SecondaryContent;
}
interface Tile {
  readonly t: "Tile";
  readonly id: UUID;
  readonly label: string[];
  readonly mold: Mold;
  readonly shards: number[];
  readonly children: Tile[];
}

export interface FlatTile {
  readonly t: "Tile";
  readonly id: UUID;
  readonly label: string[];
  readonly mold: Mold;
  readonly shards: number[];
  readonly children: UUID[][];
}

export type FlatPiece = FlatTile | Grout | Secondary;

export type HazelDoc = {
    title: string;
    map: Map<UUID, FlatPiece>;
}

// interface DeleteOp {
//   readonly t: "Delete";
//   readonly uuid: UUID;
//   readonly index: number;
// }

// interface InsertOp {
//   readonly t: "Insert";
//   readonly uuid: UUID;
//   readonly index: number;
//   readonly tiles: Tile[];
// }
// interface ReplaceOp extends EditOp {
//   readonly t: "Replace";
//   readonly content: HazelDoc;
// }

// type EditOp = DeleteOp | InsertOp | ReplaceOp;

// export type EditScript = EditOp[];
