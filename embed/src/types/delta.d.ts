type UUID = string;

type Sort = "Exp";
type Shape = "Convex";

interface Nib {
  shape: Shape;
  sort: Sort;
}

interface Mold {
  out: Sort;
  in: Sort[];
  nibs: [Nib, Nib];
}

interface Tile {
  readonly t: "Tile";
  readonly id: UUID;
  readonly label: string[];
  readonly mold: Mold;
  readonly shards: number[];
  readonly children: Tile[];
}

interface DeleteOp {
  readonly t: "Delete";
  readonly uuid: UUID;
  readonly index: number;
}

interface InsertOp {
  readonly t: "Insert";
  readonly uuid: UUID;
  readonly index: number;
  readonly tiles: Tile[];
}

type EditOp = DeleteOp | InsertOp;
export type EditScript = EditOp[];
