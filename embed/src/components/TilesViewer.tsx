import React, { useState, useImperativeHandle, forwardRef } from "react";
import type { Tile, EditOp } from "../types/delta";

export interface TilesViewerRef {
  processOp: (op: EditOp) => void;
}

const TilesViewer = forwardRef<TilesViewerRef, {}>((props, ref) => {
  const [tiles, setTiles] = useState<Map<string, Tile>>(new Map());

  // Processing logic for edit operations
  const processOp = (op: EditOp) => {
    setTiles((prevTiles) => {
      const newTiles = new Map(prevTiles);

      switch (op.t) {
        case "Insert": {
          const newTile: Tile = {
            t: "Tile",
            id: op.uuid,
            label: [],
            mold: {
              out: "Exp",
              in: ["Exp"],
              nibs: [
                { shape: "Convex", sort: "Exp" },
                { shape: "Convex", sort: "Exp" },
              ],
            },
            shards: [],
            children: op.tiles,
          };

          // add tiles recursively
          const addTile = (tile: Tile) => {
            newTiles.set(tile.id, tile);
            tile.children.forEach((child) => {
              addTile(child);
            });
          };

          addTile(newTile);
          break;
        }
        case "Delete": {
          // delete self and children recursively
          const tileToDelete = newTiles.get(op.uuid);
          if (!tileToDelete) {
            console.warn(`Tile with UUID ${op.uuid} not found for deletion`);
            return newTiles; // Return unchanged if tile not found
          }

          const deleteTile = (tile: Tile) => {
            tile.children.forEach((child) => {
              deleteTile(child);
            });
            newTiles.delete(tile.id);
          };

          deleteTile(tileToDelete);
          break;
        }
        default: {
          const exhaustiveCheck: never = op;
          console.warn(`Unknown edit operation type: ${(op as any).t}`);
        }
      }

      return newTiles;
    });
  };

  // Expose the processOp function via ref
  useImperativeHandle(ref, () => ({
    processOp,
  }));

  // Rendering logic
  return (
    <div className="delta-tree-container">
      <h3>Tiles</h3>
      <ul>
        {Array.from(tiles.entries()).map(([id, tile]) => (
          <li key={id}>
            <strong>{tile.label.join(", ")}</strong> (ID: {tile.id})
            <ul>
              {tile.children.map((child) => (
                <li key={child.id}>{child.label.join(", ")}</li>
              ))}
            </ul>
          </li>
        ))}
      </ul>
    </div>
  );
});

export default TilesViewer;
