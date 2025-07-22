import React, { useState, useEffect } from "react";
import Tree from "react-d3-tree";
import type { HazelDoc } from "../types/delta";

interface DocGraphProps {
  docState: HazelDoc | null;
}

// Convert Hazel document state to a tree structure for react-d3-tree
const convertDocToTree = (doc: HazelDoc | null) => {
  if (!doc) return { name: "No document loaded" };

  // Start with the root node
  const rootId = findRootNode(doc);
  if (!rootId) return { name: "Cannot determine root node" };

  return buildTreeNode(rootId, doc);
};

// Find the root node of the document (one that is not referenced by any other node)
const findRootNode = (doc: HazelDoc): string | null => {
  if (!doc.tiles.length) return null;
  
  // Collect all referenced IDs
  const referencedIds = new Set<string>();
  
  for (const [_, piece] of doc.tiles.entries()) {
    if (piece.t === "Tile") {
      piece.children.forEach(childArray => {
        childArray.forEach(childId => {
          referencedIds.add(childId);
        });
      });
    }
  }
  
  // Find a node that is not referenced (potential root)
  for (const [_, piece] of doc.tiles.entries()) {
    if (!referencedIds.has(piece.id)) {
      return piece.id;
    }
  }
  
  // If no clear root, just return the first key
  return doc.tiles[0]?.id || null;
};

// Build a tree node recursively
const buildTreeNode = (id: string, doc: HazelDoc): any => {
  const piece = doc.tiles.find(p => p.id === id);
  
  if (!piece) {
    return { name: `Unknown (${id.substring(0, 6)}...)` };
  }
  
  if (piece.t === "Grout") {
    return { 
      name: `Grout (${id.substring(0, 6)}...)`,
      attributes: {
        shape: piece.shape
      }
    };
  }
  
  if (piece.t === "Secondary") {
    return {
      name: `Secondary (${id.substring(0, 6)}...)`,
      attributes: {
        type: piece.content.t,
        content: piece.content.content.substring(0, 15) + (piece.content.content.length > 15 ? "..." : "")
      }
    };
  }
  
  // Must be a Tile
  const children = piece.children.flatMap(childArray => 
    childArray.map(childId => buildTreeNode(childId, doc))
  );
  
  return {
    name: piece.label.join(" ") || `Tile (${id.substring(0, 6)}...)`,
    attributes: {
      id: id.substring(0, 6) + "...",
      mold: `${piece.mold.out} <- ${piece.mold.in.join(", ")}`
    },
    children: children.length > 0 ? children : undefined
  };
};

const DocGraph: React.FC<DocGraphProps> = ({ docState }) => {
  const [treeData, setTreeData] = useState<any>(null);
  
  useEffect(() => {
    setTreeData(convertDocToTree(docState));
  }, [docState]);
  
  if (!treeData) {
    return <div>Loading graph...</div>;
  }
  
  return (
    <div style={{ width: "100%", height: "400px" }}>
      <Tree 
        data={treeData} 
        orientation="vertical"
        pathFunc="step"
        collapsible={true}
        translate={{ x: 150, y: 50 }}
        nodeSize={{ x: 150, y: 100 }}
        separation={{ siblings: 1.5, nonSiblings: 2 }}
        renderCustomNodeElement={(rd3tProps) => (
          <g>
            <circle r={10} fill="lightsteelblue" />
            <text
              dy=".31em"
              x={15}
              textAnchor="start"
              style={{ fontSize: "12px" }}
            >
              {rd3tProps.nodeDatum.name}
            </text>
            {rd3tProps.nodeDatum.attributes && (
              <text
                dy="1.31em"
                x={15}
                textAnchor="start"
                style={{ fontSize: "10px", fill: "#666" }}
              >
                {Object.entries(rd3tProps.nodeDatum.attributes).map(
                  ([key, value], i) => `${key}: ${value}`
                ).join(", ")}
              </text>
            )}
          </g>
        )}
      />
    </div>
  );
};

export default DocGraph;
