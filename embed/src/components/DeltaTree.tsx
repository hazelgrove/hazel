import React, { useState, useEffect, useRef } from "react";
import Tree from "react-d3-tree";
import type { EditScript, Tile, InsertOp, DeleteOp } from "../types/delta";

// Core data structures for maintaining the AST state
interface TreeNode {
  id: string;
  type: string;
  label: string;
  properties: Record<string, any>;
  children: string[]; // IDs of child nodes
}

interface ASTState {
  nodesById: Map<string, TreeNode>;
  parentMap: Map<string, string | null>; // Maps node ID to parent ID (null for root nodes)
}

// Interface for visualization tree format
interface D3TreeNode {
  name: string;
  id: string;
  attributes?: Record<string, any>;
  children: D3TreeNode[];
}

interface DeltaTreeProps {
  delta: EditScript;
}

const DeltaTree: React.FC<DeltaTreeProps> = ({ delta }) => {
  // Use ref to persist AST state between renders
  const astStateRef = useRef<ASTState>({
    nodesById: new Map(),
    parentMap: new Map(),
  });

  const [treeData, setTreeData] = useState<D3TreeNode[]>([]);

  // Initialize or access the current AST state
  const getASTState = (): ASTState => astStateRef.current;

  // Process a single tile and add it to the AST state
  const processTile = (
    tile: Tile,
    parentId: string | null = null,
    state: ASTState = getASTState(),
  ): void => {
    // Create a TreeNode from the Tile
    const node: TreeNode = {
      id: tile.id,
      type: "Tile",
      label: tile.label.join(" "),
      properties: {
        mold: tile.mold,
        shards: tile.shards,
      },
      children: [],
    };

    // Add the node to our nodes map
    state.nodesById.set(tile.id, node);

    // Set the parent relationship
    state.parentMap.set(tile.id, parentId);

    // Process all children recursively
    if (tile.children && tile.children.length > 0) {
      tile.children.forEach((childTile) => {
        processTile(childTile, tile.id, state);
        // Add child ID to parent's children array
        node.children.push(childTile.id);
      });
    }
  };

  // Process an insert operation
  const processInsertOp = (
    op: InsertOp,
    state: ASTState = getASTState(),
  ): void => {
    const { uuid: parentId, index, tiles } = op;

    // Get the parent node
    const parentNode = state.nodesById.get(parentId);

    if (!parentNode) {
      // If there's no parent, these must be root tiles
      tiles.forEach((tile) => processTile(tile, null, state));
      return;
    }

    // Process each tile to add to our AST
    tiles.forEach((tile, i) => {
      processTile(tile, parentId, state);

      // Insert the tile ID into the parent's children array at the specified index
      const insertPosition = index + i;
      if (insertPosition < parentNode.children.length) {
        parentNode.children.splice(insertPosition, 0, tile.id);
      } else {
        parentNode.children.push(tile.id);
      }
    });
  };

  // Process a delete operation
  const processDeleteOp = (
    op: DeleteOp,
    state: ASTState = getASTState(),
  ): void => {
    const { uuid: parentId, index } = op;

    // Get the parent node
    const parentNode = state.nodesById.get(parentId);
    if (!parentNode || index >= parentNode.children.length) return;

    // Get the ID of the child to remove
    const childId = parentNode.children[index];

    // Remove the child from the parent's children array
    parentNode.children.splice(index, 1);

    // Recursively remove the child and all its descendants
    const removeNodeAndChildren = (nodeId: string): void => {
      const node = state.nodesById.get(nodeId);
      if (!node) return;

      // First, recursively remove all children
      [...node.children].forEach((childId) => {
        removeNodeAndChildren(childId);
      });

      // Then, remove this node from our state
      state.nodesById.delete(nodeId);
      state.parentMap.delete(nodeId);
    };

    removeNodeAndChildren(childId);
  };

  // Apply all operations in a delta to update the AST state
  const applyDelta = (
    delta: EditScript,
    state: ASTState = getASTState(),
  ): void => {
    delta.forEach((op) => {
      if (op.t === "Insert") {
        processInsertOp(op as InsertOp, state);
      } else if (op.t === "Delete") {
        processDeleteOp(op as DeleteOp, state);
      }
    });
  };

  // Find all root nodes (nodes with no parent)
  const findRootNodes = (state: ASTState = getASTState()): string[] => {
    const roots: string[] = [];

    state.parentMap.forEach((parentId, nodeId) => {
      if (parentId === null) {
        roots.push(nodeId);
      }
    });

    return roots;
  };

  // Convert the AST state to a format suitable for react-d3-tree
  const convertToD3TreeFormat = (
    state: ASTState = getASTState(),
  ): D3TreeNode[] => {
    const rootIds = findRootNodes(state);

    // If no roots found, check if we have any nodes at all
    if (rootIds.length === 0) {
      // If we have nodes but no roots, create a virtual root
      if (state.nodesById.size > 0) {
        const allNodes = Array.from(state.nodesById.entries());

        // Create a virtual root node with all existing nodes as children
        const virtualRoot: D3TreeNode = {
          id: "virtual-root",
          name: "AST Root",
          attributes: { type: "Root" },
          children: [],
        };

        // Add all nodes as children of the virtual root
        allNodes.forEach(([nodeId, node]) => {
          const childNode = {
            id: node.id,
            name: node.label || node.id,
            attributes: {
              type: node.type,
              ...node.properties,
            },
            children: [] as D3TreeNode[],
          };

          // Add this node's children
          node.children.forEach((childId) => {
            const childData = state.nodesById.get(childId);
            if (childData) {
              childNode.children.push({
                id: childData.id,
                name: childData.label || childData.id,
                attributes: {
                  type: childData.type,
                  ...childData.properties,
                },
                children: [],
              });
            }
          });

          virtualRoot.children.push(childNode);
        });

        return [virtualRoot];
      }

      // If truly empty, return empty tree indicator
      return [
        {
          id: "empty",
          name: "Empty AST",
          children: [],
        },
      ];
    }

    // Helper function to recursively build the tree
    const buildTreeNode = (nodeId: string): D3TreeNode | null => {
      const node = state.nodesById.get(nodeId);
      if (!node) return null;

      const treeNode: D3TreeNode = {
        id: node.id,
        name: node.label || node.id,
        attributes: {
          type: node.type,
          ...node.properties,
        },
        children: [],
      };

      // Recursively add children
      node.children.forEach((childId) => {
        const childNode = buildTreeNode(childId);
        if (childNode) {
          treeNode.children.push(childNode);
        }
      });

      return treeNode;
    };

    // Build the tree from root nodes
    const result = rootIds
      .map((rootId) => buildTreeNode(rootId))
      .filter((node): node is D3TreeNode => node !== null);

    // If we have multiple root nodes, create a virtual root to contain them all
    if (result.length > 1) {
      return [
        {
          id: "virtual-root",
          name: "AST Root",
          attributes: { type: "Root" },
          children: result,
        },
      ];
    }

    return result;
  };

  // Custom node component that changes color based on node type
  const CustomNode: React.FC<{ nodeDatum: any }> = ({ nodeDatum }) => {
    // Determine color based on node type/attributes
    const getNodeColor = () => {
      const nodeType = nodeDatum.attributes?.type;

      switch (nodeType) {
        case "Tile":
          return "#6a9ddf"; // Blue for tiles
        case "Insert":
          return "#7bc47f"; // Green for inserts
        case "Delete":
          return "#e37575"; // Red for deletes
        default:
          return "#aaaaaa"; // Gray for unknown
      }
    };

    const bgColor = getNodeColor();

    return (
      <g>
        <rect
          width={nodeDatum.name.length * 8 + 20}
          height="30"
          x="-10"
          y="-15"
          rx="5"
          fill={bgColor}
          stroke="#333"
        />
        <text fill="#333" strokeWidth="0" dy=".35em" textAnchor="middle">
          {nodeDatum.name}
        </text>
      </g>
    );
  };

  // Process incoming deltas and update the visualization
  useEffect(() => {
    if (delta && delta.length > 0) {
      // Apply the delta to update our AST state
      applyDelta(delta);

      // Convert the updated AST to a format for visualization
      const newTreeData = convertToD3TreeFormat();

      // Update the visualization tree
      setTreeData(newTreeData);

      // Debug info
      console.log("Delta applied:", delta);
      console.log("AST nodes:", astStateRef.current.nodesById.size);
      console.log(
        "AST node IDs:",
        Array.from(astStateRef.current.nodesById.keys()),
      );
      console.log("Root nodes:", findRootNodes());
      console.log("Tree data:", JSON.stringify(newTreeData, null, 2));
    }
  }, [delta]);

  return (
    <div className="delta-tree">
      {treeData.length > 0 ? (
        <Tree
          data={treeData}
          orientation="vertical"
          pathFunc="step"
          nodeSize={{ x: 200, y: 100 }}
          translate={{ x: 150, y: 50 }}
          renderCustomNodeElement={CustomNode}
          collapsible={true}
          initialDepth={5} // Expand the full tree initially
          zoomable={true}
          zoom={0.6}
          draggable={true}
          separation={{ siblings: 2, nonSiblings: 2.5 }}
          transitionDuration={0} // Disable animations for better performance
          depthFactor={120} // Increase vertical spacing
          shouldCollapseNeighborNodes={false}
        />
      ) : (
        <div className="empty-tree">Initializing AST...</div>
      )}
    </div>
  );
};

export default DeltaTree;
