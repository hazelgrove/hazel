import React from "react";
import { ValueNode } from "./ValueNode";

interface ListEditorProps {
  value: unknown[];
  onChange: (newValue: unknown[]) => void;
  path: string[];
  level: number;
}

export function ListEditor({ value, onChange, path, level }: ListEditorProps) {
  const list = Array.isArray(value) ? value : [];

  const addItem = () => {
    const newList = [...list, 0]; // Default to integer
    onChange(newList);
  };

  const removeItem = (index: number) => {
    const newList = list.filter((_, i) => i !== index);
    onChange(newList);
  };

  const updateItem = (index: number, newValue: unknown) => {
    const newList = [...list];
    newList[index] = newValue;
    onChange(newList);
  };

  const moveItem = (fromIndex: number, direction: "up" | "down") => {
    const toIndex = direction === "up" ? fromIndex - 1 : fromIndex + 1;
    if (toIndex < 0 || toIndex >= list.length) return;

    const newList = [...list];
    [newList[fromIndex], newList[toIndex]] = [
      newList[toIndex],
      newList[fromIndex],
    ];
    onChange(newList);
  };

  const containerStyle: React.CSSProperties = {
    border: "1px dashed #ccc",
    borderRadius: "4px",
    padding: "8px",
    backgroundColor: "#f9f9f9",
  };

  const headerStyle: React.CSSProperties = {
    display: "flex",
    alignItems: "center",
    gap: "8px",
    marginBottom: "8px",
  };

  const itemContainerStyle: React.CSSProperties = {
    display: "flex",
    alignItems: "flex-start",
    gap: "8px",
    marginBottom: "8px",
  };

  const controlsStyle: React.CSSProperties = {
    display: "flex",
    flexDirection: "column",
    gap: "2px",
  };

  const buttonStyle: React.CSSProperties = {
    padding: "2px 6px",
    fontSize: "12px",
    border: "1px solid #ccc",
    borderRadius: "2px",
    backgroundColor: "#fff",
    cursor: "pointer",
  };

  return (
    <div style={containerStyle}>
      <div style={headerStyle}>
        <strong>List ({list.length} items)</strong>
        <button onClick={addItem} style={buttonStyle}>
          + Add Item
        </button>
      </div>

      {list.length === 0 && (
        <div style={{ color: "#666", fontStyle: "italic", padding: "8px" }}>
          Empty list. Click "Add Item" to start.
        </div>
      )}

      {list.map((item, index) => (
        <div key={index} style={itemContainerStyle}>
          <div style={{ flex: 1 }}>
            <div
              style={{ fontSize: "12px", color: "#666", marginBottom: "4px" }}
            >
              [{index}]:
            </div>
            <ValueNode
              value={item}
              onChange={(newValue) => updateItem(index, newValue)}
              path={[...path, `[${index}]`]}
              level={level + 1}
            />
          </div>

          <div style={controlsStyle}>
            <button
              onClick={() => moveItem(index, "up")}
              disabled={index === 0}
              style={buttonStyle}
              title="Move up"
            >
              ↑
            </button>
            <button
              onClick={() => moveItem(index, "down")}
              disabled={index === list.length - 1}
              style={buttonStyle}
              title="Move down"
            >
              ↓
            </button>
            <button
              onClick={() => removeItem(index)}
              style={{
                ...buttonStyle,
                backgroundColor: "#ffebee",
                borderColor: "#f44336",
              }}
              title="Remove"
            >
              ×
            </button>
          </div>
        </div>
      ))}
    </div>
  );
}
