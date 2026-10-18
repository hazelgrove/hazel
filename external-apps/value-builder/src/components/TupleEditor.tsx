import React from "react";
import { ValueNode } from "./ValueNode";

interface TupleEditorProps {
  value: Record<string, unknown>;
  onChange: (newValue: Record<string, unknown>) => void;
  path: string[];
  level: number;
}

export function TupleEditor({
  value,
  onChange,
  path,
  level,
}: TupleEditorProps) {
  const obj = value || {};

  // Get numeric keys in order
  const keys = Object.keys(obj)
    .filter((key) => !isNaN(parseInt(key, 10)))
    .sort((a, b) => parseInt(a, 10) - parseInt(b, 10));

  const addElement = () => {
    const nextIndex = keys.length;
    const newObj = { ...obj, [nextIndex.toString()]: 0 };
    onChange(newObj);
  };

  const removeElement = (index: number) => {
    const newObj = { ...obj };
    delete newObj[index.toString()];

    // Reindex remaining elements
    const reindexed: Record<string, unknown> = {};
    let newIndex = 0;
    keys.forEach((key) => {
      const keyIndex = parseInt(key, 10);
      if (keyIndex !== index) {
        reindexed[newIndex.toString()] = obj[key];
        newIndex++;
      }
    });

    onChange(reindexed);
  };

  const updateElement = (index: number, newValue: unknown) => {
    const newObj = { ...obj, [index.toString()]: newValue };
    onChange(newObj);
  };

  const containerStyle: React.CSSProperties = {
    borderLeft: "1px dashed #8bc34a",
    borderRadius: "4px",
    padding: "8px",
    backgroundColor: "#f1f8e9",
  };

  const headerStyle: React.CSSProperties = {
    display: "flex",
    alignItems: "center",
    gap: "8px",
    marginBottom: "8px",
  };

  const elementContainerStyle: React.CSSProperties = {
    display: "flex",
    alignItems: "flex-start",
    gap: "8px",
    marginBottom: "8px",
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
        <strong> {keys.length}-Tuple </strong>
        <button onClick={addElement} style={buttonStyle}>
          +
        </button>
      </div>

      {keys.length === 0 && (
        <div style={{ color: "#666", fontStyle: "italic", padding: "8px" }}>
          Empty tuple. Click "+" to start.
        </div>
      )}

      {keys.map((key, displayIndex) => {
        const index = parseInt(key, 10);
        return (
          <div key={key} style={elementContainerStyle}>
            <div style={{ display: "flex", gap: "6px" }}>
              {/* <div style={{ fontSize: "12px", color: "#666" }}>
                {displayIndex}:
              </div> */}
              <button
                onClick={() => removeElement(index)}
                style={{
                  ...buttonStyle,
                  backgroundColor: "#0000",
                  borderColor: "#0000",
                  color: "red",
                }}
                title="Remove element"
              >
                ×
              </button>
              <ValueNode
                value={obj[key]}
                onChange={(newValue) => updateElement(index, newValue)}
                path={[...path, `(${displayIndex})`]}
                level={level + 1}
              />
            </div>
          </div>
        );
      })}
    </div>
  );
}
