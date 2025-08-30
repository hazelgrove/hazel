import React, { useState } from "react";
import { ValueNode } from "./ValueNode";

interface LabeledTupleEditorProps {
  value: Record<string, unknown>;
  onChange: (newValue: Record<string, unknown>) => void;
  path: string[];
  level: number;
}

export function LabeledTupleEditor({
  value,
  onChange,
  path,
  level,
}: LabeledTupleEditorProps) {
  const obj = value || {};
  const [newLabelName, setNewLabelName] = useState("");

  // Get non-numeric keys (labeled fields)
  const labels = Object.keys(obj).filter((key) => isNaN(parseInt(key, 10)));

  const addField = () => {
    const trimmedLabel = newLabelName.trim();
    if (!trimmedLabel || labels.includes(trimmedLabel)) {
      return; // Don't add empty or duplicate labels
    }

    const newObj = { ...obj, [trimmedLabel]: 0 };
    onChange(newObj);
    setNewLabelName("");
  };

  const removeField = (label: string) => {
    const newObj = { ...obj };
    delete newObj[label];
    onChange(newObj);
  };

  const updateField = (label: string, newValue: unknown) => {
    const newObj = { ...obj, [label]: newValue };
    onChange(newObj);
  };

  const renameField = (oldLabel: string, newLabel: string) => {
    const trimmedLabel = newLabel.trim();
    if (
      !trimmedLabel ||
      trimmedLabel === oldLabel ||
      labels.includes(trimmedLabel)
    ) {
      return; // Don't rename to empty, same, or duplicate labels
    }

    const newObj = { ...obj };
    newObj[trimmedLabel] = newObj[oldLabel];
    delete newObj[oldLabel];
    onChange(newObj);
  };

  const containerStyle: React.CSSProperties = {
    borderLeft: "1px dashed #ff9800",
    borderRadius: "4px",
    padding: "8px",
    backgroundColor: "#fff3e0",
  };

  const headerStyle: React.CSSProperties = {
    display: "flex",
    alignItems: "center",
    gap: "8px",
    marginBottom: "8px",
  };

  const addFieldStyle: React.CSSProperties = {
    display: "flex",
    alignItems: "center",
    gap: "4px",
    // padding: "4px",
    // backgroundColor: "#ffffff",
    // borderRadius: "3px",
    // border: "1px solid #ddd",
  };

  const fieldContainerStyle: React.CSSProperties = {
    display: "flex",
    alignItems: "center",
    gap: "8px",
    marginBottom: "8px",
  };

  const labelStyle: React.CSSProperties = {
    minWidth: "80px",
    padding: "4px",
    // fontSize: "12px",
    // fontWeight: "bold",
    color: "#333",
  };

  const buttonStyle: React.CSSProperties = {
    padding: "2px 6px",
    // fontSize: "12px",
    border: "1px solid #ccc",
    borderRadius: "2px",
    backgroundColor: "#fff",
    cursor: "pointer",
  };

  const inputStyle: React.CSSProperties = {
    padding: "2px 4px",
    // fontSize: "12px",
    border: "1px solid #ccc",
    borderRadius: "2px",
    color: "black",
  };

  return (
    <div style={containerStyle}>
      <div style={headerStyle}>
        <strong>Labeled {labels.length}-Tuple</strong>
        <div style={addFieldStyle}>
          <input
            type="text"
            value={newLabelName}
            onChange={(e) => setNewLabelName(e.target.value)}
            onKeyPress={(e) => e.key === "Enter" && addField()}
            placeholder="New field name"
            style={inputStyle}
          />
          <button
            onClick={addField}
            disabled={
              !newLabelName.trim() || labels.includes(newLabelName.trim())
            }
            style={buttonStyle}
          >
            +
          </button>
        </div>
      </div>

      {labels.length === 0 && (
        <div style={{ color: "#666", fontStyle: "italic", padding: "8px" }}>
          Empty labeled tuple. Add a field (+) to start.
        </div>
      )}

      {labels.map((label) => (
        <div key={label} style={fieldContainerStyle}>
          <button
            onClick={() => removeField(label)}
            style={{
              ...buttonStyle,
              backgroundColor: "#0000",
              borderColor: "#0000",
              color: "red",
            }}
            title="Remove field"
          >
            ×
          </button>
          <div style={labelStyle}>
            <input
              type="text"
              value={label}
              onChange={(e) => renameField(label, e.target.value)}
              style={{
                ...inputStyle,
                fontWeight: "bold",
                border: "none",
                backgroundColor: "transparent",
              }}
            />
            =
          </div>

          <div style={{ display: "flex" }}>
            <ValueNode
              value={obj[label]}
              onChange={(newValue) => updateField(label, newValue)}
              path={[...path, label]}
              level={level + 1}
            />
          </div>
        </div>
      ))}
    </div>
  );
}
