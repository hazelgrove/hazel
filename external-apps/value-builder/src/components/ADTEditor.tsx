import React from "react";
import { ValueNode } from "./ValueNode";

interface ADTEditorProps {
  value: { t: string; v?: unknown };
  onChange: (newValue: { t: string; v?: unknown }) => void;
  path: string[];
  level: number;
}

// Common ADT constructors for quick selection
const COMMON_CONSTRUCTORS = [
  "None",
  "Some",
  "Left",
  "Right",
  "Ok",
  "Error",
  "True",
  "False",
  "Empty",
  "Cons",
];

export function ADTEditor({ value, onChange, path, level }: ADTEditorProps) {
  const adt = value || { t: "None" };
  const hasPayload = "v" in adt;

  const changeConstructor = (newConstructor: string) => {
    if (hasPayload) {
      onChange({ t: newConstructor, v: adt.v });
    } else {
      onChange({ t: newConstructor });
    }
  };

  const togglePayload = () => {
    if (hasPayload) {
      // Remove payload
      const newAdt: { t: string } = { t: adt.t };
      onChange(newAdt);
    } else {
      // Add payload
      onChange({ t: adt.t, v: 0 });
    }
  };

  const updatePayload = (newValue: unknown) => {
    onChange({ t: adt.t, v: newValue });
  };

  const updateConstructorName = (newName: string) => {
    const trimmedName = newName.trim();
    if (trimmedName) {
      if (hasPayload) {
        onChange({ t: trimmedName, v: adt.v });
      } else {
        onChange({ t: trimmedName });
      }
    }
  };

  const containerStyle: React.CSSProperties = {
    border: "1px dashed #9c27b0",
    borderRadius: "4px",
    padding: "8px",
    backgroundColor: "#f3e5f5",
  };

  const headerStyle: React.CSSProperties = {
    display: "flex",
    alignItems: "center",
    gap: "8px",
    marginBottom: "8px",
  };

  const constructorStyle: React.CSSProperties = {
    display: "flex",
    alignItems: "center",
    gap: "8px",
    marginBottom: hasPayload ? "8px" : "0px",
  };

  const buttonStyle: React.CSSProperties = {
    padding: "2px 6px",
    fontSize: "12px",
    border: "1px solid #ccc",
    borderRadius: "2px",
    backgroundColor: "#fff",
    cursor: "pointer",
  };

  const inputStyle: React.CSSProperties = {
    padding: "4px 6px",
    fontSize: "14px",
    border: "1px solid #ccc",
    borderRadius: "2px",
    fontWeight: "bold",
  };

  const selectStyle: React.CSSProperties = {
    padding: "4px 6px",
    fontSize: "14px",
    border: "1px solid #ccc",
    borderRadius: "2px",
  };

  return (
    <div style={containerStyle}>
      <div style={headerStyle}>
        <strong>ADT Constructor</strong>
        <button
          onClick={togglePayload}
          style={{
            ...buttonStyle,
            backgroundColor: hasPayload ? "#ffecb3" : "#e8f5e8",
          }}
        >
          {hasPayload ? "Remove Payload" : "Add Payload"}
        </button>
      </div>

      <div style={constructorStyle}>
        <span style={{ fontSize: "14px", fontWeight: "bold" }}>
          Constructor:
        </span>

        <select
          value={COMMON_CONSTRUCTORS.includes(adt.t) ? adt.t : "custom"}
          onChange={(e) => {
            if (e.target.value !== "custom") {
              changeConstructor(e.target.value);
            }
          }}
          style={selectStyle}
        >
          {COMMON_CONSTRUCTORS.map((constructor) => (
            <option key={constructor} value={constructor}>
              {constructor}
            </option>
          ))}
          <option value="custom">Custom...</option>
        </select>

        {(!COMMON_CONSTRUCTORS.includes(adt.t) ||
          (COMMON_CONSTRUCTORS.includes(adt.t) && adt.t !== "None")) && (
          <input
            type="text"
            value={adt.t}
            onChange={(e) => updateConstructorName(e.target.value)}
            style={inputStyle}
            placeholder="Constructor name"
          />
        )}
      </div>

      {hasPayload && (
        <div>
          <div style={{ fontSize: "12px", color: "#666", marginBottom: "4px" }}>
            Payload:
          </div>
          <ValueNode
            value={adt.v}
            onChange={updatePayload}
            path={[...path, `${adt.t}()`]}
            level={level + 1}
          />
        </div>
      )}

      <div style={{ fontSize: "11px", color: "#666", marginTop: "4px" }}>
        JSON: {JSON.stringify(adt)}
      </div>
    </div>
  );
}
