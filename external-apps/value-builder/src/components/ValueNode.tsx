import React from "react";
import { BaseTypeEditor } from "./BaseTypeEditor";
import { ListEditor } from "./ListEditor";
import { TupleEditor } from "./TupleEditor";
import { LabeledTupleEditor } from "./LabeledTupleEditor";
import { ADTEditor } from "./ADTEditor";

export type ValueType =
  | "int"
  | "float"
  | "string"
  | "bool"
  | "list"
  | "tuple"
  | "labeled_tuple"
  | "adt";

export interface ValueNodeProps {
  value: unknown;
  onChange: (newValue: unknown) => void;
  path?: string[];
  level?: number;
}

/**
 * Detects the type of a value based on its structure
 */
export function detectValueType(value: unknown): ValueType {
  if (typeof value === "number") {
    return Number.isInteger(value) ? "int" : "float";
  }
  if (typeof value === "string") return "string";
  if (typeof value === "boolean") return "bool";
  if (Array.isArray(value)) return "list";

  if (value && typeof value === "object") {
    const obj = value as Record<string, unknown>;

    // Check for ADT pattern: { "t": "Constructor", "v"?: value }
    if ("t" in obj && typeof obj.t === "string") {
      return "adt";
    }

    // Check if it's a plain tuple (all keys are numeric strings)
    const keys = Object.keys(obj);
    const allNumeric = keys.every((key) => {
      const num = parseInt(key, 10);
      return !isNaN(num) && num.toString() === key;
    });

    if (allNumeric) return "tuple";

    // Otherwise it's a labeled tuple
    return "labeled_tuple";
  }

  return "int"; // Default fallback
}

/**
 * Main compositional value editor component
 */
export function ValueNode({
  value,
  onChange,
  path = [],
  level = 0,
}: ValueNodeProps) {
  const valueType = detectValueType(value);

  const handleTypeChange = (newType: ValueType) => {
    // Convert value to new type with sensible defaults
    let newValue: unknown;

    switch (newType) {
      case "int":
        newValue = 0;
        break;
      case "float":
        newValue = 0.0;
        break;
      case "string":
        newValue = "";
        break;
      case "bool":
        newValue = false;
        break;
      case "list":
        newValue = [];
        break;
      case "tuple":
        newValue = { "0": 0, "1": 0 }; // Default 2-tuple
        break;
      case "labeled_tuple":
        newValue = { label1: 0, label2: 0 }; // Default 2-tuple
        break;
      case "adt":
        newValue = { t: "None" }; // Nullary constructor
        break;
      default:
        newValue = value;
    }

    onChange(newValue);
  };

  const nodeStyle: React.CSSProperties = {
    // marginLeft: `${indent}px`,
    // marginLeft: "4px",
    // border: "1px solid #e0e0e0",
    borderLeft: "1px solid rgb(166 166 166)",
    borderRadius: "4px",
    padding: "8px",
    display: "flex",
    alignItems: "flex-start",
    width: "fit-content",
    gap: "8px",
    backgroundColor: level % 2 === 0 ? "#fafafa" : "#ffffff",
  };

  const headerStyle: React.CSSProperties = {
    display: "flex",
    alignItems: "center",
    gap: "8px",
    // marginBottom: "8px",
    fontSize: "14px",
    fontWeight: "bold",
    color: "#333",
  };

  return (
    <div style={nodeStyle}>
      <div style={headerStyle}>
        {/* <span>Type</span> */}
        <select
          value={valueType}
          onChange={(e) => handleTypeChange(e.target.value as ValueType)}
          style={{ padding: "2px 4px", borderRadius: "2px" }}
        >
          <option value="int">Integer</option>
          <option value="float">Float</option>
          <option value="string">String</option>
          <option value="bool">Boolean</option>
          <option value="list">List</option>
          <option value="tuple">Tuple</option>
          <option value="labeled_tuple">Labeled</option>
          <option value="adt">ADT</option>
        </select>
        {/* {path.length > 0 && (
          <span style={{ fontSize: "12px", color: "#666" }}>
            Path: {path.join(" → ")}
          </span>
        )} */}
      </div>

      {(valueType === "int" ||
        valueType === "float" ||
        valueType === "string" ||
        valueType === "bool") && (
        <BaseTypeEditor value={value} type={valueType} onChange={onChange} />
      )}

      {valueType === "list" && (
        <ListEditor
          value={value as unknown[]}
          onChange={onChange}
          path={path}
          level={level}
        />
      )}

      {valueType === "tuple" && (
        <TupleEditor
          value={value as Record<string, unknown>}
          onChange={onChange}
          path={path}
          level={level}
        />
      )}

      {valueType === "labeled_tuple" && (
        <LabeledTupleEditor
          value={value as Record<string, unknown>}
          onChange={onChange}
          path={path}
          level={level}
        />
      )}

      {valueType === "adt" && (
        <ADTEditor
          value={value as { t: string; v?: unknown }}
          onChange={onChange}
          path={path}
          level={level}
        />
      )}
    </div>
  );
}
