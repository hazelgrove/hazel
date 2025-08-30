import React from "react";

interface BaseTypeEditorProps {
  value: unknown;
  type: "int" | "float" | "string" | "bool";
  onChange: (newValue: unknown) => void;
}

export function BaseTypeEditor({ value, type, onChange }: BaseTypeEditorProps) {
  const handleInputChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    const inputValue = event.target.value;

    switch (type) {
      case "int":
        const intValue = parseInt(inputValue, 10);
        onChange(isNaN(intValue) ? 0 : intValue);
        break;
      case "float":
        const floatValue = parseFloat(inputValue);
        onChange(isNaN(floatValue) ? 0.0 : floatValue);
        break;
      case "string":
        onChange(inputValue);
        break;
      case "bool":
        onChange(event.target.checked);
        break;
    }
  };

  const containerStyle: React.CSSProperties = {
    // padding: "4px 0",
    // display: "flex",
  };

  const inputStyle: React.CSSProperties = {
    padding: "2.5px 8px",
    border: "1px solid #ccc",
    borderRadius: "3px",
    fontSize: "14px",
    backgroundColor: "grey",
    width: type === "string" ? "200px" : "100px",
  };

  if (type === "bool") {
    return (
      <div style={containerStyle}>
        <label style={{ display: "flex", alignItems: "center", gap: "8px" }}>
          <input
            type="checkbox"
            checked={Boolean(value)}
            onChange={handleInputChange}
            style={{ transform: "scale(1.2)" }}
          />
          {/* <span>{Boolean(value) ? "true" : "false"}</span> */}
        </label>
      </div>
    );
  }

  return (
    <div style={containerStyle}>
      <input
        type={type === "int" || type === "float" ? "number" : "text"}
        step={type === "float" ? "any" : undefined}
        value={String(value)}
        onChange={handleInputChange}
        style={inputStyle}
        placeholder={`Enter ${type}...`}
      />
    </div>
  );
}
