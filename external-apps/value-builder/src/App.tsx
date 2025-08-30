import { useState } from "react";
import { useHazelIntegration } from "./hooks/useHazelIntegration";
import { ValueNode } from "./components/ValueNode";
import "./App.css";

function App() {
  const [currentValue, setCurrentValue] = useState<unknown>(42);
  const [constraints, setConstraints] = useState<{
    maxWidth: number;
    maxHeight: number;
  } | null>(null);

  // Extract ID from URL params or use default
  const urlParams = new URLSearchParams(window.location.search);
  const id = urlParams.get("id") || "value-builder-1";

  const { setSyntax } = useHazelIntegration({
    id,
    codec: "json", // We'll use our JsonCodec
    onInit: (value: string) => {
      console.log("Received init from Hazel:", value);
      try {
        setCurrentValue(JSON.parse(value));
      } catch {
        console.warn("Failed to parse init value, using as-is:", value);
        setCurrentValue(value);
      }
    },
    onConstraints: (c) => {
      console.log("Received constraints from Hazel:", c);
      setConstraints(c);
    },
  });

  const handleValueChange = (newValue: unknown) => {
    setCurrentValue(newValue);
    setSyntax(JSON.stringify(newValue));
  };

  const containerStyle: React.CSSProperties = {
    display: "flex",
    minHeight: "100px", // Allow to be smaller initially
    fontFamily: "system-ui",
    maxWidth: constraints?.maxWidth || "none",
    maxHeight: constraints?.maxHeight || "none",
    // Remove fixed height: "100vh" to allow content-driven sizing
  };

  const leftPanelStyle: React.CSSProperties = {
    flex: 2,
    padding: "8px",
    overflow: "auto",
    borderRight: "1px solid #ddd",
  };

  const headerStyle: React.CSSProperties = {
    marginBottom: "4px",
    paddingBottom: "4px",
    borderBottom: "2px solid #e0e0e0",
    display: "flex",
    flexDirection: "row",
    gap: "16px",
    alignItems: "baseline",
    justifyContent: "space-between",
  };

  const quickTestStyle: React.CSSProperties = {
    // marginBottom: "16px",
    padding: "12px",
    // backgroundColor: "#f0f0f0",
    borderRadius: "6px",
    display: "flex",
    flexDirection: "row",
    gap: "8px",
  };

  return (
    <div style={containerStyle}>
      {/* Left Panel - Value Editor */}
      <div style={leftPanelStyle}>
        <div style={headerStyle}>
          <h2 style={{ margin: 0, marginBottom: "8px" }}>Value Builder</h2>
          {/* {constraints && (
            <div style={{ fontSize: "12px", color: "#666" }}>
              Max: {constraints.maxWidth}×{constraints.maxHeight}px
            </div>
          )} */}
        </div>

        <div style={quickTestStyle}>
          <h4>Try:</h4>

          <button onClick={() => handleValueChange(123)}>
            <strong>Int</strong> 123
          </button>
          {/* <button onClick={() => handleValueChange(3.14)}>Float: 3.14</button> */}
          <button onClick={() => handleValueChange("hello")}>
            <strong>String</strong> "hello"
          </button>
          <button onClick={() => handleValueChange(true)}>
            <strong>Bool</strong> true
          </button>
          <button onClick={() => handleValueChange([1, 2, 3])}>
            <strong>List</strong> [1,2,3]
          </button>
          <button onClick={() => handleValueChange({ "0": 1, "1": 2 })}>
            <strong>Tuple</strong> (1,2)
          </button>
          <button onClick={() => handleValueChange({ name: "Alice", age: 30 })}>
            <strong>Labeled</strong> (name="Alice",age=30)
          </button>
          <button onClick={() => handleValueChange({ t: "Some", v: 42 })}>
            <strong>ADT</strong> Some(42)
          </button>
        </div>

        <div style={{}}>
          <ValueNode
            value={currentValue}
            onChange={handleValueChange}
            path={["root"]}
            level={0}
          />
        </div>

        <div
          style={{
            fontSize: "12px",
            color: "#666",
            // borderTop: "1px solid #ddd",
            // paddingTop: "8px",
          }}
        >
          <pre
            style={{
              background: "#ffffff",
              padding: "12px",
              borderRadius: "4px",
              border: "1px solid #ddd",
              fontSize: "12px",
              lineHeight: "1.4",
              overflow: "auto",
              maxHeight: "300px",
              textAlign: "left",
            }}
          >
            {JSON.stringify(currentValue, null, 2)}
          </pre>
          {/* <div>• JSON Size: {JSON.stringify(currentValue).length} chars</div> */}
        </div>
      </div>
    </div>
  );
}

export default App;
