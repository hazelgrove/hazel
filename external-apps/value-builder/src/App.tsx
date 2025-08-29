import { useState } from "react";
import { useHazelIntegration } from "./hooks/useHazelIntegration";
import { ValueNode } from "./components/ValueNode";
import "./App.css";

function App() {
  const [currentValue, setCurrentValue] = useState<unknown>(42);
  const [isConnected, setIsConnected] = useState(false);

  // Extract ID from URL params or use default
  const urlParams = new URLSearchParams(window.location.search);
  const id = urlParams.get("id") || "value-builder-1";

  const { setSyntax } = useHazelIntegration({
    id,
    codec: "json", // We'll use our JsonCodec
    onInit: (value: unknown) => {
      console.log("Received init from Hazel:", value);
      setCurrentValue(value);
      setIsConnected(true);
    },
    onUpdate: (value: unknown) => {
      console.log("Received update from Hazel:", value);
      setCurrentValue(value);
    },
  });

  const handleValueChange = (newValue: unknown) => {
    setCurrentValue(newValue);
    setSyntax(JSON.stringify(newValue));
  };

  const containerStyle: React.CSSProperties = {
    display: "flex",
    height: "100vh",
    fontFamily: "system-ui",
  };

  const leftPanelStyle: React.CSSProperties = {
    flex: 2,
    padding: "16px",
    overflow: "auto",
    borderRight: "1px solid #ddd",
  };

  const rightPanelStyle: React.CSSProperties = {
    flex: 1,
    padding: "16px",
    backgroundColor: "#f8f9fa",
    overflow: "auto",
  };

  const headerStyle: React.CSSProperties = {
    marginBottom: "16px",
    paddingBottom: "8px",
    borderBottom: "2px solid #e0e0e0",
  };

  const quickTestStyle: React.CSSProperties = {
    marginBottom: "16px",
    padding: "12px",
    backgroundColor: "#f0f0f0",
    borderRadius: "6px",
  };

  return (
    <div style={containerStyle}>
      {/* Left Panel - Value Editor */}
      <div style={leftPanelStyle}>
        <div style={headerStyle}>
          <h2 style={{ margin: 0, marginBottom: "8px" }}>ExoValueBuilder</h2>
          <div style={{ fontSize: "14px" }}>
            <strong>Connection:</strong>{" "}
            {isConnected ? "✅ Connected" : "⚠️ Waiting for Hazel..."}
          </div>
        </div>

        <div style={quickTestStyle}>
          <strong>Quick Test Values:</strong>
          <div
            style={{
              display: "flex",
              gap: "8px",
              marginTop: "8px",
              flexWrap: "wrap",
            }}
          >
            <button onClick={() => handleValueChange(123)}>Int: 123</button>
            <button onClick={() => handleValueChange(3.14)}>Float: 3.14</button>
            <button onClick={() => handleValueChange("hello")}>
              String: "hello"
            </button>
            <button onClick={() => handleValueChange(true)}>Bool: true</button>
            <button onClick={() => handleValueChange([1, 2, 3])}>
              List: [1,2,3]
            </button>
            <button onClick={() => handleValueChange({ "0": 1, "1": 2 })}>
              Tuple: (1,2)
            </button>
            <button
              onClick={() => handleValueChange({ name: "Alice", age: 30 })}
            >
              Labeled: (name="Alice", age=30)
            </button>
            <button onClick={() => handleValueChange({ t: "Some", v: 42 })}>
              ADT: Some(42)
            </button>
          </div>
        </div>

        <div style={{ marginBottom: "16px" }}>
          <strong>Value Editor:</strong>
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
            borderTop: "1px solid #ddd",
            paddingTop: "8px",
          }}
        >
          Stage B: Compositional value editing UI
        </div>
      </div>

      {/* Right Panel - JSON Preview */}
      <div style={rightPanelStyle}>
        <h3 style={{ margin: 0, marginBottom: "12px" }}>JSON Preview</h3>
        <div style={{ fontSize: "12px", color: "#666", marginBottom: "8px" }}>
          This JSON will be sent to Hazel's JsonCodec:
        </div>
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
          }}
        >
          {JSON.stringify(currentValue, null, 2)}
        </pre>

        <div style={{ marginTop: "16px" }}>
          <h4 style={{ margin: 0, marginBottom: "8px" }}>Status</h4>
          <div style={{ fontSize: "14px" }}>
            <div>• Connection: {isConnected ? "✅ Active" : "⚠️ Waiting"}</div>
            <div>• Value Type: {typeof currentValue}</div>
            <div>• JSON Size: {JSON.stringify(currentValue).length} chars</div>
          </div>
        </div>
      </div>
    </div>
  );
}

export default App;
