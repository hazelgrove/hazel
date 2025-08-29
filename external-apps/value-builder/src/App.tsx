import { useState } from "react";
import { useHazelIntegration } from "./hooks/useHazelIntegration";
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

  return (
    <div style={{ padding: "16px", fontFamily: "system-ui" }}>
      <h2>ExoValueBuilder</h2>
      <div style={{ marginBottom: "16px" }}>
        <strong>Connection:</strong>{" "}
        {isConnected ? "✅ Connected" : "⚠️ Waiting for Hazel..."}
      </div>

      <div style={{ marginBottom: "16px" }}>
        <strong>Current Value:</strong>
        <pre
          style={{
            background: "#f5f5f5",
            padding: "8px",
            borderRadius: "4px",
            margin: "8px 0",
          }}
        >
          {JSON.stringify(currentValue, null, 2)}
        </pre>
      </div>

      <div style={{ marginBottom: "16px" }}>
        <strong>Quick Test Values:</strong>
        <div style={{ display: "flex", gap: "8px", marginTop: "8px" }}>
          <button onClick={() => handleValueChange(123)}>Integer: 123</button>
          <button onClick={() => handleValueChange("hello")}>
            String: "hello"
          </button>
          <button onClick={() => handleValueChange(true)}>Boolean: true</button>
          <button onClick={() => handleValueChange([1, 2, 3])}>
            List: [1,2,3]
          </button>
          <button onClick={() => handleValueChange({ t: "Some", v: 42 })}>
            ADT: Some(42)
          </button>
        </div>
      </div>

      <div style={{ fontSize: "12px", color: "#666" }}>
        Stage A: Basic bridge communication with JSON values
      </div>
    </div>
  );
}

export default App;
