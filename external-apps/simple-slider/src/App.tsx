import { IntegerSlider } from "./components/IntegerSlider";
import "./App.css";

function App() {
  // Get URL parameters to configure the slider
  const urlParams = new URLSearchParams(window.location.search);
  const min = parseInt(urlParams.get("min") || "0", 10);
  const max = parseInt(urlParams.get("max") || "100", 10);
  const step = parseInt(urlParams.get("step") || "1", 10);
  const initialValue = parseInt(urlParams.get("initial") || "50", 10);
  const id = urlParams.get("id") || "hazel-slider";

  return (
    <div className="app">
      <IntegerSlider
        id={id}
        min={min}
        max={max}
        step={step}
        initialValue={initialValue}
      />
    </div>
  );
}

export default App;
