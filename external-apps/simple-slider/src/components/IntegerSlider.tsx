import React, { useState } from "react";
import { useHazelIntegration } from "../hooks/useHazelIntegration";
import "./IntegerSlider.css";

interface IntegerSliderProps {
  id?: string;
  min?: number;
  max?: number;
  step?: number;
  initialValue?: number;
}

export function IntegerSlider({
  id = "slider-1",
  min = 0,
  max = 100,
  step = 1,
  initialValue = 50,
}: IntegerSliderProps) {
  const [value, setValue] = useState(initialValue);
  const [isDragging, setIsDragging] = useState(false);

  const { setSyntax } = useHazelIntegration({
    id,
    codec: "exoslider",
    onInit: (hazelValue: any) => {
      // Convert Hazel value (could be string or number) to integer
      const intValue =
        typeof hazelValue === "string"
          ? parseInt(hazelValue, 10)
          : Number(hazelValue);

      if (!isNaN(intValue)) {
        setValue(Math.max(min, Math.min(max, intValue)));
      }
    },
    onUpdate: (hazelValue: any) => {
      // Handle updates from Hazel (when syntax changes externally)
      const intValue =
        typeof hazelValue === "string"
          ? parseInt(hazelValue, 10)
          : Number(hazelValue);

      if (!isNaN(intValue)) {
        setValue(Math.max(min, Math.min(max, intValue)));
      }
    },
  });

  const handleChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    const newValue = parseInt(event.target.value, 10);
    setValue(newValue);

    // Send the new value back to Hazel as a string
    // (Hazel's Bigint.of_string expects string input)
    setSyntax(newValue.toString());
  };

  const handleMouseDown = () => setIsDragging(true);
  const handleMouseUp = () => setIsDragging(false);

  return (
    <div className="integer-slider">
      <div className="slider-header">
        <span className="slider-label">Integer Value</span>
        <span className="slider-value">{value}</span>
      </div>

      <div className="slider-container">
        <input
          type="range"
          min={min}
          max={max}
          step={step}
          value={value}
          onChange={handleChange}
          onMouseDown={handleMouseDown}
          onMouseUp={handleMouseUp}
          onTouchStart={handleMouseDown}
          onTouchEnd={handleMouseUp}
          className={`slider ${isDragging ? "dragging" : ""}`}
        />
      </div>

      <div className="slider-bounds">
        <span className="min-value">{min}</span>
        <span className="max-value">{max}</span>
      </div>

      <div className="debug-info">
        <small>
          ID: {id} | Codec: int | Value: {value}
        </small>
      </div>
    </div>
  );
}
