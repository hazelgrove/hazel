import React, { useState, useEffect } from "react";
import "./DocComponents.css";
import type { HazelDoc } from "../types/delta";

interface DocStateManagerProps {
  currentState: HazelDoc | null;
  onLoadState: (state: HazelDoc) => void;
}

interface SavedState {
  id: string;
  name: string;
  timestamp: string;
  state: HazelDoc;
}

const DocStateManager: React.FC<DocStateManagerProps> = ({ currentState, onLoadState }) => {
  const [savedStates, setSavedStates] = useState<SavedState[]>([]);
  const [stateName, setStateName] = useState<string>("");

  const handleSaveState = () => {
    if (!currentState) {
      alert("No document state to save");
      return;
    }
    
    const name = stateName.trim() || `State ${savedStates.length + 1}`;
    const newState: SavedState = {
      id: Date.now().toString(),
      name,
      timestamp: new Date().toLocaleString(),
      state: currentState
    };
    
    setSavedStates([...savedStates, newState]);
    setStateName("");
  };

  const handleLoadState = (state: SavedState) => {
    if (confirm(`Load state "${state.name}"?`)) {
      onLoadState(state.state);
    }
  };

  const handleDeleteState = (id: string, e: React.MouseEvent) => {
    e.stopPropagation();
    if (confirm("Delete this saved state?")) {
      setSavedStates(savedStates.filter(state => state.id !== id));
    }
  };

  return (
    <div className="doc-state-manager">
      <h3>Document States</h3>
      
      <div className="save-state-form">
        <input
          type="text"
          value={stateName}
          onChange={(e) => setStateName(e.target.value)}
          placeholder="Name for current state"
        />
        <button 
          onClick={handleSaveState}
          disabled={!currentState}
        >
          Save Current State
        </button>
      </div>
      
      <div className="saved-states-list">
        <h4>Saved States ({savedStates.length})</h4>
        {savedStates.length === 0 ? (
          <p>No saved states yet</p>
        ) : (
          <ul>
            {savedStates.map((state) => (
              <li 
                key={state.id} 
                onClick={() => handleLoadState(state)}
                className="saved-state-item"
              >
                <div className="state-info">
                  <span className="state-name">{state.name}</span>
                  <span className="state-timestamp">{state.timestamp}</span>
                </div>
                <button
                  className="delete-button"
                  onClick={(e) => handleDeleteState(state.id, e)}
                >
                  Delete
                </button>
              </li>
            ))}
          </ul>
        )}
      </div>
    </div>
  );
};

export default DocStateManager;
