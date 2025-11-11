# Hazel Log Analysis

This directory contains tools for analyzing student submission logs from Hazel to understand proof strategies, exploration patterns, and learning behaviors.

## Files

- `parser.py` - JSON and s-expression parser for Hazel log files
- `analysis_utils.py` - Time analysis and session segmentation utilities
- `path_extractor.py` - Path extraction and pattern identification utilities
- `notebook.ipynb` - Jupyter notebook for interactive analysis
- `README.md` - This documentation

## Quick Start

### Command Line Usage

```bash
# Parse a submission file and get basic statistics
python parser.py submission.json
```

### Notebook Usage

1. Place student submission JSON files in this directory or parent directory
2. Open `notebook.ipynb` in Jupyter
3. Run the cells to load and analyze the data

## Modules

### parser.py

Core parsing functionality:
- `HazelLogParser` - Main parser class
- `SExpressionParser` - S-expression parser for log data
- `parse_file(filepath)` - Parse a submission file into a DataFrame

### analysis_utils.py

Time and session analysis utilities:
- `calculate_time_diffs(df)` - Compute time differences between actions
- `get_time_statistics(df)` - Get comprehensive timing metrics
- `identify_rapid_actions(df, threshold)` - Find rapid-fire action sequences
- `segment_by_breaks(df, break_threshold)` - Split session by inactivity gaps
- `analyze_session_segments(df)` - Break down session into work phases
- `identify_activity_bursts(df)` - Find periods of intense activity

### path_extractor.py

Path extraction and analysis:
- `extract_action_sequences(df)` - Extract action sequences as Path objects
- `identify_exploration_paths(df)` - Find undo/redo exploration patterns
- `identify_focused_paths(df)` - Find focused sequences on same element
- `identify_navigation_paths(df)` - Find navigation between proof parts
- `track_proof_progression(df)` - Trace student proof progression
- `find_induction_patterns(df)` - Identify induction usage patterns
- `summarize_paths(paths)` - Generate summary statistics for paths

## Data Structure

Hazel submission files contain:
- Top-level JSON with keys: `settings`, `explainThisModel`, `scratch`, `tutorial`, `exercise`, `documentation`, `log`
- `log` field contains s-expression formatted action logs
- Each log entry: `(timestamp, action)` where timestamp is Unix milliseconds
- Actions include editor interactions, stepper usage, and navigation

## Research Questions

This tooling supports analysis of:

1. **Exploration Patterns**: How do students explore the proof space? Do they use trial-and-error?
2. **Time Analysis**: Wall-clock time between actions, rapid clicking indicators
3. **Proof Strategies**: When do students apply induction? How do they progress through proofs?
4. **Learning Behavior**: Do students learn from mistakes? How do patterns change over time?
5. **Stepper Usage**: How do students interact with the stepper? Case analysis patterns?
6. **Focus Patterns**: Do students stay focused on one element or jump around?
7. **Backtracking**: How much undo/redo behavior indicates exploration vs. errors?

## Path Types

The `path_extractor` module classifies paths into:

- **Linear paths**: Sequential actions without backtracking
- **Exploration paths**: Sequences with undo/redo indicating trial-and-error
- **Focused paths**: Rapid sequences on the same element/goal
- **Navigation paths**: Movement between different parts of the proof
- **Rapid paths**: High frequency action sequences

## Example Analysis

```python
from parser import HazelLogParser
from analysis_utils import get_time_statistics, identify_rapid_actions
from path_extractor import extract_action_sequences, find_induction_patterns

# Parse submission
parser = HazelLogParser()
df = parser.parse_file('submission.json')

# Time analysis
time_stats = get_time_statistics(df)
rapid_actions, rapid_seqs = identify_rapid_actions(df)

# Path analysis
paths = extract_action_sequences(df)
induction = find_induction_patterns(df)

print(f"Total actions: {time_stats['total_actions']}")
print(f"Rapid sequences: {len(rapid_seqs)}")
print(f"Paths extracted: {len(paths)}")
print(f"Induction actions: {induction['total_induction_actions']}")
```
