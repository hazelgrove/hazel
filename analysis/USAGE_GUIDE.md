# Hazel Log Analysis - Usage Guide

## Overview
This guide covers how to use the comprehensive Hazel log analysis system for studying student interaction patterns, with a focus on induction detection and proof development behaviors.

## Quick Start

### 1. Environment Setup
```bash
# Activate the conda environment (has all required packages)
conda activate base

# Navigate to the analysis directory
cd /Users/nishant/hazel/analysis
```

### 2. Basic Analysis
```bash
# Run standalone analysis on a2_logs directory
python explore_logs_standalone.py

# Compare patterns between different log sets
python compare_log_patterns.py
```

### 3. Interactive Analysis
```bash
# Launch Jupyter notebook for interactive analysis
jupyter notebook notebook.ipynb
```

## Detailed Usage

### A. Standalone Analysis Scripts

#### 1. `explore_logs_standalone.py`
**Purpose**: Analyze multiple log files without dependencies on pandas

**Usage**:
```bash
python explore_logs_standalone.py
```

**What it does**:
- Analyzes first 15 log files in `a2_logs/` directory
- Extracts action patterns, induction events, session types
- Generates structured output files

**Output files**:
- `a2_logs_analysis_YYYYMMDD_HHMMSS.json` - Detailed results
- `a2_logs_summary_YYYYMMDD_HHMMSS.csv` - Summary table
- `a2_logs_report_YYYYMMDD_HHMMSS.txt` - Human-readable report

#### 2. `compare_log_patterns.py`
**Purpose**: Compare patterns between original examples and a2_logs

**Usage**:
```bash
python compare_log_patterns.py
```

**What it does**:
- Compares `haz3l-demo.json` and `final_version.json` with a2_logs
- Identifies scale differences, activity patterns, induction usage
- Generates comparison report

**Output files**:
- `log_pattern_comparison.txt` - Detailed comparison analysis

### B. Interactive Jupyter Notebook

#### 1. Launching the Notebook
```bash
conda activate base
jupyter notebook notebook.ipynb
```

#### 2. Notebook Structure

**Cell 1-2**: Setup and imports
- Loads all required libraries (pandas, numpy, matplotlib, seaborn)
- Imports custom modules (parser, path_extractor, analysis_utils)

**Cell 3**: Multi-file loading
- Loads multiple log files for comparison
- Currently configured for `haz3l-demo.json` and `final_version.json`
- Can be modified to load any log files

**Cell 4-7**: Time analysis
- Session duration, action frequency
- Rapid action detection (potential random clicking)
- Session segmentation

**Cell 8-11**: Path extraction and analysis
- Action sequence extraction
- Exploration path identification
- Proof progression tracking

**Cell 12-14**: Enhanced induction analysis
- **Induction detection**: Identifies Hazel-specific actions
- **Success analysis**: Tracks progress vs backtracking
- **Timing analysis**: Early/mid/late induction patterns
- **Progression tracking**: Step-by-step induction development

**Cell 15-16**: Induction visualizations
- Pie charts for timing distribution
- Bar charts for induction types
- Success rate visualizations
- Action timeline with induction markers

**Cell 17-24**: General visualizations
- Path length/duration distributions
- Time between actions histograms
- Session overview with activity rates
- Path visualization as trees/graphs

**Cell 25-27**: Multi-file comparison
- Side-by-side analysis across files
- Comparative visualizations
- Summary statistics

**Cell 28-29**: Additional log analysis
- Example analysis of a2_logs directory
- Instructions for analyzing more files

**Cell 30**: Research usage instructions
- How to customize for your research
- Key research questions addressed

### C. Customizing Analysis

#### 1. Adding New Log Files
Edit Cell 3 in the notebook:
```python
log_files = [
    "haz3l-demo.json",
    "final_version.json",
    "your_new_file.json"  # Add your files here
]
```

#### 2. Modifying Induction Detection
Edit `path_extractor.py`, function `find_induction_patterns`:
```python
induction_action_patterns = [
    'AddInduction',
    'StepKindFocus(InductionStep',
    'InductionStep(CaseUpdate',
    'AddAxiomStep',
    'YourNewPattern'  # Add new patterns here
]
```

#### 3. Adjusting Success/Failure Indicators
Edit `path_extractor.py`, function `_analyze_hazel_induction_success`:
```python
success_indicators = ['StepForward', 'NextStep', 'AddAxiomStep', 'Reflexive', 'YourSuccessPattern']
failure_indicators = ['Undo', 'Back', 'Revert', 'Destruct', 'YourFailurePattern']
```

#### 4. Changing Session Classification
Edit `path_extractor.py`, function `_classify_session_type`:
```python
# Modify the thresholds and categories as needed
if theorem_actions / total_actions > 0.1:
    return 'theorem_focused'
elif implementation_actions / total_actions > 0.3:
    return 'implementation_focused'
# Add your own categories
```

### D. Analyzing Your Own Logs

#### 1. Single File Analysis
```python
from parser import HazelLogParser
from path_extractor import find_induction_patterns

parser = HazelLogParser()
df = parser.parse_file("your_log_file.json")
induction_analysis = find_induction_patterns(df)
print(f"Found {induction_analysis['total_induction_actions']} induction actions")
```

#### 2. Batch Analysis
```python
import os
from parser import HazelLogParser
from path_extractor import find_induction_patterns

parser = HazelLogParser()
results = []

for filename in os.listdir("your_log_directory"):
    if filename.endswith('.json'):
        df = parser.parse_file(filename)
        analysis = find_induction_patterns(df)
        results.append({
            'filename': filename,
            'induction_actions': analysis['total_induction_actions'],
            'session_type': analysis.get('session_type', 'unknown')
        })

# Process results...
```

### E. Research Applications

#### 1. Induction Timing Studies
```python
timing_analysis = analyze_induction_timing_patterns(df)
print(f"Early induction: {timing_analysis['timing_distribution']['early_induction']}")
print(f"Mid induction: {timing_analysis['timing_distribution']['mid_induction']}")
print(f"Late induction: {timing_analysis['timing_distribution']['late_induction']}")
```

#### 2. Success Rate Analysis
```python
induction_analysis = find_induction_patterns(df)
success_rate = induction_analysis['induction_success_patterns']['success_rate']
print(f"Induction success rate: {success_rate:.1%}")
```

#### 3. Session Type Distribution
```python
# Analyze multiple files
session_types = {}
for filepath, df in all_logs.items():
    analysis = find_induction_patterns(df)
    st = analysis.get('session_type', 'unknown')
    session_types[st] = session_types.get(st, 0) + 1

print("Session type distribution:", session_types)
```

### F. Output Interpretation

#### 1. Action Types
- **`induction`**: Direct induction-related actions
- **`implementation`**: Coding/implementation actions
- **`theorem`**: Theorem proving actions
- **`stepper`**: Stepper tool usage
- **`backtrack`**: Undo/backtrack actions
- **`other`**: Miscellaneous actions

#### 2. Session Types
- **`theorem_focused`**: >10% theorem actions
- **`implementation_focused`**: >30% implementation actions
- **`stepper_focused`**: >5% stepper actions
- **`mixed`**: Balanced across types

#### 3. Induction Patterns
- **`add_induction`**: Adding induction to proof
- **`focus_case`**: Focusing on induction cases
- **`modify_case`**: Modifying induction cases
- **`add_axiom`**: Adding axioms during induction

### G. Troubleshooting

#### 1. Import Errors
```bash
# Make sure you're in the conda environment
conda activate base

# Check if packages are available
python -c "import pandas, numpy, matplotlib, seaborn; print('All packages available')"
```

#### 2. File Not Found Errors
```bash
# Check if log files exist
ls -la *.json

# Check if a2_logs directory exists
ls -la a2_logs/
```

#### 3. Parsing Errors
```bash
# Test parser on a single file
python -c "
from parser import HazelLogParser
parser = HazelLogParser()
df = parser.parse_file('haz3l-demo.json')
print(f'Successfully parsed {len(df)} entries')
"
```

### H. Advanced Features

#### 1. Custom Visualizations
```python
# Create custom plots in the notebook
import matplotlib.pyplot as plt
import seaborn as sns

# Your custom visualization code here
plt.figure(figsize=(12, 8))
# ... plotting code ...
plt.show()
```

#### 2. Export Results
```python
# Export analysis results
import json
with open('my_analysis_results.json', 'w') as f:
    json.dump(induction_analysis, f, indent=2, default=str)
```

#### 3. Statistical Analysis
```python
# Perform statistical tests
from scipy import stats
import numpy as np

# Example: Compare induction success rates between groups
group1_success_rates = [0.6, 0.7, 0.8]  # Your data
group2_success_rates = [0.4, 0.5, 0.6]  # Your data

t_stat, p_value = stats.ttest_ind(group1_success_rates, group2_success_rates)
print(f"T-test p-value: {p_value:.4f}")
```

## File Structure

```
analysis/
├── notebook.ipynb                 # Main interactive analysis
├── parser.py                      # Log file parsing
├── path_extractor.py              # Action sequence analysis
├── analysis_utils.py              # Time analysis utilities
├── explore_logs_standalone.py     # Standalone analysis script
├── compare_log_patterns.py        # Comparison analysis
├── USAGE_GUIDE.md                 # This guide
├── log_analysis_summary.md        # Research summary
├── haz3l-demo.json               # Example log file
├── final_version.json            # Example log file
└── a2_logs/                      # Directory with student logs
    ├── submission_*.json         # Student submission logs
    └── ...
```

## Research Questions Addressed

1. **When do students attempt induction?** (timing analysis)
2. **What terms do they induct over?** (target term analysis)
3. **How successful are their induction attempts?** (success rate analysis)
4. **What actions precede and follow induction?** (context analysis)
5. **How do different students approach induction?** (comparative analysis)
6. **What are the common exploration patterns?** (path analysis)
7. **How do session types vary?** (classification analysis)

## Support

For questions or issues:
1. Check this usage guide
2. Review the generated analysis files
3. Examine the notebook cells for examples
4. Modify the code as needed for your specific research questions

The system is designed to be extensible and customizable for various research needs in studying student behavior in formal proof environments.
