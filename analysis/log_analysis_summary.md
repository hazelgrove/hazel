# Hazel Log Analysis Summary

## Overview
This analysis explores student interaction patterns in Hazel logs, focusing on induction detection and comparison between different types of student submissions.

## Key Findings

### 1. Scale and Activity Patterns

**Original Examples (haz3l-demo.json, final_version.json):**
- Average: 17,730 actions per file
- Duration: 32.4 million seconds (very long sessions)
- Activity rate: 0.004 actions/second (very low)
- Induction actions: 563 per file on average

**A2 Logs Sample (15 files):**
- Average: 11,266 actions per file
- Duration: 5.0 million seconds (shorter sessions)
- Activity rate: 0.074 actions/second (18x more active)
- Induction actions: 1,814 per file on average (3.2x more induction)

### 2. Session Type Distribution

**A2 Logs:**
- **Implementation-focused**: 13/15 files (87%)
- **Theorem-focused**: 1/15 files (7%)
- **Mixed**: 1/15 files (7%)

This suggests most students in the A2 assignment are primarily working on implementation tasks rather than theorem proving.

### 3. Induction Patterns

**Key Insights:**
- **All files contain induction actions** (100% frequency in both datasets)
- **A2 logs have 3.2x more induction actions** per file than original examples
- **Induction action counts range from 2 to 5,283** in A2 logs
- **Most common induction-related actions:**
  - `induction`: 27,208 total actions
  - `implementation`: 121,340 total actions
  - `theorem`: 4,657 total actions

### 4. Action Type Analysis

**Most Common Action Types in A2 Logs:**
1. **Implementation**: 121,340 actions (72% of all actions)
2. **Induction**: 27,208 actions (16% of all actions)
3. **Other**: 14,292 actions (8% of all actions)
4. **Theorem**: 4,657 actions (3% of all actions)
5. **Stepper**: 1,488 actions (1% of all actions)

### 5. Student Behavior Patterns

**Implementation-Focused Students (87%):**
- Heavy use of implementation actions (68-80% of actions)
- Moderate induction usage (7-30% of actions)
- Lower theorem activity (1-5% of actions)
- Longer sessions with more total actions

**Theorem-Focused Students (7%):**
- High induction usage (74% of actions)
- Moderate theorem activity (20% of actions)
- Lower implementation activity (4% of actions)
- More focused, shorter sessions

**Mixed Students (7%):**
- Balanced approach across action types
- High induction usage (68% of actions)
- Moderate implementation and theorem activity

## Technical Implementation

### Induction Detection
The analysis successfully identifies Hazel-specific induction actions:
- `AddInduction`: Adding induction to a proof
- `StepKindFocus(InductionStep`: Focusing on induction cases
- `InductionStep(CaseUpdate`: Modifying induction cases
- `AddAxiomStep`: Adding axioms during induction

### Session Classification
Students are classified based on action type ratios:
- **Theorem-focused**: >10% theorem actions
- **Implementation-focused**: >30% implementation actions
- **Stepper-focused**: >5% stepper actions
- **Mixed**: Balanced across types

## Research Implications

### 1. Induction Usage
- **Universal**: All students attempt induction (100% frequency)
- **Intensive**: A2 students use induction much more heavily than examples
- **Varied**: Wide range of induction usage (2-5,283 actions per file)

### 2. Learning Patterns
- **Implementation-heavy**: Most students focus on coding/implementation
- **Proof-light**: Relatively low theorem proving activity
- **Active engagement**: High activity rates suggest engaged learning

### 3. Assignment Differences
- **A2 logs**: More focused, shorter sessions, higher induction usage
- **Original examples**: Longer sessions, lower activity, different focus

## Files Generated

1. **`a2_logs_analysis_*.json`**: Detailed analysis results for each file
2. **`a2_logs_summary_*.csv`**: Summary statistics in CSV format
3. **`a2_logs_report_*.txt`**: Human-readable detailed report
4. **`log_pattern_comparison.txt`**: Comparison between original examples and A2 logs
5. **`log_analysis_summary.md`**: This summary document

## Tools Created

1. **`explore_logs_standalone.py`**: Standalone log analysis script
2. **`compare_log_patterns.py`**: Comparison analysis script
3. **Enhanced notebook**: Updated with multi-file analysis capabilities
4. **Enhanced parser**: Handles Hazel's specific log format
5. **Enhanced path_extractor**: Detects Hazel-specific induction patterns

## Next Steps

1. **Expand analysis**: Analyze all 25+ log files in a2_logs directory
2. **Temporal analysis**: Study induction timing patterns within sessions
3. **Success analysis**: Track which induction attempts lead to progress
4. **Comparative studies**: Compare different assignment types
5. **Visualization**: Create interactive dashboards for researchers

## Usage

To analyze your own logs:
1. Place log files in the analysis directory
2. Run `python3 explore_logs_standalone.py` for basic analysis
3. Use the enhanced notebook for interactive analysis
4. Modify detection patterns in `path_extractor.py` as needed

This analysis provides a comprehensive foundation for understanding student behavior patterns in Hazel, with particular focus on induction usage and learning strategies.
