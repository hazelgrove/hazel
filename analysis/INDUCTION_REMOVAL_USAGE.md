# Using Induction Add/Remove Tracking in Notebooks

This guide shows how to use the new induction tracking functions (`track_induction_add_remove_sequences` and `analyze_induction_retention_patterns`) in a Jupyter notebook.

## Setup

### Cell 1: Imports
```python
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from parser import HazelLogParser
from path_extractor import (
    find_induction_patterns,
    track_induction_add_remove_sequences,
    analyze_induction_retention_patterns
)

# Enable inline plotting for Jupyter
%matplotlib inline
```

### Cell 2: Load Data
```python
# Load a single log file
parser = HazelLogParser()
df = parser.parse_file("path/to/your/log.json")

# Or load multiple files
log_files = [
    "a2_logs/submission_123.json",
    "a2_logs/submission_456.json",
]

dataframes = {}
for filepath in log_files:
    parser = HazelLogParser()
    dataframes[filepath] = parser.parse_file(filepath)
```

## Basic Usage

### Cell 3: Track Add/Remove Sequences
```python
# Track when students add induction and then remove it
sequences_data = track_induction_add_remove_sequences(
    df, 
    ignore_focus_actions=True  # Filter out navigation actions
)

# View summary statistics
print("Summary Statistics:")
print(f"Total AddInduction events: {sequences_data['statistics']['total_add_induction']}")
print(f"Removed count: {sequences_data['statistics']['removed_count']}")
print(f"Retained count: {sequences_data['statistics']['retained_count']}")
print(f"Removal rate: {sequences_data['statistics']['removal_rate']:.2%}")
print(f"Retention rate: {sequences_data['statistics']['retention_rate']:.2%}")

# View pattern categorization
print("\nPattern Categorization:")
for pattern, count in sequences_data['pattern_categorization']['counts'].items():
    print(f"  {pattern}: {count}")
```

### Cell 4: View Timing Analysis
```python
# Examine retention duration statistics
timing_stats = sequences_data['timing_analysis']

if timing_stats:
    print("Retention Duration Statistics:")
    print(f"Mean: {timing_stats['mean_retention']:.2f} seconds")
    print(f"Median: {timing_stats['median_retention']:.2f} seconds")
    print(f"Min: {timing_stats['min_retention']:.2f} seconds")
    print(f"Max: {timing_stats['max_retention']:.2f} seconds")
    print(f"Std Dev: {timing_stats['std_retention']:.2f} seconds")
else:
    print("No retention data available (all inductions were retained)")
```

### Cell 5: Examine Individual Sequences
```python
# Look at specific sequences
sequences = sequences_data['sequences']

# Find sequences that were immediately undone (< 5 seconds)
immediate_undos = [s for s in sequences if s['pattern'] == 'immediate_undo']
print(f"Found {len(immediate_undos)} immediate undos (< 5 seconds)")

# Print details of first few immediate undos
for i, seq in enumerate(immediate_undos[:3]):
    print(f"\nSequence {i+1}:")
    print(f"  Retention duration: {seq['retention_duration']:.2f} seconds")
    print(f"  Actions between: {seq['remove_step']['actions_between']}")
    print(f"  Add action index: {seq['add_induction']['action_index']}")
    print(f"  Remove action index: {seq['remove_step']['action_index']}")
```

## Advanced Analysis

### Cell 6: Retention Pattern Analysis
```python
# Analyze retention patterns in depth
retention_analysis = analyze_induction_retention_patterns(
    df,
    ignore_focus_actions=True
)

# View retention groups summary
print("Retention Groups Summary:")
for group_name, group_info in retention_analysis['retention_groups'].items():
    print(f"\n{group_name}:")
    print(f"  Count: {group_info['count']}")
    print(f"  Percentage: {group_info['percentage']:.1f}%")
    if 'mean_duration' in group_info:
        print(f"  Mean duration: {group_info['mean_duration']:.2f} seconds")
        print(f"  Median duration: {group_info['median_duration']:.2f} seconds")
```

### Cell 7: Intervening Actions Analysis
```python
# See what happens between AddInduction and RemoveStep
intervening_stats = retention_analysis['intervening_actions_analysis']['statistics']

print("What happens between AddInduction and RemoveStep:")
for action_type, stats in intervening_stats.items():
    print(f"\n{action_type}:")
    print(f"  Mean count per sequence: {stats['mean_count']:.2f}")
    print(f"  Median count: {stats['median_count']:.2f}")
    print(f"  Max count: {stats['max_count']}")
    print(f"  Sequences with this action: {stats['total_sequences_with_actions']}")
```

### Cell 8: Success Correlation Analysis
```python
# Correlate retention with success indicators
success_corr = retention_analysis['success_correlation']

print("Success Correlation by Pattern:")
for pattern, data in success_corr['by_pattern'].items():
    success_rate = data.get('success_rate', 0)
    print(f"{pattern}: {success_rate:.2%} ({data['success_count']}/{data['total_count']})")

print("\nSuccess Correlation by Retention Duration:")
for duration_category, data in success_corr['by_retention_duration'].items():
    if data['total_count'] > 0:
        success_rate = data.get('success_rate', 0)
        print(f"{duration_category}: {success_rate:.2%} ({data['success_count']}/{data['total_count']})")
```

## Enhanced Induction Patterns (with removal tracking)

### Cell 9: Enhanced find_induction_patterns
```python
# Use find_induction_patterns with removal tracking enabled (default)
induction_analysis = find_induction_patterns(df, track_removals=True)

# Access removal statistics
if 'induction_removal_stats' in induction_analysis:
    removal_stats = induction_analysis['induction_removal_stats']
    print("Removal Statistics:")
    print(f"Total AddInduction: {removal_stats['total_add_induction']}")
    print(f"Removed: {removal_stats['removed_count']}")
    print(f"Retained: {removal_stats['retained_count']}")
    print(f"Removal rate: {removal_stats['removal_rate']:.2%}")
    print(f"Average retention duration: {removal_stats['average_retention_duration']:.2f} seconds")

# Check individual events for removal status
print("\nFirst few AddInduction events:")
for event in induction_analysis['induction_events'][:5]:
    if event['context']['induction_type'] == 'add_induction':
        print(f"  Event at index {event['action_index']}:")
        print(f"    Was removed: {event.get('was_removed', False)}")
        if event.get('retention_duration'):
            print(f"    Retention duration: {event['retention_duration']:.2f} seconds")
```

## Visualizations

### Cell 10: Retention Duration Distribution
```python
# Create histogram of retention durations
sequences = sequences_data['sequences']
retention_durations = [s['retention_duration'] 
                       for s in sequences 
                       if s['retention_duration'] is not None]

if retention_durations:
    plt.figure(figsize=(10, 6))
    plt.hist(retention_durations, bins=30, edgecolor='black')
    plt.xlabel('Retention Duration (seconds)')
    plt.ylabel('Frequency')
    plt.title('Distribution of Induction Retention Durations')
    plt.axvline(np.mean(retention_durations), color='r', linestyle='--', 
                label=f'Mean: {np.mean(retention_durations):.1f}s')
    plt.legend()
    plt.show()
```

### Cell 11: Pattern Distribution Pie Chart
```python
# Pie chart of pattern distribution
pattern_counts = sequences_data['pattern_categorization']['counts']

if pattern_counts:
    plt.figure(figsize=(8, 8))
    plt.pie(pattern_counts.values(), labels=pattern_counts.keys(), autopct='%1.1f%%')
    plt.title('Distribution of Induction Add/Remove Patterns')
    plt.show()
```

### Cell 12: Retention vs Success Rate
```python
# Bar chart comparing success rates by retention pattern
success_corr = retention_analysis['success_correlation']
patterns = list(success_corr['by_pattern'].keys())
success_rates = [success_corr['by_pattern'][p].get('success_rate', 0) 
                 for p in patterns]

if patterns:
    plt.figure(figsize=(10, 6))
    plt.bar(patterns, success_rates)
    plt.xlabel('Retention Pattern')
    plt.ylabel('Success Rate')
    plt.title('Success Rate by Retention Pattern')
    plt.xticks(rotation=45, ha='right')
    plt.tight_layout()
    plt.show()
```

### Cell 13: Timeline Visualization
```python
# Create a timeline showing AddInduction and RemoveStep events
fig, ax = plt.subplots(figsize=(15, 4))

sequences = sequences_data['sequences']
y_pos = 0

for seq in sequences:
    add_time = seq['add_induction']['timestamp']
    y_pos += 1
    
    # Plot AddInduction point
    ax.scatter(add_time, y_pos, color='green', s=100, marker='o', 
               label='AddInduction' if y_pos == 1 else '')
    
    # Plot RemoveStep point if exists
    if seq['was_removed']:
        remove_time = seq['remove_step']['timestamp']
        ax.scatter(remove_time, y_pos, color='red', s=100, marker='x',
                   label='RemoveStep' if y_pos == 1 else '')
        # Draw line between add and remove
        ax.plot([add_time, remove_time], [y_pos, y_pos], 
                color='gray', alpha=0.5, linestyle='--')

ax.set_xlabel('Time')
ax.set_ylabel('Sequence Number')
ax.set_title('Timeline of Induction Add/Remove Sequences')
ax.legend()
plt.tight_layout()
plt.show()
```

## Comparing Multiple Logs

### Cell 14: Batch Analysis
```python
# Analyze multiple log files
results = {}

for filename, log_df in dataframes.items():
    print(f"Analyzing {filename}...")
    
    sequences_data = track_induction_add_remove_sequences(log_df)
    retention_analysis = analyze_induction_retention_patterns(log_df)
    
    results[filename] = {
        'sequences': sequences_data,
        'retention': retention_analysis
    }

# Compare removal rates across files
print("\nRemoval Rates Comparison:")
for filename, data in results.items():
    removal_rate = data['sequences']['statistics']['removal_rate']
    print(f"{filename}: {removal_rate:.2%}")
```

### Cell 15: Aggregate Statistics
```python
# Create DataFrame for comparison
comparison_data = []

for filename, data in results.items():
    stats = data['sequences']['statistics']
    comparison_data.append({
        'filename': filename,
        'total_add_induction': stats['total_add_induction'],
        'removed_count': stats['removed_count'],
        'retained_count': stats['retained_count'],
        'removal_rate': stats['removal_rate'],
        'retention_rate': stats['retention_rate'],
    })

comparison_df = pd.DataFrame(comparison_data)
print(comparison_df)

# Visualize comparison
plt.figure(figsize=(12, 6))
comparison_df.set_index('filename')[['removal_rate', 'retention_rate']].plot(kind='bar')
plt.ylabel('Rate')
plt.title('Removal vs Retention Rates Across Files')
plt.xticks(rotation=45, ha='right')
plt.tight_layout()
plt.show()
```

## Key Insights to Look For

1. **High removal rate**: If many inductions are removed, students might be experimenting or struggling
2. **Immediate undos**: Quick removal (< 5s) might indicate accidental addition or quick rejection
3. **Intervening actions**: What students do between adding and removing (case modifications, axioms) shows their process
4. **Success correlation**: Do retained inductions correlate with success indicators?
5. **Duration patterns**: Long retention (>60s) before removal might indicate sustained attempts

## Example Output Interpretation

```python
# Example: If you see this output:
# Removal rate: 45%
# Pattern: immediate_undo: 10, delayed_undo: 15, retained: 20
# Success rate for retained: 80%, for immediate_undo: 20%

# This suggests:
# - Students remove induction about half the time
# - Many removals happen quickly (10 immediate undos)
# - Retained inductions are much more successful (80% vs 20%)
# - Students might be learning through trial and error
```

