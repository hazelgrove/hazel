#!/usr/bin/env python3
"""
Script to explore and analyze logs in the a2_logs directory.
This will help us understand how different student submissions vary
and what patterns we can identify.
"""

import os
import json
import pandas as pd
from datetime import datetime
from parser import HazelLogParser
from path_extractor import find_induction_patterns, analyze_induction_timing_patterns, track_induction_progression
import re

def analyze_log_file(filepath):
    """Analyze a single log file and return structured results."""
    try:
        parser = HazelLogParser()
        df = parser.parse_file(filepath)
        
        # Basic statistics
        total_actions = len(df)
        if total_actions == 0:
            return None
            
        duration = (df['datetime'].max() - df['datetime'].min()).total_seconds()
        
        # Get induction analysis
        induction_analysis = find_induction_patterns(df)
        timing_analysis = analyze_induction_timing_patterns(df)
        progression_analysis = track_induction_progression(df)
        
        # Extract action types for analysis
        action_types = {}
        for _, row in df.iterrows():
            action_str = str(row.get('action_raw', '')).lower()
            # Categorize actions
            if 'induction' in action_str:
                action_types['induction'] = action_types.get('induction', 0) + 1
            elif 'theorem' in action_str:
                action_types['theorem'] = action_types.get('theorem', 0) + 1
            elif 'perform' in action_str or 'insert' in action_str:
                action_types['implementation'] = action_types.get('implementation', 0) + 1
            elif 'stepper' in action_str:
                action_types['stepper'] = action_types.get('stepper', 0) + 1
            elif 'undo' in action_str or 'back' in action_str:
                action_types['backtrack'] = action_types.get('backtrack', 0) + 1
            elif 'destruct' in action_str:
                action_types['destruct'] = action_types.get('destruct', 0) + 1
            else:
                action_types['other'] = action_types.get('other', 0) + 1
        
        # Sample some actual actions for inspection
        sample_actions = []
        for i in range(0, min(len(df), 10), max(1, len(df)//10)):
            action_str = str(df.iloc[i].get('action_raw', ''))
            if len(action_str) > 100:
                action_str = action_str[:100] + "..."
            sample_actions.append(action_str)
        
        return {
            'filename': os.path.basename(filepath),
            'total_actions': total_actions,
            'duration_seconds': duration,
            'actions_per_second': total_actions / duration if duration > 0 else 0,
            'induction_actions': induction_analysis['total_induction_actions'],
            'session_type': induction_analysis.get('session_type', 'unknown'),
            'induction_success_rate': induction_analysis['induction_success_patterns']['success_rate'],
            'action_types': action_types,
            'sample_actions': sample_actions,
            'has_induction': induction_analysis['total_induction_actions'] > 0,
            'induction_events': len(induction_analysis['induction_events']),
            'timing_distribution': timing_analysis['timing_distribution'] if timing_analysis['total_inductions'] > 0 else None
        }
        
    except Exception as e:
        return {
            'filename': os.path.basename(filepath),
            'error': str(e)
        }

def main():
    """Main function to explore a2_logs directory."""
    a2_logs_dir = "a2_logs"
    
    if not os.path.exists(a2_logs_dir):
        print(f"Directory {a2_logs_dir} not found!")
        return
    
    # Get all JSON files
    log_files = [f for f in os.listdir(a2_logs_dir) if f.endswith('.json')]
    print(f"Found {len(log_files)} log files in {a2_logs_dir}")
    
    # Analyze a sample of files (first 10 for initial exploration)
    sample_files = log_files[:10]
    results = []
    
    print(f"\nAnalyzing {len(sample_files)} sample files...")
    
    for i, filename in enumerate(sample_files):
        print(f"Processing {i+1}/{len(sample_files)}: {filename}")
        filepath = os.path.join(a2_logs_dir, filename)
        result = analyze_log_file(filepath)
        if result:
            results.append(result)
    
    # Save results to structured files
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    
    # 1. Save detailed results as JSON
    with open(f"a2_logs_analysis_{timestamp}.json", "w") as f:
        json.dump(results, f, indent=2, default=str)
    
    # 2. Create a summary CSV
    summary_data = []
    for result in results:
        if 'error' not in result:
            summary_data.append({
                'filename': result['filename'],
                'total_actions': result['total_actions'],
                'duration_seconds': result['duration_seconds'],
                'actions_per_second': result['actions_per_second'],
                'induction_actions': result['induction_actions'],
                'session_type': result['session_type'],
                'success_rate': result['induction_success_rate'],
                'has_induction': result['has_induction'],
                'induction_events': result['induction_events']
            })
    
    if summary_data:
        df_summary = pd.DataFrame(summary_data)
        df_summary.to_csv(f"a2_logs_summary_{timestamp}.csv", index=False)
    
    # 3. Create a detailed text report
    with open(f"a2_logs_report_{timestamp}.txt", "w") as f:
        f.write("A2 Logs Analysis Report\n")
        f.write("=" * 50 + "\n\n")
        
        f.write(f"Analyzed {len(results)} log files\n")
        f.write(f"Timestamp: {datetime.now()}\n\n")
        
        # Overall statistics
        valid_results = [r for r in results if 'error' not in r]
        if valid_results:
            total_actions = sum(r['total_actions'] for r in valid_results)
            total_duration = sum(r['duration_seconds'] for r in valid_results)
            files_with_induction = sum(1 for r in valid_results if r['has_induction'])
            
            f.write("OVERALL STATISTICS:\n")
            f.write(f"  Total actions across all files: {total_actions}\n")
            f.write(f"  Total duration: {total_duration:.1f} seconds\n")
            f.write(f"  Files with induction: {files_with_induction}/{len(valid_results)}\n")
            f.write(f"  Average actions per file: {total_actions/len(valid_results):.1f}\n")
            f.write(f"  Average duration per file: {total_duration/len(valid_results):.1f} seconds\n\n")
        
        # Detailed file analysis
        f.write("DETAILED FILE ANALYSIS:\n")
        f.write("-" * 30 + "\n\n")
        
        for result in results:
            if 'error' in result:
                f.write(f"ERROR in {result['filename']}: {result['error']}\n\n")
                continue
                
            f.write(f"File: {result['filename']}\n")
            f.write(f"  Actions: {result['total_actions']}\n")
            f.write(f"  Duration: {result['duration_seconds']:.1f}s\n")
            f.write(f"  Actions/sec: {result['actions_per_second']:.2f}\n")
            f.write(f"  Induction actions: {result['induction_actions']}\n")
            f.write(f"  Session type: {result['session_type']}\n")
            f.write(f"  Success rate: {result['induction_success_rate']:.1%}\n")
            
            # Action type breakdown
            f.write(f"  Action types:\n")
            for action_type, count in result['action_types'].items():
                percentage = (count / result['total_actions']) * 100
                f.write(f"    {action_type}: {count} ({percentage:.1f}%)\n")
            
            # Sample actions
            f.write(f"  Sample actions:\n")
            for i, action in enumerate(result['sample_actions'][:3]):
                f.write(f"    {i+1}. {action}\n")
            
            f.write("\n")
    
    print(f"\nAnalysis complete! Generated files:")
    print(f"  - a2_logs_analysis_{timestamp}.json (detailed results)")
    print(f"  - a2_logs_summary_{timestamp}.csv (summary table)")
    print(f"  - a2_logs_report_{timestamp}.txt (human-readable report)")
    
    # Print quick summary to console
    print(f"\nQUICK SUMMARY:")
    valid_results = [r for r in results if 'error' not in r]
    if valid_results:
        print(f"  Files analyzed: {len(valid_results)}")
        print(f"  Files with induction: {sum(1 for r in valid_results if r['has_induction'])}")
        print(f"  Average actions per file: {sum(r['total_actions'] for r in valid_results)/len(valid_results):.1f}")
        print(f"  Average duration per file: {sum(r['duration_seconds'] for r in valid_results)/len(valid_results):.1f}s")
        
        # Show session type distribution
        session_types = {}
        for r in valid_results:
            st = r['session_type']
            session_types[st] = session_types.get(st, 0) + 1
        print(f"  Session types: {dict(session_types)}")

if __name__ == "__main__":
    main()
