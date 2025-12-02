#!/usr/bin/env python3
"""
Standalone script to explore and analyze logs in the a2_logs directory.
This version doesn't depend on pandas or the existing parser modules.
"""

import os
import json
import re
from datetime import datetime

def parse_log_entries(log_string):
    """Parse log entries from a log string."""
    # Look for patterns like (timestamp action)
    action_pattern = r'(\d{13})\s*\(([^)]+)\)'
    matches = re.findall(action_pattern, log_string)
    entries = []
    for timestamp_str, action in matches:
        try:
            timestamp = float(timestamp_str)
            entries.append((timestamp, action))
        except ValueError:
            continue
    return entries

def analyze_log_file(filepath):
    """Analyze a single log file and return structured results."""
    try:
        with open(filepath, 'r') as f:
            log_content = f.read()
        
        # Parse log entries
        entries = parse_log_entries(log_content)
        
        if not entries:
            return {
                'filename': os.path.basename(filepath),
                'error': 'No valid log entries found'
            }
        
        # Basic statistics
        total_actions = len(entries)
        timestamps = [entry[0] for entry in entries]
        duration = (max(timestamps) - min(timestamps)) / 1000.0  # Convert to seconds
        
        # Analyze action types
        action_types = {}
        sample_actions = []
        
        for i, (timestamp, action) in enumerate(entries):
            action_lower = action.lower()
            
            # Categorize actions
            if 'induction' in action_lower:
                action_types['induction'] = action_types.get('induction', 0) + 1
            elif 'theorem' in action_lower:
                action_types['theorem'] = action_types.get('theorem', 0) + 1
            elif 'perform' in action_lower or 'insert' in action_lower:
                action_types['implementation'] = action_types.get('implementation', 0) + 1
            elif 'stepper' in action_lower:
                action_types['stepper'] = action_types.get('stepper', 0) + 1
            elif 'undo' in action_lower or 'back' in action_lower:
                action_types['backtrack'] = action_types.get('backtrack', 0) + 1
            elif 'destruct' in action_lower:
                action_types['destruct'] = action_types.get('destruct', 0) + 1
            elif 'addinduction' in action_lower:
                action_types['add_induction'] = action_types.get('add_induction', 0) + 1
            elif 'stepkindfocus' in action_lower and 'inductionstep' in action_lower:
                action_types['focus_induction'] = action_types.get('focus_induction', 0) + 1
            elif 'inductionstep' in action_lower and 'caseupdate' in action_lower:
                action_types['modify_induction'] = action_types.get('modify_induction', 0) + 1
            elif 'addaxiomstep' in action_lower:
                action_types['add_axiom'] = action_types.get('add_axiom', 0) + 1
            else:
                action_types['other'] = action_types.get('other', 0) + 1
            
            # Collect sample actions
            if i % max(1, total_actions // 10) == 0:
                if len(action) > 100:
                    action = action[:100] + "..."
                sample_actions.append(action)
        
        # Count induction-related actions
        induction_actions = (action_types.get('induction', 0) + 
                           action_types.get('add_induction', 0) + 
                           action_types.get('focus_induction', 0) + 
                           action_types.get('modify_induction', 0) + 
                           action_types.get('add_axiom', 0))
        
        # Classify session type
        total_categorized = sum(action_types.values()) - action_types.get('other', 0)
        if total_categorized == 0:
            session_type = 'unknown'
        else:
            theorem_ratio = action_types.get('theorem', 0) / total_categorized
            impl_ratio = action_types.get('implementation', 0) / total_categorized
            stepper_ratio = action_types.get('stepper', 0) / total_categorized
            
            if theorem_ratio > 0.1:
                session_type = 'theorem_focused'
            elif impl_ratio > 0.3:
                session_type = 'implementation_focused'
            elif stepper_ratio > 0.05:
                session_type = 'stepper_focused'
            else:
                session_type = 'mixed'
        
        return {
            'filename': os.path.basename(filepath),
            'total_actions': total_actions,
            'duration_seconds': duration,
            'actions_per_second': total_actions / duration if duration > 0 else 0,
            'induction_actions': induction_actions,
            'session_type': session_type,
            'action_types': action_types,
            'sample_actions': sample_actions,
            'has_induction': induction_actions > 0,
            'first_action': entries[0][1] if entries else '',
            'last_action': entries[-1][1] if entries else ''
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
    
    # Analyze a sample of files (first 15 for initial exploration)
    sample_files = log_files[:15]
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
    with open(f"a2_logs_summary_{timestamp}.csv", "w") as f:
        f.write("filename,total_actions,duration_seconds,actions_per_second,induction_actions,session_type,has_induction,first_action,last_action\n")
        for result in results:
            if 'error' not in result:
                first_action = result['first_action'].replace(',', ';')[:50]
                last_action = result['last_action'].replace(',', ';')[:50]
                f.write(f"{result['filename']},{result['total_actions']},{result['duration_seconds']:.2f},{result['actions_per_second']:.2f},{result['induction_actions']},{result['session_type']},{result['has_induction']},{first_action},{last_action}\n")
    
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
            
            # Action type breakdown
            f.write(f"  Action types:\n")
            for action_type, count in result['action_types'].items():
                percentage = (count / result['total_actions']) * 100
                f.write(f"    {action_type}: {count} ({percentage:.1f}%)\n")
            
            # Sample actions
            f.write(f"  Sample actions:\n")
            for i, action in enumerate(result['sample_actions'][:3]):
                f.write(f"    {i+1}. {action}\n")
            
            f.write(f"  First action: {result['first_action']}\n")
            f.write(f"  Last action: {result['last_action']}\n")
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
        
        # Show action type distribution
        all_action_types = {}
        for r in valid_results:
            for action_type, count in r['action_types'].items():
                all_action_types[action_type] = all_action_types.get(action_type, 0) + count
        
        print(f"  Most common action types:")
        for action_type, count in sorted(all_action_types.items(), key=lambda x: x[1], reverse=True)[:5]:
            print(f"    {action_type}: {count}")

if __name__ == "__main__":
    main()
