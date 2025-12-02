#!/usr/bin/env python3
"""
Script to compare patterns between the original example logs and the a2_logs.
This helps understand how different types of student interactions vary.
"""

import json
import os
from datetime import datetime

def load_analysis_results():
    """Load the analysis results from the generated files."""
    # Find the most recent analysis file
    analysis_files = [f for f in os.listdir('.') if f.startswith('a2_logs_analysis_') and f.endswith('.json')]
    if not analysis_files:
        print("No analysis files found!")
        return None
    
    latest_file = sorted(analysis_files)[-1]
    print(f"Loading analysis from: {latest_file}")
    
    with open(latest_file, 'r') as f:
        return json.load(f)

def analyze_original_examples():
    """Analyze the original example files for comparison."""
    original_files = ['haz3l-demo.json', 'final_version.json']
    results = []
    
    for filename in original_files:
        if os.path.exists(filename):
            print(f"Analyzing original file: {filename}")
            # This would need the full parser, but for now let's just get basic info
            try:
                with open(filename, 'r') as f:
                    content = f.read()
                
                # Basic analysis
                import re
                action_pattern = r'(\d{13})\s*\(([^)]+)\)'
                matches = re.findall(action_pattern, content)
                
                if matches:
                    total_actions = len(matches)
                    timestamps = [float(m[0]) for m in matches]
                    duration = (max(timestamps) - min(timestamps)) / 1000.0
                    
                    # Count induction actions
                    induction_count = 0
                    for _, action in matches:
                        if 'induction' in action.lower():
                            induction_count += 1
                    
                    results.append({
                        'filename': filename,
                        'total_actions': total_actions,
                        'duration_seconds': duration,
                        'actions_per_second': total_actions / duration if duration > 0 else 0,
                        'induction_actions': induction_count,
                        'has_induction': induction_count > 0,
                        'first_action': matches[0][1][:100] if matches else '',
                        'last_action': matches[-1][1][:100] if matches else ''
                    })
            except Exception as e:
                print(f"Error analyzing {filename}: {e}")
    
    return results

def main():
    """Main comparison function."""
    print("=== LOG PATTERN COMPARISON ===")
    print(f"Timestamp: {datetime.now()}\n")
    
    # Load a2_logs analysis
    a2_results = load_analysis_results()
    if not a2_results:
        return
    
    # Analyze original examples
    original_results = analyze_original_examples()
    
    # Create comparison report
    with open("log_pattern_comparison.txt", "w") as f:
        f.write("LOG PATTERN COMPARISON REPORT\n")
        f.write("=" * 50 + "\n\n")
        
        f.write("ORIGINAL EXAMPLE FILES:\n")
        f.write("-" * 30 + "\n")
        for result in original_results:
            f.write(f"File: {result['filename']}\n")
            f.write(f"  Actions: {result['total_actions']}\n")
            f.write(f"  Duration: {result['duration_seconds']:.1f}s\n")
            f.write(f"  Actions/sec: {result['actions_per_second']:.3f}\n")
            f.write(f"  Induction actions: {result['induction_actions']}\n")
            f.write(f"  Has induction: {result['has_induction']}\n")
            f.write(f"  First action: {result['first_action']}\n")
            f.write(f"  Last action: {result['last_action']}\n\n")
        
        f.write("A2 LOGS SAMPLE (15 files):\n")
        f.write("-" * 30 + "\n")
        
        # Calculate averages for a2_logs
        valid_a2_results = [r for r in a2_results if 'error' not in r]
        if valid_a2_results:
            avg_actions = sum(r['total_actions'] for r in valid_a2_results) / len(valid_a2_results)
            avg_duration = sum(r['duration_seconds'] for r in valid_a2_results) / len(valid_a2_results)
            avg_actions_per_sec = sum(r['actions_per_second'] for r in valid_a2_results) / len(valid_a2_results)
            total_induction_actions = sum(r['induction_actions'] for r in valid_a2_results)
            files_with_induction = sum(1 for r in valid_a2_results if r['has_induction'])
            
            f.write(f"Average actions per file: {avg_actions:.1f}\n")
            f.write(f"Average duration per file: {avg_duration:.1f}s\n")
            f.write(f"Average actions/sec: {avg_actions_per_sec:.3f}\n")
            f.write(f"Total induction actions: {total_induction_actions}\n")
            f.write(f"Files with induction: {files_with_induction}/{len(valid_a2_results)}\n")
            f.write(f"Average induction actions per file: {total_induction_actions/len(valid_a2_results):.1f}\n\n")
            
            # Show range of values
            action_counts = [r['total_actions'] for r in valid_a2_results]
            duration_counts = [r['duration_seconds'] for r in valid_a2_results]
            induction_counts = [r['induction_actions'] for r in valid_a2_results]
            
            f.write("RANGES:\n")
            f.write(f"  Actions: {min(action_counts)} - {max(action_counts)}\n")
            f.write(f"  Duration: {min(duration_counts):.1f}s - {max(duration_counts):.1f}s\n")
            f.write(f"  Induction actions: {min(induction_counts)} - {max(induction_counts)}\n\n")
            
            # Show session type distribution
            session_types = {}
            for r in valid_a2_results:
                st = r['session_type']
                session_types[st] = session_types.get(st, 0) + 1
            
            f.write("SESSION TYPE DISTRIBUTION:\n")
            for st, count in session_types.items():
                f.write(f"  {st}: {count} files\n")
            f.write("\n")
        
        f.write("KEY DIFFERENCES:\n")
        f.write("-" * 30 + "\n")
        
        if original_results and valid_a2_results:
            orig_avg_actions = sum(r['total_actions'] for r in original_results) / len(original_results)
            orig_avg_duration = sum(r['duration_seconds'] for r in original_results) / len(original_results)
            orig_avg_induction = sum(r['induction_actions'] for r in original_results) / len(original_results)
            
            f.write(f"1. SCALE DIFFERENCE:\n")
            f.write(f"   Original examples: {orig_avg_actions:.1f} actions, {orig_avg_duration:.1f}s duration\n")
            f.write(f"   A2 logs: {avg_actions:.1f} actions, {avg_duration:.1f}s duration\n")
            f.write(f"   A2 logs are {avg_actions/orig_avg_actions:.1f}x larger in actions\n")
            f.write(f"   A2 logs are {avg_duration/orig_avg_duration:.1f}x longer in duration\n\n")
            
            f.write(f"2. INDUCTION PATTERNS:\n")
            f.write(f"   Original examples: {orig_avg_induction:.1f} induction actions per file\n")
            f.write(f"   A2 logs: {total_induction_actions/len(valid_a2_results):.1f} induction actions per file\n")
            f.write(f"   A2 logs have {total_induction_actions/len(valid_a2_results)/orig_avg_induction:.1f}x more induction actions\n\n")
            
            f.write(f"3. ACTIVITY PATTERNS:\n")
            f.write(f"   Original examples: {sum(r['actions_per_second'] for r in original_results)/len(original_results):.3f} actions/sec\n")
            f.write(f"   A2 logs: {avg_actions_per_sec:.3f} actions/sec\n")
            f.write(f"   A2 logs are {avg_actions_per_sec/(sum(r['actions_per_second'] for r in original_results)/len(original_results)):.1f}x more active\n\n")
            
            f.write(f"4. INDUCTION FREQUENCY:\n")
            orig_with_induction = sum(1 for r in original_results if r['has_induction'])
            f.write(f"   Original examples: {orig_with_induction}/{len(original_results)} files have induction\n")
            f.write(f"   A2 logs: {files_with_induction}/{len(valid_a2_results)} files have induction\n")
            f.write(f"   A2 logs have {files_with_induction/len(valid_a2_results)*100:.1f}% files with induction\n")
            f.write(f"   Original examples have {orig_with_induction/len(original_results)*100:.1f}% files with induction\n\n")
    
    # Print summary to console
    print("COMPARISON SUMMARY:")
    if original_results and valid_a2_results:
        orig_avg_actions = sum(r['total_actions'] for r in original_results) / len(original_results)
        orig_avg_duration = sum(r['duration_seconds'] for r in original_results) / len(original_results)
        avg_actions = sum(r['total_actions'] for r in valid_a2_results) / len(valid_a2_results)
        avg_duration = sum(r['duration_seconds'] for r in valid_a2_results) / len(valid_a2_results)
        
        print(f"  Original examples: {orig_avg_actions:.1f} actions, {orig_avg_duration:.1f}s")
        print(f"  A2 logs: {avg_actions:.1f} actions, {avg_duration:.1f}s")
        print(f"  A2 logs are {avg_actions/orig_avg_actions:.1f}x larger")
        print(f"  A2 logs are {avg_duration/orig_avg_duration:.1f}x longer")
        
        orig_with_induction = sum(1 for r in original_results if r['has_induction'])
        files_with_induction = sum(1 for r in valid_a2_results if r['has_induction'])
        print(f"  Induction frequency: {orig_with_induction/len(original_results)*100:.1f}% vs {files_with_induction/len(valid_a2_results)*100:.1f}%")
    
    print(f"\nDetailed comparison saved to: log_pattern_comparison.txt")

if __name__ == "__main__":
    main()
