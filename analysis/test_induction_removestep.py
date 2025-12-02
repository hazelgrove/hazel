#!/usr/bin/env python3
"""
Test script to verify RemoveStep detection in induction exploration paths.

This test creates synthetic log data and verifies that:
1. RemoveStep actions are detected correctly
2. They're matched with AddInduction events
3. The exploration path analysis works correctly
"""

import pandas as pd
import numpy as np
from datetime import datetime, timedelta
from path_extractor import analyze_induction_exploration_paths


def create_test_dataframe():
    """Create a test DataFrame with AddInduction and RemoveStep actions."""
    base_time = datetime(2025, 1, 1, 10, 0, 0)
    
    actions = [
        # Test case 1: AddInduction followed by RemoveStep after 3 actions
        (base_time + timedelta(seconds=0), "AddInduction(((term(Var xs)))"),
        (base_time + timedelta(seconds=1), "CaseUpdate(0(StepUpdate"),
        (base_time + timedelta(seconds=2), "AddAxiomStep(ih"),
        (base_time + timedelta(seconds=3), "RemoveStep"),
        
        # Test case 2: AddInduction followed by multiple RemoveSteps
        (base_time + timedelta(seconds=10), "AddInduction(((term(Var ys)))"),
        (base_time + timedelta(seconds=11), "StepForward(0"),
        (base_time + timedelta(seconds=12), "RemoveStep"),
        (base_time + timedelta(seconds=13), "RemoveStep"),
        
        # Test case 3: AddInduction that is retained (no RemoveStep)
        (base_time + timedelta(seconds=20), "AddInduction(((term(Var zs)))"),
        (base_time + timedelta(seconds=21), "CaseUpdate(0(StepUpdate"),
        (base_time + timedelta(seconds=22), "AddAxiomStep(ih"),
        
        # Test case 4: AddInduction with many actions before RemoveStep
        (base_time + timedelta(seconds=30), "AddInduction(((term(Var ws)))"),
        (base_time + timedelta(seconds=31), "CaseUpdate(0(StepUpdate"),
        (base_time + timedelta(seconds=32), "CaseUpdate(1(StepUpdate"),
        (base_time + timedelta(seconds=33), "AddAxiomStep(ih"),
        (base_time + timedelta(seconds=34), "StepForward(0"),
        (base_time + timedelta(seconds=35), "StepForward(1"),
        (base_time + timedelta(seconds=36), "RemoveStep"),
    ]
    
    df = pd.DataFrame({
        'datetime': [a[0] for a in actions],
        'action_raw': [a[1] for a in actions],
        'timestamp': [(a[0] - base_time).total_seconds() * 1000 for a in actions],
    })
    
    return df


def test_removestep_detection():
    """Test that RemoveStep actions are detected correctly."""
    print("=" * 60)
    print("TEST 1: RemoveStep Detection")
    print("=" * 60)
    
    df = create_test_dataframe()
    
    # Check raw RemoveStep count
    removestep_count = df[df['action_raw'].str.contains('RemoveStep', case=False, na=False)].shape[0]
    print(f"\nRemoveStep actions in test data: {removestep_count}")
    assert removestep_count == 4, f"Expected 4 RemoveStep actions, found {removestep_count}"
    print("✓ Found expected number of RemoveStep actions")
    
    # Check AddInduction count
    addinduction_count = df[df['action_raw'].str.contains('AddInduction', case=False, na=False)].shape[0]
    print(f"AddInduction actions in test data: {addinduction_count}")
    assert addinduction_count == 4, f"Expected 4 AddInduction actions, found {addinduction_count}"
    print("✓ Found expected number of AddInduction actions")


def test_exploration_paths_no_filter():
    """Test exploration path analysis without filtering."""
    print("\n" + "=" * 60)
    print("TEST 2: Exploration Path Analysis (No Filtering)")
    print("=" * 60)
    
    df = create_test_dataframe()
    result = analyze_induction_exploration_paths(df, max_steps_after_add=None, ignore_focus_actions=False)
    
    stats = result['statistics']
    paths = result['exploration_paths']
    
    print(f"\nTotal AddInduction events: {stats['total_add_induction']}")
    print(f"Removed count: {stats['removed_count']}")
    print(f"Retained count: {stats['retained_count']}")
    
    # Should find 4 AddInduction events
    assert stats['total_add_induction'] == 4, f"Expected 4 AddInduction events, found {stats['total_add_induction']}"
    
    # Should find 3 removed (test cases 1, 2, 4) and 1 retained (test case 3)
    assert stats['removed_count'] == 3, f"Expected 3 removed, found {stats['removed_count']}"
    assert stats['retained_count'] == 1, f"Expected 1 retained, found {stats['retained_count']}"
    
    print("\n✓ Correct counts found")
    
    # Check individual paths
    print("\nDetailed path analysis:")
    for i, path in enumerate(paths):
        print(f"\nPath {i+1}:")
        print(f"  Was removed: {path['was_removed']}")
        print(f"  RemoveStep count: {path['removestep_count']}")
        print(f"  Total steps after: {path['total_steps_after']}")
        
        if path['removestep_count'] > 0:
            print(f"  RemoveStep positions: {[r['step_number'] for r in path['removesteps']]}")
    
    # Test case 1: Should have 1 RemoveStep at step 4
    assert paths[0]['removestep_count'] == 1, f"Path 1 should have 1 RemoveStep, found {paths[0]['removestep_count']}"
    assert paths[0]['removesteps'][0]['step_number'] == 4, f"Path 1 RemoveStep should be at step 4, found {paths[0]['removesteps'][0]['step_number']}"
    
    # Test case 2: Should have 2 RemoveSteps at steps 3 and 4
    assert paths[1]['removestep_count'] == 2, f"Path 2 should have 2 RemoveSteps, found {paths[1]['removestep_count']}"
    assert [r['step_number'] for r in paths[1]['removesteps']] == [3, 4], f"Path 2 RemoveSteps should be at steps [3, 4], found {[r['step_number'] for r in paths[1]['removesteps']]}"
    
    # Test case 3: Should be retained (no RemoveStep)
    assert paths[2]['removestep_count'] == 0, f"Path 3 should have 0 RemoveSteps, found {paths[2]['removestep_count']}"
    
    # Test case 4: Should have 1 RemoveStep at step 7
    assert paths[3]['removestep_count'] == 1, f"Path 4 should have 1 RemoveStep, found {paths[3]['removestep_count']}"
    assert paths[3]['removesteps'][0]['step_number'] == 7, f"Path 4 RemoveStep should be at step 7, found {paths[3]['removesteps'][0]['step_number']}"
    
    print("\n✓ All path details correct")
    
    # Check backtracking patterns
    backtrack = result['backtracking_patterns']
    print(f"\nBacktracking patterns:")
    print(f"  Single RemoveStep: {backtrack['single_removestep']}")
    print(f"  Multiple RemoveSteps: {backtrack['multiple_removesteps']}")
    
    assert backtrack['single_removestep'] == 2, f"Expected 2 single RemoveStep cases, found {backtrack['single_removestep']}"
    assert backtrack['multiple_removesteps'] == 1, f"Expected 1 multiple RemoveStep case, found {backtrack['multiple_removesteps']}"
    
    print("✓ Backtracking patterns correct")


def test_with_real_log_file():
    """Test with a real log file if available."""
    print("\n" + "=" * 60)
    print("TEST 3: Real Log File Test")
    print("=" * 60)
    
    try:
        from parser import HazelLogParser
        
        log_file = "a2_logs/submission_349620284.json"
        parser = HazelLogParser()
        df = parser.parse_file(log_file)
        
        print(f"\nLoaded {log_file}: {len(df)} log entries")
        
        # Check for RemoveStep in raw data
        removestep_raw = df[df['action_raw'].str.contains('RemoveStep', case=False, na=False)]
        print(f"RemoveStep actions in raw data: {len(removestep_raw)}")
        
        if len(removestep_raw) > 0:
            print("Sample RemoveStep actions:")
            for i, row in removestep_raw.head(3).iterrows():
                action_preview = str(row['action_raw'])[:150]
                print(f"  Index {i}: {action_preview}...")
        
        # Check for AddInduction in raw data
        addinduction_raw = df[df['action_raw'].str.contains('AddInduction', case=False, na=False)]
        print(f"\nAddInduction actions in raw data: {len(addinduction_raw)}")
        
        # Run analysis without filtering
        print("\nRunning analysis WITHOUT filtering...")
        result_no_filter = analyze_induction_exploration_paths(df, max_steps_after_add=None, ignore_focus_actions=False)
        stats_no_filter = result_no_filter['statistics']
        
        print(f"Total AddInduction events: {stats_no_filter['total_add_induction']}")
        print(f"Removed count: {stats_no_filter['removed_count']}")
        print(f"Retained count: {stats_no_filter['retained_count']}")
        print(f"Average total steps: {stats_no_filter['average_total_steps']}")
        
        # Run analysis with filtering
        print("\nRunning analysis WITH filtering...")
        result_filter = analyze_induction_exploration_paths(df, max_steps_after_add=None, ignore_focus_actions=True)
        stats_filter = result_filter['statistics']
        
        print(f"Total AddInduction events: {stats_filter['total_add_induction']}")
        print(f"Removed count: {stats_filter['removed_count']}")
        print(f"Retained count: {stats_filter['retained_count']}")
        print(f"Average total steps: {stats_filter['average_total_steps']}")
        
        # Compare
        if stats_no_filter['removed_count'] > 0:
            print(f"\n✓ Found {stats_no_filter['removed_count']} removed induction(s) without filtering")
            
            # Show details of removed paths
            paths = result_no_filter['exploration_paths']
            removed_paths = [p for p in paths if p['was_removed']]
            print(f"\nDetails of removed paths:")
            for i, path in enumerate(removed_paths[:3]):
                print(f"\n  Removed path {i+1}:")
                print(f"    RemoveStep count: {path['removestep_count']}")
                print(f"    Total steps after AddInduction: {path['total_steps_after']}")
                if path['removestep_count'] > 0:
                    print(f"    First removal at step: {path['first_removal_step']}")
                    print(f"    All RemoveStep positions: {[r['step_number'] for r in path['removesteps']]}")
        else:
            print("\n⚠️  No removed induction found - checking why...")
            
            # Debug: check if RemoveSteps are after AddInduction
            if len(addinduction_raw) > 0 and len(removestep_raw) > 0:
                print("\nChecking positions of AddInduction vs RemoveStep...")
                add_idxs = addinduction_raw.index.tolist()
                remove_idxs = removestep_raw.index.tolist()
                
                print(f"AddInduction indices: {add_idxs[:5]}...")
                print(f"RemoveStep indices: {remove_idxs[:5]}...")
                
                # Check if any RemoveStep is after an AddInduction
                for add_idx in add_idxs[:3]:
                    remove_after = [r for r in remove_idxs if r > add_idx]
                    print(f"  After AddInduction at {add_idx}: {len(remove_after)} RemoveStep(s) found")
                    
        if stats_filter['removed_count'] < stats_no_filter['removed_count']:
            print(f"\n⚠️  Filtering removed {stats_no_filter['removed_count'] - stats_filter['removed_count']} detection(s)")
            print("   Consider using ignore_focus_actions=False")
        
    except FileNotFoundError:
        print(f"\n⚠️  Log file {log_file} not found, skipping real log test")
    except Exception as e:
        print(f"\n⚠️  Error testing real log file: {e}")
        import traceback
        traceback.print_exc()


def main():
    """Run all tests."""
    print("Testing RemoveStep Detection in Induction Exploration Paths")
    print("=" * 60)
    
    try:
        test_removestep_detection()
        test_exploration_paths_no_filter()
        test_with_real_log_file()
        
        print("\n" + "=" * 60)
        print("ALL TESTS PASSED! ✓")
        print("=" * 60)
        
    except AssertionError as e:
        print(f"\n❌ TEST FAILED: {e}")
        import traceback
        traceback.print_exc()
        return 1
    except Exception as e:
        print(f"\n❌ ERROR: {e}")
        import traceback
        traceback.print_exc()
        return 1
    
    return 0


if __name__ == "__main__":
    exit(main())

