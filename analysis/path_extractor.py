#!/usr/bin/env python3
"""
Path Extraction for Hazel Logs

Extract and analyze sequences of operations (paths) to understand student
exploration patterns, proof strategies, and learning behaviors.
"""

import pandas as pd
import numpy as np
from datetime import datetime
from typing import Dict, List, Tuple, Any, Set, Optional


class Path:
    """Represents a sequence of actions forming a path."""
    
    def __init__(self, actions: List[Any], timestamps: List[datetime], 
                 indices: List[int], path_type: str = 'unknown'):
        self.actions = actions
        self.timestamps = timestamps
        self.indices = indices
        self.path_type = path_type
        self._analyze()
    
    def _analyze(self):
        """Analyze path properties."""
        if len(self.timestamps) > 1:
            self.duration_seconds = (self.timestamps[-1] - self.timestamps[0]).total_seconds()
        else:
            self.duration_seconds = 0.0
        
        self.length = len(self.actions)
        self.backtrack_count = self._count_backtracks()
        self.unique_targets = self._extract_unique_targets()
    
    def _count_backtracks(self) -> int:
        """Count backtracking actions (undo, redo, etc.)."""
        backtrack_keywords = ['undo', 'redo', 'back', 'revert']
        count = 0
        
        for action in self.actions:
            action_str = str(action).lower()
            if any(keyword in action_str for keyword in backtrack_keywords):
                count += 1
        
        return count
    
    def _extract_unique_targets(self) -> Set[str]:
        """Extract unique elements or goals interacted with."""
        targets = set()
        
        for action in self.actions:
            # Extract identifiers from actions
            action_str = str(action)
            # Simple heuristic: look for id-like patterns
            import re
            ids = re.findall(r'\b[a-f0-9]{8}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{12}\b', action_str)
            targets.update(ids)
        
        return targets
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert path to dictionary representation."""
        return {
            'length': self.length,
            'duration_seconds': self.duration_seconds,
            'path_type': self.path_type,
            'backtrack_count': self.backtrack_count,
            'unique_target_count': len(self.unique_targets),
            'actions_per_second': self.length / self.duration_seconds if self.duration_seconds > 0 else float('inf'),
            'start_time': self.timestamps[0] if self.timestamps else None,
            'end_time': self.timestamps[-1] if self.timestamps else None,
        }


def _is_meaningful_action(action_str: str) -> bool:
    """
    Check if an action is meaningful (not just clicking around).
    
    Focus actions like StepKindFocus, MakeActive, Move, Select, Resize
    are considered non-meaningful as they represent navigation/clicking
    rather than actual work on the problem.
    
    Args:
        action_str: String representation of the action
        
    Returns:
        True if the action is meaningful, False if it's just focus/navigation
    """
    # Define focus action patterns that are not meaningful
    focus_patterns = [
        'StepKindFocus',
        'MakeActive',      # General activation actions
        'Move(',           # Cursor movement actions
        'Select(',         # Selection actions
        'Resize(',         # Resize actions
        'SwitchExercise',  # Switching between exercises
        'SwitchMode',      # Switching between modes
        'Point((row',      # Cursor positioning
        'Point((col',      # Cursor positioning
        'Here(',           # Navigation to specific locations
        'FinishImportAll', # Initial setup actions
        'Globals(',        # Global settings changes
    ]
    
    # Check if this action matches any focus pattern
    is_focus_action = any(pattern in action_str for pattern in focus_patterns)
    
    return not is_focus_action


def _filter_non_meaningful_actions(df: pd.DataFrame) -> pd.DataFrame:
    """
    Filter out non-meaningful actions (focus/navigation actions).
    
    Args:
        df: DataFrame with action data
        
    Returns:
        DataFrame with only meaningful actions
    """
    if 'action_raw' not in df.columns:
        return df
    
    # Create a mask for meaningful actions only
    mask = pd.Series([False] * len(df), index=df.index)
    
    for idx, row in df.iterrows():
        action_str = str(row.get('action_raw', ''))
        if _is_meaningful_action(action_str):
            mask[idx] = True
    
    # Return filtered DataFrame
    filtered_df = df[mask].copy()
    
    print(f"Ignored {len(df) - len(filtered_df)} non-meaningful actions ({len(df) - len(filtered_df)}/{len(df)} = {(len(df) - len(filtered_df))/len(df)*100:.1f}%)")
    
    return filtered_df


def analyze_action_meaningfulness(df: pd.DataFrame) -> Dict[str, Any]:
    """
    Analyze the proportion of meaningful vs non-meaningful actions.
    
    Args:
        df: DataFrame with action data
        
    Returns:
        Dictionary with meaningfulness analysis
    """
    if 'action_raw' not in df.columns:
        return {'total_actions': 0, 'meaningful_actions': 0, 'non_meaningful_actions': 0}
    
    meaningful_count = 0
    non_meaningful_count = 0
    
    for _, row in df.iterrows():
        action_str = str(row.get('action_raw', ''))
        if _is_meaningful_action(action_str):
            meaningful_count += 1
        else:
            non_meaningful_count += 1
    
    total_actions = len(df)
    
    return {
        'total_actions': total_actions,
        'meaningful_actions': meaningful_count,
        'non_meaningful_actions': non_meaningful_count,
        'meaningful_percentage': (meaningful_count / total_actions * 100) if total_actions > 0 else 0,
        'non_meaningful_percentage': (non_meaningful_count / total_actions * 100) if total_actions > 0 else 0,
    }


def extract_action_sequences(df: pd.DataFrame, window_size: Optional[int] = None, 
                           ignore_focus_actions: bool = True) -> List[Path]:
    """
    Extract sequences of actions as paths.
    
    Args:
        df: DataFrame with action data
        window_size: Fixed window size for sequences (None = use natural breaks)
        ignore_focus_actions: If True, ignore focus actions as meaningful actions
        
    Returns:
        List of Path objects
    """
    df_sorted = df.sort_values('timestamp' if 'timestamp' in df.columns else 'datetime')
    
    # Filter out focus actions if requested
    if ignore_focus_actions:
        df_sorted = _filter_non_meaningful_actions(df_sorted)
    
    if window_size is None:
        # Use natural breaks (e.g., significant time gaps)
        from analysis_utils import segment_by_breaks
        segments = segment_by_breaks(df, break_threshold=10.0)
        
        paths = []
        for start_idx, end_idx in segments:
            segment_df = df_sorted.iloc[start_idx:end_idx + 1]
            
            if len(segment_df) > 0:
                actions = segment_df['action_raw'].tolist() if 'action_raw' in segment_df.columns else []
                timestamps = segment_df['datetime'].tolist() if 'datetime' in segment_df.columns else []
                indices = list(range(start_idx, end_idx + 1))
                
                path = Path(actions, timestamps, indices)
                paths.append(path)
        
        return paths
    else:
        # Fixed window size
        paths = []
        for i in range(0, len(df_sorted), window_size):
            window_df = df_sorted.iloc[i:i + window_size]
            
            if len(window_df) > 0:
                actions = window_df['action_raw'].tolist() if 'action_raw' in window_df.columns else []
                timestamps = window_df['datetime'].tolist() if 'datetime' in window_df.columns else []
                indices = list(range(i, i + len(window_df)))
                
                path = Path(actions, timestamps, indices)
                paths.append(path)
        
        return paths


def identify_exploration_paths(df: pd.DataFrame, ignore_focus_actions: bool = True) -> List[Path]:
    """
    Find undo/redo exploration patterns indicating trial-and-error.
    
    Args:
        df: DataFrame with action data
        ignore_focus_actions: If True, ignore focus actions as meaningful actions
        
    Returns:
        List of Path objects representing exploration sequences
    """
    df_sorted = df.sort_values('timestamp' if 'timestamp' in df.columns else 'datetime')
    
    # Filter out non-meaningful actions if requested
    if ignore_focus_actions:
        df_sorted = _filter_non_meaningful_actions(df_sorted)
    
    exploration_keywords = ['removestep']
    
    exploration_paths = []
    current_path_actions = []
    current_path_timestamps = []
    current_path_indices = []
    
    for idx, row in df_sorted.iterrows():
        action_str = str(row.get('action_raw', '')).lower()
        
        # Check if this is an exploration action
        is_exploration = any(keyword in action_str for keyword in exploration_keywords)
        
        if is_exploration or len(current_path_actions) > 0:
            # Add to current exploration path
            current_path_actions.append(row.get('action_raw'))
            current_path_timestamps.append(row.get('datetime'))
            current_path_indices.append(idx)
            
            # Check if path should end (e.g., long gap after exploration)
            if len(current_path_timestamps) > 1:
                time_since_last = (current_path_timestamps[-1] - current_path_timestamps[-2]).total_seconds()
                if time_since_last > 5.0 and not is_exploration:
                    # End exploration path
                    if len(current_path_actions) > 1:
                        path = Path(current_path_actions, current_path_timestamps, 
                                  current_path_indices, path_type='exploration')
                        exploration_paths.append(path)
                    
                    current_path_actions = []
                    current_path_timestamps = []
                    current_path_indices = []
    
    # Add final path if exists
    if len(current_path_actions) > 1:
        path = Path(current_path_actions, current_path_timestamps, 
                   current_path_indices, path_type='exploration')
        exploration_paths.append(path)
    
    return exploration_paths


def identify_focused_paths(df: pd.DataFrame, focus_threshold: int = 5, 
                          ignore_focus_actions: bool = True) -> List[Path]:
    """
    Find sequences where student focuses on the same element repeatedly.
    
    Args:
        df: DataFrame with action data
        focus_threshold: Minimum consecutive actions on same target
        ignore_focus_actions: If True, ignore focus actions as meaningful actions
        
    Returns:
        List of Path objects representing focused sequences
    """
    df_sorted = df.sort_values('timestamp' if 'timestamp' in df.columns else 'datetime')
    
    # Filter out non-meaningful actions if requested
    if ignore_focus_actions:
        df_sorted = _filter_non_meaningful_actions(df_sorted)
    
    focused_paths = []
    current_target = None
    current_path_actions = []
    current_path_timestamps = []
    current_path_indices = []
    
    for idx, row in df_sorted.iterrows():
        action_str = str(row.get('action_raw', ''))
        
        # Extract target from action (simplified - looks for IDs)
        import re
        ids = re.findall(r'\b[a-f0-9]{8}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{12}\b', action_str)
        target = ids[0] if ids else None
        
        if target == current_target and target is not None:
            # Continue focused path
            current_path_actions.append(row.get('action_raw'))
            current_path_timestamps.append(row.get('datetime'))
            current_path_indices.append(idx)
        else:
            # Target changed - save current path if long enough
            if len(current_path_actions) >= focus_threshold:
                path = Path(current_path_actions, current_path_timestamps,
                           current_path_indices, path_type='focused')
                focused_paths.append(path)
            
            # Start new path
            current_target = target
            current_path_actions = [row.get('action_raw')]
            current_path_timestamps = [row.get('datetime')]
            current_path_indices = [idx]
    
    # Check final path
    if len(current_path_actions) >= focus_threshold:
        path = Path(current_path_actions, current_path_timestamps,
                   current_path_indices, path_type='focused')
        focused_paths.append(path)
    
    return focused_paths


def identify_navigation_paths(df: pd.DataFrame) -> List[Path]:
    """
    Find navigation sequences (switching between different parts).
    
    Args:
        df: DataFrame with action data
        
    Returns:
        List of Path objects representing navigation sequences
    """
    df_sorted = df.sort_values('timestamp' if 'timestamp' in df.columns else 'datetime')
    
    navigation_keywords = ['makeactive', 'switch', 'move', 'navigate']
    
    navigation_paths = []
    current_path_actions = []
    current_path_timestamps = []
    current_path_indices = []
    
    for idx, row in df_sorted.iterrows():
        action_str = str(row.get('action_raw', '')).lower()
        
        is_navigation = any(keyword in action_str for keyword in navigation_keywords)
        
        if is_navigation:
            current_path_actions.append(row.get('action_raw'))
            current_path_timestamps.append(row.get('datetime'))
            current_path_indices.append(idx)
        else:
            # End navigation path
            if len(current_path_actions) > 0:
                path = Path(current_path_actions, current_path_timestamps,
                           current_path_indices, path_type='navigation')
                navigation_paths.append(path)
                
                current_path_actions = []
                current_path_timestamps = []
                current_path_indices = []
    
    # Add final path
    if len(current_path_actions) > 0:
        path = Path(current_path_actions, current_path_timestamps,
                   current_path_indices, path_type='navigation')
        navigation_paths.append(path)
    
    return navigation_paths


def track_proof_progression(df: pd.DataFrame) -> Dict[str, Any]:
    """
    Trace how students progress through proofs.
    
    Args:
        df: DataFrame with action data
        
    Returns:
        Dictionary with proof progression analysis
    """
    df_sorted = df.sort_values('timestamp' if 'timestamp' in df.columns else 'datetime')
    
    # Track different types of actions
    proof_keywords = {
        'induction': ['induction', 'inductive', 'induc'],
        'case_analysis': ['case', 'match', 'split'],
        'hypothesis': ['hypothesis', 'ih', 'assume'],
        'simplification': ['simplify', 'reduce', 'evaluate'],
        'rewrite': ['rewrite', 'replace', 'substitute'],
    }
    
    progression = {
        'phases': [],
        'action_types': {key: 0 for key in proof_keywords.keys()},
        'first_induction_action': None,
        'first_hypothesis_use': None,
    }
    
    for idx, row in df_sorted.iterrows():
        action_str = str(row.get('action_raw', '')).lower()
        
        for proof_type, keywords in proof_keywords.items():
            if any(keyword in action_str for keyword in keywords):
                progression['action_types'][proof_type] += 1
                
                # Track first occurrences
                if proof_type == 'induction' and progression['first_induction_action'] is None:
                    progression['first_induction_action'] = {
                        'timestamp': row.get('datetime'),
                        'action_index': idx,
                    }
                
                if proof_type == 'hypothesis' and progression['first_hypothesis_use'] is None:
                    progression['first_hypothesis_use'] = {
                        'timestamp': row.get('datetime'),
                        'action_index': idx,
                    }
    
    return progression


def find_induction_patterns(df: pd.DataFrame, track_removals: bool = True) -> Dict[str, Any]:
    """
    Identify when and how students apply induction based on Hazel's actual log structure.
    
    Args:
        df: DataFrame with action data
        track_removals: If True, track RemoveStep actions that undo AddInduction events
        
    Returns:
        Dictionary with induction usage patterns
    """
    df_sorted = df.sort_values('timestamp' if 'timestamp' in df.columns else 'datetime')
    
    # Hazel-specific induction action patterns
    induction_action_patterns = [
        'AddInduction',  # When students add induction to a proof
        # 'StepKindFocus(InductionStep',  # When students focus on induction cases
        'InductionStep(CaseUpdate',  # When students modify induction cases
        'AddAxiomStep',  # When students add inductive hypotheses
    ]
    
    induction_events = []
    
    for idx, row in df_sorted.iterrows():
        action_str = str(row.get('action_raw', ''))
        
        # Check for Hazel-specific induction patterns
        is_induction_action = any(pattern in action_str for pattern in induction_action_patterns)
        
        if is_induction_action:
            # Extract context about the induction
            induction_context = _extract_hazel_induction_context(action_str, row.get('action_raw'))
            
            event_data = {
                'timestamp': row.get('datetime'),
                'action_index': idx,
                'action': row.get('action_raw'),
                'context': induction_context,
            }
            
            # If tracking removals and this is an AddInduction, find matching RemoveStep
            if track_removals and induction_context.get('induction_type') == 'add_induction':
                matching_removestep = _find_matching_removestep(df_sorted, idx)
                event_data['removed_by'] = matching_removestep
                event_data['was_removed'] = matching_removestep is not None
                if matching_removestep:
                    event_data['retention_duration'] = matching_removestep.get('time_since_add')
                else:
                    event_data['retention_duration'] = None  # Never removed
            else:
                event_data['removed_by'] = None
                event_data['was_removed'] = False
                event_data['retention_duration'] = None
            
            induction_events.append(event_data)
    
    # Analyze patterns
    analysis = {
        'total_induction_actions': len(induction_events),
        'induction_events': induction_events,
        'time_to_first_induction': None,
        'average_time_between_inductions': None,
        'induction_targets': _analyze_hazel_induction_targets(induction_events),
        'induction_success_patterns': _analyze_hazel_induction_success(df_sorted, induction_events),
        'session_type': _classify_session_type(df_sorted),
    }
    
    # Add removal statistics if tracking removals
    if track_removals:
        add_induction_events = [e for e in induction_events if e.get('context', {}).get('induction_type') == 'add_induction']
        removed_count = sum(1 for e in add_induction_events if e.get('was_removed', False))
        retention_durations = [e.get('retention_duration') for e in add_induction_events if e.get('retention_duration') is not None]
        
        analysis['induction_removal_stats'] = {
            'total_add_induction': len(add_induction_events),
            'removed_count': removed_count,
            'retained_count': len(add_induction_events) - removed_count,
            'removal_rate': removed_count / len(add_induction_events) if add_induction_events else 0,
            'retention_rate': (len(add_induction_events) - removed_count) / len(add_induction_events) if add_induction_events else 0,
            'average_retention_duration': np.mean(retention_durations) if retention_durations else None,
        }
    
    if len(induction_events) > 0:
        first_action_time = df_sorted.iloc[0].get('datetime')
        first_induction_time = induction_events[0]['timestamp']
        
        analysis['time_to_first_induction'] = (first_induction_time - first_action_time).total_seconds()
    
    if len(induction_events) > 1:
        time_diffs = []
        for i in range(1, len(induction_events)):
            diff = (induction_events[i]['timestamp'] - induction_events[i-1]['timestamp']).total_seconds()
            time_diffs.append(diff)
        
        analysis['average_time_between_inductions'] = np.mean(time_diffs)
    
    return analysis


def track_induction_add_remove_sequences(df: pd.DataFrame, 
                                        ignore_focus_actions: bool = True) -> Dict[str, Any]:
    """
    Track sequences where AddInduction is followed by RemoveStep actions.
    
    This function identifies when students add induction and then remove it,
    enabling analysis of induction usage and undoing behavior.
    
    Args:
        df: DataFrame with action data
        ignore_focus_actions: If True, filter out focus actions when analyzing
        
    Returns:
        Dictionary containing:
        - sequences: List of induction sequences (each with AddInduction event and 
                     subsequent RemoveStep if any)
        - statistics: Total AddInduction events, how many were removed, retention rate
        - timing_analysis: Time between add and remove, distribution of retention durations
        - pattern_categorization: immediate undo (<5s), delayed undo (5-60s), 
                                   retained (>60s or never removed)
    """
    df_sorted = df.sort_values('timestamp' if 'timestamp' in df.columns else 'datetime')
    
    # Filter out focus actions if requested
    if ignore_focus_actions:
        df_sorted = _filter_non_meaningful_actions(df_sorted)
    
    # Find all AddInduction events
    add_induction_events = []
    
    for idx, row in df_sorted.iterrows():
        action_str = str(row.get('action_raw', ''))
        
        if 'AddInduction' in action_str:
            induction_context = _extract_hazel_induction_context(action_str, row.get('action_raw'))
            
            add_induction_events.append({
                'timestamp': row.get('datetime'),
                'action_index': idx,
                'action': row.get('action_raw'),
                'context': induction_context,
            })
    
    # Find matching RemoveStep for each AddInduction
    sequences = []
    
    for add_event in add_induction_events:
        matching_removestep = _find_matching_removestep(
            df_sorted, 
            add_event['action_index'],
            ignore_focus_actions=ignore_focus_actions
        )
        
        retention_duration = None
        if matching_removestep:
            retention_duration = matching_removestep.get('time_since_add')
        
        sequence = {
            'add_induction': add_event,
            'remove_step': matching_removestep,
            'was_removed': matching_removestep is not None,
            'retention_duration': retention_duration,
        }
        
        # Categorize the pattern
        if matching_removestep is None:
            sequence['pattern'] = 'retained'
        elif retention_duration is not None:
            if retention_duration < 5.0:
                sequence['pattern'] = 'immediate_undo'
            elif retention_duration < 60.0:
                sequence['pattern'] = 'delayed_undo'
            else:
                sequence['pattern'] = 'retained_then_removed'
        else:
            sequence['pattern'] = 'removed_unknown_time'
        
        sequences.append(sequence)
    
    # Calculate statistics
    total_add = len(sequences)
    removed_count = sum(1 for s in sequences if s['was_removed'])
    retained_count = total_add - removed_count
    
    # Timing analysis
    retention_durations = [s['retention_duration'] for s in sequences if s['retention_duration'] is not None]
    
    # Pattern categorization counts
    pattern_counts = {}
    for seq in sequences:
        pattern = seq['pattern']
        pattern_counts[pattern] = pattern_counts.get(pattern, 0) + 1
    
    timing_stats = {}
    if retention_durations:
        timing_stats = {
            'mean_retention': np.mean(retention_durations),
            'median_retention': np.median(retention_durations),
            'min_retention': np.min(retention_durations),
            'max_retention': np.max(retention_durations),
            'std_retention': np.std(retention_durations) if len(retention_durations) > 1 else 0,
        }
    
    return {
        'sequences': sequences,
        'statistics': {
            'total_add_induction': total_add,
            'removed_count': removed_count,
            'retained_count': retained_count,
            'removal_rate': removed_count / total_add if total_add > 0 else 0,
            'retention_rate': retained_count / total_add if total_add > 0 else 0,
        },
        'timing_analysis': timing_stats,
        'pattern_categorization': {
            'counts': pattern_counts,
            'immediate_undo_count': pattern_counts.get('immediate_undo', 0),
            'delayed_undo_count': pattern_counts.get('delayed_undo', 0),
            'retained_count': pattern_counts.get('retained', 0),
            'retained_then_removed_count': pattern_counts.get('retained_then_removed', 0),
        },
    }


def analyze_induction_retention_patterns(df: pd.DataFrame, 
                                         ignore_focus_actions: bool = True) -> Dict[str, Any]:
    """
    Analyze induction retention patterns more deeply.
    
    This function groups sequences by retention duration categories, tracks what
    happens between AddInduction and RemoveStep (case modifications, axiom additions, etc.),
    and correlates retention with success indicators.
    
    Args:
        df: DataFrame with action data
        ignore_focus_actions: If True, filter out focus actions when analyzing
        
    Returns:
        Dictionary with retention pattern analysis including:
        - Grouped sequences by retention categories
        - Actions between AddInduction and RemoveStep
        - Correlation with success indicators
    """
    # Get the sequences from track_induction_add_remove_sequences
    sequences_data = track_induction_add_remove_sequences(df, ignore_focus_actions)
    sequences = sequences_data['sequences']
    
    if not sequences:
        return {
            'retention_groups': {},
            'intervening_actions_analysis': {},
            'success_correlation': {},
            'total_sequences': 0,
        }
    
    df_sorted = df.sort_values('timestamp' if 'timestamp' in df.columns else 'datetime')
    
    # Apply same filtering as track_induction_add_remove_sequences to ensure indices align
    if ignore_focus_actions:
        df_sorted = _filter_non_meaningful_actions(df_sorted)
    
    # Group sequences by retention duration categories
    retention_groups = {
        'immediate_undo': [],  # < 5 seconds
        'delayed_undo': [],    # 5-60 seconds
        'retained_then_removed': [],  # > 60 seconds
        'retained': [],        # Never removed
    }
    
    for seq in sequences:
        pattern = seq.get('pattern', 'unknown')
        if pattern in retention_groups:
            retention_groups[pattern].append(seq)
        else:
            # Handle other patterns
            if seq.get('was_removed'):
                retention_groups['retained_then_removed'].append(seq)
            else:
                retention_groups['retained'].append(seq)
    
    # Analyze intervening actions between AddInduction and RemoveStep
    intervening_actions_analysis = {
        'case_modifications': [],
        'axiom_additions': [],
        'step_forwards': [],
        'other_actions': [],
    }
    
    for seq in sequences:
        if not seq.get('was_removed'):
            continue
        
        add_idx = seq['add_induction']['action_index']
        remove_idx = seq['remove_step']['action_index']
        
        # Get actions between AddInduction and RemoveStep
        intervening_df = df_sorted.iloc[add_idx + 1:remove_idx]
        
        case_mod_count = 0
        axiom_count = 0
        step_forward_count = 0
        other_count = 0
        
        for _, row in intervening_df.iterrows():
            action_str = str(row.get('action_raw', '')).lower()
            
            if 'caseupdate' in action_str or 'inductionstep(case' in action_str:
                case_mod_count += 1
            elif 'addaxiomstep' in action_str:
                axiom_count += 1
            elif 'stepforward' in action_str:
                step_forward_count += 1
            else:
                other_count += 1
        
        intervening_actions_analysis['case_modifications'].append({
            'sequence_index': len(intervening_actions_analysis['case_modifications']),
            'count': case_mod_count,
            'retention_duration': seq.get('retention_duration'),
            'pattern': seq.get('pattern'),
        })
        intervening_actions_analysis['axiom_additions'].append({
            'sequence_index': len(intervening_actions_analysis['axiom_additions']),
            'count': axiom_count,
            'retention_duration': seq.get('retention_duration'),
            'pattern': seq.get('pattern'),
        })
        intervening_actions_analysis['step_forwards'].append({
            'sequence_index': len(intervening_actions_analysis['step_forwards']),
            'count': step_forward_count,
            'retention_duration': seq.get('retention_duration'),
            'pattern': seq.get('pattern'),
        })
        intervening_actions_analysis['other_actions'].append({
            'sequence_index': len(intervening_actions_analysis['other_actions']),
            'count': other_count,
            'retention_duration': seq.get('retention_duration'),
            'pattern': seq.get('pattern'),
        })
    
    # Calculate statistics for intervening actions
    intervening_stats = {}
    for action_type, action_list in intervening_actions_analysis.items():
        if action_list:
            counts = [a['count'] for a in action_list]
            intervening_stats[action_type] = {
                'mean_count': np.mean(counts),
                'median_count': np.median(counts),
                'max_count': np.max(counts),
                'total_sequences_with_actions': sum(1 for c in counts if c > 0),
            }
    
    # Correlate retention with success indicators
    success_indicators = ['StepForward', 'NextStep', 'AddAxiomStep', 'Reflexive']
    
    success_correlation = {
        'by_pattern': {},
        'by_retention_duration': {
            'short_retention': {'success_count': 0, 'total_count': 0},  # < 10s
            'medium_retention': {'success_count': 0, 'total_count': 0},  # 10-60s
            'long_retention': {'success_count': 0, 'total_count': 0},  # > 60s
        },
    }
    
    for seq in sequences:
        pattern = seq.get('pattern', 'unknown')
        
        # Initialize pattern if needed
        if pattern not in success_correlation['by_pattern']:
            success_correlation['by_pattern'][pattern] = {
                'success_count': 0,
                'total_count': 0,
            }
        
        add_idx = seq['add_induction']['action_index']
        
        # Look at actions after AddInduction (up to 30 actions or until RemoveStep)
        end_idx = seq['remove_step']['action_index'] if seq.get('was_removed') else min(len(df_sorted), add_idx + 31)
        post_actions = df_sorted.iloc[add_idx + 1:end_idx]
        
        # Check for success indicators
        has_success = False
        for _, row in post_actions.iterrows():
            action_str = str(row.get('action_raw', '')).lower()
            if any(indicator.lower() in action_str for indicator in success_indicators):
                has_success = True
                break
        
        # Update pattern-based correlation
        success_correlation['by_pattern'][pattern]['total_count'] += 1
        if has_success:
            success_correlation['by_pattern'][pattern]['success_count'] += 1
        
        # Update duration-based correlation (only for removed sequences)
        if seq.get('was_removed') and seq.get('retention_duration') is not None:
            duration = seq['retention_duration']
            if duration < 10.0:
                category = 'short_retention'
            elif duration < 60.0:
                category = 'medium_retention'
            else:
                category = 'long_retention'
            
            success_correlation['by_retention_duration'][category]['total_count'] += 1
            if has_success:
                success_correlation['by_retention_duration'][category]['success_count'] += 1
    
    # Calculate success rates
    for pattern_data in success_correlation['by_pattern'].values():
        total = pattern_data['total_count']
        if total > 0:
            pattern_data['success_rate'] = pattern_data['success_count'] / total
        else:
            pattern_data['success_rate'] = 0
    
    for duration_data in success_correlation['by_retention_duration'].values():
        total = duration_data['total_count']
        if total > 0:
            duration_data['success_rate'] = duration_data['success_count'] / total
        else:
            duration_data['success_rate'] = 0
    
    # Summary of retention groups
    retention_groups_summary = {}
    for group_name, group_sequences in retention_groups.items():
        retention_groups_summary[group_name] = {
            'count': len(group_sequences),
            'percentage': len(group_sequences) / len(sequences) * 100 if sequences else 0,
        }
        if group_sequences:
            durations = [s.get('retention_duration') for s in group_sequences if s.get('retention_duration') is not None]
            if durations:
                retention_groups_summary[group_name]['mean_duration'] = np.mean(durations)
                retention_groups_summary[group_name]['median_duration'] = np.median(durations)
    
    return {
        'retention_groups': retention_groups_summary,
        'retention_groups_detailed': {k: len(v) for k, v in retention_groups.items()},
        'intervening_actions_analysis': {
            'detailed': intervening_actions_analysis,
            'statistics': intervening_stats,
        },
        'success_correlation': success_correlation,
        'total_sequences': len(sequences),
    }


def _extract_hazel_induction_context(action_str: str, action_raw: Any) -> Dict[str, Any]:
    """
    Extract context about Hazel induction actions.
    
    Args:
        action_str: String representation of action
        action_raw: Raw action data
        
    Returns:
        Dictionary with Hazel induction context information
    """
    import re
    
    context = {
        'induction_type': 'unknown',
        'action_subtype': 'unknown',
        'target_term': None,
        'case_number': None,
        'has_axiom': False,
        'axiom_name': None,
    }
    
    # Determine the type of induction action
    if 'AddInduction' in action_str:
        context['induction_type'] = 'add_induction'
        # Try to extract the term being inducted on
        term_match = re.search(r'AddInduction\(\(\(\(term\(Var\s+(\w+)\)', action_str)
        if term_match:
            context['target_term'] = term_match.group(1)
    
    elif 'StepKindFocus(InductionStep' in action_str:
        context['induction_type'] = 'focus_case'
        # Extract case number
        case_match = re.search(r'Case\s+(\d+)', action_str)
        if case_match:
            context['case_number'] = int(case_match.group(1))
    
    elif 'InductionStep(CaseUpdate' in action_str:
        context['induction_type'] = 'modify_case'
        # Extract case number
        case_match = re.search(r'CaseUpdate\s+(\d+)', action_str)
        if case_match:
            context['case_number'] = int(case_match.group(1))
    
    elif 'AddAxiomStep' in action_str:
        context['induction_type'] = 'add_axiom'
        context['has_axiom'] = True
        # Extract axiom name (like 'ih' for inductive hypothesis)
        axiom_match = re.search(r'AddAxiomStep\s+(\w+)', action_str)
        if axiom_match:
            context['axiom_name'] = axiom_match.group(1)
    
    return context


def _find_matching_removestep(df_sorted: pd.DataFrame, add_induction_idx: int, 
                              max_search_ahead: int = 100, 
                              ignore_focus_actions: bool = False) -> Optional[Dict[str, Any]]:
    """
    Find the next RemoveStep action after an AddInduction, if any.
    
    This function looks forward from the AddInduction action to find a matching
    RemoveStep. It considers that RemoveStep can remove any step type, so we
    check that no other step creation actions occurred between AddInduction
    and RemoveStep.
    
    Args:
        df_sorted: Sorted DataFrame with action data
        add_induction_idx: Index of the AddInduction action
        max_search_ahead: Maximum number of actions to search forward (default 100)
        ignore_focus_actions: If True, filter out focus actions when checking intervening actions
        
    Returns:
        Dictionary with RemoveStep event info if found, None otherwise.
        Contains: 'timestamp', 'action_index', 'action', 'actions_between', 'time_since_add'
    """
    # Actions that create new steps (would prevent RemoveStep from removing the induction)
    step_creation_actions = ['AddInduction', 'AddForall', 'AddAxiomStep', 'AddAlgebriteStep', 'StepForward']
    
    # Start searching from the action after AddInduction
    start_idx = add_induction_idx + 1
    end_idx = min(len(df_sorted), start_idx + max_search_ahead)
    
    actions_between = []
    
    for idx in range(start_idx, end_idx):
        row = df_sorted.iloc[idx]
        action_str = str(row.get('action_raw', ''))
        action_lower = action_str.lower()
        
        # Check if another step creation action occurred (breaks the chain)
        # This should be checked BEFORE checking for RemoveStep
        is_step_creation = any(step_action.lower() in action_lower for step_action in step_creation_actions)
        if is_step_creation and 'AddInduction' not in action_str:
            # Another step was created (not AddInduction), so any subsequent RemoveStep 
            # won't be removing our induction
            break
        
        # Check if this is a RemoveStep
        if 'RemoveStep' in action_str:
            # Found a RemoveStep, check if any step creation occurred in actions_between
            had_step_creation = False
            for action_item in actions_between:
                between_action_str = str(action_item.get('action', ''))
                between_action_lower = between_action_str.lower()
                if any(step_action.lower() in between_action_lower 
                       for step_action in step_creation_actions 
                       if 'AddInduction' not in between_action_str):
                    had_step_creation = True
                    break
            
            if not had_step_creation:
                # This RemoveStep likely removed our induction
                add_induction_time = df_sorted.iloc[add_induction_idx].get('datetime')
                remove_step_time = row.get('datetime')
                time_since_add = (remove_step_time - add_induction_time).total_seconds() if remove_step_time and add_induction_time else None
                
                return {
                    'timestamp': remove_step_time,
                    'action_index': idx,
                    'action': row.get('action_raw'),
                    'actions_between': len(actions_between),
                    'time_since_add': time_since_add,
                }
        
        # Track actions between AddInduction and RemoveStep
        # Filter out focus actions if requested
        if ignore_focus_actions:
            if _is_meaningful_action(action_str):
                actions_between.append({
                    'action_index': idx,
                    'action': row.get('action_raw'),
                    'timestamp': row.get('datetime'),
                })
        else:
            actions_between.append({
                'action_index': idx,
                'action': row.get('action_raw'),
                'timestamp': row.get('datetime'),
            })
    
    return None


def _analyze_hazel_induction_targets(induction_events: List[Dict[str, Any]]) -> Dict[str, Any]:
    """
    Analyze what terms students attempt induction on in Hazel.
    
    Args:
        induction_events: List of induction event dictionaries
        
    Returns:
        Dictionary with target analysis
    """
    target_counts = {}
    type_counts = {}
    case_counts = {}
    axiom_counts = {}
    
    for event in induction_events:
        context = event.get('context', {})
        
        # Count induction types
        induction_type = context.get('induction_type', 'unknown')
        type_counts[induction_type] = type_counts.get(induction_type, 0) + 1
        
        # Count target terms
        target_term = context.get('target_term')
        if target_term:
            target_counts[target_term] = target_counts.get(target_term, 0) + 1
        
        # Count case numbers
        case_number = context.get('case_number')
        if case_number is not None:
            case_counts[case_number] = case_counts.get(case_number, 0) + 1
        
        # Count axiom names
        axiom_name = context.get('axiom_name')
        if axiom_name:
            axiom_counts[axiom_name] = axiom_counts.get(axiom_name, 0) + 1
    
    return {
        'induction_type_distribution': type_counts,
        'target_term_distribution': target_counts,
        'case_number_distribution': case_counts,
        'axiom_name_distribution': axiom_counts,
        'most_common_type': max(type_counts.items(), key=lambda x: x[1])[0] if type_counts else None,
        'most_common_target': max(target_counts.items(), key=lambda x: x[1])[0] if target_counts else None,
    }


def _analyze_hazel_induction_success(df_sorted: pd.DataFrame, induction_events: List[Dict[str, Any]]) -> Dict[str, Any]:
    """
    Analyze whether induction attempts lead to successful progress in Hazel.
    
    Args:
        df_sorted: Sorted DataFrame with action data
        induction_events: List of induction event dictionaries
        
    Returns:
        Dictionary with success analysis
    """
    # Hazel-specific success indicators
    success_indicators = ['StepForward', 'NextStep', 'AddAxiomStep', 'Reflexive']
    failure_indicators = ['Undo', 'Back', 'Revert', 'Destruct', 'RemoveStep']
    
    induction_success = []
    
    for event in induction_events:
        event_idx = event['action_index']
        
        # Look at actions in the next 20 actions after induction
        window_size = min(20, len(df_sorted) - event_idx - 1)
        if window_size <= 0:
            continue
        
        post_induction_actions = df_sorted.iloc[event_idx + 1:event_idx + 1 + window_size]
        
        success_count = 0
        failure_count = 0
        
        for _, action_row in post_induction_actions.iterrows():
            action_str = str(action_row.get('action_raw', '')).lower()
            
            if any(indicator.lower() in action_str for indicator in success_indicators):
                success_count += 1
            elif any(indicator.lower() in action_str for indicator in failure_indicators):
                failure_count += 1
        
        # Determine if this induction attempt was successful
        success_ratio = success_count / (success_count + failure_count) if (success_count + failure_count) > 0 else 0
        is_successful = success_ratio > 0.5 and success_count > 0
        
        induction_success.append({
            'event_index': event_idx,
            'success_count': success_count,
            'failure_count': failure_count,
            'success_ratio': success_ratio,
            'is_successful': is_successful,
        })
    
    if not induction_success:
        return {'total_inductions': 0, 'successful_inductions': 0, 'success_rate': 0}
    
    successful_count = sum(1 for s in induction_success if s['is_successful'])
    success_rate = successful_count / len(induction_success)
    
    return {
        'total_inductions': len(induction_success),
        'successful_inductions': successful_count,
        'success_rate': success_rate,
        'induction_details': induction_success,
    }


def _classify_session_type(df_sorted: pd.DataFrame) -> str:
    """
    Classify the type of student session based on action patterns.
    
    Args:
        df_sorted: Sorted DataFrame with action data
        
    Returns:
        Session type classification
    """
    # Count different types of actions
    theorem_actions = 0
    implementation_actions = 0
    stepper_actions = 0
    
    for _, row in df_sorted.iterrows():
        action_str = str(row.get('action_raw', '')).lower()
        
        if 'theorem' in action_str or 'induction' in action_str:
            theorem_actions += 1
        elif 'perform' in action_str or 'insert' in action_str or 'destruct' in action_str:
            implementation_actions += 1
        elif 'stepper' in action_str:
            stepper_actions += 1
    
    total_actions = len(df_sorted)
    
    # Classify based on proportions
    if theorem_actions / total_actions > 0.1:
        return 'theorem_focused'
    elif implementation_actions / total_actions > 0.3:
        return 'implementation_focused'
    elif stepper_actions / total_actions > 0.05:
        return 'stepper_focused'
    else:
        return 'mixed'


def _extract_induction_context(action_str: str, action_raw: Any) -> Dict[str, Any]:
    """
    Extract context about what the student is trying to do induction on.
    
    Args:
        action_str: Lowercase string representation of action
        action_raw: Raw action data
        
    Returns:
        Dictionary with induction context information
    """
    import re
    
    context = {
        'induction_type': 'unknown',
        'target_term': None,
        'target_type': None,
        'has_hypothesis': False,
        'has_base_case': False,
        'has_step_case': False,
    }
    
    # Determine induction type
    if 'nat_ind' in action_str or 'nat_rec' in action_str:
        context['induction_type'] = 'natural_number'
    elif 'list_ind' in action_str or 'list_rec' in action_str:
        context['induction_type'] = 'list'
    elif 'tree_ind' in action_str or 'tree_rec' in action_str:
        context['induction_type'] = 'tree'
    elif 'struct_ind' in action_str or 'struct_rec' in action_str:
        context['induction_type'] = 'structural'
    elif 'well_founded' in action_str or 'wf_ind' in action_str:
        context['induction_type'] = 'well_founded'
    else:
        context['induction_type'] = 'general'
    
    # Try to extract target term/type from action
    if isinstance(action_raw, list) and len(action_raw) > 1:
        # Look for identifiers or type information
        for item in action_raw[1:]:
            if isinstance(item, str):
                # Look for common patterns
                if re.match(r'^[a-zA-Z_][a-zA-Z0-9_]*$', item):
                    context['target_term'] = item
                elif 'nat' in item.lower():
                    context['target_type'] = 'natural_number'
                elif 'list' in item.lower():
                    context['target_type'] = 'list'
                elif 'tree' in item.lower():
                    context['target_type'] = 'tree'
    
    # Check for hypothesis indicators
    hypothesis_keywords = ['ih', 'hypothesis', 'assume', 'inductive_hypothesis']
    context['has_hypothesis'] = any(kw in action_str for kw in hypothesis_keywords)
    
    # Check for case analysis indicators
    case_keywords = ['case', 'base', 'step', 'split']
    context['has_base_case'] = any(kw in action_str for kw in ['base', 'zero', 'nil', 'empty'])
    context['has_step_case'] = any(kw in action_str for kw in ['step', 'succ', 'cons', 'node'])
    
    return context


def _analyze_induction_targets(induction_events: List[Dict[str, Any]]) -> Dict[str, Any]:
    """
    Analyze what types of terms students attempt induction on.
    
    Args:
        induction_events: List of induction event dictionaries
        
    Returns:
        Dictionary with target analysis
    """
    target_counts = {}
    type_counts = {}
    
    for event in induction_events:
        context = event.get('context', {})
        
        # Count induction types
        induction_type = context.get('induction_type', 'unknown')
        type_counts[induction_type] = type_counts.get(induction_type, 0) + 1
        
        # Count target terms
        target_term = context.get('target_term')
        if target_term:
            target_counts[target_term] = target_counts.get(target_term, 0) + 1
    
    return {
        'induction_type_distribution': type_counts,
        'target_term_distribution': target_counts,
        'most_common_type': max(type_counts.items(), key=lambda x: x[1])[0] if type_counts else None,
        'most_common_target': max(target_counts.items(), key=lambda x: x[1])[0] if target_counts else None,
    }


def _analyze_induction_success(df_sorted: pd.DataFrame, induction_events: List[Dict[str, Any]]) -> Dict[str, Any]:
    """
    Analyze whether induction attempts lead to successful progress.
    
    Args:
        df_sorted: Sorted DataFrame with action data
        induction_events: List of induction event dictionaries
        
    Returns:
        Dictionary with success analysis
    """
    success_indicators = ['qed', 'done', 'complete', 'proved', 'finished']
    failure_indicators = ['undo', 'back', 'revert', 'destruct', 'abandon']
    
    induction_success = []
    
    for event in induction_events:
        event_idx = event['action_index']
        
        # Look at actions in the next 20 actions after induction
        window_size = min(20, len(df_sorted) - event_idx - 1)
        if window_size <= 0:
            continue
        
        post_induction_actions = df_sorted.iloc[event_idx + 1:event_idx + 1 + window_size]
        
        success_count = 0
        failure_count = 0
        
        for _, action_row in post_induction_actions.iterrows():
            action_str = str(action_row.get('action_raw', '')).lower()
            
            if any(indicator in action_str for indicator in success_indicators):
                success_count += 1
            elif any(indicator in action_str for indicator in failure_indicators):
                failure_count += 1
        
        # Determine if this induction attempt was successful
        success_ratio = success_count / (success_count + failure_count) if (success_count + failure_count) > 0 else 0
        is_successful = success_ratio > 0.5 and success_count > 0
        
        induction_success.append({
            'event_index': event_idx,
            'success_count': success_count,
            'failure_count': failure_count,
            'success_ratio': success_ratio,
            'is_successful': is_successful,
        })
    
    if not induction_success:
        return {'total_inductions': 0, 'successful_inductions': 0, 'success_rate': 0}
    
    successful_count = sum(1 for s in induction_success if s['is_successful'])
    success_rate = successful_count / len(induction_success)
    
    return {
        'total_inductions': len(induction_success),
        'successful_inductions': successful_count,
        'success_rate': success_rate,
        'induction_details': induction_success,
    }


def analyze_induction_timing_patterns(df: pd.DataFrame) -> Dict[str, Any]:
    """
    Analyze when students use induction in relation to other proof strategies.
    
    Args:
        df: DataFrame with action data
        
    Returns:
        Dictionary with timing pattern analysis
    """
    df_sorted = df.sort_values('timestamp' if 'timestamp' in df.columns else 'datetime')
    
    # Get induction events
    induction_analysis = find_induction_patterns(df)
    induction_events = induction_analysis['induction_events']
    
    if not induction_events:
        return {
            'total_inductions': 0,
            'induction_timing': 'no_induction',
            'pre_induction_actions': [],
            'post_induction_actions': [],
        }
    
    # Analyze timing patterns
    timing_patterns = {
        'early_induction': 0,  # Induction in first 25% of session
        'mid_induction': 0,    # Induction in middle 50% of session  
        'late_induction': 0,   # Induction in last 25% of session
    }
    
    total_actions = len(df_sorted)
    quarter_point = total_actions // 4
    three_quarter_point = 3 * total_actions // 4
    
    for event in induction_events:
        action_idx = event['action_index']
        
        if action_idx < quarter_point:
            timing_patterns['early_induction'] += 1
        elif action_idx < three_quarter_point:
            timing_patterns['mid_induction'] += 1
        else:
            timing_patterns['late_induction'] += 1
    
    # Analyze what students do before and after induction
    pre_induction_actions = []
    post_induction_actions = []
    
    for event in induction_events:
        event_idx = event['action_index']
        
        # Look at 10 actions before induction
        pre_start = max(0, event_idx - 10)
        pre_actions = df_sorted.iloc[pre_start:event_idx]['type'].tolist()
        pre_induction_actions.extend(pre_actions)
        
        # Look at 10 actions after induction
        post_end = min(len(df_sorted), event_idx + 11)
        post_actions = df_sorted.iloc[event_idx + 1:post_end]['type'].tolist()
        post_induction_actions.extend(post_actions)
    
    # Count action types
    from collections import Counter
    pre_action_counts = Counter(pre_induction_actions)
    post_action_counts = Counter(post_induction_actions)
    
    return {
        'total_inductions': len(induction_events),
        'timing_distribution': timing_patterns,
        'pre_induction_action_counts': dict(pre_action_counts),
        'post_induction_action_counts': dict(post_action_counts),
        'most_common_pre_action': pre_action_counts.most_common(1)[0] if pre_action_counts else None,
        'most_common_post_action': post_action_counts.most_common(1)[0] if post_action_counts else None,
        'induction_events': induction_events,
    }


def track_induction_progression(df: pd.DataFrame) -> Dict[str, Any]:
    """
    Track how students progress through induction proofs step by step.
    
    Args:
        df: DataFrame with action data
        
    Returns:
        Dictionary with progression analysis
    """
    df_sorted = df.sort_values('timestamp' if 'timestamp' in df.columns else 'datetime')
    
    # Get induction events
    induction_analysis = find_induction_patterns(df)
    induction_events = induction_analysis['induction_events']
    
    if not induction_events:
        return {'induction_progressions': [], 'common_patterns': {}}
    
    progressions = []
    
    for event in induction_events:
        event_idx = event['action_index']
        
        # Look at the next 30 actions after induction
        window_size = min(30, len(df_sorted) - event_idx - 1)
        if window_size <= 0:
            continue
        
        post_induction_df = df_sorted.iloc[event_idx + 1:event_idx + 1 + window_size]
        
        # Track progression steps
        progression_steps = []
        current_step = {
            'step_type': 'unknown',
            'action_count': 0,
            'actions': [],
        }
        
        step_keywords = {
            'base_case': ['base', 'zero', 'nil', 'empty', 'trivial'],
            'inductive_step': ['step', 'succ', 'cons', 'node', 'inductive'],
            'hypothesis': ['ih', 'hypothesis', 'assume', 'inductive_hypothesis'],
            'simplification': ['simplify', 'reduce', 'evaluate', 'compute'],
            'rewrite': ['rewrite', 'replace', 'substitute', 'apply'],
            'case_split': ['case', 'split', 'destruct', 'match'],
        }
        
        for _, action_row in post_induction_df.iterrows():
            action_str = str(action_row.get('action_raw', '')).lower()
            action_type = action_row.get('type', 'unknown')
            
            # Determine step type
            step_type = 'unknown'
            for step_name, keywords in step_keywords.items():
                if any(kw in action_str for kw in keywords):
                    step_type = step_name
                    break
            
            if step_type != current_step['step_type'] and current_step['action_count'] > 0:
                # Save current step and start new one
                progression_steps.append(current_step.copy())
                current_step = {
                    'step_type': step_type,
                    'action_count': 1,
                    'actions': [action_type],
                }
            else:
                # Continue current step
                current_step['step_type'] = step_type
                current_step['action_count'] += 1
                current_step['actions'].append(action_type)
        
        # Add final step
        if current_step['action_count'] > 0:
            progression_steps.append(current_step)
        
        progressions.append({
            'induction_event': event,
            'progression_steps': progression_steps,
            'total_steps': len(progression_steps),
        })
    
    # Analyze common patterns
    step_sequences = []
    for prog in progressions:
        step_types = [step['step_type'] for step in prog['progression_steps']]
        step_sequences.append(step_types)
    
    # Find common patterns
    from collections import Counter
    pattern_counts = Counter()
    for seq in step_sequences:
        if len(seq) >= 2:
            # Look at 2-step patterns
            for i in range(len(seq) - 1):
                pattern = f"{seq[i]} -> {seq[i+1]}"
                pattern_counts[pattern] += 1
    
    return {
        'induction_progressions': progressions,
        'common_patterns': dict(pattern_counts.most_common(10)),
        'total_progressions': len(progressions),
    }


def classify_path_type(path: Path) -> str:
    """
    Classify a path based on its characteristics.
    
    Args:
        path: Path object to classify
        
    Returns:
        Path type string
    """
    # Already has a type
    if path.path_type != 'unknown':
        return path.path_type
    
    # Classification heuristics
    if path.backtrack_count > path.length * 0.3:
        return 'exploration'
    
    if len(path.unique_targets) == 1 and path.length >= 5:
        return 'focused'
    
    if path.duration_seconds > 0 and path.length / path.duration_seconds > 2.0:
        return 'rapid'
    
    if len(path.unique_targets) > path.length * 0.7:
        return 'navigation'
    
    return 'linear'


def summarize_paths(paths: List[Path]) -> Dict[str, Any]:
    """
    Generate summary statistics for a collection of paths.
    
    Args:
        paths: List of Path objects
        
    Returns:
        Dictionary with path statistics
    """
    if not paths:
        return {'total_paths': 0}
    
    path_dicts = [p.to_dict() for p in paths]
    
    # Group by type
    type_counts = {}
    for p in paths:
        path_type = classify_path_type(p)
        type_counts[path_type] = type_counts.get(path_type, 0) + 1
    
    return {
        'total_paths': len(paths),
        'path_types': type_counts,
        'average_length': np.mean([p.length for p in paths]),
        'average_duration': np.mean([p.duration_seconds for p in paths]),
        'total_backtracks': sum([p.backtrack_count for p in paths]),
        'paths_with_backtracks': sum([1 for p in paths if p.backtrack_count > 0]),
    }


def analyze_induction_exploration_paths(df: pd.DataFrame, 
                                        max_steps_after_add: Optional[int] = None,
                                        ignore_focus_actions: bool = True) -> Dict[str, Any]:
    """
    Analyze exploration paths after AddInduction actions.
    
    This function tracks what happens after students add induction:
    - How many actions they take before removing (if they do)
    - What types of actions occur in the exploration path
    - Patterns of backtracking with RemoveStep
    
    Args:
        df: DataFrame with action data
        max_steps_after_add: Maximum number of actions to analyze after AddInduction 
                             (None = no limit, analyze until end of log or next AddInduction)
        ignore_focus_actions: If True, filter out focus actions when analyzing
        
    Returns:
        Dictionary containing:
        - exploration_paths: List of exploration sequences after each AddInduction
        - statistics: Summary statistics about exploration paths
        - backtracking_patterns: Analysis of RemoveStep usage patterns
    """
    df_sorted = df.sort_values('timestamp' if 'timestamp' in df.columns else 'datetime')
    
    # Filter out focus actions if requested
    if ignore_focus_actions:
        df_sorted = _filter_non_meaningful_actions(df_sorted)
    
    # Reset index for consistent integer indexing
    df_sorted = df_sorted.reset_index(drop=True)
    
    # Find all AddInduction events in the (possibly filtered) dataframe
    add_induction_events = []
    
    for idx, row in df_sorted.iterrows():
        action_str = str(row.get('action_raw', ''))
        
        if 'AddInduction' in action_str:
            add_induction_events.append({
                'timestamp': row.get('datetime'),
                'action_index': idx,  # This is now the position in filtered df
                'action': row.get('action_raw'),
            })
    
    exploration_paths = []
    
    for add_idx_local, add_event in enumerate(add_induction_events):
        add_idx = add_event['action_index']
        add_time = add_event['timestamp']
        
        # Determine end index - either max_steps_after_add, or next AddInduction, or end of log
        if max_steps_after_add is not None:
            end_idx = min(len(df_sorted), add_idx + 1 + max_steps_after_add)
        else:
            # No limit - go until next AddInduction or end of log
            if add_idx_local + 1 < len(add_induction_events):
                next_add_idx = add_induction_events[add_idx_local + 1]['action_index']
                end_idx = next_add_idx
            else:
                end_idx = len(df_sorted)
        
        # Use integer position, not index label
        if add_idx < len(df_sorted) and add_idx + 1 < end_idx:
            post_actions = df_sorted.iloc[add_idx + 1:end_idx].copy()
        else:
            post_actions = pd.DataFrame()
        
        # Find RemoveStep actions in this window
        removestep_indices = []
        all_actions = []  # Track all actions including RemoveSteps
        
        
        for i, row in post_actions.iterrows():
            # Use integer position in the post_actions dataframe
            idx = i
            action_str = str(row.get('action_raw', ''))
            step_number = i + 1  # Step number after AddInduction
            
            # Check for RemoveStep (case-insensitive to be safe)
            is_removestep = 'removestep' in action_str.lower()
            
            if is_removestep:
                removestep_indices.append({
                    'action_index': idx,
                    'step_number': step_number,
                    'timestamp': row.get('datetime'),
                    'time_since_add': (row.get('datetime') - add_time).total_seconds() if add_time and row.get('datetime') else None,
                    'action': row.get('action_raw'),
                })
            
            # Collect ALL actions for analysis (including RemoveSteps)
            all_actions.append({
                'step_number': step_number,
                'action': row.get('action_raw'),
                'timestamp': row.get('datetime'),
                'time_since_add': (row.get('datetime') - add_time).total_seconds() if add_time and row.get('datetime') else None,
                'is_removestep': is_removestep,
            })
        
        # actions_before_removal: actions up to (but not including) the first RemoveStep
        # This is for understanding what happened before removal
        actions_before_removal = []
        if removestep_indices:
            first_removal_step = removestep_indices[0]['step_number']
            actions_before_removal = [a for a in all_actions if a['step_number'] < first_removal_step]
        else:
            actions_before_removal = all_actions.copy()
        
        # Categorize actions in the exploration path
        action_categories = {
            'case_modifications': 0,  # CaseUpdate
            'axiom_additions': 0,      # AddAxiomStep
            'step_forwards': 0,        # StepForward
            'other_step_creations': 0, # AddForall, AddAlgebriteStep, etc.
            'removesteps': 0,          # RemoveStep
            'other': 0,
        }
        
        # Categorize all actions (including RemoveSteps) for full picture
        for action_item in all_actions:
            action_str = str(action_item.get('action', '')).lower()
            is_removestep = action_item.get('is_removestep', False)
            
            if is_removestep or 'removestep' in action_str:
                action_categories['removesteps'] += 1
            elif 'caseupdate' in action_str or 'inductionstep(case' in action_str:
                action_categories['case_modifications'] += 1
            elif 'addaxiomstep' in action_str:
                action_categories['axiom_additions'] += 1
            elif 'stepforward' in action_str:
                action_categories['step_forwards'] += 1
            elif 'addforall' in action_str or 'addalgebritestep' in action_str:
                action_categories['other_step_creations'] += 1
            else:
                action_categories['other'] += 1
        
        exploration_path = {
            'add_induction': add_event,
            'total_steps_after': len(all_actions),  # Total including RemoveSteps
            'steps_before_first_removal': len(actions_before_removal),
            'removesteps': removestep_indices,
            'removestep_count': len(removestep_indices),
            'was_removed': len(removestep_indices) > 0,
            'first_removal_step': removestep_indices[0]['step_number'] if removestep_indices else None,
            'actions_before_removal': actions_before_removal,
            'all_actions': all_actions,  # Include all actions including RemoveSteps
            'action_categories': action_categories,
        }
        
        exploration_paths.append(exploration_path)
    
    # Calculate statistics
    total_paths = len(exploration_paths)
    removed_paths = [p for p in exploration_paths if p['was_removed']]
    retained_paths = [p for p in exploration_paths if not p['was_removed']]
    
    steps_before_removal = [p['first_removal_step'] for p in removed_paths if p['first_removal_step'] is not None]
    
    # Analyze backtracking patterns (multiple RemoveSteps)
    backtracking_patterns = {
        'single_removestep': 0,      # Only one RemoveStep after AddInduction
        'multiple_removesteps': 0,   # Multiple RemoveSteps (backtracking)
        'removestep_sequences': [],  # Sequences with multiple RemoveSteps
    }
    
    for path in exploration_paths:
        if path['removestep_count'] == 1:
            backtracking_patterns['single_removestep'] += 1
        elif path['removestep_count'] > 1:
            backtracking_patterns['multiple_removesteps'] += 1
            backtracking_patterns['removestep_sequences'].append({
                'add_induction_index': path['add_induction']['action_index'],
                'removestep_count': path['removestep_count'],
                'removestep_steps': [r['step_number'] for r in path['removesteps']],
            })
    
    statistics = {
        'total_add_induction': total_paths,
        'removed_count': len(removed_paths),
        'retained_count': len(retained_paths),
        'average_steps_before_removal': np.mean(steps_before_removal) if steps_before_removal else None,
        'median_steps_before_removal': np.median(steps_before_removal) if steps_before_removal else None,
        'min_steps_before_removal': np.min(steps_before_removal) if steps_before_removal else None,
        'max_steps_before_removal': np.max(steps_before_removal) if steps_before_removal else None,
        'average_total_steps': np.mean([p['total_steps_after'] for p in exploration_paths]) if exploration_paths else 0,
    }
    
    return {
        'exploration_paths': exploration_paths,
        'statistics': statistics,
        'backtracking_patterns': backtracking_patterns,
    }


