#!/usr/bin/env python3
"""
Analysis Utilities for Hazel Logs

Time analysis and session segmentation utilities for analyzing student
interaction patterns in Hazel submission logs.
"""

import pandas as pd
import numpy as np
from datetime import datetime, timedelta
from typing import Dict, List, Tuple, Any


def calculate_time_diffs(df: pd.DataFrame) -> pd.Series:
    """
    Compute time differences between consecutive actions.
    
    Args:
        df: DataFrame with 'timestamp' or 'datetime' column
        
    Returns:
        Series of time differences in seconds
    """
    df_sorted = df.sort_values('timestamp' if 'timestamp' in df.columns else 'datetime')
    
    if 'timestamp' in df_sorted.columns:
        # Timestamp is in milliseconds
        time_diffs = df_sorted['timestamp'].diff().dropna() / 1000.0
    else:
        # Use datetime
        time_diffs = df_sorted['datetime'].diff().dropna()
        time_diffs = time_diffs.dt.total_seconds()
    
    return time_diffs


def get_time_statistics(df: pd.DataFrame) -> Dict[str, Any]:
    """
    Return comprehensive timing metrics for a session.
    
    Args:
        df: DataFrame with action data
        
    Returns:
        Dictionary with timing statistics
    """
    if len(df) == 0:
        return {}
    
    time_diffs = calculate_time_diffs(df)
    
    df_sorted = df.sort_values('datetime' if 'datetime' in df.columns else 'timestamp')
    
    if 'datetime' in df_sorted.columns:
        start_time = df_sorted['datetime'].min()
        end_time = df_sorted['datetime'].max()
        total_duration = (end_time - start_time).total_seconds()
    else:
        start_time = df_sorted['timestamp'].min()
        end_time = df_sorted['timestamp'].max()
        total_duration = (end_time - start_time) / 1000.0
    
    return {
        'start_time': start_time,
        'end_time': end_time,
        'total_duration_seconds': total_duration,
        'total_actions': len(df),
        'mean_time_between_actions': time_diffs.mean(),
        'median_time_between_actions': time_diffs.median(),
        'std_time_between_actions': time_diffs.std(),
        'min_time_between_actions': time_diffs.min(),
        'max_time_between_actions': time_diffs.max(),
        'percentile_25': time_diffs.quantile(0.25),
        'percentile_75': time_diffs.quantile(0.75),
        'percentile_90': time_diffs.quantile(0.90),
        'percentile_95': time_diffs.quantile(0.95),
    }


def identify_rapid_actions(df: pd.DataFrame, threshold: float = 0.1) -> Tuple[pd.Series, List[List[int]]]:
    """
    Find rapid-fire action sequences (potential random clicking).
    
    Args:
        df: DataFrame with action data
        threshold: Time threshold in seconds for "rapid" actions
        
    Returns:
        Tuple of (boolean Series indicating rapid actions, list of rapid sequences)
    """
    time_diffs = calculate_time_diffs(df)
    rapid_actions = time_diffs < threshold
    
    # Find sequences of rapid actions
    rapid_sequences = []
    current_sequence = []
    
    df_sorted = df.sort_values('timestamp' if 'timestamp' in df.columns else 'datetime')
    
    for i, is_rapid in enumerate(rapid_actions):
        if is_rapid:
            current_sequence.append(i)
        else:
            if len(current_sequence) > 1:
                rapid_sequences.append(current_sequence)
            current_sequence = []
    
    # Don't forget the last sequence
    if len(current_sequence) > 1:
        rapid_sequences.append(current_sequence)
    
    return rapid_actions, rapid_sequences


def segment_by_breaks(df: pd.DataFrame, break_threshold: float = 10.0) -> List[Tuple[int, int]]:
    """
    Split session into segments based on inactivity gaps.
    
    Args:
        df: DataFrame with action data
        break_threshold: Time threshold in seconds for session breaks
        
    Returns:
        List of (start_idx, end_idx) tuples representing segments
    """
    time_diffs = calculate_time_diffs(df)
    breaks = time_diffs > break_threshold
    
    df_sorted = df.sort_values('timestamp' if 'timestamp' in df.columns else 'datetime')
    
    segments = []
    current_segment_start = 0
    
    for i, is_break in enumerate(breaks):
        if is_break:
            segments.append((current_segment_start, i))
            current_segment_start = i + 1
    
    # Add final segment
    segments.append((current_segment_start, len(df_sorted) - 1))
    
    return segments


def analyze_session_segments(df: pd.DataFrame, break_threshold: float = 10.0) -> List[Dict[str, Any]]:
    """
    Break down session into work phases and analyze each.
    
    Args:
        df: DataFrame with action data
        break_threshold: Time threshold in seconds for session breaks
        
    Returns:
        List of dictionaries with segment statistics
    """
    segments = segment_by_breaks(df, break_threshold)
    df_sorted = df.sort_values('timestamp' if 'timestamp' in df.columns else 'datetime')
    
    segment_stats = []
    
    for i, (start_idx, end_idx) in enumerate(segments):
        segment_df = df_sorted.iloc[start_idx:end_idx + 1]
        
        if len(segment_df) == 0:
            continue
        
        if 'datetime' in segment_df.columns:
            duration = (segment_df['datetime'].max() - segment_df['datetime'].min()).total_seconds()
        else:
            duration = (segment_df['timestamp'].max() - segment_df['timestamp'].min()) / 1000.0
        
        # Get action type distribution
        action_types = segment_df['type'].value_counts().to_dict() if 'type' in segment_df.columns else {}
        
        # Calculate actions per second
        actions_per_second = len(segment_df) / duration if duration > 0 else 0
        
        segment_stats.append({
            'segment_id': i,
            'start_idx': start_idx,
            'end_idx': end_idx,
            'action_count': len(segment_df),
            'duration_seconds': duration,
            'actions_per_second': actions_per_second,
            'action_types': action_types,
            'top_action': max(action_types.items(), key=lambda x: x[1])[0] if action_types else None,
        })
    
    return segment_stats


def identify_activity_bursts(df: pd.DataFrame, burst_threshold: float = 0.5, 
                             min_burst_length: int = 3) -> List[Dict[str, Any]]:
    """
    Find periods of intense activity (rapid succession of actions).
    
    Args:
        df: DataFrame with action data
        burst_threshold: Maximum time between actions in a burst (seconds)
        min_burst_length: Minimum number of actions to constitute a burst
        
    Returns:
        List of dictionaries describing each burst
    """
    time_diffs = calculate_time_diffs(df)
    df_sorted = df.sort_values('timestamp' if 'timestamp' in df.columns else 'datetime')
    
    bursts = []
    current_burst_start = 0
    current_burst_indices = [0]
    
    for i, time_diff in enumerate(time_diffs, start=1):
        if time_diff <= burst_threshold:
            # Continue current burst
            current_burst_indices.append(i)
        else:
            # End current burst if it's long enough
            if len(current_burst_indices) >= min_burst_length:
                burst_df = df_sorted.iloc[current_burst_indices]
                
                if 'datetime' in burst_df.columns:
                    duration = (burst_df['datetime'].max() - burst_df['datetime'].min()).total_seconds()
                else:
                    duration = (burst_df['timestamp'].max() - burst_df['timestamp'].min()) / 1000.0
                
                bursts.append({
                    'start_idx': current_burst_indices[0],
                    'end_idx': current_burst_indices[-1],
                    'action_count': len(current_burst_indices),
                    'duration_seconds': duration,
                    'actions_per_second': len(current_burst_indices) / duration if duration > 0 else float('inf'),
                })
            
            # Start new burst
            current_burst_start = i
            current_burst_indices = [i]
    
    # Check final burst
    if len(current_burst_indices) >= min_burst_length:
        burst_df = df_sorted.iloc[current_burst_indices]
        
        if 'datetime' in burst_df.columns:
            duration = (burst_df['datetime'].max() - burst_df['datetime'].min()).total_seconds()
        else:
            duration = (burst_df['timestamp'].max() - burst_df['timestamp'].min()) / 1000.0
        
        bursts.append({
            'start_idx': current_burst_indices[0],
            'end_idx': current_burst_indices[-1],
            'action_count': len(current_burst_indices),
            'duration_seconds': duration,
            'actions_per_second': len(current_burst_indices) / duration if duration > 0 else float('inf'),
        })
    
    return bursts


def get_action_rate_over_time(df: pd.DataFrame, window_size: int = 10) -> pd.DataFrame:
    """
    Calculate rolling action rate over time.
    
    Args:
        df: DataFrame with action data
        window_size: Number of actions to include in rolling window
        
    Returns:
        DataFrame with timestamp and action rate
    """
    df_sorted = df.sort_values('timestamp' if 'timestamp' in df.columns else 'datetime').copy()
    
    time_diffs = calculate_time_diffs(df)
    
    # Calculate rolling mean of time differences
    rolling_mean = time_diffs.rolling(window=window_size, min_periods=1).mean()
    
    # Action rate is 1 / time_between_actions
    action_rate = 1.0 / rolling_mean
    
    df_sorted['action_rate'] = action_rate
    
    return df_sorted[['datetime' if 'datetime' in df_sorted.columns else 'timestamp', 'action_rate']]


def compare_session_phases(df: pd.DataFrame, phase_count: int = 3) -> Dict[str, Any]:
    """
    Divide session into N equal phases and compare activity patterns.
    
    Args:
        df: DataFrame with action data
        phase_count: Number of phases to divide session into
        
    Returns:
        Dictionary with phase comparison statistics
    """
    df_sorted = df.sort_values('timestamp' if 'timestamp' in df.columns else 'datetime')
    
    phase_size = len(df_sorted) // phase_count
    phases = []
    
    for i in range(phase_count):
        start_idx = i * phase_size
        end_idx = start_idx + phase_size if i < phase_count - 1 else len(df_sorted)
        
        phase_df = df_sorted.iloc[start_idx:end_idx]
        phase_stats = get_time_statistics(phase_df)
        phase_stats['phase_id'] = i
        phase_stats['action_count'] = len(phase_df)
        
        phases.append(phase_stats)
    
    return {
        'phases': phases,
        'phase_count': phase_count,
        'total_actions': len(df),
    }


