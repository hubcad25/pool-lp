#!/usr/bin/env python3
"""
Collect player stats from Hockey Reference and join with model projections.

This script:
1. Reads a list of players from players_to_collect.csv
2. Scrapes 2025-26 standard stats and possession metrics from Hockey Reference
3. Loads model projections from RDS file
4. Joins actual stats with projections
5. Saves results to CSV
"""

import pandas as pd
import requests
from bs4 import BeautifulSoup
import time
from pathlib import Path
import pyreadr

# Configuration
SCRIPT_DIR = Path(__file__).parent
PLAYERS_FILE = SCRIPT_DIR / "players_to_collect.csv"
PROJECTIONS_FILE = Path("/home/hubcad25/code/hockey/pool-lp/data/01_point_projections/projection/projections_2026_final.rds")
OUTPUT_STATS = SCRIPT_DIR / "player_stats_2025.csv"
OUTPUT_MERGED = SCRIPT_DIR / "stats_vs_projections.csv"

HOCKEY_REF_BASE = "https://www.hockey-reference.com/players"


def get_column_value(row, possible_names):
    """Helper to get value from a row with multiple possible column names."""
    for name in possible_names:
        if name in row.index:
            return row[name]
    return None


def scrape_player_stats(player_id):
    """
    Scrape 2025-26 stats for a player from Hockey Reference.

    Args:
        player_id: Hockey Reference player ID (e.g., 'demidiv01')

    Returns:
        dict with player stats and possession metrics
    """
    url = f"{HOCKEY_REF_BASE}/{player_id[0]}/{player_id}.html"

    print(f"Fetching {url}...")

    try:
        response = requests.get(url, headers={'User-Agent': 'Mozilla/5.0'})

        # Check if page exists (404 = bad player ID)
        if response.status_code == 404:
            print(f"  ❌ ERROR: Player page not found (404). Bad hockey_ref_id: {player_id}")
            return {
                'player_id': player_id,
                'error': 'Page not found (404) - Invalid hockey_ref_id'
            }

        response.raise_for_status()

        soup = BeautifulSoup(response.content, 'html.parser')

        # Extract player name (try multiple methods)
        player_name = None
        # Try method 1: h1 with itemprop
        name_elem = soup.find('h1', {'itemprop': 'name'})
        if name_elem:
            player_name = name_elem.get_text().strip()
        else:
            # Try method 2: first h1 in div id='meta'
            meta = soup.find('div', {'id': 'meta'})
            if meta:
                h1 = meta.find('h1')
                if h1:
                    player_name = h1.get_text().strip()

        # Find standard stats table
        stats_table = soup.find('table', {'id': 'player_stats'})

        # Find advanced stats table
        adv_table = soup.find('table', {'id': 'skaters_advanced_all'})

        result = {
            'player_id': player_id,
            'player_name': player_name,
        }

        # Parse standard stats for 2025-26
        if stats_table:
            from io import StringIO
            stats_df = pd.read_html(StringIO(str(stats_table)))[0]

            # Handle multi-level columns if present
            if isinstance(stats_df.columns, pd.MultiIndex):
                stats_df.columns = ['_'.join(col).strip() for col in stats_df.columns.values]

            # Filter for 2025-26 season - check all possible season column names
            season_col = None
            for col in stats_df.columns:
                if 'Season' in str(col):
                    season_col = col
                    break

            if season_col is None:
                season_col = stats_df.columns[0]  # Fallback to first column

            current = stats_df[stats_df[season_col] == '2025-26']

            if not current.empty:
                row = current.iloc[0]
                result.update({
                    'season': '2025-26',
                    'age': get_column_value(row, ['Age', 'Unnamed: 1_level_0_Age']),
                    'team': get_column_value(row, ['Tm', 'Team', 'Unnamed: 2_level_0_Team']),
                    'position': get_column_value(row, ['Pos', 'Unnamed: 4_level_0_Pos']),
                    'gp': pd.to_numeric(get_column_value(row, ['GP', 'Unnamed: 5_level_0_GP']), errors='coerce'),
                    'g': pd.to_numeric(get_column_value(row, ['G', 'Scoring_G']), errors='coerce'),
                    'a': pd.to_numeric(get_column_value(row, ['A', 'Scoring_A']), errors='coerce'),
                    'pts': pd.to_numeric(get_column_value(row, ['PTS', 'Scoring_PTS']), errors='coerce'),
                    'plus_minus': pd.to_numeric(get_column_value(row, ['+/-', 'Unnamed: 9_level_0_+/-']), errors='coerce'),
                    'pim': pd.to_numeric(get_column_value(row, ['PIM', 'Unnamed: 10_level_0_PIM']), errors='coerce'),
                    'sog': pd.to_numeric(get_column_value(row, ['SOG', 'Shot_SOG']), errors='coerce'),
                    'sh_pct': pd.to_numeric(get_column_value(row, ['S%', 'Shot_S%']), errors='coerce'),
                    'toi': get_column_value(row, ['TOI', 'Ice_TOI']),
                    'atoi': get_column_value(row, ['ATOI', 'Ice_ATOI']),
                })

                # Calculate 82-game pace
                if result['gp'] and result['pts']:
                    result['pace_82'] = round(result['pts'] / result['gp'] * 82, 1)

        # Parse advanced stats for 2025-26
        if adv_table:
            from io import StringIO
            adv_df = pd.read_html(StringIO(str(adv_table)))[0]

            # Handle multi-level columns if present
            if isinstance(adv_df.columns, pd.MultiIndex):
                adv_df.columns = ['_'.join(col).strip() for col in adv_df.columns.values]

            # Filter for 2025-26 season - check all possible season column names
            season_col = None
            for col in adv_df.columns:
                if 'Season' in str(col):
                    season_col = col
                    break

            if season_col is None:
                season_col = adv_df.columns[0]  # Fallback to first column

            current_adv = adv_df[adv_df[season_col] == '2025-26']

            if not current_adv.empty:
                row = current_adv.iloc[0]
                result.update({
                    'cf_pct': pd.to_numeric(get_column_value(row, ['CF%', 'Corsi_CF%', 'Corsi (All)_CF%']), errors='coerce'),
                    'cf_rel': pd.to_numeric(get_column_value(row, ['CF% rel', 'Corsi_CF% rel', 'Corsi (All)_CF% rel']), errors='coerce'),
                    'ff_pct': pd.to_numeric(get_column_value(row, ['FF%', 'Fenwick_FF%', 'Fenwick (All)_FF%']), errors='coerce'),
                    'ff_rel': pd.to_numeric(get_column_value(row, ['FF% rel', 'Fenwick_FF% rel', 'Fenwick (All)_FF% rel']), errors='coerce'),
                    'oish_pct': pd.to_numeric(get_column_value(row, ['oiSH%', 'PDO (All)_oiSH%']), errors='coerce'),
                    'oisv_pct': pd.to_numeric(get_column_value(row, ['oiSV%', 'PDO (All)_oiSV%']), errors='coerce'),
                    'pdo': pd.to_numeric(get_column_value(row, ['PDO', 'Unnamed: 18_level_0_PDO', 'PDO (All)_PDO']), errors='coerce'),
                    'ozs_pct': pd.to_numeric(get_column_value(row, ['oZS%', 'Zone_oZS%', 'Zone Starts (All)_oZS%']), errors='coerce'),
                    'dzs_pct': pd.to_numeric(get_column_value(row, ['dZS%', 'Zone_dZS%', 'Zone Starts (All)_dZS%']), errors='coerce'),
                })

        # Check if we got any data
        if 'gp' not in result or result['gp'] is None:
            print(f"  ⚠️  WARNING: No 2025-26 stats found for {player_name} ({player_id})")
            result['error'] = 'No 2025-26 stats found'
        else:
            print(f"  ✓ Collected: {player_name} - {result.get('gp')} GP, {result.get('pts')} pts")

        return result

    except Exception as e:
        print(f"  ❌ ERROR scraping {player_id}: {e}")
        return {'player_id': player_id, 'error': str(e)}


def load_projections():
    """
    Load model projections from RDS file.

    Returns:
        DataFrame with projections (scenario = 'mid' only)
    """
    print(f"\nLoading projections from {PROJECTIONS_FILE}...")

    result = pyreadr.read_r(str(PROJECTIONS_FILE))
    proj_df = result[None]  # RDS files have None as key

    # Filter to mid scenario only
    proj_df = proj_df[proj_df['scenario'] == 'mid'].copy()

    # Create full name for matching
    proj_df['player_name'] = proj_df['first_name'] + ' ' + proj_df['last_name']

    # Select relevant columns
    proj_cols = [
        'player_id', 'player_name', 'position', 'team', 'age',
        'points', 'goals', 'assists',
        'wpm_g', 'wpm_a',
        'evtoi_per_gp', 'pptoi_per_gp',
        'cap_hit'
    ]

    proj_df = proj_df[proj_cols].rename(columns={
        'player_id': 'nhl_player_id',
        'points': 'proj_pts',
        'goals': 'proj_g',
        'assists': 'proj_a',
        'position': 'proj_position',
        'team': 'proj_team',
        'age': 'proj_age',
    })

    print(f"Loaded {len(proj_df)} player projections")

    return proj_df


def main():
    """Main execution function."""

    # Load players to collect
    print(f"Reading players from {PLAYERS_FILE}...")
    players_df = pd.read_csv(PLAYERS_FILE)
    print(f"Found {len(players_df)} players to collect\n")

    # Scrape stats for each player
    all_stats = []

    for idx, row in players_df.iterrows():
        player_id = row['hockey_ref_id']

        stats = scrape_player_stats(player_id)
        all_stats.append(stats)

        # Be polite to Hockey Reference servers
        time.sleep(3)

    # Convert to DataFrame
    stats_df = pd.DataFrame(all_stats)

    # Save raw stats
    stats_df.to_csv(OUTPUT_STATS, index=False)
    print(f"\nSaved raw stats to {OUTPUT_STATS}")

    # Load projections
    proj_df = load_projections()

    # Normalize names for matching (remove accents)
    import unicodedata

    def normalize_name(name):
        """Remove accents from name for matching."""
        if pd.isna(name):
            return name
        # Normalize to NFD (decomposed form) then filter out combining characters
        nfd = unicodedata.normalize('NFD', str(name))
        return ''.join(char for char in nfd if unicodedata.category(char) != 'Mn')

    stats_df['player_name_norm'] = stats_df['player_name'].apply(normalize_name)
    proj_df['player_name_norm'] = proj_df['player_name'].apply(normalize_name)

    # Merge stats with projections using normalized names
    merged_df = stats_df.merge(
        proj_df,
        on='player_name_norm',
        how='left',
        suffixes=('', '_proj')
    )

    # Keep original player_name from stats (with accents)
    if 'player_name_proj' in merged_df.columns:
        merged_df = merged_df.drop(columns=['player_name_proj', 'player_name_norm'])

    # Calculate differences vs projection
    if 'pts' in merged_df.columns and 'proj_pts' in merged_df.columns:
        merged_df['pts_vs_proj'] = merged_df['pts'] - (merged_df['proj_pts'] * merged_df['gp'] / 82)
        merged_df['pace_vs_proj'] = merged_df['pace_82'] - merged_df['proj_pts']
        merged_df['pct_of_proj'] = (merged_df['pace_82'] / merged_df['proj_pts'] * 100).round(1)

    # Reorder columns for readability (only include columns that exist)
    first_cols = [
        'player_name', 'player_id', 'nhl_player_id', 'team', 'position',
        'gp', 'g', 'a', 'pts', 'pace_82',
        'proj_pts', 'proj_g', 'proj_a',
        'pace_vs_proj', 'pct_of_proj',
        'cf_pct', 'cf_rel', 'ff_pct', 'ff_rel', 'oish_pct', 'oisv_pct', 'pdo', 'ozs_pct',
        'cap_hit'
    ]

    # Only use columns that actually exist
    first_cols_present = [col for col in first_cols if col in merged_df.columns]
    remaining_cols = [col for col in merged_df.columns if col not in first_cols_present]
    merged_df = merged_df[first_cols_present + remaining_cols]

    # Save merged data
    merged_df.to_csv(OUTPUT_MERGED, index=False)
    print(f"Saved merged stats vs projections to {OUTPUT_MERGED}")

    # Print summary
    print("\n" + "="*80)
    print("SUMMARY")
    print("="*80)

    for idx, row in merged_df.iterrows():
        print(f"\n{row['player_name']} ({row['team']} - {row['position']})")
        print(f"  Current: {row['pts']} pts in {row['gp']} GP (pace: {row['pace_82']} pts)")
        if pd.notna(row.get('proj_pts')):
            print(f"  Projection: {row['proj_pts']:.1f} pts")
            print(f"  vs Projection: {row['pct_of_proj']:.0f}% of pace")
        if pd.notna(row.get('cf_pct')):
            oish = f", oiSH% {row['oish_pct']:.1f}" if pd.notna(row.get('oish_pct')) else ""
            print(f"  Possession: CF% {row['cf_pct']:.1f} ({row['cf_rel']:+.1f} rel){oish}")


if __name__ == '__main__':
    main()
