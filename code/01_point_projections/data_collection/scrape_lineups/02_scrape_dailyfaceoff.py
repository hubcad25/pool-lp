"""
Script: Scraper les lineups de DailyFaceoff
Source: https://www.dailyfaceoff.com/teams/{team}/line-combinations
Output: data/01_point_projections/lineups/dailyfaceoff_raw.csv
"""

import pandas as pd
from playwright.async_api import async_playwright
from bs4 import BeautifulSoup
import asyncio
import re
from pathlib import Path

# Configuration
OUTPUT_DIR = Path("data/01_point_projections/lineups")
OUTPUT_FILE = OUTPUT_DIR / "dailyfaceoff_raw.csv"

# Créer le dossier si nécessaire
OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

# Mapping slug -> code équipe (pour correspondre avec PuckPedia)
TEAM_SLUG_TO_CODE = {
    "anaheim-ducks": "ANA",
    "boston-bruins": "BOS",
    "buffalo-sabres": "BUF",
    "calgary-flames": "CGY",
    "carolina-hurricanes": "CAR",
    "chicago-blackhawks": "CHI",
    "colorado-avalanche": "COL",
    "columbus-blue-jackets": "CBJ",
    "dallas-stars": "DAL",
    "detroit-red-wings": "DET",
    "edmonton-oilers": "EDM",
    "florida-panthers": "FLA",
    "los-angeles-kings": "LAK",
    "minnesota-wild": "MIN",
    "montreal-canadiens": "MTL",
    "nashville-predators": "NSH",
    "new-jersey-devils": "NJD",
    "new-york-islanders": "NYI",
    "new-york-rangers": "NYR",
    "ottawa-senators": "OTT",
    "philadelphia-flyers": "PHI",
    "pittsburgh-penguins": "PIT",
    "san-jose-sharks": "SJS",
    "seattle-kraken": "SEA",
    "st-louis-blues": "STL",
    "tampa-bay-lightning": "TBL",
    "toronto-maple-leafs": "TOR",
    "utah-mammoth": "UTA",
    "vancouver-canucks": "VAN",
    "vegas-golden-knights": "VGK",
    "washington-capitals": "WSH",
    "winnipeg-jets": "WPG"
}

NHL_TEAMS = list(TEAM_SLUG_TO_CODE.keys())

print(f"Scraping DailyFaceoff lineups pour {len(NHL_TEAMS)} équipes...\n")


def extract_jersey_number(link):
    """Extraire le numéro de chandail depuis l'image"""
    # Chercher l'image dans le lien parent ou sibling
    img = link.find_previous('img')
    if not img:
        img = link.find_next('img')

    if img:
        # Chercher dans src ou srcset
        src = img.get('src', '') or img.get('srcset', '')
        # Pattern: MTL_48_Hutson.png
        match = re.search(r'_(\d+)_', src)
        if match:
            return match.group(1)

    return None


def parse_all_players(soup):
    """Parser TOUS les joueurs de la page dans l'ordre"""
    players = []

    # Trouver tous les liens vers /players/news/ avec un span de nom
    all_links = soup.find_all('a', href=lambda x: x and '/players/news/' in x)

    for link in all_links:
        # Filtrer pour garder seulement ceux avec un span de nom (pas les images)
        name_span = link.find('span', class_=lambda x: x and 'text-xs' in x)
        if name_span:
            name = name_span.get_text(strip=True)
            jersey = extract_jersey_number(link)

            players.append({
                'player_name': name,
                'jersey_number': jersey
            })

    return players


async def scrape_dailyfaceoff_team(team_slug, browser):
    """Scraper une équipe depuis DailyFaceoff"""
    url = f"https://www.dailyfaceoff.com/teams/{team_slug}/line-combinations"

    print(f"  Scraping: {team_slug}")

    try:
        page = await browser.new_page()
        await page.goto(url, wait_until="domcontentloaded", timeout=10000)

        # Attendre le chargement du contenu
        await asyncio.sleep(3)

        html = await page.content()
        await page.close()

        # Parser avec BeautifulSoup
        soup = BeautifulSoup(html, 'html.parser')

        # Parser TOUS les joueurs
        all_players_raw = parse_all_players(soup)

        # Prendre seulement les 28 premiers (12F + 6D + 10PP)
        all_players_raw = all_players_raw[:28]

        # Assigner par tranches
        forwards_raw = all_players_raw[0:12]   # Positions 0-11
        defense_raw = all_players_raw[12:18]   # Positions 12-17
        pp1_raw = all_players_raw[18:23]       # Positions 18-22 (5 joueurs)
        pp2_raw = all_players_raw[23:28]       # Positions 23-27 (5 joueurs)

        # Créer les forwards avec position et ligne
        forwards = []
        for i, player in enumerate(forwards_raw):
            line_number = (i // 3) + 1  # Lignes 1-4 (groupes de 3)
            forwards.append({
                'player_name': player['player_name'],
                'position': 'F',
                'jersey_number': player['jersey_number'],
                'line': line_number,
                'pp_unit': None
            })

        # Créer les défenseurs avec position et ligne
        defense = []
        for i, player in enumerate(defense_raw):
            line_number = (i // 2) + 1  # Lignes 1-3 (groupes de 2)
            defense.append({
                'player_name': player['player_name'],
                'position': 'D',
                'jersey_number': player['jersey_number'],
                'line': line_number,
                'pp_unit': None
            })

        # Créer un dict PP pour merger avec forwards/defense
        pp_dict = {}
        for player in pp1_raw:
            pp_dict[player['player_name']] = 'PP1'
        for player in pp2_raw:
            # Si déjà dans PP1, ajouter PP2
            if player['player_name'] in pp_dict:
                pp_dict[player['player_name']] += ', PP2'
            else:
                pp_dict[player['player_name']] = 'PP2'

        # Merger les données PP
        all_players = forwards + defense
        for player in all_players:
            if player['player_name'] in pp_dict:
                player['pp_unit'] = pp_dict[player['player_name']]

        # Ajouter team (code à 3 lettres) et source
        team_code = TEAM_SLUG_TO_CODE.get(team_slug, team_slug)
        for player in all_players:
            player['team'] = team_code
            player['source'] = 'dailyfaceoff'

        print(f"  ✓ {len(all_players)} joueurs trouvés (PP: {len(pp_dict)})")

        return all_players

    except Exception as e:
        print(f"  ✗ Erreur: {e}")
        return []


async def test_single_team():
    """Tester avec une seule équipe"""
    async with async_playwright() as p:
        browser = await p.chromium.launch(
            headless=False,
            args=['--disable-blink-features=AutomationControlled']
        )

        result = await scrape_dailyfaceoff_team("montreal-canadiens", browser)
        await browser.close()

        print(f"\nRésultat: {len(result)} joueurs\n")

        if result:
            df = pd.DataFrame(result)
            print(df.to_string())

        return result


async def main():
    """Fonction principale - scraper toutes les équipes"""
    all_lineups = []

    async with async_playwright() as p:
        # Lancer le navigateur en mode headless
        browser = await p.chromium.launch(
            headless=True,
            args=['--disable-blink-features=AutomationControlled']
        )

        # Scraper toutes les équipes
        for team_slug in NHL_TEAMS:
            players = await scrape_dailyfaceoff_team(team_slug, browser)
            all_lineups.extend(players)
            await asyncio.sleep(1)  # Délai entre requêtes

        await browser.close()

    # Créer DataFrame
    df = pd.DataFrame(all_lineups)

    # Résumé
    print("\n" + "="*40)
    print("RÉSUMÉ DU SCRAPING")
    print("="*40 + "\n")

    if len(df) > 0:
        print(f"Joueurs scrapés: {len(df)}")
        print(f"Équipes: {df['team'].nunique()}\n")

        summary = df.groupby('team').agg(
            n_players=('player_name', 'count'),
            n_forwards=('position', lambda x: (x == 'F').sum()),
            n_defense=('position', lambda x: (x == 'D').sum()),
            n_pp=('pp_unit', lambda x: x.notna().sum())
        )

        print(summary)
    else:
        print("⚠ Aucun joueur scrapé\n")

    # Sauvegarder
    df.to_csv(OUTPUT_FILE, index=False)

    # Aperçu
    if len(df) > 0:
        print("\nAperçu (20 premières lignes):")
        print(df.head(20))

    print("\n" + "="*40)
    print("TERMINÉ")
    print("="*40 + "\n")
    print(f"✓ Données sauvegardées: {OUTPUT_FILE}")
    print(f"✓ {len(df)} joueurs de {df['team'].nunique()} équipes scrapés\n")


if __name__ == "__main__":
    # Scraper toutes les équipes
    asyncio.run(main())

    # Pour tester une seule équipe, utilisez:
    # asyncio.run(test_single_team())
