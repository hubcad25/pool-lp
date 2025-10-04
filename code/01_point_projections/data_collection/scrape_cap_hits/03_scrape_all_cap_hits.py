"""
Script: Scraper les cap hits de toutes les équipes NHL depuis Puckpedia
Source: https://puckpedia.com/team/{team-slug}
Output: data/01_point_projections/lineups/cap_hits.csv
"""

import pandas as pd
from playwright.async_api import async_playwright
from bs4 import BeautifulSoup
import asyncio
from pathlib import Path

# Configuration
OUTPUT_DIR = Path("data/01_point_projections/lineups")
OUTPUT_FILE = OUTPUT_DIR / "cap_hits.csv"

# Créer le dossier si nécessaire
OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

# Équipes NHL (slug -> code)
NHL_TEAMS = {
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

print(f"Scraping cap hits pour {len(NHL_TEAMS)} équipes...\n")


def parse_roster_html(html, team_code):
    """Parser le HTML pour extraire joueurs et cap hits"""
    soup = BeautifulSoup(html, 'html.parser')
    players = []

    # Trouver toutes les lignes de joueurs
    rows = soup.find_all('tr')

    for row in rows:
        # Chercher le lien du joueur
        player_link = row.find('a', href=lambda x: x and '/player/' in x)
        if not player_link:
            continue

        # Extraire nom et slug
        player_slug = player_link['href'].replace('/player/', '')
        player_name = player_link.get_text(strip=True)

        # Chercher le cap hit dans cette ligne
        cap_td = row.find('td', attrs={'data-extract_ch': True})
        if not cap_td:
            continue

        cap_hit_raw = cap_td.get('data-extract_ch', '0')

        # Convertir en float
        try:
            cap_hit = float(cap_hit_raw.replace(',', ''))
        except:
            cap_hit = 0

        players.append({
            'player_name': player_name,
            'player_slug': player_slug,
            'cap_hit': cap_hit,
            'team': team_code
        })

    return players


async def scrape_team_roster(team_slug, team_code, browser):
    """Scraper le roster d'une équipe"""
    url = f"https://puckpedia.com/team/{team_slug}"

    print(f"  Scraping: {team_code:3s} - {url}")

    try:
        page = await browser.new_page()
        await page.goto(url, wait_until="domcontentloaded", timeout=10000)

        # Attendre que le tableau se charge
        try:
            await page.wait_for_selector('td[data-extract_ch]', timeout=10000)
        except:
            print(f"    ⚠ Timeout en attendant le tableau")

        # Attendre le chargement complet
        await asyncio.sleep(5)

        # Scroller pour charger tout le contenu
        await page.evaluate("window.scrollTo(0, document.body.scrollHeight)")
        await asyncio.sleep(2)

        # Extraire le HTML
        html = await page.content()
        await page.close()

        # Parser
        players = parse_roster_html(html, team_code)

        print(f"    ✓ {len(players)} joueurs trouvés")

        return players

    except Exception as e:
        print(f"    ✗ Erreur: {e}")
        return []


async def main():
    """Fonction principale - scraper toutes les équipes"""
    all_players = []

    async with async_playwright() as p:
        # Lancer le navigateur en mode headless
        browser = await p.chromium.launch(
            headless=True,
            args=['--disable-blink-features=AutomationControlled']
        )

        # TEST: scraper seulement MTL d'abord
        test_teams = {"montreal-canadiens": "MTL"}

        # Scraper toutes les équipes
        for team_slug, team_code in test_teams.items():
            players = await scrape_team_roster(team_slug, team_code, browser)
            all_players.extend(players)
            await asyncio.sleep(1)  # Délai entre requêtes

        await browser.close()

    # Créer DataFrame
    df = pd.DataFrame(all_players)

    # Résumé
    print("\n" + "="*60)
    print("RÉSUMÉ DU SCRAPING")
    print("="*60 + "\n")

    if len(df) > 0:
        print(f"Joueurs scrapés: {len(df)}")
        print(f"Équipes: {df['team'].nunique()}")
        print(f"Total cap hit: ${df['cap_hit'].sum():,.0f}\n")

        summary = df.groupby('team').agg(
            n_players=('player_name', 'count'),
            total_cap=('cap_hit', 'sum')
        ).sort_values('total_cap', ascending=False)

        print("Cap hit par équipe (top 10):")
        print(summary.head(10))
    else:
        print("⚠ Aucun joueur scrapé\n")

    # Sauvegarder
    df.to_csv(OUTPUT_FILE, index=False)

    print("\n" + "="*60)
    print("TERMINÉ")
    print("="*60 + "\n")
    print(f"✓ Données sauvegardées: {OUTPUT_FILE}")
    print(f"✓ {len(df)} joueurs de {df['team'].nunique()} équipes scrapés\n")


if __name__ == "__main__":
    asyncio.run(main())
