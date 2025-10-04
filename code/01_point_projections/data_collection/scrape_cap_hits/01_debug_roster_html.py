"""
Script: Extraire le HTML d'une page de roster Puckpedia pour analyse
URL test: https://puckpedia.com/team/montreal-canadiens
"""

import asyncio
from playwright.async_api import async_playwright
from pathlib import Path

# Configuration
OUTPUT_DIR = Path("code/01_point_projections/data_collection/scrape_cap_hits")
OUTPUT_FILE = OUTPUT_DIR / "mtl_roster.html"

TEST_URL = "https://puckpedia.com/team/montreal-canadiens"


async def fetch_html():
    """Télécharger le HTML de la page MTL"""
    print(f"Fetching: {TEST_URL}\n")

    async with async_playwright() as p:
        browser = await p.chromium.launch(
            headless=False,  # Mode visible pour debug
            args=['--disable-blink-features=AutomationControlled']
        )

        page = await browser.new_page()
        await page.goto(TEST_URL, wait_until="domcontentloaded", timeout=10000)

        # Attendre le chargement
        await asyncio.sleep(3)

        # Scroller pour charger tout le contenu
        await page.evaluate("window.scrollTo(0, document.body.scrollHeight)")
        await asyncio.sleep(2)

        # Extraire le HTML
        html = await page.content()

        await browser.close()

        return html


async def main():
    html = await fetch_html()

    # Sauvegarder
    OUTPUT_FILE.write_text(html, encoding='utf-8')

    print(f"✓ HTML sauvegardé: {OUTPUT_FILE}")
    print(f"  Taille: {len(html):,} caractères\n")

    # Aperçu
    print("Aperçu (200 premiers caractères):")
    print(html[:200])


if __name__ == "__main__":
    asyncio.run(main())
