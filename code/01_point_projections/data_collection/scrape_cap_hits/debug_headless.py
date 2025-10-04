"""
Debug: Comparer HTML en mode headless vs visible
"""

import asyncio
from playwright.async_api import async_playwright
from pathlib import Path

TEST_URL = "https://puckpedia.com/team/montreal-canadiens"
OUTPUT_DIR = Path("code/01_point_projections/data_collection/scrape_cap_hits")


async def fetch_html(headless=True):
    """Télécharger le HTML"""
    async with async_playwright() as p:
        browser = await p.chromium.launch(
            headless=headless,
            args=['--disable-blink-features=AutomationControlled']
        )

        page = await browser.new_page()
        await page.goto(TEST_URL, wait_until="load", timeout=20000)

        # Attendre le tableau
        try:
            await page.wait_for_selector('td[data-extract_ch]', timeout=10000)
            print(f"  ✓ Sélecteur trouvé (headless={headless})")
        except:
            print(f"  ✗ Sélecteur NON trouvé (headless={headless})")

        await asyncio.sleep(3)

        # Extraire le HTML
        html = await page.content()

        await browser.close()

        return html


async def main():
    print("Test 1: Mode headless=True")
    html_headless = await fetch_html(headless=True)

    # Compter les data-extract_ch
    count_headless = html_headless.count('data-extract_ch')
    print(f"  Occurrences de 'data-extract_ch': {count_headless}\n")

    # Sauvegarder
    output_file = OUTPUT_DIR / "test_headless.html"
    output_file.write_text(html_headless, encoding='utf-8')
    print(f"  Sauvegardé: {output_file}\n")

    print("Test 2: Mode headless=False")
    html_visible = await fetch_html(headless=False)

    count_visible = html_visible.count('data-extract_ch')
    print(f"  Occurrences de 'data-extract_ch': {count_visible}\n")

    output_file2 = OUTPUT_DIR / "test_visible.html"
    output_file2.write_text(html_visible, encoding='utf-8')
    print(f"  Sauvegardé: {output_file2}\n")

    print("="*60)
    print(f"Headless: {count_headless} occurrences")
    print(f"Visible:  {count_visible} occurrences")
    print("="*60)


if __name__ == "__main__":
    asyncio.run(main())
