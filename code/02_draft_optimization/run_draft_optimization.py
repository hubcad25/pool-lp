"""
Orchestrateur du workflow d'optimisation du draft

Exécute les 3 étapes dans l'ordre:
1. Optimisation: Génère 5 rosters avec différents profils de risque
2. Simulation: Simule 5,000 saisons Monte Carlo pour chaque roster
3. Comparaison: Analyse comparative et recommandations

Usage:
    python run_draft_optimization.py
"""

import subprocess
import sys
from pathlib import Path
import time

# Configuration
SCRIPT_DIR = Path(__file__).parent

SCRIPTS = [
    ("optimize_draft.py", "Optimisation des rosters"),
    ("simulate_rosters.py", "Simulations Monte Carlo"),
    ("compare_rosters.py", "Analyse comparative")
]


def run_script(script_name, description):
    """
    Exécute un script Python et gère les erreurs

    Args:
        script_name: Nom du fichier script
        description: Description pour affichage

    Returns:
        True si succès, False si erreur
    """
    script_path = SCRIPT_DIR / script_name

    print("\n" + "="*80)
    print(f"ÉTAPE: {description}")
    print(f"Script: {script_path}")
    print("="*80)

    start_time = time.time()

    try:
        # Exécuter le script
        result = subprocess.run(
            [sys.executable, str(script_path)],
            check=True,
            capture_output=False,  # Afficher output en temps réel
            text=True
        )

        elapsed = time.time() - start_time
        print(f"\n✓ {description} terminé en {elapsed:.1f}s")
        return True

    except subprocess.CalledProcessError as e:
        print(f"\n✗ Erreur lors de l'exécution de {script_name}")
        print(f"Code de retour: {e.returncode}")
        return False

    except Exception as e:
        print(f"\n✗ Erreur inattendue: {e}")
        return False


def main():
    """Exécution principale - orchestre les 3 étapes"""

    print("\n" + "="*80)
    print("WORKFLOW COMPLET D'OPTIMISATION DU DRAFT")
    print("="*80)
    print("\nCe workflow exécute 3 étapes:")
    print("  1. Optimisation: Génère 5 rosters (ultra_conservative → aggressive)")
    print("  2. Simulation:   Simule 5,000 saisons pour chaque roster")
    print("  3. Comparaison:  Analyse comparative et recommandations")
    print("\n")

    input("Appuyez sur Entrée pour démarrer...")

    # Exécuter chaque script dans l'ordre
    start_time = time.time()
    success_count = 0

    for script_name, description in SCRIPTS:
        success = run_script(script_name, description)

        if success:
            success_count += 1
        else:
            print("\n" + "="*80)
            print("WORKFLOW INTERROMPU - Erreur lors d'une étape")
            print("="*80)
            sys.exit(1)

    # Résumé final
    total_time = time.time() - start_time

    print("\n\n" + "="*80)
    print("WORKFLOW TERMINÉ AVEC SUCCÈS")
    print("="*80)
    print(f"\n✓ {success_count}/{len(SCRIPTS)} étapes complétées")
    print(f"✓ Temps total: {total_time:.1f}s ({total_time/60:.1f} minutes)")

    print("\n📊 RÉSULTATS:")
    print("  - Rosters optimaux: data/02_draft_optimization/rosters/")
    print("  - Simulations:      data/02_draft_optimization/simulations/")
    print("  - Rapport:          data/02_draft_optimization/comparison_report.txt")

    print("\n💡 PROCHAINES ÉTAPES:")
    print("  1. Lire le rapport de comparaison")
    print("  2. Choisir une stratégie selon votre tolérance au risque")
    print("  3. Utiliser le roster correspondant pour votre draft")
    print("  4. Ajuster avec vos 20 changements pendant la saison")

    print("\n" + "="*80)


if __name__ == "__main__":
    main()
