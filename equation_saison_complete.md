# INTRO

On a plusieurs composantes au modèle.

On a un prior de G, et on a surtout un prior de sh%.

Comme on a fait dans vignettes/explo_dynamic_valuation/r_scripts/01b_calculate_posterior_bayesian.R, on peut utiliser les 10 premiers matchs et ajuster légèrement le SH% pour créer un posterior qui devient son nouveau baseline.

On a aussi le CF% qui montre si le joueur "drive" le jeu: plus un joueur a un CF% élevé, plus le jeu se passe en zone offensive quand il est là. Par contre, cette valeur entre en interaction avec les mesures de chance.

On a aussi le CF/60 qui montre simplement si l'équipe du joueur a beaucoup de chances de marquer quand il est sur la glace. Mesure de "fréquence". Entre aussi en interaction avec les mesures de chances.

On a aussi SOG qui est le nombre de tirs du joueur lui-même.

on-iceSH% est une autre mesure de "chance": plus c'est élevé, plus son équipe est "chanceuse" lorsqu'il est sur le jeu. Plus c'est élevé par rapport au baseline, plus on devrait régresser sa projection. Mais comment avoir un baseline pour ça?

Finalement, on a le TOI et le PPTOI. On peut dériver ça selon le TOI-PPTOI du joueur dans les derniers matchs + lineups projections comme on fait dans code/01_point_projections/data_collection/scrape_lineups/.

# ARCHITECTURE VOULUE

1. Modèle simple style lm (ou peut-être bayésien quand même pour avoir des IC) qui fait simplement prendre comme VD buts ou passes et comme VI nos inputs assez simples (facile à entrainer sur 2024-2025)
2. Mettre plus de sophistication sur la projection bayésienne des inputs. C'est dans la projection des inputs qu'on incorpore les priors pré-saison de buts et passes.

# LISTE DES COMPOSANTES/INPUTS

- SH%
- Ratio SH% / baseline (où baseline = prior pré-saison de SH% updaté avec 10 premiers matchs)
- oiSH%
- Ratio oiSH% / baseline
- CF%
- CF/60
- SOG / 60
- TOI
- PPTOI
- Prior G
- Prior A

Certaines composantes nommées ici ne seront utilisées que pour la projection d'inputs du modèle final.

# MODÈLE FINAL

G = SH% * SOG/60 * TOI
A = f(CF/60, TOI, PPTOI, oiSH%, CF%)

Pour les G, c'est une simple multiplication. La difficulté sera de bien prédire SH% et SOG sur une fenêtre k.

Pour le modèle A, chaque VI est vraiment la valeur pendant la période où le joueur a amassé x nombre de A. On veut que le R2 soit vraiment bon ici. C'est dans la projection des inputs qu'on peut être indulgent.

# PROJECTION DES INPUTS

Je crois que chaque modèle de projection devrait faire sa projection sur une fenêtre k. Comme ça, si k = 70 matchs, le modèle sait qu'il y aura une forte regression to the mean. Si k = 5, il s'ajuste en conséquence. C'est là qu'il va falloir être capables d'incorporer la vitesse que le ratio SH% / baseline revient vers 1.

## SH%


- SOG/60
- TOI (total, EV + PP + SH)
- PPTOI
- CF/60
- CF%
- oiSH%


