# CLAUDE.md - Modèle d'Évaluation Dynamique

Ce document décrit l'architecture et la stratégie pour le troisième composant du projet : le modèle d'évaluation dynamique des joueurs.

## 1. Objectif Principal

L'objectif est de créer un système d'aide à la décision pour la gestion active du pool durant la saison. Le modèle doit fournir des recommandations claires pour les 20 mouvements de joueurs autorisés, en répondant aux questions suivantes :

-   **Sell High** : Quels joueurs de mon équipe surperforment actuellement et devraient être échangés avant une régression vers la moyenne ?
-   **Buy Low** : Quels joueurs disponibles sous-performent et représentent de bonnes cibles d'acquisition ?
-   **Analyse de "Streaks"** : La séquence actuelle d'un joueur (positive ou négative) est-elle statistiquement anormale et susceptible de se terminer ?
-   **Valeur d'une Transaction** : Est-ce que l'acquisition d'un joueur en vaut la peine, considérant le coût (1 des 20 mouvements) ?

## 2. Approche Méthodologique : Mise à Jour Bayésienne

Le cœur du modèle sera une approche de mise à jour bayésienne. Cela nous permet de combiner de manière robuste nos connaissances a priori sur un joueur avec ses performances observées durant la saison.

1.  **Prior (Connaissance a priori)** : Les projections de points générées par le modèle `01_point_projections` serviront de **distribution a priori (prior)**. Cela représente notre "croyance" sur le talent réel du joueur avant le début de la saison.

2.  **Likelihood (Vraisemblance des données observées)** : Les performances du joueur au cours de la saison (ex: 10 points en 15 matchs) constituent les données observées.

3.  **Posterior (Connaissance a posteriori)** : En combinant le *prior* et la *likelihood*, le modèle génère une **distribution a posteriori**. Cette nouvelle distribution représente notre croyance mise à jour sur le talent du joueur.

## 3. Stratégie de Validation : Backtesting sur 2024-2025

Le défi majeur est de valider le modèle sans avoir accès aux données dynamiques historiques (ex: combinaisons de trios de la saison passée). Notre stratégie repose sur une distinction claire entre le modèle de backtesting et le modèle de production.

-   **Saison de Backtesting** : **2024-2025**
-   **Saison de Production ("Live")** : **2025-2026**

### 3.1. Modèle de Backtesting (sur 2024-2025)

L'objectif est de valider la logique du moteur bayésien avec des données accessibles rétroactivement.

-   **Prior** : Nous générerons un *prior* spécifique pour la saison 2024-2025 en utilisant une **version simplifiée du modèle `01_point_projections`**, qui ne s'appuie que sur des données disponibles à la fin de la saison 2023-2024.
-   **Données Dynamiques** : Nous utiliserons **uniquement l'API de la LNH** pour collecter les données match par match. Les variables se limiteront à :
    -   Performance de base (buts, passes, points, tirs).
    -   Temps de glace (Total, 5v5, Avantage Numérique).
    -   Statistiques "on-ice" à 5v5 pour calculer des **proxys de chance** (`On-Ice Shooting %`, `PDO`). Le temps de glace servira de **proxy pour le déploiement**.

### 3.2. Modèle de Production (pour 2025-2026)

Ce modèle utilisera toutes les variables du modèle de backtesting, enrichies avec des données collectées en temps réel.

-   **Variables additionnelles** :
    -   Données de `dailyfaceoff` pour les combinaisons de trios.
    -   Qualité des compagnons de trio.
    -   Calendrier détaillé (nombre de matchs, force des adversaires).

## 4. Workflow de Développement Proposé

Le développement se concentrera d'abord sur la création et la validation du modèle de backtesting.

1.  **Phase 1 : Création du "Prior" de Backtesting**
    -   **Action** : Adapter le modèle `01_point_projections` pour générer les projections de la saison 2024-2025.
    -   **Objectif** : Obtenir une distribution *a priori* fiable pour chaque joueur.

2.  **Phase 2 : Collecte des Données Temporelles (2024-2025)**
    -   **Action** : Développer un script robuste pour collecter les données match par match de la saison 2024-2025, incluant les statistiques "on-ice" nécessaires pour le calcul du PDO.
    -   **Stratégie de Collecte** :
        1.  **Itération par Match** : Au lieu d'interroger l'API pour chaque joueur, le script récupérera d'abord le calendrier complet de la saison pour obtenir tous les `gameId`.
        2.  **Analyse Play-by-Play** : Pour chaque `gameId`, le script téléchargera les données complètes du "Play-by-Play" via l'endpoint `gamecenter` de l'API.
        3.  **Agrégation des Données** : En analysant ces données brutes, le script calculera pour chaque joueur et chaque match :
            -   Les statistiques individuelles (buts, passes, tirs, TOI).
            -   Les statistiques "on-ice" à 5v5 (Buts Pour/Contre, Tirs Pour/Contre) essentielles pour le PDO.
    -   **Objectif** : Produire un unique fichier de données statique (`game_data_2025.rds`) contenant la "vérité terrain" de toute la saison. Cette opération est lourde mais n'est effectuée qu'une seule fois pour le backtest.
    -   **Adaptation pour la Saison "Live"** : Cette même logique sera facilement adaptable pour la saison de production. Un script quotidien pourra être exécuté pour ne collecter que les données des matchs de la veille, assurant une mise à jour rapide et efficace (~1 minute par jour).

3.  **Phase 3 : Développement du Modèle de Mise à Jour Bayésienne**
    -   **Action** : Coder le modèle en `R` (`brms` ou `Stan`).
    -   **Input** : Projections initiales (priors) + données de performance en cours de saison.
    -   **Output** : Projections mises à jour (posteriors) pour chaque joueur, chaque semaine.

4.  **Phase 4 : Backtesting sur la Saison 2024-2025**
    -   **Action** : Simuler la saison semaine par semaine. Pour chaque semaine, le modèle génère des recommandations ("Buy Low" / "Sell High") en utilisant uniquement les données disponibles jusqu'à ce point.
    -   **Objectif** : Évaluer si les recommandations auraient été profitables. Mesurer la performance en comparant la prédiction à la réalité observée dans les semaines suivantes.

5.  **Phase 5 : Création du Système de Recommandation**
    -   **Action** : Développer la logique qui traduit les sorties du modèle en recommandations claires (ex: règle de `(rythme_actuel - projection_postérieure) > X`).
    -   **Objectif** : Fournir des alertes actionnables.

## 5. Sorties Attendues du Modèle

-   Un score de "surperformance" / "sous-performance" pour chaque joueur.
-   Une projection de points mise à jour pour le reste de la saison.
-   Une recommandation finale : `BUY`, `SELL`, `HOLD`.
-   Un indicateur de confiance pour chaque recommandation.
