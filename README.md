# Series-chronologiques
# Analyse de Séries Temporelles : Prévision de la Consommation Énergétique Domestique

Ce dépôt contient le code source, le rapport technique et la présentation d'un projet de recherche appliquée portant sur l'analyse et la prévision de séries temporelles. 

Ce travail vise à démontrer comment une approche statistique univariée classique, lorsqu'elle est combinée à une stratégie de prévision dynamique, peut concurrencer des modèles prédictifs complexes pour anticiper la demande énergétique.

##  Contexte et Objectifs de Recherche
L'objectif principal de ce projet est d'évaluer la capacité prédictive d'un modèle purement univarié pour la consommation des appareils électroménagers. 

Contrairement aux approches multivariées qui intègrent des données exogènes coûteuses à collecter (données météorologiques, capteurs d'humidité multiples), cette étude cherche à isoler et à modéliser la part de la consommation dite "intrinsèque", dictée uniquement par les habitudes cycliques des occupants d'un logement.

##  Jeu de Données
Le projet s'appuie sur le jeu de données public **Appliances energy prediction** (issu de l'UCI Machine Learning Repository). 
* **Caractéristiques :** Enregistrements d'une maison basse consommation située en Belgique sur une période de 4,5 mois.
* **Résolution originale :** Relevés toutes les 10 minutes.
* **Traitement :** Agrégation à l'échelle horaire pour extraire le signal pertinent et lisser le bruit stochastique haute fréquence.

##  Méthodologie Statistique
La modélisation suit rigoureusement la méthodologie de Box-Jenkins :

1. **Analyse Exploratoire (EDA) :** Étude des corrélations, détection de la multicolinéarité entre les variables environnementales et mise en évidence de la forte périodicité diurne.
2. **Stationnarisation :** * Décomposition temporelle via STL (Tendance, Saisonnalité, Bruit).
   * Validation de la stationnarité via des tests statistiques croisés : Augmented Dickey-Fuller (ADF) et KPSS.
   * Application de différenciations saisonnières.
3. **Modélisation SARIMA :**
   * Identification manuelle des hyperparamètres via les fonctions ACF (Autocorrélation) et PACF (Autocorrélation Partielle).
   * Optimisation et sélection automatique basées sur la minimisation des critères d'information AIC/BIC, aboutissant à un modèle $SARIMA(3,0,1)(2,0,0)[24]$.
4. **Diagnostic du Modèle :** Évaluation de la normalité des résidus et vérification de l'absence d'autocorrélation résiduelle via le test de Ljung-Box.

##  Stratégies de Prévision et Résultats
Afin de tester la robustesse du modèle en conditions quasi-réelles (logique *Smart Grid*), deux protocoles d'évaluation ont été comparés sur l'ensemble de test (les derniers 20% de la série) :

* **Prévision Statique (Static Forecast) :** Prédit l'horizon global de manière isolée. Comme attendu pour un processus stationnaire à long terme, le modèle converge vers la moyenne ($R^2 \approx 0$).
* **Prévision Glissante (Rolling Forecast) :** Approche itérative mettant à jour le modèle à chaque pas de temps $t+1$ en intégrant l'erreur précédente.

**Conclusion clé :** La stratégie dynamique (Rolling Forecast) atteint un coefficient de détermination $R^2 = 0.418$. Ce résultat met en lumière qu'environ 42% de la variance de la consommation électrique s'explique uniquement par l'inertie et l'autocorrélation temporelle des habitudes des usagers. Il constitue une *baseline* statistique solide et interprétable face à des algorithmes de Machine Learning de type "boîte noire".

##  Technologies et Outils
* **Langage de programmation :** R
* **Packages clés :** `forecast`, `tseries`, `ggplot2`, `dplyr`, `zoo`, `GGally`
## ✍️ Équipe de Recherche
Ce projet a été réalisé en collaboration par :
* Weihong Gao
* Feifan Ping
* Baiyi Ren
* Junwen Xiao
