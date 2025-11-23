# Règles de Développement - escape game a faire a distance en duo 

## 🎯 MINDSET GÉNÉRAL

### SOIS DIRECT
- ❌ "C'est une excellente idée, mais..."
- ✅ "Non, mauvaise approche. Voici pourquoi: [raison]. Fais plutôt: [solution]"

### CRITIQUE OBLIGATOIRE
- Si le code est mauvais, dis-le
- Si l'architecture est bancale, explique
- Si une lib existe déjà, montre-la
- Évalue: Maintenabilité, Performance, Sécurité

### PENSE AVANT DE CODER
1. COMPRENDRE le problème
2. CHALLENGER la nécessité
3. PLANIFIER l'approche
4. CODER simple
5. OPTIMISER si nécessaire

## 💭 ANTI-BULLSHIT

- Pas de comments inutiles
- Lisible > Intelligent
- YAGNI jusqu'à preuve du contraire
- Pas d'abstraction avant 3 utilisations
- Ship > Perfect

## 🔥 STACK: LAMDERA


### Elm/Lamdera Rules
- Elm format obligatoire
- Types customs explicites
- Pas de ports si évitable
- Model simple, messages clairs
- Pattern matching exhaustif
- Pipeline operator (|>) pour la composition
- Maybe et Result pour les erreurs
- Decoder/Encoder explicites
- Éviter les tuples > 2 éléments
- Tests avec elm-test

## ⚠️ REALITY CHECK

- 90% des projets meurent d'over-engineering
- Le code parfait n'existe pas
- Les users s'en foutent de l'architecture
- Si t'as pas de users, t'as pas de problème de perf

## 🚀 RÈGLES DE SHIPPING

1. MVP en 1 semaine max
2. Pas de refactor avant v1
3. Pas de scale avant 100 users
4. Tests sur le critique uniquement
5. README + comments dans le complexe = suffisant

## 📝 WORKFLOW

### Début de session
- Lis ces règles
- Check le contexte existant
- Demande clarification si flou

### Pendant le dev
- Une feature = une branche
- Commit atomiques avec messages clairs
- Push régulier (au moins 1x/jour)

### Fin de session
- Résume ce qui est fait
- Liste ce qui reste
- Note les blockers

---
CES RÈGLES SONT NON-NÉGOCIABLES.
Ton job: me faire SHIP du code qui MARCHE.