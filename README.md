# Codes associés à l'article sur l'Interprétabilité des Modèles
- Les codes pour l'article associés à la deuxième base de données (freMTPL2 : freMTPL2freq et freMTPL2sev) se trouvent dans : Transparence/Article_Interprétabilite/Code/
- Les codes associés à la premièré étude (celle du mémoire : freMTPL : freMTPLfreq et freMTPLsev) se trouvent dans : Transparence/Article_Interprétabilite/Code/Brouillon/Première Etude - freMTPL/
- Les autres codes sont des "brouillons".

## Remarques générales :
- les variables retraitées pour le GLM (rendues catégorielles) sont celles avec *GLM* rajouté à la fin (ex : la variable _Area_ devient _AreaGLM_ une fois retraitée)
- l'indice _num_ dans les différents modèles du code réfèrent au modèle pour lequel les variables n'ont pas été catégorisées au préalable 
- l'indice _cat_ réfère aux modèles ajustés avec les variables retaitées au préalables.
- On s'intéresse uniquement à la modélisation en fréquence, et pas à la sévérité. On a pas la correspondance entre tous les sinsitres de la base freMTPL2freq et ceux de la base freMTPL2sev. (environ 10 000 sinistres n'ont pas le montant associé indiqué)

## Quelques questions ?
- *Importance des variables* : selon les méthodes utilisées on a soit une valeur par coefficient (variables "Dummy") ou soit une valeur par variable (avec le package _DALEX_ par exemple) : lequel est le plus pertinent ?
