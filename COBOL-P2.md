# Cobol VSAM

VSAM permet 3 types d'accès :

* Séquentiel
* Direct
* Dynamique

Un peu de vocabulaire :

* *Enregistrement logique :* unité de traitement
* *Control Interval (CI)* : unité de transfert physique en VSAM
* *Control Area (CA)* : un ensemble de CI. C'est l'unité d'allocation. Définir
  un fichier VSAM revient à allouer un nombre entier de CA.

## IDCAMS

IDCAMS est un programme utilitaire permettant de maintenir un catalogue de
fichier VSAM. **AMS** pour Access Method Service. Avec IDCAMS, on pourra définir
un cluster, un aix, un path, supprimer un cluster.

## ESDS : Entry Sequenced Data Set

L'ESDS en VSAM correspond au fichier séquentiel.

* Ajout à la fin du fichier seulement.
* L'accès est séquentiel.
* On ne peut pas supprimer d'enregistrement.
* Il est possible de modifier un enregistrement sans en changer la longueur.


Le `SELECT` pour un ESDS :

```COBOL
SELECT [OPTIONAL] fich-name ASSIGN TO AS-ddname
	ORGANIZATION IS SEQUENTIAL
	ACCESS MODE IS SEQUENTIAL
	.
``` 

### Chargement du fichier

Le fichier doit être vide avant l'ouverture en OUTPUT sauf si le cluster a été
défini avec `REUSE` (sinon, code 3000 à l'exécution).

```COBOL
OPEN OUTPUT fich-name
```

Le write crée un enregistrement à la suite des précédents.

```COBOL
WRITE masque
```

### Lecture du fichier

Le fichier qui a été chargé doit être ouvert en INPUT

```COBOL
OPEN INPUT fich-name
```

La lecture d'un enregistrement se fait avec un `READ`

```COBOL
READ fich-name 
	AT END MOVE "1" TO EOF
END-READ
```

### Ajout dans le fichier

Le fichier chargé doit être ouvert en `EXTEND`

```COBOL
OPEN EXTEND fich-name
```

Un `WRITE` ajoutera un enregistrement à la fin.    

Pour un fichier non chargé, l'ouverture en extend est possible que si `OPTIONAL`
est spécifié lors du `SELECT`

### Modification d'un enregistrement

Le fichier chargé doit être ouvert en `I-O`.

```COBOL
OPEN I-O fich-name
```

La modification se fait avec :

```COBOL
REWRITE masque
```

`REWRITE` modifie le dernier enregistrement qui a été lu. La longueur de
l'enregistrement ne peut pas changer !

