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
SELECT [optional] fich-name ASSIGN TO AS-ddname
	ORGANIZATION IS SEQUENTIAL
	ACCESS MODE IS SEQUENTIAL
	.
``` 

