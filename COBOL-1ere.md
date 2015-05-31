# Cobol 1ère

Le COBOL est un langage extrèmement structuré :

* Divisions
* Sections
* Paragraphes
* Phrases
* Verbes, noms, opérateurs, signes de ponctuation

## Structure de base d'un programme COBOL

Un programme `COBOL` contient 4 divisions : 

* `IDENTIFICATION DIVISION`
* `ENVIRONMENT DIVISION`
* `DATA DIVISION`
* `PROCEDURE DIVISION`

### IDENTIFICATION DIVISION

Cette division sert à identifier le programme, renseigner l'auteur, la date,
etc. Elle contient que des paragraphes dont un seul est obligatoire.

```COBOL
IDENTIFICATION DIVISION.
PROGRAM-ID. nomProg.
[AUTHOR. nomAuteur.]
[DATE-WRITTEN. date1.]
[DATE-COMPILED. date2.]
```

Notes : le nom du programme est limité à 8 caractères sur mainframe, 10 sur
AS-400.

### ENVIRONMENT DIVISION.

Cette division donne des informations relatives à la gestion des entrées /
sorties et à l'environnement du programme. Cette division est optionnelle.

Ci-dessous, les champs importants utilisés lors du cours, cette liste n'est pas
exhaustive.

```COBOL
[ENVIRONEMENT DIVISION.]
[INPUT-OUTPUT SECTION.]
[FILE-CONTROL.]
```

### DATA DIVISION

Cette division regroupe les données, les variables, les structures.
Cette division est optionnelle.

```COBOL
[DATA DIVISION.]
[FILE-SECTION.]
[WORKING-STORAGE SECTION.]
[LINKAGE SECTION.]
```

Notes :

* `FILE-SECTION` définit les enregistrements des fichiers.
* `WORKING-STORAGE SECTION` définit les données autres que celles des fichiers.
* `LINKAGE SECTION` définit les données communes à un programme et un
  sous-progrmame (paramètres).
 
### PROCEDURE DIVISION.

Cette division contient les traitements à effectuer. Elle peut contenir des
paragraphes pour la clarté du programme. La dernière instruction à effectuer est
`STOP RUN`.

## Jeu de caractères et format d'un programme COBOL

### Jeu de caractères

* 0, 1, 2, ... 9 : Les chiffres
* A, B, C, ... Z : Les lettres
* L'espace (séparateur)
* + - * / ( ) , < > " ; . : $  = ' : Les caractères spéciaux

### Les mots COBOL

1 à 30 caractères : lettres / chiffres / tirets.

Les tirets ne sont ni au début ni en fin de mot. Un mot est toujours séparé de
blancs.

### Les constantes

* Constantes numériques : 1 à 18 chiffres avec signe éventuel et point décimal.
* Constantes non numériques : 1 à 120 caractères entres "
* Constantes figuratives : SPACE(S), ZERO(S), HIGH-VALUE, LOW-VALUE, ALL

### Les règles d'écritures

* L'espace est un séparateur de mots.
* Le point est significatif après les noms de divisions, sections, paragraphes, en
fin de divisions, section, paragraphe, phrase.
* La virgule et le point-virgule sont non significatif.
* Les ":" sont utilisés dans une forme particulière d'affectation (cours 2ème).

#### Format de codification

Le format date des cartes perforées (80 colonnes).

* 1 à 6 : Vestiges du COBOL (plus utilisé).
* 7 : Statut de la ligne 
	+ Espace : ligne de code
	+ * : ligne de commentaire
	+ D : debuging
* 8 à 72 : Instructions
	+ 8 : Marge A : noms de divisions, sections, paragraphes, nombre niveau
	+ 12 : Marbe B : Les instructions courantes.
* 73 à 80 : Ignoré par le compilateur (commentaire)

#### Déclaration de variable 

La déclaration de variable se fait comme ceci : 

```   
                   ---------------
                   |  		     |
 ----------------- | NomVariable | ---------------------------------
 | Numéro-niveau | | 			 | | PIC [IS] IMAGE-D [VALUE val]. |
 ----------------- | [filler] 	 | ---------------------------------
				   |			 |
				   ---------------

```

IMAGE-D : 

* 9 : numérique
* X : alphanumérique
* A : alphabétique

Toutes variables est associée à un numéro de niveau.

#### Les structures

Le numéro de niveau de base est 01. Les autres niveaux sont de 02 à 49. La
clause PIC(TURE) ne se place que dans les niveaux inférieurs. Après chaque
déclaration, on met un point.

```COBOL
01 date.
	03 an   PIC 9(4).
	03 mois PIC 99.
	03 jour PIC 99.
```

On laisse une marche de manoeuvre en laissant un numéro entre les deux niveaux.

#### Variables non structurées.

Le numéro de niveau pour une variable non structurée peut être 77 ou 01. Par
convention, on utilise le numéro 77.

#### Code caractère 9 

* Un S en entête signifie que le nombre est signé.
* un V signifie qu'il y a une virgule à la position du V.

```COBOL
77 nombre PIC S99V99.
```

### La représentation interne des données numérique

```COBOL
77 nb PIC S9(4)V9(3) [[USAGE IS] TYPE]
```

TYPE est :

* DISPLAY (par défaut)
* BINARY
* PACKED-DECIMAL
* COMP-5

#### DISPLAY

1 chiffre par octet.

Exemple : `77 nb1 PIC 999` : chiffres non signés.

Si on place 123 dans nb1, on aura 3 octets avec : F1F2F3

Exemple 2 : 77 nb2 PIC S999V99. 
Le signe est codé sur les 4 premiers bits du dernier octet. 

* C si plus grand ou égal à 0.
* D si négatif.

Si on place 123.45 dans nb2, on aura : F1F2F3F4C5

#### PACKED-DECIMAL

On place deux chiffres par octet. Le signe est dans les 4 dernier bits du
dernier octet.

Exemple : `77 nb1 PIC S9(3) packed-decimal`

Si on place 123 dans nb1, on aura : 123C

#### BINARY

Un bit de signe :

* Si positif : représentation par position.
* Si négatif : représentation par complément à 2.

### La clause VALUE

```COBOL
[ VALUE [IS] constante ]
```

La clause VALUE sert à définir une valeur initiale à une donnée au lancement du
programme. La clause VALUE n'a de sens qu'en WORKING-STORAGE.

## Instructions de bases et fichiers séquentiels.

### Arrêt d'exécution

```COBOL
STOP RUN.
```

### Affectation

```COBOL
MOVE emetteur TO recepteur(s)
```

* émetteur : variable ou constante
* récepteur(s) : une ou plusieurs variables.

#### Affectation impliquant une structure

Une autre instruction est disponible : MOVE CORR. Cette instruction déplace les
champs de même noms d'une structure dans une autre structure.

```COBOL
01 mat.
	03 an PIC XX.
	03 num PIC XXX.

01 matEdt.
	03 an PIC XX
	03 PIC X.
	03 num PIC XXX.

MOVE CORR mat TO matEdt

est égal à :

MOVE an of mat TO an of matEdt
MOVE num of mat TO num of matEdt
```

### Affichage

```COBOL 
DISPLAY var / constante [ var / constante ]
```

DISPLAY imprime sur SYSOUT.

## Fichiers séquentiels

```COBOL
ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
	SELECT fich ASSIGN TO liaison
	.
```

Le nom de liaison doit figurer dans l'ordre DD du JCL. Le nom de liaison est
limité à 8 caractères.

```COBOL
DATA DIVISION.
FILE SECTION.
FD fich.
01 masque1.
	03 ...
	03 ...
```

Tant que le fichier n'est pas ouvert, ces variables n'existent pas.

* OPEN : ouvrir un fichier 
* CLOSE : fermer un fichier
* READ : lire un fichier ouvert en INPUT
* WRITE : écrire dans un fichier ouvert en OUTPUT

```COBOL
OPEN {INPUT | OUTPUT | EXTEND} fich1 fich2
	 {INPUT | OUTPUT | EXTEND} fich3

READ fich1 
	AT END MOVE "1" TO EOF
END-READ

WRITE masque1 

CLOSE fich1 fich2 fich3
```

## PERFORM

Le PERFORM sert à exécuter une procédure ou un ensemble de procédures.

### PERFORM simple 

```COBOL
PERFORM proc1 [THRU proc2]
```

Cette instruction exécute proc1. Si THRU proc2 est spécifié, cela exécutera
proc1 jusqu'à proc2.

### PERFORM UNTIL

```COBOL
PERFORM proc1 [THRU proc2]
	[WITH TEST BEFORE / AFTER]
	UNTIL condition
```

### PERFORM en ligne (bloc perform)

```COBOL
PERFORM [WITH TEST BEFORE / AFTER]
	UNTIL CONDITION

	instructions

END-PERFORM
```

### PERFORM N TIMES

```COBOL
PERFORM [proc1 [THRU proc2]]
	id1/lit1 TIMES
[END-PERFORM]
```

Si on modifie id1 dans la procédure, le nombre de fois que la boucle s'exécute
ne change pas.

### PERFORM VARYING (pour)

```COBOL
PERFORM [proc1 [THRU proc2]]
	VARYING id1 FROM lit2/id2 
	BY lit3/id3 UNTIL condition
	
	[AFTER id4 FROM id5/lit5
	BY lit6/id6 UNTIL condition]

	[AFTER id7 FROM id8/lit8
	BY lit9/id9 UNTIL condition]

	instructions

[END-PERFORM]
```

Les `AFTER` correspondent à des imbrications de `pour`. Si on utilise AFTER, on
ne peut pas faire de PERFORM en ligne.

## Conditions

* Opérandes : variables / constantes
* Opérateurs : <, >, =, <=, >=
* Opérateur logiques : OR, AND, NOT

### IF 

```COBOL
IF condition [THEN]
	instructions
[ELSE
	instructions]
END-IF
```

## Opérations arithmétiques

### COMPUTE

```COBOL
COMPUTE 
	id1 [ROUNDED] = expression 
	[ON SIZE ERROR proc1]
	[NOT ON SIZE ERROR proc2]
END-COMPUTE
```

Les signes : 

* +, -, \*, /, \*\* 

```COBOL
COMPUTE nb1 = nb1 + 1
	ON SIZE ERROR DISPLAY "SIZE ERROR sur nb1"
END-COMPUTE
```

Il faut toujours faire les divisions en dernier dans un COMPUTE.

### ADD

```COBOL
ADD id1 / lit1 [id2 / lit2]
	TO id3 [ROUNDED] [id4 [ROUNDED...]
	[ON SIZE ERROR proc1]
	[NOT ON SIZE ERROR proc2]
[END-ADD]
```

### SUBTRACT

```COBOL
SUBTRACT id1 / lit1 [id2 / lit2]
	FROM id3 / lit3
	GIVING id4 [ROUNDED] [id5 [ROUNDED]...]
	[ON SIZE ERROR proc1]
	[NOT ON SIZE ERROR proc2]
[END-SUBTRACT]
```

`id4 = id3|lit3 - id1|lit1`

### MULTIPLY

```COBOL
MULTIPLY id1/lit1 BY id2/lit2
	GIVING id3 [ROUNDED] [id4 [ROUNDED] ...]
	[ON SIZE ERROR proc1]
	[NOT ON SIZE ERROR proc2]
[END-MULTIPLY]
```

### DIVIDE

```COBOL
DIVIDE id1/lit1 BY id2/lit2
	GIVING id3 [ROUNDED] [id4 [ROUNDED] ...]
	[REMAINDER id5]
	[ON SIZE ERROR proc1]
	[NOT ON SIZE ERROR proc2]
[END-DIVIDE]
```

Exemple : 20 / 3 

```COBOL
DIVIDE 20 BY 3 GIVING result REMAINDER reste
```

* result = 6
* reste = 2

## Les tableaux

Les indices commencent à 1. Il est mieux de déclarer les variables d'indices en
BINARY.

### Accéder à un tableau

```COBOL
nomTab(i)
nomTab(i j)
nomTab(i j k)
```

### Clause OCCURS

La clause OCCURS permet de déclarer un tableau. Un tableau se trouve
obligatoirement dans une structure

```COBOL
01 tab.
	03 dim1 OCCURS 4.
		05 dim2 occurs 3 PIC 99.

01 identites.
	03 uneIdentite OCCURS 80.
		05 nom PIC X(25).
		05 prenom PIC X(25).
```

## REDEFINES

REDEFINES permet de donner plusieurs interprétations à une zone mémoire en la
redéfinissant partiellement ou complètement.

Règles :

* Doit avoir le même numéro de niveau.
* Pas de VALUE sur un REDEFINES.
* Pas en FILE-SECTION.
* Pas de REDEFINES sur un REDEFINES.

```COBOL
01 structure.
	03 ch1 PIC X(25).
	03 ch2 PIC X(30).

01 structure2 redefines structure.
	03 ch3 PIC X(40).
	03 ch4 PIC X(15).
```

```
           ----------------
structure  | 25   | 30    |
		   ----------------
structure2 |    40   | 15 |
		   ----------------
```

```COBOL
01 nomsMois.
	03 PIC X(9) VALUE "Janvier".
	03 PIC X(9) VALUE "Février".
	...
	03 PIC X(9) VALUE "Décembre".

01 lesMois redefines nomsMois.
	03 unMois OCCURS 12 PIC X(9).
```

## Quelques constantes de date / heure

* DATE (YYMMDD)
* DATE YYYYMMDD
* DAY (YYDDD)
* DAY YYYYDDD
* DAY-OF-WEEK (D)
* TIME (HHMMSSHsHs)

## INITIALIZE

```COBOL
INITIALIZE id1 [id2] 
	[REPLACING {ALPHABETIC / NUMERIC / ALPHANUMERIC / 
				NUMERIC-EDITED / ALPHANUMERIC-EDITED}
	BY id3/lit3]
```

## PIC d'édition

### Caractère d'insertion

#### Pour tout type de données

* B : Insère un blanc
* O : Insère un O
* / : Insère un /

#### Pour les données numériques

##### Sans tenir compte de la valeur

* , : provoque l'insertion d'une virgule
* $ : provoque l'insertion d'un $

##### En tenant compte de la valeur

Edition de signe :

* + : Insère un + ou un - en fonction du signe.
* - : Insère un - si le nombre est négatif, blanc sinon.
* . : Insère un point décimal.

### Caractères de substitution et insertion flottante

#### Substitution simple

Pour remplacer les 0 non significatifs :

* Z : remplace par des blancs
* * : remplace par des *

#### Substitution et insertion flottante

* + ou - ou $ : Joue le rôle du Z et insère le signe en tête d'édition.

`BLANK WHEN ZERO` : si la donnée est nulle, la variable est mise à blanc.
