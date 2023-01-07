# Mini Ml

Le Mini-Ml est un sous ensemble de ML, il est composé de 2 parties :
1. Les types
2. Les expressions

## Fonctionnement 

<img src="./doc/diagram.png" 
   width="500"/>

### Lexer:
  
  Utilise Ocamllex et génère des Token définis dans le parser.

### Parser:
  
  Utilise Menhir, prend les token générés par le lexer
  et crée des expressions qui respectent la grammaire.
  
  [grammaire](./doc/grammar.txt)

### Typdef Verif:

  Parcourt les définitions de types pour voir si les
  types sont connus.

### Type Checker:

  Vérifie le bon typage du programme sans polymorphisme et sans contrainte.

  /!\ Ne fonctionne plus

### Type Infer:
  
  Détermine les types des expressions et vérifie les contraintes,
  si elles ne sont pas respectées il produit une erreur.

  [règle de typage](./doc/typage.md)

  (Utilisation de l'algo W [^1])

## Les types

les types sont les suivants :

### Bases:

- `int`   : les entiers
- `bool`  : les booléens
- `char`  : les charactères
- `string`: les chaines de charactères
- `unit`  : l'unité

### Utilisateur 
- `type s = {id1 = e1; ...; idn = en;}` : Structures
- `type s = | Enum1 |Enum2 ...` : Types énumérés
- `type s = | Cons1 of t1 * ... * tn | ... | Const n` : Types algébriques
- `type 'a s = | Some of 'a` : Types algébriques paramétré

## Les expressions

Les expressions sont les suivantes :
- `n` : un entier
- `true` : un booléen vrai
- `false` : un booléen faux
- `'c'` : un charactère 
- `"..."` : une chaine de charactère
- `String.length` : taille de la chaine de charactère s
- `s.[i]` : récupère le i ème charactère de la chaine de charactère s 
- `()` : l'unité
- `e1 ~ e2` : Les expression binaires
    - ~ est un opérateur binaire parmis `+`, `-`, `*`, `/`, `mod`, `==`, `!=`, `<`, `<=`, `>`, '>=', `&&`, `||`
    - l'égalité structurelle et la négation `=`, `<>`
- `~ e` : Les expression unaires
    - ~ est un opérateur unaire parmis `-`, `not`
- `if e1 then e2 else e3` : l'expression conditionnelle
- `let x = e1 in e2` : l'expression de déclaration de variable
- `let rec x = e1 in e2` : l'expression de déclaration de fonction récursive
- `fun x -> e` : l'expression de déclaration de fonction anonyme
- `fun () -> e` : fonction avec type unit en entrée
- `e1 e2` : l'expression d'appel de fonction
- `{a1 = e1; ...; an = en;}` : l'expression de création de structure
- `e.a` : l'expression d'accès à un champ de structure
- `e1.a <- e2` : l'expression d'écriture à un champ de structure
- `[| e1; ...; en |]` : tableau mutable
- `Array.make n e` : tableau de taille n contentant la valeur de e
- `Array.length a` : taille du tableau a
- `e.(i)` : case i du tableu e
- `t.(i) <- e` : chnage la valeur de la case i du tableau t en la valeur de e 
- `Enum` : Construction d'une variable enum
- `Constr(e1, ...)`: Construction d'une variable algébrique
- `match e with | p1 -> e1 ... | pn -> en` : pattern matching sur e avec les pattern pi

## Pattern 

```
pattern := 
  | Jok
  | Var         of string
  | Int         of int
  | Bool        of true
  | Construct   of (string, pattern list)

```

## Affichage 

On peut affiches les valeur avec :

- `print_int n`
- `print_book b`
- `print_char c`
- `print_string s`
- `print_endline s`
- `print_newline ()`

# Exemple

- [fact](./tests/fact.mml)
- [fact2](./tests/fact2.mml)
- [syracuses](./tests/syracuse.mml)
- [sort](./tests/array/sort.mml)
- [Crible d'Ératosthène](./tests/array/crible.mml)

# Message erreur

Quand il y a une erreur la ligne et la colonne est toujour indiqué
et affiché comme ça:
```
File "file", line l, character n-n:
l| .............
      ^^^
Error: ....
```

# Utilisation 

compilation:
```
dune build
```

lancement:
```
dune exec -- miniml file.mml [-a]   # exécute le code (-a affiche les adresse dans le tas)
dune exec -- debug file.mml         # affiche l'AST
```

# Avancée

|              | mmllexer | mmlparser | typechecker | interpreter |
|:------------:|:--------:|:---------:|:-----------:|:-----------:|
| Arithmétique |     X    |     X     |      X      |      X      |
|   Variables  |     X    |     X     |      X      |      X      |
|   Fonctions  |     X    |     X     |      X      |      X      |
|  Structures  |     X    |     X     |      X      |      X      |
|   Récursion  |     X    |     X     |      X      |      X      |

# Références

[^1]: B. Heeren, J. Hage, and S. D. Swierstra. Generalizing Hindley-Milner type inference algorithms. Technical Report UU-CS-2002-031, Institute of Information and Computing Science, University Utrecht, Netherlands, July 2002.
