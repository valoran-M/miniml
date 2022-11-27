# Mini Ml

Le Mini-Ml est un sous ensemble de ML, il est composé de 2 parties :
1. Les types
2. Les expressions

### Les types

Les types sont les suivants :
- `int` : les entiers
- `bool` : les booléens
- `unit` : l'unité
- `typedef s = {...}` : les structures définis par l'utilisateur

### Les expressions

Les expressions sont les suivantes :
- `n` : un entier
- `true` : un booléen vrai
- `false` : un booléen faux
- `()` : l'unité
- `e1 ~ e2` : Les expression binaires
    - ~ est un opérateur binaire parmis `+`, `-`, `*`, `/`, `mod`, `==`, `!=`, `<`, `<=`, `&&`, `||`
- `~ e` : Les expression unaires
    - ~ est un opérateur unaire parmis `-`, `not`
- `if e1 then e2 else e3` : l'expression conditionnelle
- `let x = e1 in e2` : l'expression de déclaration de variable
- `let rec x = e1 in e2` : l'expression de déclaration de fonction récursive
- `fun x -> e` : l'expression de déclaration de fonction anonyme
- `e1 e2` : l'expression d'appel de fonction
- `{a1 = e1; ...; an = en}` : l'expression de création de structure
- `e.a` : l'expression d'accès à un champ de structure
- `e1.a <- e2` : l'expression d'écriture à un champ de structure



# 