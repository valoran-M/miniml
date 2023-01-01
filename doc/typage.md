# Types

- int
- bool
- unit
- polymorph 'a 
- types -> types 
- struct(a)
- algé(a) (paramaétrique 'a)
- array(types)


Le types polymorph est données à chaques arguments
quand le types n'est pas spécifié. Donc quand on donne 
le type `'a` à une varaible, si jamais elle est utilisé 
avec une opération qui force le type alors elle prend ce type 
ce qui ne peut pas être explicité dans 
le rêgles ci dessous.

# Constante

```
------------  ----------------  -----------------   --------------
E |- n : int  E |- true : bool  E |- false : bool   E |- () : unit

                                E |- e1 : t
------------------------   -----------------------------
E |- { ... } : struct(s)   E |- [| e1; ... |] : array(t)  

------------------------------------
E |- Constr [(...)]? : algé(a)

```

# Opération

## Unaire

```
 E |- e : int
--------------
E |- - e : int

  E |- e : bool
-----------------
E |- not e : bool

```

## Binaire

```
 E |- e1 : int              E |- e2 : int
------------------------------------------
  E |- e1 (+ | - | * | / | mod) e2 : int


  E |- e1 : t1     E |- e2 : t1
---------------------------------
  E |- e1 (= | <> | == | !=) e2


  E |- e1 : int       E |- e2 : int
-------------------------------------
  E |- e1 (< | <= | >= | >) e2 : bool
```

# Varaibles


``` 
  E(x)=t
----------
E |- x : t

  E |- e1 : t1    E, x:t1 |- e2 : t2
--------------------------------------
      E |- let x = e1 in e2 : t2

```

# Condition

```
  E |- c : bool     E |- e : unit
-----------------------------------
            if c then e

  E | c : bool      E |- e1 : t     E |- e2 : t
-------------------------------------------------
              if c then e1 else e2 : t
```

# Fonction

```
       E, x : t1 |- e : t2               E, x : t |- e : te
---------------------------------     -------------------------
E |- fun (x : t1) -> e : t1 -> t2     E |- fun x -> e : t -> te

E |- e1 : t2 -> t1    E |- e2 : t2
--------------------------------------
            E |- e1 e2 : t1

  E, x : t1 |- e : t2         E, x : t |- e : t2
-----------------------   -------------------------
E |- Fix(x, t1, e) : t2   E |- Fix(x, None, e) : t2

```

# Struct 

```
E |- e : s     s.x : t      E |- e1 : s     s.x : t mutable     E |- e2 : t 
----------------------      -----------------------------------------------
    E |- e.x : t                       E |- e1.x <- e2 : unit

s.x1 : t1     E |- e1 : t1     ...     s.xN : tN     E |- eN : tN
-----------------------------------------------------------------
              E |- { x1 = e1; ...; xN = eN; } : s
```

# Algébrique

On pose:
`Uident(t1, ..., tn)`

```
E |- e1 : t1 ... E |- en : tn
-----------------------------
E |- Uident (e1, ..., en) : t
```

types paramétrique (`type 'a id = ...`) compliqué à écrire

# Tableau

```
  E |- e1 : t        E |- en : t          E |- e1 : int     E |- e2 : t
----------------------------------      ---------------------------------
 E |- [| e1; ...; e2 |] : t array       E |- Array.create e1 e2 : t array

  E |- e : t array    E |- n : int        E |- e1 : t array   E |- n : int   E |- e2 : t
------------------------------------    --------------------------------------------------
              E |- e.(n) : t                         E |- e1.(n) <- e2 : unit
```
