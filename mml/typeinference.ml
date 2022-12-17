module VSet = Set.Make (String)

type schema = {
    vars : VSet.t;
    typ : Mml.typ;
  }
