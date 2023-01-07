
(**
    Fait l'inference de type à l'aide de l'algo W 
    vue en cours (article mis en refernce dans le README)

    @param prog Mml.prog programme
    @param file string nom du fichier

    @return Mml.typ type du programme
  *)
val type_inference : Mml.prog -> string -> Mml.typ
