with import <nixpkgs> { };
pkgs.mkShell rec {
  name = "ocaml-env";
  buildInputs = [
    ocaml
    ocamlformat
    opam
    rlwrap
    dune_2
    # packages
    ocamlPackages.merlin
    ocamlPackages.ocaml-lsp
    ocamlPackages.opam-state
    ocamlPackages.opam-core
    ocamlPackages.ocp-indent
    ocamlPackages.menhir
  ];
}
