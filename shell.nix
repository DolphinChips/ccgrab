{ pkgs ? import <nixos> {} }:
pkgs.mkShell {
  buildInputs = with pkgs; with ocamlPackages; [
    ocaml
    dune_3
    lambdasoup
    cmdliner_1_1
    lwt_ppx
    uri
    cohttp-lwt-unix
    findlib
    core
    core_unix
    camlzip
    utop
    ocamlformat
    magic-mime
    epubcheck
  ];
}
