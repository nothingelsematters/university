{ pkgs ? import <nixpkgs> { } }:

pkgs.buildFHSUserEnv rec {
  name = "databases-env";

  targetPkgs = pkgs:
    with pkgs; [
      postgresql_12

      # goodies
      git
      zsh
      exa
      fd
      bat
      ripgrep
    ];

  runScript = ''
    sh -c "NIX_NAME=${name} zsh"
  '';
}
