let pkgs = import <nixpkgs> { };
in pkgs.haskellPackages.callPackage ./practice1.nix { }
