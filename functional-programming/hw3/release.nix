let pkgs = import <nixpkgs> { };
in pkgs.haskellPackages.callPackage ./hw3.nix { }
