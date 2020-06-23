let pkgs = import <nixpkgs> { };
in pkgs.haskellPackages.callPackage ./hw1.nix { }
