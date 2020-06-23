let pkgs = import <nixpkgs> { };
in pkgs.haskellPackages.callPackage ./hw0.nix { }
