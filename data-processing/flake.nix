{
  description = "data processing and analysis course home works";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      with nixpkgs.legacyPackages.${system}; {
        devShell = mkShell {
          buildInputs = [
            (python39.withPackages (ps:
              with ps; [
                nltk
                notebook
                ipykernel
                pandas
                scikitlearn
                pyenchant
              ]))
          ];
        };
      });
}
