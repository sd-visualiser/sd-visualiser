{
  nixConfig = {
    extra-substituters = [ "https://sd-visualiser.cachix.org" ];
    extra-trusted-public-keys = [
      "sd-visualiser.cachix.org-1:vGc1hqbmMqHo/iWJyOzn9rCZEAn7ebUrFT7Gp4tM700="
    ];
  };
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  inputs.parts.url = "github:hercules-ci/flake-parts";
  inputs.parts.inputs.nixpkgs-lib.follows = "nixpkgs";
  inputs.nci.url = "github:yusdacra/nix-cargo-integration";
  inputs.nci.inputs.nixpkgs.follows = "nixpkgs";
  inputs.nci.inputs.parts.follows = "parts";
  inputs.nci.inputs.rust-overlay.follows = "rust-overlay";
  inputs.rust-overlay.url = "github:oxalica/rust-overlay";
  inputs.rust-overlay.inputs.nixpkgs.follows = "nixpkgs";

  outputs =
    inputs@{
      parts,
      nixpkgs,
      nci,
      rust-overlay,
      ...
    }:
    parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" ];
      imports = [
        nci.flakeModule
      ];
      perSystem =
        {
          pkgs,
          config,
          system,
          lib,
          ...
        }:
        {
          _module.args.pkgs = import nixpkgs {
            inherit system;
            overlays = [
              inputs.rust-overlay.overlays.default
            ];
          };

          devShells.default = config.nci.outputs.sd-gui.devShell.overrideAttrs (attrs: {
            LIBCLANG_PATH = "${pkgs.libclang.lib}/lib";
            packages =
              (attrs.packages or [ ])
              ++ (with pkgs; [
                (lib.hiPrio rust-bin.nightly.latest.rustfmt)
                cargo-insta
                # it seems devshells can not be set to specific targets, so we add cbc here
                cbc
                # add for convienience
                trunk
              ]);
          });

          packages = rec {
            default = linux;
            linux = config.nci.outputs.sd-gui.packages.release;
            web = config.nci.outputs.sd-gui.allTargets."wasm32-unknown-unknown".packages.release;
          };

          nci = {
            projects.sd = {
              path = ./.;
              profiles.dev = { };
              profiles.release = {
                runTests = false;
              };
            };
            crates = {
              sd-gui = {
                targets."x86_64-unknown-linux-gnu" = {
                  default = true;
                };

                targets."wasm32-unknown-unknown" = {
                  drvConfig.mkDerivation = {
                    nativeBuildInputs = with pkgs; [
                      trunk
                      wasm-bindgen-cli
                      binaryen
                    ];

                    buildPhase = ''
                      cd sd-gui
                      HOME=$TMPDIR \
                        trunk -v build \
                        --public-url /sd-visualiser \
                        --dist $out \
                        --release \
                        --skip-version-check \
                        --offline \
                        ''${cargoBuildFlags:-}
                    '';
                    # disable install phase because trunk will directly output to $out
                    dontInstall = true;
                  };
                };

                runtimeLibs = with pkgs; [
                  libGL
                  libxkbcommon
                  wayland
                ];
              };
            };
          };
        };
    };
}
