{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  inputs.nci.url = "github:yusdacra/nix-cargo-integration";
  inputs.nci.inputs.nixpkgs.follows = "nixpkgs";
  inputs.parts.url = "github:hercules-ci/flake-parts";
  inputs.parts.inputs.nixpkgs-lib.follows = "nixpkgs";

  outputs =
    inputs @ { parts
    , nci
    , ...
    }:
    parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" ];
      imports = [ nci.flakeModule ];
      perSystem = { config, lib, pkgs, ... }:
        {
          nci = {
            projects."sd".relPath = "";
            crates = {
              "sd-core" = { };
              "sd-visualiser" = { };
              "sd-web" = {
                overrides.add-inputs.overrideAttrs = oldAttrs: {
                  buildInputs = (oldAttrs.buildInputs or [ ]) ++ (with pkgs; [
                    pkg-config
                    libsoup
                    atk
                    gdk-pixbuf
                    pango
                    cairo
                    gtk3
                    webkitgtk
                  ]);
                };
              };
            };
            export = true;
          };
          devShells.default = lib.mkForce config.nci.outputs."sd".devShell;
          packages.default = config.nci.outputs."sd".packages.release;
        };
    };
}
