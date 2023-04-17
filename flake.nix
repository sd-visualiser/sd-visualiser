{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  inputs.parts.url = "github:hercules-ci/flake-parts";
  inputs.parts.inputs.nixpkgs-lib.follows = "nixpkgs";
  inputs.pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
  inputs.pre-commit-hooks.inputs.nixpkgs.follows = "nixpkgs";
  inputs.nci.url = "github:yusdacra/nix-cargo-integration";
  inputs.nci.inputs.nixpkgs.follows = "nixpkgs";
  inputs.nci.inputs.parts.follows = "parts";

  outputs =
    inputs @ { parts
    , nci
    , pre-commit-hooks
    , ...
    }:
    parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" ];
      imports = [ nci.flakeModule ];
      perSystem = { self', config, lib, pkgs, system, ... }:
        {
          checks = {
            pre-commit-check = pre-commit-hooks.lib.${system}.run {
              src = ./.;
              hooks = {
                cargo-check.enable = true;
                clippy.enable = true;
                rustfmt.enable = true;
              };
            };
          };
          nci = {
            projects."sd" =
              {
                relPath = "";
              };
            crates = {
              "sd-core" = { };
              "sd-hyper" = { };
              "sd-graphics" = {
                overrides = {
                  add-inputs.overrideAttrs = oldAttrs: rec {
                    nativeBuildInputs = (oldAttrs.nativeBuildInputs or [ ]) ++ (with pkgs; [
                      cbc
                    ]);
                  };
                };
              };
              "sd-gui" =
                {
                  runtimeLibs = with pkgs; [
                    # graphics libraries are loaded at runtime
                    # see: https://scvalex.net/posts/63/
                    libGL
                    libxkbcommon
                    wayland
                    xorg.libX11
                    xorg.libXcursor
                    xorg.libXi
                    xorg.libXrandr
                  ];
                  overrides = {
                    windows-cross-compile = {
                      CARGO_TARGET_X86_64_PC_WINDOWS_GNU_LINKER = "${pkgs.pkgsCross.mingwW64.stdenv.cc}/bin/x86_64-w64-mingw32-gcc";
                      CARGO_TARGET_X86_64_PC_WINDOWS_GNU_RUSTFLAGS = "-L native=${pkgs.pkgsCross.mingwW64.windows.pthreads}/lib";
                    };
                    add-inputs.overrideAttrs = oldAttrs: rec {
                      nativeBuildInputs = (oldAttrs.nativeBuildInputs or [ ]) ++ (with pkgs; [
                        trunk
                        wasm-bindgen-cli
                      ]);
                    };
                  };
                };
            };
          };
          devShells.default = lib.mkForce config.nci.outputs."sd".devShell;
          packages.default = config.nci.outputs."sd-gui".packages.release;
        };
    };
}
