{
  nixConfig = {
    extra-substituters = [ "https://sd-visualiser.cachix.org" ];
    extra-trusted-public-keys = [ "sd-visualiser.cachix.org-1:vGc1hqbmMqHo/iWJyOzn9rCZEAn7ebUrFT7Gp4tM700=" ];
  };
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  inputs.parts.url = "github:hercules-ci/flake-parts";
  inputs.parts.inputs.nixpkgs-lib.follows = "nixpkgs";
  inputs.nci.url = "github:yusdacra/nix-cargo-integration";
  inputs.nci.inputs.nixpkgs.follows = "nixpkgs";
  inputs.nci.inputs.parts.follows = "parts";
  inputs.nci.inputs.rust-overlay.follows = "rust-overlay";
  inputs.rust-overlay.url = "github:oxalica/rust-overlay";

  outputs =
    inputs @ { parts
    , nixpkgs
    , nci
    , rust-overlay
    , ...
    }:
    let
      nciFromPkgs = pkgs: target: extraDepsOverrides: extraOverrides: {
        # common config across targets
        projects.sd = {
          relPath = "";
          profiles.release.runTests = target == "linux";
        };
        crates = {
          sd-core = {
            overrides = {
              add-inputs.overrideAttrs = oldAttrs: rec {
                nativeBuildInputs = (oldAttrs.nativeBuildInputs or [ ]) ++ (with pkgs; [
                  cargo-insta
                ]);
              };
            };
          };
          sd-graphics = {
            overrides = {
              add-inputs.overrideAttrs = oldAttrs: rec {
                nativeBuildInputs = (oldAttrs.nativeBuildInputs or [ ]) ++ (with pkgs; [
                  cbc
                ]);
              };
            };
          };
          sd-gui =
            {
              export = true;
              renameTo = "sd-gui-${target}";
              overrides = {
                windows-cross-compile = {
                  CARGO_TARGET_X86_64_PC_WINDOWS_GNU_LINKER = "${pkgs.pkgsCross.mingwW64.stdenv.cc}/bin/x86_64-w64-mingw32-gcc";
                  CARGO_TARGET_X86_64_PC_WINDOWS_GNU_RUSTFLAGS = "-L native=${pkgs.pkgsCross.mingwW64.windows.pthreads}/lib";
                };
                add-inputs.overrideAttrs = oldAttrs: rec {
                  nativeBuildInputs = (oldAttrs.nativeBuildInputs or [ ]) ++ (with pkgs; [
                    trunk
                    wasm-bindgen-cli
                    binaryen
                  ]);
                };
              } // extraOverrides;
              depsOverrides = extraDepsOverrides;
            };
        };
      };
    in
    nixpkgs.lib.recursiveUpdate
      (nixpkgs.lib.recursiveUpdate
        (parts.lib.mkFlake { inherit inputs; }
          {
            systems = [ "x86_64-linux" ];
            imports = [ nci.flakeModule ];
            perSystem = { self', config, lib, pkgs, system, ... }:
              {
                _module.args.pkgs = import inputs.nixpkgs {
                  inherit system;
                  overlays = [
                    inputs.rust-overlay.overlays.default
                  ];
                  config = { };
                };
                nci = nciFromPkgs pkgs "linux"
                  {
                    gtk.overrideAttrs = oldAttrs: {
                      nativeBuildInputs = (oldAttrs.nativeBuildInputs or [ ]) ++ (with pkgs; [
                        pkg-config
                        atk
                        gdk-pixbuf
                        gtk3
                        pango
                      ]);
                    };
                  }
                  {
                    egui-wayland = {
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
                    };
                    gtk.overrideAttrs = oldAttrs: {
                      nativeBuildInputs = (oldAttrs.nativeBuildInputs or [ ]) ++ (with pkgs; [
                        pkg-config
                        atk
                        gdk-pixbuf
                        gtk3
                        pango
                      ]);
                      XDG_DATA_DIRS = with pkgs; "${gtk3}/share/gsettings-schemas/${gtk3.name}";
                    };
                  };
                devShells.default = config.nci.outputs.sd.devShell.overrideAttrs (oldAttrs: {
                  packages = (oldAttrs.packages or [ ]) ++ [ (lib.hiPrio pkgs.rust-bin.nightly.latest.rustfmt) ];
                });
                packages.default = config.nci.outputs.sd-gui.packages.release;
              };
          })
        (parts.lib.mkFlake
          {
            inherit inputs;
          }
          {
            systems = [ "x86_64-linux" ];
            imports = [ nci.flakeModule ];
            perSystem = { self', config, lib, pkgs, system, ... }:
              {
                nci = nciFromPkgs pkgs "windows"
                  {
                    set-target = {
                      CARGO_BUILD_TARGET = "x86_64-pc-windows-gnu";
                    };
                  }
                  {
                    set-target = {
                      CARGO_BUILD_TARGET = "x86_64-pc-windows-gnu";
                    };
                  };
              };
          }))
      (parts.lib.mkFlake { inherit inputs; } {
        systems = [ "x86_64-linux" ];
        imports = [ nci.flakeModule ];
        perSystem = { self', config, lib, pkgs, system, ... }:
          {
            nci = nciFromPkgs pkgs "web"
              {
                set-target = {
                  CARGO_BUILD_TARGET = "wasm32-unknown-unknown";
                };
              }
              {
                build-with-trunk.overrideAttrs = oldAttrs: {
                  # add trunk and other dependencies
                  nativeBuildInputs =
                    (oldAttrs.nativeBuildInputs or [ ])
                    ++ (with pkgs; [ trunk wasm-bindgen-cli binaryen ]);
                  # override build phase to build with trunk instead
                  buildPhase = ''
                    cd sd-gui
                    HOME=$TMPDIR \
                      trunk -v build \
                      --public-url /sd-visualiser \
                      --dist $out \
                      --release \
                      ''${cargoBuildFlags:-}
                  '';
                  # disable install phase because trunk will directly output to $out
                  dontInstall = true;
                };
              };
          };
      });
}
