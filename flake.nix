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
          profiles.release = {
            features = if target != "web" then [ "cbc" ] else null;
            runTests = target == "linux";
          };
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
                add-inputs.overrideAttrs = oldAttrs: rec {
                  nativeBuildInputs = (oldAttrs.nativeBuildInputs or [ ]) ++ (with pkgs; [
                    trunk
                    wasm-bindgen-cli
                    binaryen
                    gtk3
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
                nci =
                  let
                    add-gtk.overrideAttrs = oldAttrs: {
                      nativeBuildInputs = (oldAttrs.nativeBuildInputs or [ ]) ++ (with pkgs; [
                        pkg-config
                        atk
                        gdk-pixbuf
                        gtk3
                        pango
                      ]);
                      XDG_DATA_DIRS = with pkgs; "${gtk3}/share/gsettings-schemas/${gtk3.name}";
                    };
                    add-cbc.overrideAttrs = oldAttrs: {
                      nativeBuildInputs = (oldAttrs.nativeBuildInputs or [ ]) ++ (with pkgs; [
                        cbc
                      ]);
                    };
                  in
                  nciFromPkgs pkgs "linux"
                    {
                      inherit add-gtk add-cbc;
                    }
                    {
                      inherit add-gtk add-cbc;
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
                    };
                devShells.default = config.nci.outputs.sd.devShell.overrideAttrs (oldAttrs: {
                  packages = (oldAttrs.packages or [ ]) ++ [ (lib.hiPrio pkgs.rust-bin.nightly.latest.rustfmt) ];
                });
                packages.default = config.nci.outputs.sd-gui.packages.release;
              };
          })
        # windows
        (parts.lib.mkFlake
          {
            inherit inputs;
          }
          {
            systems = [ "x86_64-linux" ];
            imports = [ nci.flakeModule ];
            perSystem = { self', config, lib, pkgs, system, ... }:
              {
                nci =
                  let
                    cbc = pkgs.pkgsCross.mingwW64.cbc.overrideAttrs (oldAttrs: {
                      # not marked as compatible with mingwW64, but it is
                      meta.platforms = lib.platforms.all;
                    });
                    windows-cross-compile = {
                      CARGO_BUILD_TARGET = "x86_64-pc-windows-gnu";
                      CARGO_TARGET_X86_64_PC_WINDOWS_GNU_LINKER = "${pkgs.pkgsCross.mingwW64.stdenv.cc}/bin/x86_64-w64-mingw32-gcc";
                      CARGO_TARGET_X86_64_PC_WINDOWS_GNU_RUSTFLAGS = ''
                        -L native=${pkgs.pkgsCross.mingwW64.windows.pthreads}/lib
                        -L native=${pkgs.pkgsCross.mingwW64.bzip2}/lib
                        -L native=${pkgs.pkgsCross.mingwW64.zlib}/lib
                        -L native=${cbc}/lib
                      '';
                    };
                  in
                  nciFromPkgs pkgs "windows"
                    {
                      inherit windows-cross-compile;
                    }
                    {
                      inherit windows-cross-compile;
                      windows-add-dlls.overrideAttrs = oldAttrs: {
                        postInstall = ''
                          cp ${pkgs.pkgsCross.mingwW64.stdenv.cc.cc}/x86_64-w64-mingw32/lib/{libgcc_s_seh-1.dll,libstdc++-6.dll} $out/bin
                          cp ${pkgs.pkgsCross.mingwW64.windows.mcfgthreads_pre_gcc_13}/bin/mcfgthread-12.dll $out/bin
                          cp ${pkgs.pkgsCross.mingwW64.bzip2}/bin/libbz2-1.dll $out/bin
                          cp ${pkgs.pkgsCross.mingwW64.zlib}/bin/zlib1.dll $out/bin
                          cp ${cbc}/bin/{libCbc-3.dll,libCbcSolver-3.dll,libCgl-1.dll,libClp-1.dll,libCoinUtils-3.dll,libOsi-1.dll,libOsiClp-1.dll} $out/bin
                        '';
                      };
                    };
              };
          }))
      # web
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
