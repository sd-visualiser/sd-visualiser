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
              "sd-gui" =
                let
                  # graphics libraries are loaded at runtime
                  # see: https://scvalex.net/posts/63/
                  libPath = with pkgs; lib.makeLibraryPath [
                    libGL
                    libxkbcommon
                    wayland
                    xorg.libX11
                    xorg.libXcursor
                    xorg.libXi
                    xorg.libXrandr
                  ];
                in
                {
                  overrides.add-inputs.overrideAttrs = oldAttrs: rec {
                    nativeBuildInputs = (oldAttrs.nativeBuildInputs or [ ]) ++ (with pkgs; [
                      wasm-bindgen-cli
                    ]);
                    LD_LIBRARY_PATH = libPath;
                    CARGO_TARGET_X86_64_PC_WINDOWS_GNU_LINKER = "${pkgs.pkgsCross.mingwW64.stdenv.cc}/bin/x86_64-w64-mingw32-gcc";
                    CARGO_TARGET_X86_64_PC_WINDOWS_GNU_RUSTFLAGS = "-L native=${pkgs.pkgsCross.mingwW64.windows.pthreads}/lib";
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
