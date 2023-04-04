{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  inputs.parts.url = "github:hercules-ci/flake-parts";
  inputs.parts.inputs.nixpkgs-lib.follows = "nixpkgs";
  inputs.nci.url = "github:yusdacra/nix-cargo-integration?rev=93801e6a0b0790f36b062ee1369e3f674bc941a5"; # TODO(@NickHu): change rev when https://github.com/yusdacra/nix-cargo-integration/pull/119 merged
  inputs.nci.inputs.nixpkgs.follows = "nixpkgs";
  inputs.nci.inputs.parts.follows = "parts";

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
            stdenv = pkgs.clangStdenv; # needed for rust-sitter wasm compatibility
            projects."sd".relPath = "";
            crates = {
              "sd-core" = { };
              "sd-hyper" = { };
              "sd-graphics" = { };
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
                        cbc
                      ]);
                    };
                  };
                };
            };
            export = true;
          };
          devShells.default = lib.mkForce config.nci.outputs."sd".devShell;
          packages.default = config.nci.outputs."sd-gui".packages.release;
        };
    };
}
