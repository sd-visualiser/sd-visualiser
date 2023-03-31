{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };
    nci = {
      url = "github:yusdacra/nix-cargo-integration?rev=44a29689e1b5d7ad415016a81271c40997a9c9d5"; # TODO(@NickHu): change rev when https://github.com/yusdacra/nix-cargo-integration/pull/119 merged
      inputs = {
        nixpkgs.follows = "nixpkgs";
        parts.follows = "parts";
      };
    };
    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs = {
        nixpkgs.follows = "nixpkgs";
      };
    };
  };

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
                rustfmt.enable = true;
                clippy.enable = true;
                cargo-check.enable = true;
              };
            };
          };
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
                      ]);
                    };
                  };
                };
            };
            export = true;
          };
          devShells.default = lib.mkForce (config.nci.outputs."sd".devShell.overrideAttrs (oldAttrs: {
            shellHook = ''
              ${oldAttrs.shellHook or ""}
              ${self'.checks.pre-commit-check.shellHook}
            '';
          }));
          packages.default = config.nci.outputs."sd-gui".packages.release;
        };
    };
}
