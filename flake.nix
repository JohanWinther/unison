{
  description = "Unison";
  nixConfig = {
    extra-substituters = [ "https://unison.cachix.org" ];
    extra-trusted-public-keys = [
      "unison.cachix.org-1:i1DUFkisRPVOyLp/vblDsbsObmyCviq/zs6eRuzth3k="
    ];
  };
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };
  outputs = { self, nixpkgs, flake-utils, haskellNix, flake-compat }:
    flake-utils.lib.eachSystem [
      "x86_64-linux"
      "x86_64-darwin"
      "aarch64-darwin"
    ]
      (system:
        let
          overlays = [
            haskellNix.overlay
            (final: prev: {
              unison-project = with prev.lib.strings;
                let
                  cleanSource = pth:
                    let
                      src' = prev.lib.cleanSourceWith {
                        filter = filt;
                        src = pth;
                      };
                      filt = path: type:
                        let
                          bn = baseNameOf path;
                          isHiddenFile = hasPrefix "." bn;
                          isFlakeLock = bn == "flake.lock";
                          isNix = hasSuffix ".nix" bn;
                        in
                        !isHiddenFile && !isFlakeLock && !isNix;
                    in
                    src';
                in
                final.haskell-nix.project' {
                  src = cleanSource ./.;
                  projectFileName = "stack.yaml";
                  modules = [
                    # enable profiling
                    {
                      enableLibraryProfiling = true;
                      profilingDetail = "none";
                    }
                    # remove buggy build tool dependencies
                    ({ lib, ... }: {
                      # this component has the build tool
                      # `unison-cli:unison` and somehow haskell.nix
                      # decides to add some file sharing package
                      # `unison` as a build-tool dependency.
                      packages.unison-cli.components.exes.cli-integration-tests.build-tools =
                        lib.mkForce [ ];
                    })
                  ];
                  branchMap = {
                    "https://github.com/unisonweb/configurator.git"."e47e9e9fe1f576f8c835183b9def52d73c01327a" =
                      "unison";
                  };
                };
            })
            (final: prev: {
              unison-stack = prev.symlinkJoin {
                name = "stack";
                paths = [ final.stack ];
                buildInputs = [ final.makeWrapper ];
                postBuild =
                  let
                    flags = [ "--no-nix" "--system-ghc" "--no-install-ghc" ];
                    add-flags =
                      "--add-flags '${prev.lib.concatStringsSep " " flags}'";
                  in
                  ''
                    wrapProgram "$out/bin/stack" ${add-flags}
                  '';
              };
            })
          ];
          pkgs = import nixpkgs {
            inherit system overlays;
            inherit (haskellNix) config;
          };
          flake = pkgs.unison-project.flake { };

          commonShellArgs = args:
            args // {
              # workaround:
              # https://github.com/input-output-hk/haskell.nix/issues/1793
              # https://github.com/input-output-hk/haskell.nix/issues/1885
              allToolDeps = false;
              additional = hpkgs: with hpkgs; [ Cabal stm exceptions ghc ghc-heap ];
              buildInputs =
                let
                  native-packages = pkgs.lib.optionals pkgs.stdenv.isDarwin
                    (with pkgs.darwin.apple_sdk.frameworks; [ Cocoa ]);
                in
                (args.buildInputs or [ ]) ++ (with pkgs; [ unison-stack pkg-config zlib glibcLocales ]) ++ native-packages;
              # workaround for https://gitlab.haskell.org/ghc/ghc/-/issues/11042
              shellHook = ''
                export LD_LIBRARY_PATH=${pkgs.zlib}/lib:$LD_LIBRARY_PATH
              '';
              tools =
                let ormolu-ver = "0.5.2.0";
                in (args.tools or { }) // {
                  cabal = { };
                  ormolu = { version = ormolu-ver; };
                  haskell-language-server = {
                    version = "latest";
                    modules = [
                      {
                        packages.haskell-language-server.components.exes.haskell-language-server.postInstall = ''
                          ln -sr "$out/bin/haskell-language-server" "$out/bin/haskell-language-server-wrapper"
                        '';
                      }
                    ];
                    # specify flags via project file rather than a module override
                    # https://github.com/input-output-hk/haskell.nix/issues/1509
                    cabalProject = ''
                      packages: .
                      package haskell-language-server
                        flags: -brittany -fourmolu -stylishhaskell -hlint
                      constraints: ormolu == ${ormolu-ver}
                    '';
                  };
                };
            };

          shellFor = args: pkgs.unison-project.shellFor (commonShellArgs args);

          localPackages = with pkgs.lib;
            filterAttrs (k: v: v.isLocal or false) pkgs.unison-project.hsPkgs;
          localPackageNames = builtins.attrNames localPackages;
          devShells =
            let
              mkDevShell = pkgName:
                shellFor {
                  packages = hpkgs: [ hpkgs."${pkgName}" ];
                  withHoogle = true;
                };
              localPackageDevShells =
                pkgs.lib.genAttrs localPackageNames mkDevShell;
            in
            {
              default = devShells.only-tools;
              only-tools = shellFor {
                packages = _: [ ];
                withHoogle = false;
              };
              local = shellFor {
                packages = hpkgs: (map (p: hpkgs."${p}") localPackageNames);
                withHoogle = true;
              };
            } // localPackageDevShells;
        in
        flake // {
          defaultPackage = flake.packages."unison-cli:exe:unison";
          inherit (pkgs) unison-project;
          inherit devShells localPackageNames;
          packages = flake.packages // {
            all = pkgs.symlinkJoin {
              name = "all-packages";
              paths =
                let
                  all-other-packages = builtins.attrValues (builtins.removeAttrs self.packages."${system}" [ "all" ]);
                  devshell-inputs = builtins.concatMap (devShell: devShell.buildInputs ++ devShell.nativeBuildInputs) [ devShells.only-tools ];
                in
                all-other-packages ++ devshell-inputs;
            };
          };
        });
}
