{ doCheck ? false, e2eTestsHack ? false }:

let
  sources = import ./nix/sources.nix { };

  pkgsHost = import sources.nixpkgs { };
  pkgsLinux = import sources.nixpkgs { system = "x86_64-linux"; };

  gitignoreNix = import sources."gitignore.nix" { lib = pkgsHost.lib; };

  ocamlDeps = pkgs: with pkgs.ocamlPackages; [
    ocaml
    ocaml-lsp
    dune_2
    findlib # Lets merlin see packages like ounit
    ocp-indent
    merlin
    ounit
    qcheck
    ppxlib
    bisect_ppx
    ppx_tools_versioned
    ppx_deriving
    zarith
    odoc
    core_kernel
  ];

  # We wrap the upstream tezos-client derivation since it also brings some ocaml
  # packages which are incompatible with ours.
  tezosClient =
    let orig = (import "${sources.tezos-packaging}/nix" { }).binaries.tezos-client;
    in
    pkgsLinux.runCommand "tezos-client" { } ''
      mkdir -p $out/bin; ln -s ${orig}/bin/tezos-client $out/bin/tezos-client
    '';

  checkerSource =
    let filter =
      let ignored = gitignoreNix.gitignoreFilter ./.;
      in path: type: ignored path type;
    in
    pkgsHost.lib.cleanSourceWith {
      inherit filter;
      src = ./.;
      name = "checker-source";
    };
in
rec
{
  shell =
    let
      pkgs = pkgsHost;
    in
    pkgs.mkShell {
      name = "checker-shell";
      buildInputs =
        pkgs.lib.optionals (pkgs.stdenv.isLinux) [ tezosClient ]
        ++ pkgs.lib.optionals (!(pkgs.stdenv.isDarwin && pkgs.stdenv.isAarch64)) [ pkgs.niv ]
        ++ (with pkgs; [ nixpkgs-fmt ])
        ++ ocamlDeps pkgs;
      shellHook = ''
      '';
    };
}
