with (import <nixpkgs> {});
with haskell.lib;

rec {
  hsSrcSet = {BiobaseENA = ./.;};
  hsPkgs = haskellPackages.extend (packageSourceOverrides hsSrcSet);
  hsShell = with hsPkgs; shellFor {
    packages = p: [ p.BiobaseENA ];
    withHoogle = true;
    buildInputs = [
      cabal-install ghc
    ];
  };
}
