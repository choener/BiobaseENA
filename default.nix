with (import <nixpkgs> {});
with haskell.lib;

rec {
  hsPkgs = haskellPackages.extend (packageSourceOverrides {
    BiobaseENA = ./.;
  });
  hsShell = with hsPkgs; shellFor {
    packages = p: [ p.BiobaseENA ];
    withHoogle = true;
    buildInputs = [
      cabal-install ghc
    ];
  };
}
