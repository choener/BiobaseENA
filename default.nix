with import <nixpkgs> {};
let
  packageOverrides = haskellPackages.override {
    overrides = self: super: {
      # old doctest
      pipes-group = haskell.lib.dontCheck super.pipes-group;
    };
  };
  sourceOverrides = packageOverrides.extend (haskell.lib.packageSourceOverrides {
    BiobaseENA = ./.;
  });
in
sourceOverrides

