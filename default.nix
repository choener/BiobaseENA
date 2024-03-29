{ mkDerivation, base, BiobaseTypes, bytestring, containers
, directory, file-embed, lens, lib, megaparsec, mtl, QuickCheck
, tasty, tasty-quickcheck, tasty-th, text, vector, vector-th-unbox
}:
mkDerivation {
  pname = "BiobaseENA";
  version = "0.0.0.2";
  src = ./.;
  libraryHaskellDepends = [
    base BiobaseTypes bytestring containers directory file-embed lens
    megaparsec mtl text vector vector-th-unbox
  ];
  testHaskellDepends = [
    base BiobaseTypes bytestring containers directory file-embed lens
    megaparsec mtl QuickCheck tasty tasty-quickcheck tasty-th text
    vector vector-th-unbox
  ];
  homepage = "https://github.com/choener/BiobaseENA";
  description = "European Nucleotide Archive data";
  license = lib.licenses.bsd3;
}
