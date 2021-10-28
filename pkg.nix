{ mkDerivation, base, binary, bytestring, ghc-prim
, lens-family-core, lib, pipes, pipes-bytestring, pipes-parse
, tasty, tasty-hunit, tasty-quickcheck, transformers
}:
mkDerivation {
  pname = "pipes-binary";
  version = "0.4.3";
  src = ./.;
  libraryHaskellDepends = [
    base binary bytestring ghc-prim pipes pipes-bytestring pipes-parse
    transformers
  ];
  testHaskellDepends = [
    base binary bytestring ghc-prim lens-family-core pipes pipes-parse
    tasty tasty-hunit tasty-quickcheck transformers
  ];
  homepage = "https://github.com/k0001/pipes-binary";
  description = "Encode and decode binary streams using the pipes and binary libraries";
  license = lib.licenses.bsd3;
}
