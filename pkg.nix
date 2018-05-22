{ mkDerivation, base, binary, bytestring, ghc-prim
, lens-family-core, pipes, pipes-bytestring, pipes-parse
, smallcheck, stdenv, tasty, tasty-hunit, tasty-smallcheck
, transformers
}:
mkDerivation {
  pname = "pipes-binary";
  version = "0.4.2";
  src = ./.;
  libraryHaskellDepends = [
    base binary bytestring ghc-prim pipes pipes-bytestring pipes-parse
    transformers
  ];
  testHaskellDepends = [
    base binary bytestring ghc-prim lens-family-core pipes pipes-parse
    smallcheck tasty tasty-hunit tasty-smallcheck transformers
  ];
  homepage = "https://github.com/k0001/pipes-binary";
  description = "Encode and decode binary streams using the pipes and binary libraries";
  license = stdenv.lib.licenses.bsd3;
}
