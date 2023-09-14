{ mkDerivation, base, binary, bytestring, exceptions, lib, streamly
, streamly-core, transformers
}:
mkDerivation {
  pname = "streamly-more";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    base binary bytestring exceptions streamly streamly-core
    transformers
  ];
  testHaskellDepends = [ base streamly streamly-core ];
  homepage = "https://github.com/k0001/hs-streamly-more";
  description = "More streamly related tools";
  license = "unknown";
}
