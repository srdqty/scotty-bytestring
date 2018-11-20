{ mkDerivation, base, bytestring, case-insensitive, hpack, hspec
, hspec-wai, http-types, mtl, scotty, stdenv, text, wai
}:
mkDerivation {
  pname = "scotty-bytestring";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring case-insensitive http-types mtl scotty wai
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    base bytestring case-insensitive hspec hspec-wai http-types mtl
    scotty text wai
  ];
  preConfigure = "hpack";
  homepage = "https://github.com/srdqty/scotty-bytestring#readme";
  description = "ByteString versions of some Scotty functions";
  license = stdenv.lib.licenses.bsd3;
}
