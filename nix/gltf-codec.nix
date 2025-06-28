{ mkDerivation, aeson, base, base64-bytestring, binary, bytestring
, directory, filepath, lib, scientific, shower, text
, unordered-containers, vector
}:
mkDerivation {
  pname = "gltf-codec";
  version = "0.1.0.4";
  sha256 = "dd44c5b851b9f36cc3025aeb8e0af0366cc9fdb76e76eb58a500c7fb1a33f0cb";
  revision = "1";
  editedCabalFile = "0scl65y7lilbqq913qnha2kqy1zkcg5gs4734vlbkcwvzd01f8m2";
  libraryHaskellDepends = [
    aeson base base64-bytestring binary bytestring scientific text
    unordered-containers vector
  ];
  testHaskellDepends = [ base bytestring directory filepath shower ];
  description = "glTF scene loader";
  license = lib.licenses.bsd3;
  doCheck = false;
}
