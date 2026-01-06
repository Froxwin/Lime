{
  mkDerivation,
  aeson,
  base,
  base64-bytestring,
  binary,
  bytestring,
  directory,
  filepath,
  scientific,
  shower,
  text,
  unordered-containers,
  vector,
  lib,
}:
mkDerivation {
  pname = "gltf-codec";
  version = "0.1.0.5";
  sha256 = "57nDhyEwoeV/L8WfpSOsMQaxZsk+gN7vGmv0aa298Hw=";
  libraryHaskellDepends = [
    aeson
    base
    base64-bytestring
    binary
    bytestring
    scientific
    text
    unordered-containers
    vector
  ];
  testHaskellDepends = [
    base
    bytestring
    directory
    filepath
    shower
  ];
  description = "glTF scene loader";
  license = lib.licenses.bsd3;
  jailbreak = true;
  doCheck = false;
}
