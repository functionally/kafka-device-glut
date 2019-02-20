{
  mkDerivation, stdenv
, base, GLUT, kafka-device, OpenGL
}:

mkDerivation {
  pname = "kafka-device-glut";
  version = "1.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base GLUT kafka-device OpenGL
  ];
  executableHaskellDepends = [
  ];
  homepage = "https://bitbucket.org/functionally/kafka-device-glut";
  description = "GLUT events via a Kafka message broker";
  license = stdenv.lib.licenses.mit;
}
