{ mkDerivation, stdenv, ghc, base, pure-elm, pure-json, pure-maybe, 
  pure-websocket, containers
}:
mkDerivation {
  pname = "pure-websocket-cache";
  version = "0.8.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base
    pure-elm
    pure-json
    pure-maybe
    pure-websocket
    containers
  ];
  homepage = "github.com/grumply/pure-websocket-cache";
  license = stdenv.lib.licenses.bsd3;
}
