with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, attoparsec, base, conduit, conduit-extra, mtl
             , operational, process, resourcet, stdenv, text
             }:
             mkDerivation {
               pname = "serf";
               version = "0.1.0.0";
               src = ./.;
               buildDepends = [
                 attoparsec base conduit conduit-extra mtl operational process
                 resourcet text
               ];
               homepage = "http://github.com/sanetracker/serf";
               description = "Interact with Serf via Haskell";
               license = stdenv.lib.licenses.mit;
             }) {};
in
  pkg.env
