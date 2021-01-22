let

  bootstrap = import <nixpkgs> {};

  misoTarball = bootstrap.fetchFromGitHub {
    owner = "dmjio";
    repo = "miso";
    rev = "ea25964565074e73d4052b56b60b6e101fa08bc5";
    sha256 = "1yb9yvc0ln4yn1jk2k5kwwa1s32310abawz40yd8cqqkm1z7w6wg";
  };

  miso = import "${misoTarball}" { };

  inherit (miso) pkgs;

in miso.pkgs
