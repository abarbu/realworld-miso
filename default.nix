with (import (builtins.fetchTarball {
  url = "https://github.com/dmjio/miso/archive/ea25964565074e73d4052b56b60b6e101fa08bc5.tar.gz";
  sha256 = "1yb9yvc0ln4yn1jk2k5kwwa1s32310abawz40yd8cqqkm1z7w6wg";
}) {});
let
 ghc865 = pkgs.haskell.packages.ghc865.extend(self: super: {
    servant-client-core = self.callHackage "servant-client-core" "0.16" {};
    servant = self.callHackage "servant" "0.16" {};
    servant-server = self.callHackage "servant-server" "0.16" {};
    servant-lucid = self.callHackage "servant-lucid" "0.9" {};
    servant-jsaddle = pkgs.haskell.lib.dontCheck (self.callCabal2nixWithOptions "servant-jsaddle"
      (pkgs.fetchFromGitHub {
        owner  = "haskell-servant";
        repo   = "servant-jsaddle";
        rev    = "2ccf13d185e26d4cb4a51622e748ec64336435f4";
        sha256 = "066vr1rfq6bjn3xx9g52z2vgp1ibyz50z3hzwaqq3fzxnr2srpjs";
      }) "-fjsaddle" { });
    miso = super.miso-jsaddle;
  });
 ghcjs86 = pkgs.haskell.packages.ghcjs86.extend(self: super: {
    servant-client-core = self.callHackage "servant-client-core" "0.16" {};
    servant = self.callHackage "servant" "0.16" {};
    # servant-server = self.callHackage "servant-server" "0.16" {};
    # servant-lucid = self.callHackage "servant-lucid" "0.9" {};
    servant-jsaddle = pkgs.haskell.lib.dontCheck (self.callCabal2nixWithOptions "servant-jsaddle"
      (pkgs.fetchFromGitHub {
        owner  = "haskell-servant";
        repo   = "servant-jsaddle";
        rev    = "2ccf13d185e26d4cb4a51622e748ec64336435f4";
        sha256 = "066vr1rfq6bjn3xx9g52z2vgp1ibyz50z3hzwaqq3fzxnr2srpjs";
      }) "-fjsaddle" { });
  });
in
{
  dev = ghc865.callCabal2nixWithOptions "app" ./. "-fjsaddle" { };
  release = ghcjs86.callCabal2nix "app" ./. {};
  inherit pkgs;
}
