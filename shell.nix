(import ./default.nix).project.shellFor {
  withHoogle = false;
  tools = {
    cabal = "3.2.0.0";
  };
}
