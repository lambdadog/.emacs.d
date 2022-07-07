let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {
    overlays = [
      (import sources.emacs-overlay)
    ];
  };
in {
  emacs = pkgs.emacsPgtk;
}
