{
  description = "A flake providing my emacs";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/release-22.11";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    flake-utils.url = "github:numtide/flake-utils";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
  };

  outputs = { self, nixpkgs, flake-utils, emacs-overlay, ... }:
    flake-utils.lib.eachSystem [
      "x86_64-linux"  "aarch64-linux"
      "x86_64-darwin" "aarch64-darwin"
    ] (system: let
      pkgs = import nixpkgs {
        inherit system;
        overlays = [
          emacs-overlay.overlays.emacs
        ];
      };
    in rec {
      packages.emacs = pkgs.emacsGitNativeComp.pkgs.withPackages (_: with pkgs; [
        # Adding packages here adds their /bin to emacs' PATH so I
        # don't need to install them globally.
        imagemagick
      ]);
      apps.emacs = flake-utils.lib.mkApp {
        name = "emacs";
        drv = packages.emacs;
      };

      defaultPackage = packages.emacs;
      defaultApp = apps.emacs;
    });
}
