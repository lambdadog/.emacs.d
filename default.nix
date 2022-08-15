let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {
    overlays = [
      (import sources.emacs-overlay)
    ];
  };
in {
  inherit pkgs;
  emacs = pkgs.emacsGit.overrideAttrs (old: {
    # TODO: figure out how to get version
    name = "emacs-lambdadog";

    patches = old.patches ++ [
      ./emacs-patches/0001-Naive-implementation.patch
      ./emacs-patches/0002-No-truncation-continuation-characters.patch
    ];

    postPatch = old.postPatch + ''
      substituteInPlace lisp/loadup.el \
        --replace '(emacs-repository-get-version)' '"29-lambdadog"' \
        --replace '(emacs-repository-get-branch)' '"lambdadog"'
    '';
  });
}
