let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};
in {
  inherit pkgs;
  emacs = pkgs.emacs.overrideAttrs (old: {
    # TODO: figure out how to get version
    name = "emacs-lambdadog";
    srcRepo = true;
    src = ./emacs;

    dontStrip = true;
    enableDebugging = true;

    postPatch = old.postPatch + ''
      substituteInPlace lisp/loadup.el \
        --replace '(emacs-repository-get-version)' '"29-lambdadog"' \
        --replace '(emacs-repository-get-branch)' '"lambdadog"'
    '';
  });
}
