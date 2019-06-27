let pkgs = (import ./nix/nixpkgs.nix).nixpkgs {}; in
let easyPureScript = import ./nix/pkgs/easy-purescript.nix { inherit pkgs; }; in

pkgs.mkShell {
  buildInputs = [
    easyPureScript.inputs.purs
    easyPureScript.inputs.spago
    pkgs.nodejs
  ];
}
