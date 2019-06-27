let rev = "19.03"; in

let src = builtins.fetchTarball {
  sha256 = "0q2m2qhyga9yq29yz90ywgjbn9hdahs7i8wwlq7b55rdbyiwa5dy";
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
}; in

{
  inherit rev src;
  nixpkgs = import src;
}
