let
  nixpkgsSrc = builtins.fetchTarball {
    url =
      "https://github.com/NixOS/nixpkgs/archive/72bdd03f0d5696412b25a93218acaad530570d30.tar.gz"; # refs/heads/nixpkgs-unstable
    sha256 = "19ddck10kx4vrb0anm6m2hmni6rghspf48ddk5w6w7afyjgal0f3";
  };

in import nixpkgsSrc { }

