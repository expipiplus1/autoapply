let
  nixpkgsSrc = builtins.fetchTarball {
    url =
      "https://github.com/NixOS/nixpkgs/archive/9aeeb7574fb784eaf6395f4400705b5f619e6cc3.tar.gz"; # nixos-unstable
    sha256 = "061x7m82mzlzpyxa0yc0xymdicc842dz6wii4b7mcyx96mfypy8g";
  };

in import nixpkgsSrc { }

