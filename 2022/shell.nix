with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "rust-env";
  nativeBuildInputs = [
    rustc cargo

    # Example Build-time Additional Dependencies
    pkg-config
  ];
  buildInputs = [
    # Example Run-time Additional Dependencies
    stdenv openssl pkgconfig cargo rustc rustup freetype
  ];

  # Set Environment Variables
  RUST_BACKTRACE = 1;
}
