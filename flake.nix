{
  description = "Rust flake";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.11"; # or whatever vers
		rust-overlay.url = "github:oxalica/rust-overlay";
		rust-overlay.inputs.nixpkgs.follows = "nixpkgs";
		flake-utils.url = "github:numtide/flake-utils";
  };
  
  outputs = { self, nixpkgs, rust-overlay, flake-utils, ... }@inputs:
  flake-utils.lib.eachDefaultSystem (system:
		let
			overlays = [ (import rust-overlay) ];
			pkgs = import nixpkgs {
				inherit system overlays;
			};
		in {
			devShells.default = with pkgs; mkShell {
	      buildInputs = [
	        openssl
	        pkg-config
	        # eza
	        # fd
	        rust-bin.beta.latest.default
	      ];

	      # shellHook = ''
	      #   alias ls=eza
	      #   alias find=fd
	      # '';
	    };
		}
	);
}
