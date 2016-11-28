with (import <nixpkgs> {}).pkgs;
let modifiedHaskellPackages = haskellPackages.override {
      overrides = self: super: {
	    pipes-binary = self.callPackage ./. {};
      };
};
in modifiedHaskellPackages.pipes-binary.env
