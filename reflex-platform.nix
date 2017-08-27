let  
  initialNixpkgs = import <nixpkgs> {};

  sources = {
    reflex-platform = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "reflex-frp";
      repo = "reflex-platform";
      rev = "b7c00b3574d0ef42974eda0f2812c794c7b5d4f3";
      sha256 = "1jfz17y2fq051caby4y4aslxrpvgwwa30ivfw0l5wn5pp5zlrpad";
    };
  };

  reflex-platform = import sources.reflex-platform {};
in
  reflex-platform

