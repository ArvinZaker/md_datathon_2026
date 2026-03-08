{
  description = "PDX-metric development environment";

  inputs = {
    # Specify the Nixpkgs input
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
  };

  outputs = {
    self,
    nixpkgs,
  }: let
    # system = "x86_64-linux";
    system = builtins.currentSystem;
    pkgs = nixpkgs.legacyPackages.${system};
  in {
    devShells.${system} = {
      # x86_64-linux
      default = pkgs.mkShell {
        name = "impurePythonEnv";
        venvDir = "./.venv";

        # Add your package dependencies here
        buildInputs = with pkgs; [
          ########### Python pacakges ############
          python312Packages.bokeh
          python312Packages.colorcet
          python312Packages.datashader
          python312Packages.h5netcdf
          python312Packages.h5py
          python312Packages.holoviews
          python312Packages.hyperopt
          python312Packages.igraph
          python312Packages.leidenalg
          python312Packages.matplotlib
          python312Packages.numpy
          python312Packages.pandas
          python312Packages.python
          python312Packages.requests
          python312Packages.rpy2
          python312Packages.scikit-image
          python312Packages.scikit-learn
          python312Packages.scipy
          python312Packages.seaborn
          python312Packages.statsmodels
          python312Packages.umap-learn
          ########### R pacakges ############
          rPackages.circlize
          rPackages.cowplot
          rPackages.dendextend
          rPackages.devtools
          rPackages.dplyr
          rPackages.foreach
          rPackages.ggalluvial
          rPackages.ggforce
          rPackages.ggpattern
          rPackages.ggplot2
          rPackages.ggpubr
          rPackages.ggraph
          rPackages.ggrepel
          rPackages.ggridges
          rPackages.ggsci
          rPackages.neuralnet
          rPackages.gridExtra
          rPackages.gtable
          rPackages.hrbrthemes
          rPackages.htmltools
          rPackages.earth
          rPackages.mboost
          rPackages.plyr
          rPackages.rpart
          rPackages.rpartScore
          rPackages.party
          rPackages.htmlwidgets
          rPackages.httpuv
          rPackages.httr
          rPackages.igraph
          rPackages.kableExtra
          rPackages.keras
          rPackages.kernlab
          rPackages.knitr
          rPackages.languageserver
          rPackages.languageserversetup
          rPackages.markdown
          rPackages.network
          rPackages.networkD3
          rPackages.openxlsx
          rPackages.preprocessCore
          rPackages.randomForest
          rPackages.readxl
          rPackages.rmarkdown
          rPackages.roxygen2
          rPackages.caret
          rPackages.sass
          rPackages.scales
          rPackages.stringi
          rPackages.stringr
          rPackages.survival
          rPackages.survminer
          rPackages.tidyr
          rPackages.tidyverse
          rPackages.umap
          rPackages.vip
          rPackages.viridis
          rPackages.visNetwork
          rPackages.writexl
          ########### System pacakges ############
          taglib
          liblinear
          openssl
          git
          libxml2
          libxslt
          libzip
          zlib
          stdenv.cc.cc.lib
          bash
          wget
          zlib
        ];

        LD_LIBRARY_PATH = "${pkgs.stdenv.cc.cc.lib}/lib";

        # Run this command, only after creating the virtual environment
        postVenvCreation = ''
          unset SOURCE_DATE_EPOCH
          # uncomment if you have packages not in nixos repository
          # pip install -r requirements.txt
          LD_LIBRARY_PATH=${pkgs.stdenv.cc.cc.lib}/lib/
        '';

        # Now we can execute any commands within the virtual environment.
        # This is optional and can be left out to run pip manually.
        postShellHook = ''
          # allow pip to install wheels
          unset SOURCE_DATE_EPOCH
          # fixes libstdc++ issues and libgl.so issues
          LD_LIBRARY_PATH=${pkgs.stdenv.cc.cc.lib}/lib/
        '';
      };
    };
  };
}
