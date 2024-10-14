# Shiny FluHit

These scripts allow to launch a local version of the Shiny web-interface to explore the RNA-seq data from [Ashraf et al. (2020)](https://academic.oup.com/nargab/article/2/4/lqaa095/5998301).


## Prerequisites

R and the following R packages should be installed:
* shiny
* DT
* ggplot2
* DESeq2
* shinycssloaders
* RPostgreSQL
* dplyr
* colourpicker

Before cloning the github repository, install `git-lfs` in order to get the complete RData (check [github git-lfs installation guide](https://docs.github.com/en/repositories/working-with-files/managing-large-files/installing-git-large-file-storage) for more details).
In case the RData are very small after cloning instead of the size showed in GitHub, please execute the command `git-lfs pull .` in the cloned directory.


## Launch the Shiny App

After cloning the github repository, open the global.R script in RStudio.
Then, click on the “Run App” button on the upper right corner of the text editor. The Shiny App will load. __The loading can take some time...__

_Warning: If the Shiny App does not load in an external web browser, it could cause some issue with several feature of the Shiny App._
