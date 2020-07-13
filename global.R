##### Library to load #####
suppressPackageStartupMessages( library(DT) )
suppressPackageStartupMessages( library(ggplot2) )
suppressPackageStartupMessages( library(DESeq2) )
suppressPackageStartupMessages( library(shinycssloaders) )
suppressPackageStartupMessages( library(RPostgreSQL) )
suppressPackageStartupMessages( library(dplyr) )
suppressPackageStartupMessages( library(colourpicker) )

##### Load data #####
load('Data/KS.RData')
load('Data/DESeq.RData')
load("Data/RT.RData")
load("Data/PCA.RData")

##### Source helpers script #####
source("helpers.R")

##### Source modules #####
source("plotPanel.R")
source("tablePanel.R")
source("pcaPlotPanel.R")

##### Load splicing factor file #####
splicF <- loadSplicingFactorList()

##### Load virhostnet data #####
load('Data/virhostnet.RData')
