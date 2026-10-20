##############################################
## R/analysis.R                             ##
## Analysis and visualisation pipeline      ##
##############################################
#
# Loads the packages and shared modules the analysis pipeline needs, then its
# six parts, so that sourcing this one file defines every analysis function as
# before. Each part holds one domain; a change to a stage belongs in the part
# that defines it.

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(patchwork)
library(knitr)
library(data.table)
library(RColorBrewer)
library(triangle)

source("R/constants.R")
source("R/censoring.R")
source("R/queue_series.R")
source("R/figures.R")

source("R/analysis_common.R")
source("R/analysis_role4_ame.R")
source("R/analysis_single_run.R")
source("R/analysis_replications.R")
source("R/analysis_sweeps.R")
source("R/analysis_time_series.R")
