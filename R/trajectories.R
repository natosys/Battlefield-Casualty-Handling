##############################################
## R/trajectories.R                         ##
## All simmer trajectory definitions        ##
##############################################
#
# Loads the packages and constants the trajectories need, then their parts, so
# that sourcing this one file defines every trajectory function as before. Each
# part holds one echelon or schedule; a change to a trajectory belongs in the
# part that defines it.

library(simmer)
library(simmer.bricks)
library(triangle)

source("R/constants.R")

source("R/trajectories_common.R")
source("R/trajectories_r2b.R")
source("R/trajectories_r2e.R")
source("R/trajectories_casualty.R")
source("R/trajectories_schedules.R")
