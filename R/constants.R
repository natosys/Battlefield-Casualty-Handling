##############################################
## R/constants.R                            ##
## Values shared by every module            ##
##############################################
#
# Sourced by each module that needs one of these rather than by one module on
# every other's behalf, because the modules under R/ are otherwise independent:
# an entry point sources the set it needs, and a regression check often sources
# only one. Sourcing this file more than once is harmless.

# Minutes in a simulated day. This is the single definition of the quantity.
# The `day_min` global that the execution model carries (see CLAUDE.md's Code
# Standards) is assigned from it by every entry point; use that global inside
# the model and the analysis pipeline, and this constant where no entry point
# has run yet, or in a parameter default, which cannot name a global of its own
# name without resolving to the parameter itself.
DAY_MIN <- 1440L
