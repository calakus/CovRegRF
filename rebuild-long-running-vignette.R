# The idea of pre-computing long-running vignettes is from here:
# https://www.kloppenborg.ca/2021/06/long-running-vignettes/

old_wd <- getwd()

setwd("vignettes")
knitr::knit("CovRegRF.Rmd.orig", output = "CovRegRF.Rmd")
knitr::purl("CovRegRF.Rmd.orig", output = "CovRegRF.R")

setwd(old_wd)
