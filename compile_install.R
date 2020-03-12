# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# Instructions to generate and install a custom R package
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


rm(list = ls())

lapply(c("knitr",
         "magrittr",
         "tidyverse",
         "devtools",
         "roxygen2"), library, character.only = TRUE)

rootdir <- file.path("C:", "Users", "MK", "Documents", "pCloud_Sync_offline")
projdir <- file.path(rootdir, "Proj_Personal", "Proj_R_Packages")


# Package 'mcf' (miscellaneous custom functions) =========================================

# Remove any previous installation of 'mcf'
remove.packages("mcf")

# Use Roxygen2 to compile documentation from 'DESCRIPTION' and comments in R scripts
devtools::document(file.path(projdir, "mcf"))

# Build/rebuild the package
devtools::build(file.path(projdir, "mcf"))

# Install the package from the tar file generated in the previous step
install.packages(file.path(projdir, "mcf_1.3.tar.gz"), type = "source", repos = NULL)

# Load the package and check it
library("mcf")

packageDescription("mcf")
help(package = "mcf") # Displayed in the 'Help' tab in RStudio
library(help = "mcf") # Displayed in RStudio's console

?mcf::keep_objects
?mcf::tbl_str
?mcf::tbl_freq
