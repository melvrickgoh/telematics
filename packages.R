#
# Example R code to install packages
# See http://cran.r-project.org/doc/manuals/R-admin.html#Installing-packages for details
#

###########################################################
# Update this line with the R packages to install:

my_packages = c("plyr","data.table","plotrix","parallel","caret","hash")

remove_packages = c("dplyr")

###########################################################

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p, dependencies = TRUE)
  }
  else {
    cat(paste("Skipping already installed package:", p, "\n"))
  }
}

remove_if_avail = function(p) {
  if (p %in% rownames(installed.packages()) == TRUE) {
    remove.packages(p)
  }
  else {
    cat(paste("Skipping already installed package:", p, "\n"))
  }
}

invisible(sapply(my_packages, install_if_missing))
invisible(sapply(remove_packages, remove_if_avail))
