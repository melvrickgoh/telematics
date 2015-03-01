# Telematics: R

This uses [RStudio](http://www.rstudio.com/) for statistical computing and [CRAN](http://cran.r-project.org/) for R packages.
The project uses [Packrat](http://rstudio.github.io/packrat/) to help us manage our dependencies

## NOTE - Running the App

You run the init script as per follows:

```
source("init.R", echo = TRUE)
```

## Programmatic Loading of Packages
Example usage:

```
my_packages = c("plyr","data.table","plotrix","parallel","caret")
```

## Helper methods
Store all helper methods in helpers.R

```
list.dirs <- function(path=".", pattern=NULL, all.dirs=FALSE,
                      full.names=FALSE, ignore.case=FALSE) {
  # use full.names=TRUE to pass to file.info
  all <- list.files(path, pattern, all.dirs,
                    full.names=TRUE, recursive=FALSE, ignore.case)
  dirs <- all[file.info(all)$isdir]
  # determine whether to return full names or just dir names
  if(isTRUE(full.names))
    return(dirs)
  else
    return(basename(dirs))
}
```