#############################
####    Renv settings    ####
#############################

#----     R packages   ----

# List of packages used in the analysis.


#----    renv comands    ----
# For renv function and options see: https://rstudio.github.io/renv/

# renv::settings$snapshot.type("packrat")   # Set snapshot options
# renv::dependencies(dev = TRUE)            # Find dependencies
# renv::install("")                         # Install Packages
# renv::hydrate("formatR")                  # Upload packages from user lybrary
# sapply(packages_list, renv::hydrate)
# renv::remove()                            # Delate packages from project lybrary
# renv::purge()                             # Delate packages from cache (delate for all projects!!)
# renv::snapshot()                          # Create snapshot packages used


#----    Procedure to remove packages   -----
# ip <- as.data.frame(installed.packages())
# ip <- subset(ip, !grepl("MRO", ip$LibPath))
# ip <- ip[!(ip[,"Priority"] %in% c("base", "recommended")),]
# path.lib <- unique(ip$LibPath)
# pkgs.to.remove <- ip[,1]
#
# sapply(pkgs.to.remove, renv::remove, lib = path.lib)
