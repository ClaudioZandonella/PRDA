###########################
####    CRAN checks    ####
###########################

#----    ASCII characters    -----

# cat(stringi::stri_escape_unicode("This is a bullet •"))

#----    Build and see vignettes    ----

# devtools::build()  # Build package source
# install.packages("../PRDAbeta_0.1.0.9000.tar.gz", repos = NULL, type = "source") # Install source
# browseVignettes("PRDAbeta")  # See vignette


#----
