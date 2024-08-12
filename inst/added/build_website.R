library(pkgdown)
####

pkg ="/home/ecor/local/rpackages/rendena100/RMAWGEN"
###pkgdown::clean_site(pkg=pkg)
pkgdown::build_site(pkg=pkg) ##,override = list(destination = destination_site))
