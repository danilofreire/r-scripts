# How to install old version of an R package.
# Here I will install an old version of Zelig.
# Why? It works better! There are a few bugs with the "sim" command in versions 4.X

# Just move the tar.gz package to the R folder and type:

install.packages("/yourfolder/Zelig_3.5.4.tar.gz", repos = NULL, type="source")

# You can also use Christopher Gandrud's InstallOldPackages():
library(repmis)
InstallOldPackages(pkgs = "Zelig", versions = "3.5.4")