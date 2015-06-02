# Here's a brief list of packages I regularly use:

# Models
install.packages(c(
        "Zelig", "mgcv", "gam", "MCMCpack", "arm", "lme4", "car", "MASS", "AER", "quantreg", "robust", 
        "heavy", "rms", "sandwich", "Synth", "rjags", "MCMCglmm", "survival", "sem", "pscl", "pcse",
        "list", "endorse", "rr"
        ))

# Hadleyverse
install.packages(c(
        "ggplot2", "ggvis", "dplyr", "tidyr", "lubridate", "reshape2", "stringr", "readr", "readxl",
        "haven", "rvest", "devtools", "purrr"
        ))
        
# Machine learning tools
install.packages(c(
        "e1071", "caret", "rpart", "rpart.plot", "rattle", "randomForest", "randomForestSRC", "party", 
        "C50", "quantregForest", "ROCR", "kknn"
        ))
        
# Data manipulation and graphs
install.packages(c(
        "ggthemes", "ggmcmc", "effects", "ngramr", "data.table", "gdata", "wesanderson", "DiagrammeR"
        ))

# Other useful tools
install.packages(c(
        "WhatIf", "Amelia", "Matching", "MatchingFrontier", "MatchIt", "cem", "ebal", "stargazer",
        "rworldmap", "rfacebook", "coda", "lmtest", "parallel", "boot", "broom", "pander"
        ))

# Packages on Github
library(devtools)
install_git("https://github.com/sinhrks/ggfortify")
install_git("https://github.com/twitter/BreakoutDetection")
install_git("https://github.com/twitter/AnomalyDetection")
install_git("https://github.com/google/CausalImpact")
install_git("https://github.com/rasmusab/bayesian_first_aid")
install_git("https://github.com/zmjones/edarf/")
install_git("https://github.com/leeper/margins")
install_git("https://github.com/rmcelreath/rethinking")

# RStan
source('http://mc-stan.org/rstan/install.R', echo = TRUE, max.deparse.length = 2000)
install_rstan()
