# Here's a brief list of packages I regularly use:

# Models
install.packages(c(
        "mgcv", "gam", "MCMCpack", "arm", "lme4", "car", "MASS", "AER", "quantreg", "robust", 
        "heavy", "rms", "sandwich", "Synth", "MCMCglmm", "survival", "sem", "pscl", "pcse",
        "rstan", "rstanarm", "brms", "ivmodel", "rdrobust", "clubSandwich", "clusterSEs"
        ))

# Hadleyverse
install.packages(c(
        "tidyverse", "devtools", "modelr"
        ))
        
# Machine learning tools
install.packages(c(
        "e1071", "caret", "rpart", "rpart.plot", "rattle", "randomForest", "randomForestSRC", "party", 
        "C50", "quantregForest", "ROCR", "kknn","KRLS", "mlr"
        ))
        
# Data manipulation and graphs
install.packages(c(
        "ggthemes", "ggmcmc", "effects", "ngramr", "data.table", "gdata", "wesanderson", "DiagrammeR",
        "cowplot", "gganimate", "gtrendsR", "interplot", "plotly", "simputation"
        ))

# Analysis of Experiments        
install.packages(c(
        "experiment", "list", "endorse", "rr", "mediation", "FindIt", "randomizr", "wfe"
        ))
        
# Other useful tools
install.packages(c(
        "WhatIf", "Amelia", "Matching", "MatchingFrontier", "MatchIt", "cem", "rbounds", "stargazer",
        "rworldmap", "coda", "lmtest", "parallel", "ebal", "broom", "pander", "MTurkR", "WDI", "Rmonkey",
        "SocialMediaLab", "analogsea", "wakefield", "datasynthR", "pdftools", "repmis"
        ))

# Packages on Github
library(devtools)
install_git("https://github.com/sinhrks/ggfortify")
install_git("https://github.com/randel/MixRF")
install_git("https://github.com/twitter/BreakoutDetection")
install_git("https://github.com/twitter/AnomalyDetection")
install_git("https://github.com/google/CausalImpact")
install_git("https://github.com/rasmusab/bayesian_first_aid")
install_git("https://github.com/zmjones/edarf/")
install_git("https://github.com/leeper/margins")
install_git("https://github.com/trinker/wakefield")
