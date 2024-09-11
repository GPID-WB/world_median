source("R/setup.R")
source("R/functions.R")


wm <- implicit_povline(povline = 6,
                       country = "WLD",
                       year = 2024,
                       lkup = lkup)


dt <- pipapi::pip_grp_logic(povline = wm,
                      country = "WLD",
                      year = 2024,
                      lkup = lkup)
