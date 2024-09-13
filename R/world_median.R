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

pls <- c(2.15, 30)

cl <- vector("list", length(pls))
wl <- vector("list", length(pls))

for (i in seq_along(pls)) {
  pl <- pls[i]
  cl[[i]] <- pipapi::pip(povline = pl,
                     year = 2024,
                     lkup = lkup,
                     fill_gaps = TRUE)

  wl[[i]] <- pipapi::pip_grp_logic(povline = pl,
                     country = "WLD",
                     year = 2024,
                     lkup = lkup)
}

cl <- rbindlist(cl, use.names = TRUE)
wl <- rbindlist(wl, use.names = TRUE)

fs::path(tdirp, "country_2024", ext = "csv") |>
readr::write_csv(x = cl,
                 file = _)

fs::path(tdirp, "world_2024", ext = "csv") |>
readr::write_csv(x = wl,
                 file = _)
