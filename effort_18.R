library(magrittr)
library(lubridate)
library(ggplot2)
e1_raw <- 
  readxl::read_xlsx(".\\Neck 2018 AWL Data Combined To Adam 12_17_18.xlsx",
                    sheet = 3,
                    range = "A4:V152",
                    col_names = c("area", "trap", "gear", "rods", "date_set", "date_pull", "time_set", "time_pull", "depth",
                                  "effort0", "ct_large", "ct_small", "dv", "coho", "sculpin", "kok", "sb", "comment"),
                    col_types = c(rep("skip", 2), rep("text", 4), rep("date", 4), "text", rep("skip", 2), rep("numeric", 8), "text"))
head(e1_raw)
tail(e1_raw)
lapply(e1_raw, table, useNA = "ifany")
hour(e1_raw$date_set) <- hour(e1_raw$time_set)
minute(e1_raw$date_set) <- minute(e1_raw$time_set)
hour(e1_raw$date_pull) <- hour(e1_raw$time_pull)
minute(e1_raw$date_pull) <- minute(e1_raw$time_pull)
e1 <- 
  e1_raw %>% 
  dplyr::mutate(rods = as.numeric(rods),
                gear = ifelse(is.na(rods), gear, "HL"),
                depth = as.numeric(ifelse(depth == "HL", NA, depth)),
                effort = ifelse(gear ==  "HL", 
                                difftime(date_pull, date_set, units = "mins") * rods,
                                difftime(date_pull, date_set, units = "mins")),
                check = effort - effort0,
                event = "mark"
                ) %>%
  dplyr::select(-trap, -time_set, -date_pull, -time_pull, -sculpin, -kok, -sb)
lapply(e1, table, useNA = "ifany")
e1[e1$check != 0, ]

e2_raw <- 
  readxl::read_xlsx(".\\Neck 2018 AWL Data Combined To Adam 12_17_18.xlsx",
                    sheet = 4,
                    range = "A4:U185",
                    col_names = c("area", "trap", "gear", "rods", "date_set", "date_pull", "time_set", "time_pull", "depth",
                                  "effort0", "ct_large", "ct_small", "dv", "coho", "sculpin", "kok", "sb", "comment"),
                    col_types = c(rep("skip", 2), rep("text", 4), rep("date", 4), "text", "skip", rep("numeric", 8), "text"))
head(e2_raw)
tail(e2_raw)
lapply(e2_raw, table, useNA = "ifany")
hour(e2_raw$date_set) <- hour(e2_raw$time_set)
minute(e2_raw$date_set) <- minute(e2_raw$time_set)
hour(e2_raw$date_pull) <- hour(e2_raw$time_pull)
minute(e2_raw$date_pull) <- minute(e2_raw$time_pull)
e2 <- 
  e2_raw %>% 
  dplyr::mutate(rods = as.numeric(rods),
                gear = ifelse(is.na(rods), gear, "HL"),
                depth = as.numeric(ifelse(depth == "HL", NA, depth)),
                effort = ifelse(gear ==  "HL", 
                                difftime(date_pull, date_set, units = "mins") * rods,
                                difftime(date_pull, date_set, units = "mins")),
                check = effort - effort0,
                event = "recap"
  ) %>%
  dplyr::select(-trap, -time_set, -date_pull, -time_pull, -sculpin, -kok, -sb)
lapply(e2, table, useNA = "ifany")
e2[grepl("open|Open", e2$comment), ]
e2[e2$check != 0, ]

e <-
  rbind(e1, e2) %>%
  dplyr::select(subarea = area, gear, depth, ct_large, ct_small, dv, coho, effort, event) %>%
  dplyr::left_join(data.frame(subarea = as.character(1:9), area = rep(LETTERS[1:3], each = 3), stringsAsFactors = FALSE), by = "subarea") %>%
  dplyr::select(-subarea) %>%
  tidyr::gather(spp, n, -area, -gear, -depth, -effort, -event) %>%
  dplyr::mutate(cpue = n / effort)

myboot <- 
  function(dat, event, gear, spp){
    #dat2 <- dat$cpue[dat$event == event & dat$gear == gear & dat$spp == spp]
    dat2 <- dat$cpue[dat$gear == gear & dat$spp == spp]
    boot <- boot::boot(dat2, function(x, n){mean(x[n])}, 500)
    boot
    boot_ci <- boot::boot.ci(boot, type = "perc")
     data.frame(
       #event = event,
       gear = gear,
       spp = spp,
       mean = boot$t0,
       lci = boot_ci$perc[4],
       uci = boot_ci$perc[5]
     )
  }

grid <-
  expand.grid(#event = unique(e$event),
              gear = unique(e$gear),
              spp = unique(e$spp), 
              stringsAsFactors = FALSE)
mapply(myboot, 
       #event = grid$event,
       gear = grid$gear,
       spp = grid$spp,
       MoreArgs = list(dat = e),
       SIMPLIFY = FALSE,
       USE.NAMES = FALSE) %>%
  do.call(rbind, .)
  
ggplot(e, aes(x = depth, y = effort, color = gear)) + geom_point() + facet_grid(event~.)
ggplot(e[e$gear != "HL", ], aes(x = depth, y = cpue, shape = gear, color = area)) + geom_point() + facet_grid(spp~event, scales = "free")

