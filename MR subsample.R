##### Note: Code from line 128 to the end used in the final report #####


library(magrittr)
library(ggplot2)
(mr <- 
    readRDS(".//mr.rds") %>%
    dplyr::select(-comment, -area, -dup))
(e <- 
    readRDS(".//effort.rds") %>%
    dplyr::select(event, subarea, trap, gear, ct_large))

#Will ultimately merge by event, area and trap. 
#standardize trap in e 
table(e$trap, e$subarea, e$event)
#e$trap[grepl("^H.", e$trap)] <- "HL"
e$trap[grepl("^HL1", e$trap)] <- "H1"
e$trap[grepl("^HL2", e$trap)] <- "H2"
e$trap[grepl("3 or 88", e$trap)] <- "88/3"
table(e$trap, e$subarea, e$event)

#standardize trap in mr 
table(mr$trap, mr$subarea, mr$event)
#mr$trap[grepl("^H.", mr$trap)] <- "HL"
mr$trap[mr$trap == "HL1"] <- "H1"
mr$trap[mr$trap == "HL2"] <- "H2"
mr$trap[mr$trap == "HL3"] <- "H3"
mr$trap[mr$trap == "HL14"] <- "HL 14"
mr$trap[mr$trap == "HL15"] <- "HL 15"
table(mr$trap, mr$subarea, mr$event)

#check standardization
length(unique(e$trap))
length(unique(mr$trap))
e$trap[!e$trap %in% mr$trap] #confirmed only 1 >180 caught in these sets during mark, either not marked or missing from marking database.

#Note some HL 2 fish mislabeled as subarea 3 when all 45 HL 2 fish from subarea 4
mr$subarea[mr$trap == "HL 2"] <- 4

#For nets: sample wo replacement amongst traps within each area and event
#For H&L: sample wo replacement amongst boats within each event
#calc N, se N
#consistency tests
draw_samp <- 
  function(p, dat_e, dat_mr){
  if(p < 0 | p > 1) stop("P must be between 0 and 1")
  
  #sample from nets
  e_net <- dat_e[!grepl("^H.", dat_e$trap),] %>% dplyr::arrange(event, subarea, trap)
  e_net$row <- seq(nrow(e_net))
  net_rowid <- aggregate(row ~ subarea + event, data = e_net, FUN = range)
  net_rows <- unlist(mapply(function(x, y){sample(x:y, round(length(x:y)*p))}, net_rowid[[3]][,1], net_rowid[[3]][,2]))
  samp_net <- dplyr::inner_join(e_net[net_rows, c("event", "subarea", "trap")], dat_mr, c("event", "subarea", "trap"))
  
  #sample from HL
  e_HL <- dat_e[grepl("^H.", dat_e$trap),] %>% dplyr::arrange(event, subarea, trap)
  e_HL$row <- seq(nrow(e_HL))
  HL_rowid <- aggregate(row ~ event, data = e_HL, FUN = range)
  HL_rows <- unlist(mapply(function(x, y){sample(x:y, round(length(x:y)*p))}, HL_rowid[[2]][,1], HL_rowid[[2]][,2]))
  samp_HL <- dplyr::inner_join(e_HL[HL_rows, c("event", "subarea", "trap")], dat_mr, c("event", "subarea", "trap"))
  
  temp <- 
    rbind(samp_net, samp_HL) %>%
    dplyr::arrange(event, subarea)
  
  samp_mr <- 
    temp %>% dplyr::mutate(m2 = ifelse(m2 == 0, 0, 
                                       ifelse(m2 == 1 & !(tag %in% temp$tag[temp$n1 == 1]), 0, 1)))
  area <- 1:9; names(area) <- rep(LETTERS[1:3], each = 3)
  samp_mr$area <- names(area[samp_mr$subarea])
  
  #Consitency tests
  n1 <- as.vector(table(samp_mr$n1, samp_mr$area)["1", ])
  n2 <- as.vector(table(samp_mr$n2, samp_mr$area)["1", ])
  m2 <- as.vector(table(samp_mr$m2, samp_mr$area)["1", ])
  u2 <- n2 - m2
  #mixing
  area <- 
    dplyr::left_join(samp_mr[samp_mr$n1 == 1, c("tag", "area")], 
                     samp_mr[samp_mr$m2 == 1, c("tag", "area")], 
                     by = "tag")
  area_tab <- table(area$area.x, area$area.y)
  mix_tab <- cbind(area_tab, n1 - apply(area_tab, 1, sum))
  mix <- chisq.test(mix_tab)$p.value
  
  #marked fractions
  mf_tab <- 
    data.frame(recovery_area = LETTERS[1:3],
               marked = m2, 
               unmarked = u2,
               mf = round(m2/(m2 + u2), 2))
  mf <- chisq.test(mf_tab[, c("marked", "unmarked")])$p.value
  
  #recovery rates
  rr_tab <- 
    data.frame(area = LETTERS[1:3],
               recaptured = apply(area_tab, 1, sum), 
               no_recap = n1 - apply(area_tab, 1, sum),
               mf = round(apply(area_tab, 1, sum)/n1, 2))
  rr <- chisq.test(rr_tab[, c("recaptured", "no_recap")])$p.value
  
  data.frame(
    N = (sum(samp_mr$n1) + 1) * (sum(samp_mr$n2) + 1) / (sum(samp_mr$m2) + 1) - 1,
    seN = sqrt((sum(samp_mr$n1) + 1) * (sum(samp_mr$n2) + 1) * (sum(samp_mr$n1) - sum(samp_mr$m2)) * (sum(samp_mr$n2) - sum(samp_mr$m2)) / (sum(samp_mr$m2) + 1)^2 / (sum(samp_mr$m2) + 2)),
    consistent = mix > 0.05 | mf > 0.05 | rr > 0.05, 
    p = p)
}
#Naive 5,173 fish (SE = 367, 95% CI 4,454 – 5,892)

#Randy like plot
#sample 3/9 to 8/9 representing samples from approx 3 to 8 day trips
dat_plot <- 
  lapply(rep(3:8/9, each = 50), draw_samp, e, mr) %>%
  do.call(rbind, .) 
dat_plot %>%
  dplyr::mutate(sample = rep(1:50, times = 6),
                lb = N - qnorm(0.975)*seN,
                ub = N + qnorm(0.975)*seN,
                days = factor(p, 3:8/9, paste0(3:8, " days"))) %>%
  ggplot(aes(x = sample, y = N, color = consistent)) +
  geom_point() +
  geom_errorbar(aes(ymin = lb, ymax = ub)) +
  annotate(geom = "rect", ymin = 4454, ymax = 5892, xmin = 1, xmax = 50, alpha = 0.2) +
  facet_wrap(days ~ ., ncol = 2) +
  theme_bw()


#Note: Traditional approach comes to same conclusion but is more flexible for future use
#average daily sample size
(sum0 <- aggregate(ct_large ~ gear + event + subarea, data = e, FUN = sum))
(mu0 <- aggregate(ct_large ~ gear, data = sum0, FUN = mean))

#R&R accuracy prediction (large sample approximation breaks down for small samples)
RR <- function(D, A = 0.5, alpha = 0.05) abs(pnorm(A*sqrt(D)/(1-A)) - pnorm(-A*sqrt(D)/(1+A)) - (1 - alpha))
(n <-sapply(3:8, function(x) x * sum(mu0[,2])))
D <- lapply(n, function(x) x*x*(5000 - 1)/(5000 - x)/(5000 - x))
A <- sapply(D, function(x) optimize(RR, c(0, 1), D = x, alpha = 0.05)$minimum)

#Accuracy of sampling approach
dat_plot %>%
  dplyr::mutate(A = 1.96*seN / N) %>%
  dplyr::group_by(p) %>%
  dplyr::summarise(A = mean(A))

(n_equal <- sapply(3:8, function(x) x * sum(mu0[,2] * c(2, 1, 1/2))))
D_equal <- lapply(n_equal, function(x) x*x*(5000 - 1)/(5000 - x)/(5000 - x))
A_equal <- sapply(D_equal, function(x) optimize(RR, c(0, 1), D = x, alpha = 0.05)$minimum)

(n_switch <- sapply(3:8, function(x) x * sum(mu0[,2] * c(4, 1, 1/4))))
D_switch <- lapply(n_switch, function(x) x*x*(5000 - 1)/(5000 - x)/(5000 - x))
A_switch <- sapply(D_switch, function(x) optimize(RR, c(0, 1), D = x, alpha = 0.05)$minimum)

(n_funnel <- sapply(3:8, function(x) x * sum(mu0[1:2,2] * c(5, 1))))
D_funnel <- lapply(n_funnel, function(x) x*x*(5000 - 1)/(5000 - x)/(5000 - x))
A_funnel <- sapply(D_funnel, function(x) optimize(RR, c(0, 1), D = x, alpha = 0.05)$minimum)

knitr::kable(data.frame(n*2, n_equal*2, n_switch*2, n_funnel*2), digits = 0)
knitr::kable(data.frame(A, A_equal, A_switch, A_funnel), digits = 2)
