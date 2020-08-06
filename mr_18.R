##### Note: this code contains most of the analsyis used in the final report #####

library(magrittr)
raw <- 
  readxl::read_xlsx(".\\Neck 2018 AWL Data Combined To Adam 12_17_18 CJS Edits 1_23_19.xlsx",
                    sheet = 1,
                    range = "A4:K2192",
                    col_names = c("date", "subarea", "depth", "trap", "gear", "fl", "tag", "clip", "comment"),
                    col_types = c(rep("skip", 2), "date", "numeric", rep("text", 7)))
head(raw) # depth, fl and tag wrong class
tail(raw)
lapply(raw, table, useNA = "ifany") 
  # depth = "HL" should be NA
  # fl = "RECAP" should be NA
raw[raw$tag %in% c("Lost tag", "NONE", "second event recap"), ] %>% 
  print(n = 100) 
  #delete all
raw[raw$clip %in% c("NO", "NONE"), ] %>% 
  print(n = 100) 
  raw[raw$tag == "823", ]
    # delete clip = "NONE"
hist(as.numeric(raw$fl)) #OK
raw[which(raw$comment != "NA"), ] %>% print(n = 100)

fl <- 
  raw %>% 
  dplyr::left_join(data.frame(subarea = 1:9, area = rep(LETTERS[1:3], each = 3)), by = "subarea") %>%
  dplyr::filter(tag != "Lost tag") %>% #remove 1 records
  dplyr::filter(tag != "NONE") %>% #remove 17 records
  dplyr::filter(tag != "second event recap") %>% #remove 20 records
  dplyr::mutate(event = ifelse(date < as.POSIXct("2018-06-15 UTC"), "mark", "recap"),
                date = as.Date(date, format = "%B%d"),
                depth = as.numeric(ifelse(depth == "HL", NA, depth)),
                fl = as.numeric(ifelse(fl ==  "RECAP", NA, fl)),
                tag = as.numeric(tag),
                clip = ifelse(clip %in% c("NO", "NA"), NA, clip),
                comment = ifelse(comment == "NA", NA, comment)
  )
fl
lapply(fl, table, useNA = "ifany")
fl[is.na(fl$fl), ] %>% 
  print(n = 100)

#check for tags only recorded during recap event
n_records <- fl %>% dplyr::group_by(tag) %>% dplyr::summarise(n = n())
dplyr::left_join(fl, n_records, "tag") %>% dplyr::filter(n == 1 & event == "recap")

mr0 <- 
  fl %>%
  dplyr::filter((fl >= 180 | is.na(fl)),
                !(comment == "Mort" & event == "mark" & is.na(tag))) %>%
  dplyr::mutate(n1 = ifelse(event == "mark", 1, 0),
                n2 = ifelse(event == "recap", 1, 0),
                m2 = ifelse(event == "recap" & !is.na(tag), 1, 0))
table(mr0$m2)
table(mr0$n1, mr0$n2, mr0$m2) #OK

lapply(mr0, table, useNA = "ifany")
mr0[is.na(mr0$clip), ] # Missing clips are morts after recapture event or tag = 823 which was recaptured
mr0[mr0$tag == 823, ]

#Multiple captures during the same event
move <-
  mr0 %>% 
  dplyr::filter(!is.na(tag)) %>%
  dplyr::arrange(tag, event, date, area) %>% 
  dplyr::group_by(tag, event) %>% 
  dplyr::summarise(area2 = ifelse(length(area) == 1, LETTERS[area], paste0(area, collapse = "")))
table(move$event, move$area2)
#39 extra rows

tags <- 
  mr0 %>%
  dplyr::filter(!is.na(tag)) %>%
  dplyr::arrange(tag, event, date) %>%
  dplyr::select(tag, event, date) %>%
  dplyr::mutate(dup = 1)

mr <-
  dplyr::left_join(mr0, 
                   tags[duplicated(tags[, c("tag", "event")]), -which(colnames(tags) == "event")], 
                   by = c("tag", "date")) %>%
  dplyr::filter(is.na(dup))
#saveRDS(mr, ".//mr.rds")
#WriteXLS::WriteXLS("mr", ".\\clean mr data to craig_Mar10.xlsx", BoldHeaderRow = TRUE)

#some calculations for report
mean(mr$fl[mr$n1 == 1]); sd(mr$fl[mr$n1 == 1])/sqrt(sum(mr$n1)); #marking event average size
mean(mr$fl[mr$n2 == 1]); sd(mr$fl[mr$n2 == 1])/sqrt(sum(mr$n2)); #Recapture event average size

mean(mr$fl[mr$gear == "FT"]); sd(mr$fl[mr$gear == "FT"])/sqrt(sum(mr$gear == "FT")); #marking event average size
mean(mr$fl[mr$gear == "HT"]); sd(mr$fl[mr$gear == "HT"])/sqrt(sum(mr$gear == "HT")); #Recapture event average size
mean(mr$fl[mr$gear == "HL"]); sd(mr$fl[mr$gear == "HL"])/sqrt(sum(mr$gear == "HL")); #marking event average size

  
n1 <- as.vector(table(mr$n1, mr$area)["1", ])
n2 <- as.vector(table(mr$n2, mr$area)["1", ])
m2 <- as.vector(table(mr$m2, mr$area)["1", ])
u2 <- n2 - m2

#growth recruitment
library(ggplot2)
grow <- 
  dplyr::left_join(mr[mr$m2 == 1 & !is.na(mr$tag), ], mr[mr$n1 == 1 & !is.na(mr$tag), c("tag", "fl")], "tag") %>%
  dplyr::mutate(d = fl.x - fl.y)

range(grow$d)
ggplot(grow, aes(x = d)) +
  geom_histogram(bins = 15) +
  xlab(label = "Growth Increment (mm)") +
  ylab("Number of fish") +
  ggtitle("Neck Lake Cutthroat Trout Growth") +
  theme_grey(base_size = 16)
mean(grow$d, na.rm = TRUE)
sd(grow$d, na.rm = TRUE)/sqrt(sum(m2))
t.test(grow$d)
plot(grow$fl.y, grow$d)
summary(lm(d~fl.y, data = grow))

#Catch vrs. depth
#Note: used excel figure in report.
#not an exact match but the trends match
#never investigated differences
mr[mr$gear != "HL", ] %>%
   dplyr::mutate(depth_g = cut(depth * 0.3048, breaks = c(0, 3.1, 6.1, 9.1, 12.2, 15.2, 100))) %>%
   ggplot(aes(x = depth_g, fill = event)) +
   geom_bar(position = "dodge") +
   facet_grid(gear ~ .)

#mixing
area <- 
  dplyr::left_join(mr[mr$n1 == 1, c("tag", "area")], 
                   mr[mr$m2 == 1, c("tag", "area")], 
                   by = "tag")
(area_tab <- table(area$area.x, area$area.y))
apply(area_tab, 1, sum)
sum(apply(area_tab, 1, sum))
apply(area_tab, 2, sum)
sum(apply(area_tab, 2, sum))
(mix_tab <- cbind(area_tab, n1 - apply(area_tab, 1, sum)))
chisq.test(mix_tab)

#marked fractions
mf <- 
  data.frame(recovery_area = LETTERS[1:3],
             marked = m2, 
             unmarked = u2,
             mf = round(m2/(m2 + u2), 2))
mf
chisq.test(mf[, c("marked", "unmarked")])

#recovery rates
rr <- 
  data.frame(area = LETTERS[1:3],
             recaptured = apply(area_tab, 1, sum), 
             no_recap = n1 - apply(area_tab, 1, sum),
             mf = round(apply(area_tab, 1, sum)/n1, 2))
rr
chisq.test(rr[, c("recaptured", "no_recap")])

(e2ks <- ks.test(mr$fl[mr$n1 == 1], mr$fl[mr$m2 == 1]))
(e1ks <- ks.test(mr$fl[mr$n2 == 1], mr$fl[mr$m2 == 1]))
(ecks <- ks.test(mr$fl[mr$n1 == 1], mr$fl[mr$n2 == 1]))

test_results <- 
  data.frame(
    x = 350, y = 0.33, 
    plot = c("Marking event selectivity", "Recapture event selectivity", "Event comparison"), 
    text = paste0(c("K-S test results \n", "K-S test results \n", "K-S test results (2018 data) \n"),"Dmax=", 
                  round(c(e1ks$statistic, e2ks$statistic, ecks$statistic), 2), "\n p-value=", 
                  round(c(e1ks$p.value, e2ks$p.value, ecks$p.value), 3)))
#98 data
raw98 <- 
  readxl::read_xlsx(".\\Copy of 1998 Neck AWL Data.xlsx",
                    sheet = 1,
                    range = "A6:M1835",
                    col_names = c("date", "subarea", "drop", "trap", "gear", "spp", "fl", "depth", rep("drop", 4), "tag"),
                    col_types = c("date", "numeric", "skip", "numeric", rep("text", 2), rep("numeric", 2), rep("skip", 4), "numeric"))
head(raw98)
tail(raw98)
lapply(raw98, table, useNA = "ifany")
range(table(raw98[format(raw98$date, "%m") == "05", ]$tag))
raw98_clean <- 
  raw98 %>% 
  dplyr::filter(format(date, "%m") == "05") %>%
  dplyr::group_by(tag) %>%
  dplyr::summarise(fl = mean(fl))

#ecdf
rbind(
  data.frame(lg = mr$fl[mr$n2 == 1], group = "Captured fish", plot = "Marking event selectivity"),
  data.frame(lg = mr$fl[mr$m2 == 1], group = "Recaptured fish", plot = "Marking event selectivity"),
  data.frame(lg = mr$fl[mr$n1 == 1], group = "Marked fish", plot = "Recapture event selectivity"),
  data.frame(lg = mr$fl[mr$m2 == 1], group = "Recaptured fish", plot = "Recapture event selectivity"),
  data.frame(lg = mr$fl[mr$n1 == 1], group = "Marked fish", plot = "Event comparison"),
  data.frame(lg = mr$fl[mr$n2 == 1], group = "Captured fish", plot = "Event comparison"),
  data.frame(lg = raw98_clean$fl, group = "May 1998", plot = "Event comparison")) %>%
  ggplot(aes(x = lg, color = group)) +
    stat_ecdf() +
    geom_label(aes(x = x, y = y, label = text), data = test_results, inherit.aes = FALSE) +
    facet_grid(plot ~ .) +
    xlab(label = "Fork length (mm)") +
    ylab("Cumulative fraction of Samples") +
    theme_grey(base_size = 16)



#Naive N
naiveN <- (sum(n1) + 1) * (sum(n2) + 1) / (sum(m2) + 1) - 1
naiveN
naivevN <- (sum(n1) + 1) * (sum(n2) + 1) * (sum(n1) - sum(m2)) * (sum(n2) - sum(m2)) / (sum(m2) + 1)^2 / (sum(m2) + 2)
sqrt(naivevN)
1.96 * sqrt(naivevN) / naiveN

#N with growth recruitment
library(jagsUI)
dat <- list(n1 = sum(n1),
            m2 = sum(m2),
            n2 = length(fl$fl[fl$event == "recap"]),
            n2_fl = fl$fl[fl$event == "recap"],
            n_diff = length(grow$d[!is.na(grow$d)]),
            diff = grow$d[!is.na(grow$d)]
)

post <- jags(data = dat,
             parameters.to.save = c("N", "n2_star", "mu", "sigma"),
             model.file = ".\\mr_jags.r",
             n.chains = 4,
             n.iter = 2500,
             n.burnin = 500,
             n.thin = 5,
             parallel = TRUE
)
plot(post)
post$summary
(N <- post$summary["N", "mean"]) 
N <- 4959
(se_N <- post$summary["N", "sd"])
se_N <- 361
(c(post$summary["N", "2.5%"], post$summary["N", "97.5%"]))
abs((c(post$summary["N", "97.5%"], post$summary["N", "2.5%"]) - post$summary["N", "mean"]) / post$summary["N", "mean"])

library(aslpack)
lc <- 
  mr %>% 
  dplyr::mutate(sex = cut(fl, breaks = seq(180, 440, 20), include.lowest = TRUE)) %>%
  dplyr::filter(event == "mark") %>%
  dplyr::rename(length = fl)
asl(lc, totaldat = data.frame(total = 4959, se_total = 361))
#by harvest cutoff-marking
lc2 <- 
  mr %>% 
  dplyr::mutate(sex = cut(fl, breaks = c(180, 273, 440), include.lowest = TRUE)) %>%
  dplyr::filter(event == "mark") %>%
  dplyr::rename(length = fl)
asl(lc2, totaldat = data.frame(total = 4959, se_total = 361))
# #by harvest cutoff-recapture (warning biased sample)
# lc <- 
#   mr %>% 
#   dplyr::mutate(sex = cut(fl, breaks = c(180, 273, 440), include.lowest = TRUE)) %>%
#   dplyr::filter(event == "recap") %>%
#   dplyr::rename(length = fl)
# asl(lc, 
#     totaldat = data.frame(total = 4959, se_total = 361))

tab <- table(mr$gear, mr$event)
list(counts = addmargins(tab),
     proportions = round(addmargins(tab)[,1:dim(tab)[2]]/addmargins(tab)[, dim(tab)[2] + 1], 2),
     test = DescTools::GTest(tab))
library(ggplot2)
fl$event <- factor(fl$event, levels = c("mark", "recap"), labels = c("Marking Event", "Recapture Event"))
ggplot(fl, aes(x = fl, color = gear)) + 
  geom_line(bw = "bcv", stat = "density") + 
  geom_vline(xintercept = 180) + facet_grid(event~.) +
  scale_x_continuous(name = "Fork Length", breaks = seq(75, 425, 50)) +
  scale_y_continuous(name = "density") +
  scale_color_discrete(name = "Gear Type") +
  theme_grey(base_size = 16)
#lg samples from each net share a distribution during the marking event but differ during the recapture event
ks.test(fl$fl[fl$gear == "FT" & fl$event == "Marking Event"], fl$fl[fl$gear == "HT" & fl$event == "Marking Event"])
ks.test(fl$fl[fl$gear == "FT" & fl$event == "Marking Event"], fl$fl[fl$gear == "HL" & fl$event == "Marking Event"])
ks.test(fl$fl[fl$gear == "HT" & fl$event == "Marking Event"], fl$fl[fl$gear == "HL" & fl$event == "Marking Event"])

ks.test(fl$fl[fl$gear == "FT" & fl$event == "Recapture Event"], fl$fl[fl$gear == "HT" & fl$event == "Recapture Event"])
ks.test(fl$fl[fl$gear == "FT" & fl$event == "Recapture Event"], fl$fl[fl$gear == "HL" & fl$event == "Recapture Event"])
ks.test(fl$fl[fl$gear == "HT" & fl$event == "Recapture Event"], fl$fl[fl$gear == "HL" & fl$event == "Recapture Event"])



fl$group <- ifelse(fl$fl < 180, "< 180 mm", ifelse(fl$fl < 273, "Illegal", "Legal"))
fl$gear2 <- factor(fl$gear, labels = c(FT = "Funnel Trap", HL = "Hook & Line", HT = "Hoop Trap"))
means <- aggregate(fl ~ event + gear2, fl, mean)
ggplot(fl, aes(x = fl, fill = group)) + 
  geom_histogram(bins = 50) + 
  facet_grid(gear2 ~ event)+ 
  geom_vline(aes(xintercept = fl), data = means, linetype = 2) +
  scale_x_continuous(name = "Fork Length", breaks = seq(75, 425, 50)) +
  scale_y_continuous(name = "Number of Fish") +
  scale_fill_discrete(name = "Length Group") +
  theme_grey(base_size = 16)

#reduced samples to detect % legal
dates_mark <- lapply(1:8, function(x) {combn(unique(lc2$date), x, simplify = FALSE)})
# get mean for each date combination
mean_mark <- 
  lapply(1:8, function(y) {
    sapply(dates_mark[[y]], 
           function(x){
             tab <- asl(lc2[lc$date %in% x, ], totaldat = data.frame(total = 4959, se_total = 361))
             c(mu = tab$p.z[1], sd = tab$sd_p.z[1], n1 = tab$n.z[1], n = tab$n.z[3], lb = tab$lci_p.z[1], ub = tab$uci_p.z[1],
               areas = length(unique(factor(lc[lc$date %in% x, ]$subarea, 1:9, rep(LETTERS[1:3], each = 3)))),
               subareas = length(unique(lc[lc$date %in% x, ]$subarea)))}) %>% 
      t() %>%
      as.data.frame() %>%
      dplyr::mutate(days = paste0(y, " days"),
                    combination = 1:dplyr::n())}
  ) %>% 
  do.call(rbind, .)
names(mean_mark) <- c("mu", "sd", "n1", "n", "lb", "ub", "areas", "subareas", "days", "combination")

ggplot(mean_mark, aes(x = combination, y = mu)) + #, color = as.factor(areas))) +
  geom_point() +
  geom_errorbar(aes(ymin = lb, ymax = ub)) +
  annotate(geom = "rect", ymin = 0.839, ymax = 0.885, xmin = 1, xmax = max(mean_mark$combination), alpha = 0.2) +
  facet_wrap(days ~ ., nrow = 4, ncol = 2) +
  scale_y_continuous(name = "Percent < 11 inches total length") +
  scale_x_continuous(name = "Combination") +
  theme_bw(base_size = 16)

aggregate(subareas ~ days, mean_mark, range)
aggregate(subareas ~ days, mean_mark, mean)

mean_mark$inCI <- 0.839 <= mean_mark$mu & mean_mark$mu <= 0.885
aggregate(inCI ~ days, mean_mark, mean)
aggregate(inCI ~ days, mean_mark, sum)
aggregate(inCI ~ days, mean_mark, length)
aggregate(mu ~ days, mean_mark, range)

aggregate(n1 ~ days, mean_mark, mean)
(n <- aggregate(n ~ days, mean_mark, mean))
#Average SE
aggregate(sd ~ days, mean_mark, mean)
sqrt((5000 - n[2]) / 5000 * .86 * .14 / n[2])
#worst case
sqrt(.5 * .5 / n[2])


#### Consistancy test by gear type???
# n1g <- as.vector(table(mr$n1, mr$gear)["1", ])
# n2g <- as.vector(table(mr$n2, mr$gear)["1", ])
# m2g <- as.vector(table(mr$m2, mr$gear)["1", ])
# u2g <- n2g - m2g
# 
# #mixing
# gear <- 
#   dplyr::left_join(mr[mr$n1 == 1, c("tag", "gear")], 
#                    mr[mr$m2 == 1, c("tag", "gear")], 
#                    by = "tag")
# (gear_tab <- table(gear$gear.x, gear$gear.y))
# apply(gear_tab, 1, sum)
# sum(apply(gear_tab, 1, sum))
# apply(gear_tab, 2, sum)
# sum(apply(gear_tab, 2, sum))
# (mix_tabgear <- cbind(gear_tab, n1g - apply(gear_tab, 1, sum)))
# chisq.test(mix_tabgear)
# 
# #marked fractions
# mfg <- 
#   data.frame(recovery_gear = c("FT", "HL", "HT"),
#              marked = m2g, 
#              unmarked = u2g,
#              mf = round(m2g/(m2g + u2g), 2))
# mfg
# chisq.test(mfg[, c("marked", "unmarked")])
# 
# #recovery rates
# rrg <- 
#   data.frame(area = c("FT", "HL", "HT"),
#              recaptured = apply(gear_tab, 1, sum), 
#              no_recap = n1g - apply(gear_tab, 1, sum),
#              mf = round(apply(gear_tab, 1, sum)/n1g, 2))
# rrg
# chisq.test(rrg[, c("recaptured", "no_recap")])
