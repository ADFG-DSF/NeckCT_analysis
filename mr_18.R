library(magrittr)
raw <- 
  readxl::read_xlsx(".\\Neck 2018 AWL Data Combined To Adam 12_17_18.xlsx",
                    sheet = 1,
                    range = "A4:K2191",
                    col_names = c("date", "subarea", "depth", "trap", "gear", "fl", "tag", "clip", "comment"),
                    col_types = c(rep("skip", 2), "date", "numeric", rep("text", 7)))
head(raw) # depth, fl and tag wrong class
tail(raw)
lapply(raw, table, useNA = "ifany") 
  # depth = "HL" should be NA
  # fl = "RECAP" should be NA
raw[raw$tag %in% c("Lost tag", "NONE", "second event recap"), ] %>% 
  print(n = 100) 
  #Keep "Lost tag", delete "NONE" and "second event recap"
raw[raw$clip %in% c("NO", "NONE"), ] %>% 
  print(n = 100) 
  raw[raw$tag == "823", ]
    # delete clip = "NONE"
hist(as.numeric(raw$fl)) #OK
raw[which(raw$comment != "NA"), ] %>% print(n = 100)

fl <- 
  raw %>% 
  dplyr::left_join(data.frame(subarea = 1:9, area = rep(LETTERS[1:3], each = 3)), by = "subarea") %>%
  dplyr::filter(!(tag %in% c("NONE", "second event recap")),
                clip != "NONE") %>%
  dplyr::mutate(event = ifelse(date < as.POSIXct("2018-06-15 UTC"), "mark", "recap"),
                date = as.Date(date, format = "%B%d"),
                depth = as.numeric(ifelse(depth == "HL", NA, depth)),
                fl = as.numeric(ifelse(fl ==  "RECAP", NA, fl)),
                comment = ifelse(tag == "second event recap", tag, comment),
                tag = as.numeric(ifelse(tag %in% c("Lost tag", "NA", "NONE", "second event recap"), NA, tag)),
                clip = ifelse(clip %in% c("NO", "NONE", "NA"), NA, clip),
                comment = ifelse(comment == "NA", NA, comment)
  ) %>%
  dplyr::select(-trap, -subarea)
fl
lapply(fl, table, useNA = "ifany")
fl[is.na(fl$fl), ] %>% 
  print(n = 100)

#tags only recorded during recap event
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
mr0[which(mr0$comment == "lost floy tag"), ] # should be a recap
mr0$m2[which(mr0$comment == "lost floy tag")] <- 1
mr0[which(mr0$comment == "lost floy tag"), ] # should be a recap
table(mr0$n1, mr0$n2, mr0$m2) #OK

lapply(mr0, table, useNA = "ifany")
mr0[is.na(mr0$clip), ] # Missing clips are morts are after recapture event or tag = 823 which was recaptured

#Multiple captures during the same event
move <-
  mr0 %>% 
  dplyr::filter(!is.na(tag)) %>%
  dplyr::arrange(tag, event, date, area) %>% 
  dplyr::group_by(tag, event) %>% 
  dplyr::summarise(area2 = ifelse(length(area) == 1, LETTERS[area], paste0(area, collapse = "")))
table(move$event, move$area2)
#40 extra rows

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
  dplyr::filter(is.na(dup) | (tag == 547 & fl == 225))
  
n1 <- as.vector(table(mr$n1, mr$area)["1", ])
n2 <- as.vector(table(mr$n2, mr$area)["1", ])
m2 <- as.vector(table(mr$m2, mr$area)["1", ])
u2 <- n2 - m2

#mixing
area <- 
  dplyr::left_join(mr[mr$n1 == 1, c("tag", "area")], 
                   mr[mr$m2 == 1, c("tag", "area")], 
                   by = "tag")
(area_tab <- table(area$area.x, area$area.y))
apply(area_tab, 1, sum)
apply(area_tab, 2, sum)

#check why sum of movement table does not match sum of m2
mr %>%
  dplyr::group_by(tag) %>%
  dplyr::summarise(n1 = sum(n1), n2 = sum(n2), m2 = sum(m2)) %>%
  dplyr::filter(m2 == 1 & (n1 + n2 != 2))
#tags 211 and 542 recorded during recapture event but not recorded during marking event.
#One fish lost tag but still known recap due to fin clip.

#marked fractions
mf <- 
  data.frame(area = LETTERS[1:3],
             marked = apply(area_tab, 1, sum), 
             unmarked = u2 - apply(area_tab, 1, sum),
             mf = round(apply(area_tab, 1, sum)/u2, 2))
mf
chisq.test(mf[, c("marked", "unmarked")])

#recovery rates
rr <- 
  data.frame(area = LETTERS[1:3],
             marked = apply(area_tab, 1, sum), 
             unmarked = n1 - apply(area_tab, 1, sum),
             mf = round(apply(area_tab, 1, sum)/n1, 2))
rr
chisq.test(rr[, c("marked", "unmarked")])

plot(ecdf(mr$fl[mr$n1 == 1]))
lines(ecdf(mr$fl[mr$m2 == 1]), col = "red")
ks.test(mr$fl[mr$n1 == 1], mr$fl[mr$m2 == 1])

plot(ecdf(mr$fl[mr$n2 == 1]))
lines(ecdf(mr$fl[mr$m2 == 1]), col = "red")
ks.test(mr$fl[mr$n2 == 1], mr$fl[mr$m2 == 1])

plot(ecdf(mr$fl[mr$n1 == 1]))
lines(ecdf(mr$fl[mr$n2 == 1]), col = "red")
ks.test(mr$fl[mr$n1 == 1], mr$fl[mr$n2 == 1])

grow <- 
  dplyr::left_join(mr[mr$m2 == 1, ], mr[mr$n1 == 1, c("tag", "fl")], "tag") %>%
  dplyr::mutate(d = fl.x - fl.y)
hist(grow$d)
mean(grow$d, na.rm = TRUE)
plot(grow$fl.y, grow$d)

#Naive N
N <- (sum(n1) + 1) * (sum(n2) + 1) / (sum(m2) + 1) - 1
N
vN <- (sum(n1) + 1) * (sum(n2) + 1) * (sum(n1) - sum(m2)) * (sum(n2) - sum(m2)) / (sum(m2) + 1)^2 / (sum(m2) + 2)
sqrt(vN)
1.96 * sqrt(vN) / N

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
             n.iter = 2000,
             n.burnin = 1000,
             n.thin = 1,
             parallel = TRUE
)
plot(post)
post$summary
N <- post$summary["N", "mean"]
se_N <- post$summary["N", "sd"]
abs((c(post$summary["N", "97.5%"], post$summary["N", "2.5%"]) - post$summary["N", "mean"]) / post$summary["N", "mean"])

library(aslpack)
lc <- 
  mr %>% 
  dplyr::mutate(sex = cut(fl, breaks = seq(180, 440, 20), include.lowest = TRUE)) %>%
  dplyr::filter(event == "mark") %>%
  dplyr::rename(length = fl)
asl(lc, 
    totaldat = data.frame(total = N, se_total = se_N))

tab <- table(mr$gear, mr$event)
list(counts = addmargins(tab),
     proportions = round(addmargins(tab)[,1:dim(tab)[2]]/addmargins(tab)[, dim(tab)[2] + 1], 2),
     test = DescTools::GTest(tab))
library(ggplot2)
ggplot(fl, aes(x = fl, color = gear)) + geom_density() + geom_vline(xintercept = 180) + facet_grid(event~.)
