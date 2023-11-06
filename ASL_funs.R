#' Standard ASL data calculations
#'
#' Creates a dataframe with standard ASL summary statistics.
#'
#' @param asl data.frame with columns named age, sex, length and any column names specified in groupvars
#' @param totaldat data.frame with any column names specified in groupvars and a column named total giving the total for each group, and se_total giving the
#' se of the total for each group.  If total is not random then se_total column can be omitted. Default allows proportion estimates wo apportionment.
#' @param groupvars a character vector with the names of any grouping varibles
#'
#' @return a tbl with proportion, apportioned total, and length estimates for all combinations of sex and age.
#'
#' @examples
#' asl(asl, summarise(weir, total = sum(passage)))
#' asl(asl, weir %>% group_by(strata) %>% summarise(total = sum(passage)), "strata")
#'
#' @export
asl <- function(asl, totaldat = data.frame(), groupvars = NULL){
  ld_summary <- function(data, gv){
    dplyr::group_by_(data, .dots = gv) %>%
      dplyr::summarise(n.z = dplyr::n(),
                       lg.z = mean(length, na.rm = TRUE),
                       se_lg.z = sd(length, na.rm = TRUE)/sqrt(n.z),
                       min_lg.z = min(length, na.rm = TRUE),
                       max_lg.z = max(length, na.rm = TRUE))
  }

  sa <- if(sum(c("sex", "age") %in% names(asl)) == 2) ld_summary(asl, c(groupvars, "sex", "age")) else NULL
  s <- if("sex" %in% names(asl)) {ld_summary(asl, c(groupvars, "sex")) %>% dplyr::mutate(age = "All")} else NULL
  a <- if("age" %in% names(asl)) {ld_summary(asl, c(groupvars, "age")) %>% dplyr::mutate(sex = "Both")} else NULL
  all <- ld_summary(asl, c(groupvars)) %>% dplyr::mutate(age = "All", sex = "Both")
  groupn <- dplyr::select_(all, .dots = setNames(c(groupvars, "n.z"), c(groupvars, "n")))

  if(is.data.frame(totaldat) && nrow(totaldat) != 0){
    temp <- if(!is.null(groupvars)){
      dplyr::bind_rows(sa, s, a, all) %>%
        dplyr::left_join(groupn, by = groupvars) %>%
        dplyr::left_join(totaldat, by = groupvars) %>%
        dplyr::mutate(se_total = ifelse("se_total" %in% colnames(totaldat), se_total, 0))
    } else{
      dplyr::bind_rows(sa, s, a, all) %>%
        dplyr::mutate(n = all[["n.z"]],
                      total = totaldat[["total"]],
                      se_total = ifelse("se_total" %in% colnames(totaldat), totaldat[["se_total"]], 0))
    }

    out <-
      temp %>%
      dplyr::mutate(p.z = n.z/n,
                    var_p.z = (1 - n / total) * p.z * (1-p.z) /(n - 1),
                    sd_p.z = sqrt(var_p.z),
                    lci_p.z = DescTools::BinomCI(n.z, n)[, 2],
                    uci_p.z = DescTools::BinomCI(n.z, n)[, 3],
                    t.z = total * p.z,
                    var_t.z = total^2 * var_p.z + se_total^2 * p.z - se_total^2 * var_p.z,
                    sd_t.z = sqrt(var_t.z)) %>%
      dplyr::ungroup() %>%
      dplyr::select_(.dots = c(groupvars, "sex", "age", "n.z", "p.z", "sd_p.z", "lci_p.z", "uci_p.z", "t.z", "sd_t.z", "lg.z", "se_lg.z", "min_lg.z", "max_lg.z"))
  }

  if(is.data.frame(totaldat) && nrow(totaldat) == 0){
    temp <- if(!is.null(groupvars)){
      dplyr::bind_rows(sa, s, a, all) %>%
        dplyr::left_join(groupn, by = groupvars)
    } else{
      dplyr::bind_rows(sa, s, a, all) %>%
        dplyr::mutate(n = all[["n.z"]])
    }

    out <-
      temp %>%
      dplyr::mutate(p.z = n.z/n,
                    var_p.z = p.z * (1-p.z) /(n - 1),
                    sd_p.z = sqrt(var_p.z),
                    lci_p.z = DescTools::BinomCI(n.z, n)[, 2],
                    uci_p.z = DescTools::BinomCI(n.z, n)[, 3]) %>%
      dplyr::ungroup() %>%
      dplyr::select_(.dots = c(groupvars, "sex", "age", "n.z", "p.z", "sd_p.z", "lci_p.z", "uci_p.z", "lg.z", "se_lg.z", "min_lg.z", "max_lg.z"))
  }
  out
}





#' Formats an ASL table
#'
#' Creates a dataframe in a convenient format for creating excel ASL tables.
#'
#' @param dat the output from aslpack::asl().
#' @param totalname character. Name of the apportioned quantity, ex: "Weir", "Harvest".
#' @param output "al" for age-length table, "sl" for sex-length table, "asl" for age-sex-length table.
#' @param display_cols "stat" to display statistics across columns. The default (NULL) will display sex or age across columns.
#' @param gather_cols statistics to display.
#' @param overall_se Manually provide the overall SE when it is overestimated by stratification.
#'
#' @return a tbl with one ASL table ages, sexes and statistics labeled.
#'
#' @examples
#' asl(asl, summarise(weir, total = sum(passage))) %>% tab_asl()
#' asl(asl, weir %>% group_by(strata) %>% summarise(total = sum(passage)), "strata") %>% combine_strata() %>% tab_asl()
#'
#' @export
tab_asl <- function(dat,
                    totalname = "Total",
                    output = "asl",
                    display_cols = NULL,
                    gather_cols = c("n.z", "p.z", "ci_p.z", "lg.z", "range_lg.z"),
                    overall_se = NULL){
  stopifnot(!(display_cols %in% c(NULL, "stats")))

  if(!is.null(overall_se)) dat$sd_t.z[dat$age == "All" & dat$sex == "Both"] = overall_se

  temp0 <-
    dat %>%
    dplyr::mutate_at(c("p.z", "sd_p.z", "lci_p.z", "uci_p.z"), dplyr::funs(trimws(format(round(., 3), nsmall = 3)))) %>%
    dplyr::mutate_at(c("lg.z", "se_lg.z"), dplyr::funs(trimws(format(round(., 1), big.mark = ",", nsmall = 1)))) %>%
    dplyr::mutate_at(c("n.z", "min_lg.z", "max_lg.z"), as.integer) %>%
    dplyr::mutate(p.z = paste0(p.z, " (", ifelse(sex =="Both" & age == "All", 0, sd_p.z), ")"),
                  ci_p.z = ifelse(sex =="Both" & age == "All", "", paste0(lci_p.z, " - ", uci_p.z)),
                  lg.z = paste0(lg.z, " (", se_lg.z, ")"),
                  range_lg.z = paste0("(", min_lg.z, "-", max_lg.z, ")"))
  if("t.z" %in% colnames(temp0)){
    temp0 <-
      temp0 %>%
      dplyr::mutate(t.z = paste0(trimws(format(round(t.z, 0), big.mark = ",", nsmall = 0)),
                                 " (",
                                 trimws(format(round(sd_t.z, 0), big.mark = ",", nsmall = 0)),
                                 ")"))
    gather_cols <- c(gather_cols[1:3], "t.z", gather_cols[4:5])
  }

  temp <- temp0 %>%
    dplyr::select(sex, age, gather_cols) %>%
    tidyr::gather_("measure", "value", gather_cols = gather_cols) %>%
    dplyr::mutate(sex_lab = factor(sex, levels = c("F",       "M",     "Both"),
                                   labels = c("Females", "Males", "Both Sexes"),
                                   ordered = TRUE),
                  stat_lab = factor(measure, levels = c("n.z",
                                                        "p.z",
                                                        "ci_p.z",
                                                        "t.z",
                                                        "lg.z",
                                                        "range_lg.z"),
                                    labels = c("Sample size",
                                               "Proportion (SE)",
                                               "95% CI(Proportion)",
                                               paste0(totalname, "(SE)"),
                                               "Mean Length (SE)",
                                               "Range(Length)"),
                                    ordered = TRUE),
                  age_lab = factor(age, levels = c("1.0", "1.1", "1.2", "1.3", "1.4", "1.5", "2.1", "2.2", "2.3", "3.1", "All"),
                                   labels = c("1.0", "1.1", "1.2", "1.3", "1.4", "1.5", "2.1", "2.2", "2.3", "3.1", "All ages"),
                                   ordered = TRUE)) %>%
    dplyr::select(sex_lab, stat_lab, age_lab, value)

  switch(output,
         "al" = temp %>%
           dplyr::filter(sex_lab == "Both Sexes") %>%
           dplyr::select(-sex_lab) %>%
           tidyr::spread(if(is.null(display_cols)) age_lab else(stat_lab), value),
         "sl" = temp %>%
           dplyr::filter(age_lab == "All ages") %>%
           dplyr::select(-age_lab) %>%
           tidyr::spread(if(is.null(display_cols)) sex_lab else(stat_lab), value),
         "asl" = temp %>% tidyr::spread(if(is.null(display_cols)) age_lab else(stat_lab), value))
}




#' Combines stratified ASL estimates
#'
#' Reduce a stratified ASL estimate to an overall estimates with weighed proportion and length estimates.
#'
#' @param dat the output from aslpack::asl(asl, total, "strata")
#'
#' @return a tbl with the same format as the argument but combined across strata
#'
#' @examples
#' asl(asl, weir %>% group_by(strata) %>% summarise(total = sum(passage)), "strata") %>% combine_strata()
#'
#' @export
combine_strata <- function(dat){
  weight <-
    dat %>%
    dplyr::select(age, sex, t.z, lg.z) %>%
    dplyr::group_by(age, sex) %>%
    dplyr::summarize(sum_t.z = sum(t.z),
                     var = sum(lg.z * t.z))

  dplyr::left_join(dat, weight, by = c("age", "sex")) %>%
    dplyr::mutate(w_lg.z = lg.z * t.z/sum_t.z,
                  w_se_lg.z = se_lg.z^2 * (t.z/sum_t.z)^2 + (lg.z * sum_t.z - var)^2 / sum_t.z^4 * sd_t.z^2) %>%
    dplyr::group_by(age, sex) %>%
    dplyr::summarise(n.z = sum(n.z),
                     t.z = sum(t.z),
                     lg.z = sum(w_lg.z),
                     sd_t.z = sqrt(sum(sd_t.z^2)),
                     se_lg.z = sqrt(sum(w_se_lg.z, na.rm = TRUE)),
                     min_lg.z = min(min_lg.z),
                     max_lg.z = max(max_lg.z)) %>%
    dplyr::mutate(total = weight[[which(weight$age == "All" & weight$sex == "Both"), "sum_t.z"]],
                  p.z = t.z/total,
                  var_p.z = sd_t.z^2 * total^-2,
                  sd_p.z = sqrt(var_p.z),
                  logitlci_p.z = log(p.z / (1-p.z)) - 1.96 * sqrt(1 / p.z^2 / (1-p.z)^2 * sd_p.z^2),
                  logituci_p.z = log(p.z / (1-p.z)) + 1.96 * sqrt(1 / p.z^2 / (1-p.z)^2 * sd_p.z^2),
                  lci_p.z = exp(logitlci_p.z)/(1 + exp(logitlci_p.z)),
                  uci_p.z = exp(logituci_p.z)/(1 + exp(logituci_p.z)))%>%
    dplyr::select(age, sex, n.z, p.z, sd_p.z, lci_p.z, uci_p.z, t.z, sd_t.z, lg.z, se_lg.z, min_lg.z, max_lg.z) %>%
    dplyr::ungroup()
}





#' Check strata definitions
#'
#' Compare date ranges, days included and distribution of data between strata for asl and total datasets
#'
#' @param asl a data.frame with columns strata and date which contains asl sampling data
#' @param total a data.frame with columns strata, date and total_name which contains total data
#' @param total_name the name of the total column
#'
#' @return a tibble
#'
#' @examples
#' compare_strata(sockeye_asl, sockeye_weir, passage)
#'
#' @export
compare_strata <- function(asl, total, total_name){
  stopifnot("strata" %in% colnames(total),
            "strata" %in% colnames(asl),
            "date" %in% colnames(total),
            "date" %in% colnames(asl))

  total_name <- dplyr::enquo(total_name)

  one <- asl %>%
    dplyr::group_by(strata) %>%
    dplyr::summarise(asl_min = as.Date(min(date)),
                     asl_max = as.Date(max(date)),
                     asl_n = dplyr::n(),
                     asl_days = length(unique(date))) %>%
    dplyr::mutate(asl_pct = format(round(asl_n/sum(asl_n), 2), nsmall = 2))

  two <- total %>%
    dplyr::group_by(strata) %>%
    dplyr::summarise(total_min = as.Date(min(date)),
                     total_max = as.Date(max(date)),
                     total_days = length(unique(date)),
                     total_n = sum(!!total_name)) %>%
    dplyr::mutate(total_pct = format(round(total_n/sum(total_n), 2), nsmall = 2))

  dplyr::full_join(one, two, "strata") %>%
    dplyr::mutate(check = (asl_min >= total_min) && (asl_max <= total_max),
                  asl_dates = paste0(format(asl_min, "%b %d"), " to ", format(asl_max, "%b %d")),
                  total_dates = paste0(format(total_min, "%b %d"), " to ", format(total_max, "%b %d"))) %>%
    dplyr::select(strata, asl_dates,  asl_days, asl_n, asl_pct, total_dates, total_days, total_n, total_pct, check)
}






#' Compare distribution of samples and abundance thru time.
#'
#' Creates a empirical cumulative density plot of abundance and samples collected with respect to time.
#'
#' @param dat a data.frame with columns named date, samples and the variable name specified in total_name
#' @param total_name A character string specifying the name of the variable specifying abundance
#'
#' @return a figure.
#'
#' @examples
#' plot_ks(sockeye_weir, "passage")
#'
#' @export
plot_ks <- function(dat, total_name){
  stopifnot("date" %in% colnames(dat),
            total_name %in% colnames(dat),
            "samples" %in% colnames(dat))

  fish <- rep(as.Date(dat$date), times = dat[[total_name]])
  samples <- rep(as.Date(dat$date), times = dat$samples)
  plot(ecdf(fish),
       main = paste0("Cumulative Proportion of ", gsub("(^[[:alpha:]])", "\\U\\1", total_name, perl=TRUE), " and Samples by Date"),
       ylab = "Proportion",
       xlab = "Date",
       xaxt = "n")
  axis(1, as.Date(dat$date), format(dat$date, "%b %d"))
  plot(ecdf(samples), add = TRUE, col = "red")
  legend(as.Date(quantile(dat$date[dat[[total_name]] != 0], .75)), .21,
         legend = c(total_name, "asl samples"),
         col = c("black", "red"),
         pch = 16,
         yjust = 0)
  test <- ks.test(as.numeric(fish),
                  as.numeric(samples))
  text(as.Date(quantile(dat$date[dat[[total_name]] != 0], .75)), .19,
       labels = paste0(test$method, "\n p.val = ", format.pval(test$p.value, digits = 2, eps = 0.001, nsmall = 3)),
       adj = c(0, 1))
}





#' Test for differences in composition between strata
#'
#' Likelihood ratio test of differences in composition between strata.
#'
#' @param dat a data.frame with the columns name specified in as and strata
#' @param as A character string specifying the name of the variable to test
#' @param strata A character string specifying the name of the variable to stratify. Defaults to strata.
#'
#' @return a list with counts by strata and category, frequencies by strata and category and results from the LR test
#'
#' @examples
#' tab_lr(sockeye_asl, "sex")
#'
#' @export
tab_lr <- function(dat, as, strata = "strata"){
  stopifnot(strata %in% colnames(dat),
            as %in% colnames(dat))

  tab<-table(dat[[strata]],dat[[as]], useNA = "ifany")
  list(counts = addmargins(tab),
       proportions = round(addmargins(tab)[,1:dim(tab)[2]]/addmargins(tab)[, dim(tab)[2] + 1], 2),
       test = DescTools::GTest(tab))
}






