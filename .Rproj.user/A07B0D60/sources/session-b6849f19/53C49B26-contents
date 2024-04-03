#' handle data
#'
#' @param datain a dataframe or tibble
#'
#' @return a dataframe
#' @export
#'
#' @examples
#' gmtdata(adis)
gmtdata <- function(datain){
  adis1 <- datain %>%
    filter(IPPSFL == "Y", PARAMCD == "SAR2NAB", ANL01FL == "Y", COHORT == "Cohort 2") %>%
    mutate(TPTN = case_when(AVISITN == 0 ~ 1+(TRTAN-1)*2,
                            AVISITN %in% c(28, 42) ~ 2+(TRTAN-1)*2,
                            AVISITN %in% c(120, 150) ~ 3+(TRTAN-1)*2,
                            AVISITN %in% c(148, 178) ~ 4+(TRTAN-1)*2),
           LOGAVAL = log10(AVAL),
           LOGFOLD = log10(AVAL/BASE)
    )

  mean1 <- adis1 %>%
    group_by(TRTAN, TPTN, AVISITN) %>%
    select(TRTAN, TPTN, AVISITN, LOGAVAL, LOGFOLD) %>%
    dplyr::summarise(n = n(),
                     mean1 = mean(LOGAVAL),
                     mean2 = mean(LOGFOLD),
                     sd1 = sd(LOGAVAL),
                     sd2 = sd(LOGFOLD)) %>%
    mutate(se = sd1/sqrt(n),
           lclm = mean1 - 1.96 * se,
           uclm = mean1 + 1.96 * se,
           yerrl = 10**lclm,
           yerru = 10**uclm,
           means = 10**mean1,
           foldmean = ifelse(AVISITN != 0,10**mean2, NA),
           nlabel = str_c("(N=",as.character(n),")"),
           foldlabel = str_c(as.character(round(foldmean,1)),"x"),
           ymin = 10^5*1.5,
           ymax = 10^5*2)

  final <- plyr::rbind.fill(adis1, mean1)
  return(final)
}

