#' draw GMT plot
#'
#' @param datain a dataframe or tibble
#' @param GrpVar group by Column
#' @param AvisintVar analysis visit numeric column
#' @param Aval analysis value numeric column
#' @param Base baseline column
#' @param YLabel a character for Y axis label
#' @param LegendLabel a vector for Legend label
#' @param FigureName a character for figure name
#'
#' @return a figure as ggplot2 object
#' @export
#'
#' @examples
#' gmtplot(adis,"gmt_plot")
gmtplot <- function(datain, GrpVar, AvisintVar, Aval, Base, YLabel, LegendLabel, FigureName){
  adis1 <- datain %>%
    mutate(Grp = GrpVar,
           Avisitn = AvisintVar,
           Xp = case_when(AvisintVar == 0 ~ Grp - 0.2,
                          AvisintVar %in% c(28, 42) ~ Grp + 0.2),
           AVAL = Aval,
           BASE = Base,
           LOGAVAL = log10(AVAL),
           LOGFOLD = log10(AVAL/BASE)
    )

  jitter <- adis1 %>%
    count(Grp, Xp, AVAL)

  adis2 <- left_join(adis1, jitter, by = c("Grp", "Xp", "AVAL")) %>%
    arrange(Grp, Xp, AVAL) %>%
    group_by(Grp, Xp, AVAL) %>%
    mutate(jittern = row_number(),
           jitterDot = case_when(jittern < ceiling(n/2) ~ jittern*(-1),
                                 jittern > ceiling(n/2) ~ jittern-ceiling(n/2),
                                 TRUE ~ 0),
           XpDot = case_when(n <= 8 ~ Xp+jitterDot*0.04,
                             n <= 16 ~ Xp+jitterDot*0.02,
                             TRUE ~ Xp+jitterDot*0.01))

  mean1 <- adis2 %>%
    group_by(Grp, Xp, Avisitn) %>%
    select(Grp, Xp, Avisitn, LOGAVAL, LOGFOLD) %>%
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
           foldmean = ifelse(Avisitn != 0,10**mean2, NA),
           nlabel = str_c("(N=",as.character(n),")"),
           foldlabel = str_c(as.character(round(foldmean,1)),"x"),
           ymin = 10^5*1.5,
           ymax = 10^5*2)

  final <- plyr::rbind.fill(adis2, mean1)

  windowsFonts(
    KT = windowsFont("楷体"),
    Arial = windowsFont("Arial"),
    WRYH = windowsFont("微软雅黑"),
    ArialUnicode = windowsFont("Arial Unicode MS")
  )

  GMTPlotColor <- colorRampPalette(c("grey", "blue", "red", "green"))

  ggplot(final) +
    geom_col(aes(x = Xp, y = means, fill = factor(Grp)), alpha = 0.3, na.rm = TRUE)+
    geom_point(aes(x = XpDot, y = AVAL, color = factor(Grp)), na.rm = TRUE)+
    geom_line(aes(x = XpDot, y = AVAL, group = USUBJID, color = factor(Grp)), na.rm = TRUE, alpha = 0.3)+
    geom_errorbar(aes(x = Xp, ymin = yerrl, ymax = yerru))+
    geom_text(aes(x = Xp, y = 10^5, label = as.character(round(means))), na.rm = TRUE, family = "WRYH", fontface = "bold", size = 4)+
    geom_text(aes(x = Xp, y = 0.25, label = nlabel), na.rm = TRUE, family = "WRYH", fontface = "bold", size = 4)+
    geom_text(aes(x = Grp, y = 10^5*3, label = foldlabel), na.rm = TRUE, family = "WRYH", fontface = "bold", size = 4)+
    geom_linerange(aes(x = Xp, ymin = ymin, ymax = ymax), na.rm = TRUE)+
    geom_linerange(aes(xmin = Grp-0.2, xmax = Grp+0.2, y = 10^5*2))+
    geom_vline(xintercept = unique(jitter$Grp)[-which(unique(jitter$Grp) == max(unique(jitter$Grp)))]+0.5, linetype = "f8", alpha = 0.4)+
    theme_classic()+
    annotation_logticks(sides = "l")+
    scale_y_log10(breaks = 10^(0:5),
                  label = c(expression(bold("10"^"0")), expression(bold("10"^"1")), expression(bold("10"^"2")), expression(bold("10"^"3")), expression(bold("10"^"4")), expression(bold("10"^"5"))),
                  expand = c(0, 0))+
    scale_x_continuous(breaks = distinct(jitter,Xp)$Xp,
                       labels = c("D0 ", "D28", "D0 ", "D28", "D0 ", "D28", "D0 ", "D28"))+
    labs(y = YLabel, x = NULL)+
    scale_fill_manual(values = GMTPlotColor(length(unique(jitter$Grp))),
                      guide = "none")+
    scale_color_manual(values = GMTPlotColor(length(unique(jitter$Grp))),
                       label = LegendLabel) +
    theme(legend.title = element_blank(),
          panel.grid = element_blank(),
          text = element_text(family = "WRYH",
                              face = "bold",
                              color = "black",
                              size = 12),
          axis.text = element_text(family = "ArialUnicode",
                                   face = "bold",
                                   color = "black",
                                   size = 12),
          line = element_line(color = "black"),
          plot.margin = unit(c(2,0,1,0.5),"lines")
    )+
    coord_cartesian(ylim = c(1, 100000),
                    clip = "off")

  p <- ggsave(paste0(FigureName,".png"), width = 7, height = 3.5)
  return(p)
}



