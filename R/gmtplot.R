#' draw GMT plot
#'
#' @param datain a dataframe or tibble
#' @param GrpVar group by numeric Column
#' @param GrpLabel a vector for group by column label
#' @param AvisitnVar analysis visit numeric column
#' @param AvisintVal analysis visit value
#' @param AvisitLabel a vector for analysis visit label
#' @param Aval analysis value numeric column
#' @param Base baseline column
#' @param YLabel a character for Y axis label
#' @param LegendLabel a vector for Legend label
#' @param colorSet a vector for color
#' @param LineYN whether draw geom_line
#' @param LegendYN whether draw legend
#' @param FigureName a character for figure name
#'
#' @return a figure as ggplot2 object
#' @export
#'
#' @examples
#' gmtplot(adis,"gmt_plot")
#'
gmtplot <- function(datain,
                    GrpVar = TRTAN,
                    GrpLabel,
                    AvisitnVar = AVISITN,
                    AvisintVal,
                    AvisitLabel,
                    Aval = AVAL,
                    Base = BASE,
                    YLabel,
                    LegendLabel,
                    colorSet,
                    LineYN = FALSE,
                    LegendYN = FALSE,
                    FigureName = "gmtplot"){

  avisitdata <- tibble(Avisitn = AvisintVal) %>%
    mutate(Avisitnum = seq(1, by = 2, length.out = length(AvisintVal)),
           AvisitRev = rev(Avisitnum),
           lineheight = 1.9^seq(1, by = 1, length.out = length(AvisintVal)))

  adis1 <- datain %>%
    mutate(Grp = GrpVar,
           Avisitn = AvisitnVar,
           AVAL = Aval,
           BASE = Base)

  YaxisMax <- as.numeric(str_split_fixed(sprintf("%e", max(adis1$AVAL)), "\\+", n=2)[,2])+1
  Xfactor <- case_when(length(AvisintVal) <= 2 ~ 0.2,
                       length(AvisintVal) <= 4 ~ 0.12,
                       length(AvisintVal) <= 6 ~ 0.06)

  adis1 <- left_join(adis1, avisitdata, by = "Avisitn") %>%
    mutate(
           Xp = case_when(Avisitnum <= floor(max(Avisitnum)/2) ~ Grp - Xfactor*(AvisitRev-ceiling(max(Avisitnum)/2)),
                          Avisitnum > max(Avisitnum)/2 ~ Grp + Xfactor*(Avisitnum-ceiling(max(Avisitnum)/2)),
                          TRUE ~ Avisitnum),
           LOGAVAL = log10(AVAL),
           LOGFOLD = log10(AVAL/BASE)
    )

  jitter <- adis1 %>%
    count(Grp, Xp, AVAL)

  LengthGrpAvisit <- length(GrpLabel)*length(AvisitLabel)

  dot_j <- case_when(
    length(GrpLabel) <= 2 & length(AvisitLabel) <= 2 ~ 0.005,
    length(GrpLabel) <= 2 & length(AvisitLabel) <= 4 ~ 0.003,
    length(GrpLabel) <= 4 & length(AvisitLabel) <= 2 ~ 0.01,
    length(GrpLabel) <= 4 & length(AvisitLabel) <= 4 ~ 0.005,
                     )

  adis2 <- left_join(adis1, jitter, by = c("Grp", "Xp", "AVAL")) %>%
    arrange(Grp, Xp, AVAL) %>%
    group_by(Grp, Xp, AVAL) %>%
    mutate(jittern = row_number(),
           jitterDot = case_when(jittern < ceiling(n/2) ~ jittern*(-1),
                                 jittern > ceiling(n/2) ~ jittern-ceiling(n/2),
                                 TRUE ~ 0),
           XpDot = case_when(n <= 8 ~ Xp+jitterDot*dot_j*4,
                             n <= 16 ~ Xp+jitterDot*dot_j*2,
                             TRUE ~ Xp+jitterDot*dot_j))

  mean1 <- adis2 %>%
    group_by(Grp, Xp, Avisitn, Avisitnum, lineheight) %>%
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
           ymin = 10^YaxisMax*1.5
           ) %>%
    ungroup() %>%
    group_by(Grp) %>%
    mutate(xmin = if_else(Avisitnum == 1, NA, min(Xp)),
           xmax = if_else(Avisitnum != 1, Xp, NA),
           ymax = if_else(Avisitnum == 1, 10^YaxisMax*(max(lineheight)/1.4), 10^YaxisMax*(lineheight/1.4))) %>%
    ungroup()

  GrpLabelData <- tibble(Grp = c(unique(jitter$Grp)),
                     GrpLabel = GrpLabel)

  final <- plyr::rbind.fill(adis2, mean1, GrpLabelData)
  # browser()

  sysfonts::font_add(family = "KT", regular = "simkai.ttf")
  showtext::showtext_auto()
  showtext::showtext_opts(dpi = 300)


  GMTPlotColor <- colorRampPalette(colorSet)


  p <- ggplot(final) +
    geom_col(aes(x = Xp, y = means, fill = factor(Grp)), alpha = 0.3, na.rm = TRUE)+
    geom_point(aes(x = XpDot, y = AVAL, color = factor(Grp)), na.rm = TRUE, size = 0.5)+
    geom_errorbar(aes(x = Xp, ymin = yerrl, ymax = yerru))+
    geom_text(aes(x = Xp, y = 10^YaxisMax, label = as.character(round(means))), na.rm = TRUE, family = "sans", fontface = "bold", size = 4)+
    geom_text(aes(x = Xp, y = 0.25, label = nlabel), na.rm = TRUE, family = "sans", fontface = "bold", size = 4)+
    geom_text(aes(x = Grp, y = 0.1, label = GrpLabel), na.rm = TRUE, family = "sans", fontface = "bold", size = 4)+
    geom_text(aes(x = Xp-Xfactor, y = 10^YaxisMax*(lineheight), label = foldlabel), na.rm = TRUE, family = "sans", fontface = "bold", size = 4)+
    geom_linerange(aes(x = Xp, ymin = ymin, ymax = ymax), na.rm = TRUE)+
    geom_linerange(aes(xmin = xmin, xmax = xmax, y = 10^YaxisMax*(lineheight/1.4)))+
    geom_vline(xintercept = unique(jitter$Grp)[-which(unique(jitter$Grp) == max(unique(jitter$Grp)))]+0.5, linetype = "f8", alpha = 0.4)+
    theme_classic()+
    annotation_logticks(sides = "l")+
    scale_y_log10(breaks = 10^(0:7),
                  label = c(expression(bold("10"^"0")),
                            expression(bold("10"^"1")),
                            expression(bold("10"^"2")),
                            expression(bold("10"^"3")),
                            expression(bold("10"^"4")),
                            expression(bold("10"^"5")),
                            expression(bold("10"^"6")),
                            expression(bold("10"^"7"))),
                  expand = c(0, 0))+
    scale_x_continuous(breaks = distinct(jitter,Xp)$Xp,
                       labels = rep(AvisitLabel,length(unique(jitter$Grp))))+
    labs(y = YLabel, x = NULL)+
    scale_fill_manual(values = GMTPlotColor(length(unique(jitter$Grp))),
                      guide = "none")+
    scale_color_manual(values = GMTPlotColor(length(unique(jitter$Grp))),
                       label = LegendLabel) +
    coord_cartesian(ylim = c(1, 10^YaxisMax),
                    clip = "off")

  if (LineYN == TRUE){
    p <- p + geom_line(aes(x = XpDot, y = AVAL, group = USUBJID, color = factor(Grp)), na.rm = TRUE, alpha = 0.7, linetype = 2)
  }

  marginunit <- case_when(
    length(AvisitLabel) <= 2 ~ c(2,0,2,0.5),
    length(AvisitLabel) <= 4 ~ c(3.5,0,1.7,0.5),
  )

  if (LegendYN == FALSE){
    p <- p + theme(legend.position = "none",
                   panel.grid = element_blank(),
                   text = element_text(family = "sans",
                                       face = "bold",
                                       color = "black",
                                       size = 12),
                   axis.text = element_text(family = "sans",
                                            face = "bold",
                                            color = "black",
                                            size = 12),
                   axis.ticks.x = element_blank(),
                   line = element_line(color = "black"),
                   plot.margin = unit(marginunit,"lines"))
  } else {
    p <- p + theme(legend.title = element_blank(),
                   panel.grid = element_blank(),
                   text = element_text(family = "sans",
                                       face = "bold",
                                       color = "black",
                                       size = 12),
                   axis.text = element_text(family = "sans",
                                            face = "bold",
                                            color = "black",
                                            size = 12),
                   axis.ticks.x = element_blank(),
                   line = element_line(color = "black"),
                   plot.margin = unit(marginunit,"lines"))
  }


  fig_width <- case_when(
    LengthGrpAvisit <= 4 ~ 4,
    LengthGrpAvisit <= 8 & LengthGrpAvisit > 4 ~ 7,
    LengthGrpAvisit <= 16 & LengthGrpAvisit > 12 ~ 10.5,
  )

  if (LegendYN == TRUE){
    fig_width <- fig_width + 0.5
  }

  fig_height <- case_when(
    YaxisMax <= 5 ~ 3.5,
    YaxisMax <= 7 ~ 4.2
  )

  myplot <- ggsave(paste0(FigureName,".png"), width = fig_width, height = fig_height)
  return(myplot)
}



