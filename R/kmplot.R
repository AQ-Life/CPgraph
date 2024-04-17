#' Draw KM plot
#'
#' @param datain a dataframe or tibble
#' @param GrpVar group by numeric Column
#' @param GrpLabel a vector for group by column label
#' @param AVALVar analysis time numeric column
#' @param CNSRVar censor numeric column
#' @param ByTime by step in X axis
#' @param XLabel a character for X axis label
#' @param YLabel a character for Y axis label
#' @param RiskLabel a character for risktable label
#' @param colorSet a vector for color
#' @param ShowAreaYN whether draw area plot
#' @param LineMedianYN whether draw reference line when y = 0.5
#' @param PvalYN whether display P value
#' @param FigureName a character for figure name
#'
#' @return a figure as ggplot2 object
#' @export
#'
#' @examples
#' kmplot(datain = adtte,
#' GrpVar = adtte$COHORTN,
#' GrpLabel = c("Group A", "B","C"),
#' AVALVar = adtte$AVAL,
#' CNSRVar = adtte$CNSR,
#' ByTime = 2,
#' XLabel = "XXXX",
#' YLabel = "YYYY",
#' RiskLabel = "Number at risk",
#' colorSet = c("grey", "blue", "red"),
#' FigureName = "kmplot")
#'
kmplot <- function(datain,
                   GrpVar = TRTAN,
                   GrpLabel,
                   AVALVar = AVAL,
                   CNSRVar = CNSR,
                   ByTime,
                   XLabel,
                   YLabel,
                   RiskLabel,
                   colorSet,
                   ShowAreaYN = FALSE,
                   LineMedianYN = FALSE,
                   PvalYN = FALSE,
                   FigureName = "kmplot"){

anadata <- datain %>%
  mutate(Trtan = GrpVar,
         Aval = AVALVar,
         Cnsr = CNSRVar)

GrpLabelData <- data.frame(StratumNum = c(1:length(GrpLabel)),
                           Stratum = GrpLabel)

survdata <- data.frame()
risktable <- data.frame()
Xvline <- c()
MaxAval <- ceiling(max(anadata$Aval))

for (i in 1:length(GrpLabel)) {
  fitdata <- anadata %>%
    filter(Trtan == i)

  fit <- survfit(Surv(Aval, Cnsr==0) ~ Trtan, data = fitdata)

  fit1 <- data.frame(time = fit$time,
                      risk = fit$n.risk,
                      event = fit$n.event,
                      censor = fit$n.censor,
                      surv = fit$surv,
                      upper = fit$upper,
                      lower = fit$lower,
                     StratumNum = i) %>%
    mutate(surv_cnsr = if_else(censor==1,surv,NA))

  risk1 <- summary(fit, times = seq(0,MaxAval,by = ByTime))
  risk2 <- data.frame(time = risk1$time,
                      risk = risk1$n.risk,
                      event = risk1$n.event,
                      censor = risk1$n.censor,
                      surv = risk1$surv,
                      upper = risk1$upper,
                      lower = risk1$lower,
                      StratumNum = i)

  Xvline[i] <- median(fit)

  survdata <- bind_rows(survdata, fit1)
  risktable <- bind_rows(risktable, risk2)
}

Pvalue1 <- survdiff(Surv(Aval, Cnsr==0) ~ Trtan, data = anadata)
Pvalue2 <- data.frame(chisq = Pvalue1$chisq, pvalue = Pvalue1$pvalue)
PvalueText <- PvalueFormat(Pvalue1$pvalue)

XendData <- as.data.frame(Xvline)

dummy <- data.frame()
for (i in seq(0, MaxAval, by = ByTime)) {
  for (j in c(1:length(GrpLabel))) {
    dummy1 <- data.frame(time = i,
                         StratumNum = j)
    dummy <- bind_rows(dummy, dummy1)
  }
}

survdata <- left_join(survdata, GrpLabelData, by = c("StratumNum"))
risktable <- left_join(dummy, risktable, by = c("time", "StratumNum")) %>%
  mutate(risk = if_else(is.na(risk), 0, risk)) %>%
  left_join(GrpLabelData, by = c("StratumNum"))

PlotColor <- colorRampPalette(colorSet)

# browser()
p1 <- ggplot(survdata) +
  geom_step(aes(x = time, y = surv, group = StratumNum, color = factor(StratumNum))) +
  geom_point(aes(x=time, y=surv_cnsr), shape = 3) +
  geom_label(label = "+ Censored",x=MaxAval,y=1, size = 3, hjust = "inward") +
  labs(x=XLabel, y=YLabel) +
  scale_x_continuous(breaks=seq(0,MaxAval,by=ByTime), limits = c(0,MaxAval)) +
  scale_y_continuous(breaks=seq(0,1,by=0.2), limits = c(0,1)) +
  theme_classic()+
  scale_color_manual(values = PlotColor(length(GrpLabel)),
                     labels = GrpLabel)+
  scale_fill_manual(values = PlotColor(length(GrpLabel)))+
  # coord_cartesian( clip = "off") +
  theme(legend.position = c(0.1,0.15),
        # legend.direction = "horizontal",
        legend.title = element_blank(),
        legend.background = element_blank(),
        axis.title.y = element_text(margin = margin(r = -20)),
        axis.text = element_text(color = "black"),
        text = element_text(family = "sans",
                            face = "bold",
                            color = "black",
                            size = 10))

if (ShowAreaYN == TRUE){
p1 <- p1 + geom_ribbon(aes(x = time, ymin = lower, ymax = upper, group = StratumNum, fill = factor(StratumNum)), alpha = 0.2, show.legend = FALSE)
}

if (LineMedianYN == TRUE){
  p1 <- p1 +
    geom_segment(data=XendData, aes(x = max(Xvline, na.rm = TRUE), y = 0.5, xend = -Inf, yend = 0.5), linetype = 2, alpha = 0.2) +
    geom_segment(data=XendData, aes(x = Xvline, y=-Inf, xend = Xvline, yend = 0.5), linetype = 2, alpha = 0.2)
}

if (PvalYN == TRUE){
  p1 <- p1 + geom_text(x = MaxAval, y = 0.9, label = paste0("P = ",PvalueText), size = 3, hjust = "inward")
}

p2 <- ggplot(risktable) +
  geom_text(aes(x = time, y = -StratumNum, label = risk, color = factor(StratumNum)),
            size = 2.5)+
  theme_classic()+
  scale_x_continuous(breaks=seq(0,MaxAval,by=ByTime), limits = c(0,MaxAval)) +
  scale_y_continuous(breaks=-seq(1:length(GrpLabel)),
                     expand = c(0.1, 0.1),
                     labels = GrpLabel) +
  scale_color_manual(values = PlotColor(length(GrpLabel)))+
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.border = element_blank(),
        axis.line = element_line(linewidth = 0),
        plot.title = element_text(family = "sans",
                                  face = "bold",
                                  color = "black",
                                  size = 10),
        text = element_text(family = "sans",
                            face = "bold",
                            color = "black",
                            size = 10),
        axis.text.x = element_blank(), axis.ticks = element_blank(),
        axis.text.y = element_text(hjust = 1, color = "black"))+
  labs(x = NULL, y = NULL, title = RiskLabel)

plot_grid(p1, p2, ncol = 1, align = "v", rel_heights = c(1.2, 0.3))
ggsave(paste0("kmplot",".png"), width = 7, height = 4)

}
