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
                   FigureName = "kmplot"){

anadata <- datain %>%
  mutate(Trtan = GrpVar,
         Aval = AVALVar,
         Cnsr = CNSRVar)

GrpLabelData <- data.frame(StratumNum = c(1:length(GrpLabel)),
                           Stratum = GrpLabel)

survdata <- data.frame()
risktable <- data.frame()
MaxAval <- ceiling(max(anadata$Aval))

# browser()

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

  survdata <- bind_rows(survdata, fit1)
  risktable <- bind_rows(risktable, risk2)
}



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
