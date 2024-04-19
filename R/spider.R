#' Draw spiderplot
#'
#' @param datain a dataframe or tibble
#' @param GrpSubjVar group by column for each line
#' @param XaxisVar X axis column
#' @param YaxisVar Y axis column
#' @param GrpVar group by Column for line or point
#' @param YLine1 1st reference line in Y axis
#' @param Yline2 2nd reference line in Y axis
#' @param colorSet a vector for color
#' @param XLabel a character for X axis label
#' @param YLabel a character for Y axis label
#' @param FigureName a character for figure name
#'
#' @return a figure as ggplot2 object
#' @export
#'
#' @examples
#' spiderplot(datain = spiderdata,
#' GrpSubjVar = spiderdata$Subject,
#' XaxisVar = spiderdata$durx,
#' YaxisVar = spiderdata$chgx,
#' GrpVar = spiderdata$BOR,
#' YLine1 = 20,
#' Yline2 = -30,
#' colorSet = c("blue", "red", "green", "black", "yellow"),
#' XLabel = "Time Since Treatment Initiation (months)",
#' YLabel = "Tumor Change From Baseline",
#' FigureName = "spiderplot")
spiderplot <- function(datain,
                       GrpSubjVar,
                       XaxisVar,
                       YaxisVar,
                       GrpVar,
                       YLine1 = 20,
                       Yline2 = -30,
                       colorSet = c("blue", "red", "green", "black", "yellow"),
                       XLabel,
                       YLabel,
                       FigureName = "spiderplot"
                       ){
final <- datain %>%
  mutate(GrpSubj = GrpSubjVar,
         XVar = XaxisVar,
         YVar = YaxisVar,
         GroupCol = GrpVar)

ggplot(final, aes(group = GrpSubj)) +
  geom_point(aes(x = XVar, y = YVar, color = GroupCol)) +
  geom_line(aes(x = XVar, y = YVar, color = GroupCol)) +
  # geom_segment(aes(x = dura, xend = dura + 1, y = chga, yend = chga),
  #              arrow = arrow(length = unit(0.2, "cm")), color = "brown") +
  geom_hline(yintercept = YLine1, linetype = "dashed") +
  geom_hline(yintercept = Yline2, linetype = "dashed") +
  theme_classic()+
  labs(y = YLabel, x = XLabel)+
  scale_color_manual(breaks = c("CR", "PR", "SD", "PD", "NE"),
                     values = colorSet) +
  scale_y_continuous(breaks = seq(-100, 100, by = 20)) +
  # scale_x_continuous(breaks = seq(0,18, by = 3)) +
  theme(legend.position = c(0.9, 0.9),
        legend.margin = margin(t = 0, l = 0, b = 0, r = 0),
        legend.title = element_blank(),
        legend.spacing = unit(0.001,"cm"),
        # panel.grid.major.x = element_line(),
        text = element_text(family = "sans",
                            face = "bold",
                            color = "black",
                            size = 12),
        # plot.margin = unit(c(0,80,5,5),"pt")
  ) +
  coord_cartesian(ylim = c(-100, 100))

ggsave(paste0(FigureName,".png"), width = 7, height = 4)
}
