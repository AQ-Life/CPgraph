#' Draw swimmer plot
#'
#' @param datain a dataframe or tibble
#' @param YaxisVar Y axis column
#' @param BarVar barplot column
#' @param BarColor barplot color
#' @param BarWidth bar width
#' @param BarLabel bar label in legend
#' @param BarLabelY position of bar label in Y axis
#' @param Line1VarMin 1st line X axis min column
#' @param Line1VarMax 1st line X axis max column
#' @param Line1color 1st line color
#' @param Line1Width 1st line width
#' @param Line1Label 1st line label
#' @param Line1LabelY position of 1st line label in Y axis
#' @param Line2VarMin 2nd line X axis min column
#' @param Line2VarMax 2nd line X axis max column
#' @param Line2color 2nd line color
#' @param Line2Width 2nd line width
#' @param Line2Label 2nd line label
#' @param Line2LabelY position of 2nd line label in Y axis
#' @param Line3VarMin 3rd line X axis min column
#' @param Line3VarMax 3rd line X axis max column
#' @param Line3color 3rd line color
#' @param Line3Width 3rd line width
#' @param Line3Label 3rd line label
#' @param Line3LabelY position of 3rd line label in Y axis
#' @param Point1Var 1st point column
#' @param Point1Color 1st point color
#' @param Point1Shape 1st point shape
#' @param Point1Label 1st point label
#' @param Point1LabelY 1st point label position in Y axis
#' @param Point2Var 2nd point column
#' @param Point2Color 2nd point color
#' @param Point2Shape 2nd point shape
#' @param Point2Label 2nd point label
#' @param Point2LabelY 2nd point label position in Y axis
#' @param Point3Var 3rd point column
#' @param Point3Color 3rd point color
#' @param Point3Shape 3rd point shape
#' @param Point3Label 3rd point label
#' @param Point3LabelY 3rd point label position in Y axis
#' @param Point4Var 4th point column
#' @param Point4Color 4th point color
#' @param Point4Shape 4th point shape
#' @param Point4Label 4th point label
#' @param Point4LabelY 4th point label position in Y axis
#' @param Point5Var 5th point column
#' @param Point5Color 5th point color
#' @param Point5Shape 5th point shape
#' @param Point5Label 5th point label
#' @param Point5LabelY 5th point label position in Y axis
#' @param Point6Var 6th point column
#' @param Point6Color 6th point color
#' @param Point6Shape 6th point shape
#' @param Point6Label 6th point label
#' @param Point6LabelY 6th point label position in Y axis
#' @param Point7Var 7th point column
#' @param Point7Color 7th point color
#' @param Point7Shape 7th point shape
#' @param Point7Label 7th point label
#' @param Point7LabelY 7th point label position in Y axis
#' @param Point8Var 8th point column
#' @param Point8Color 8th point color
#' @param Point8Shape 8th point shape
#' @param Point8Label 8th point label
#' @param Point8LabelY 8th point label position in Y axis
#' @param Segment1Var 1st segment column
#' @param Segment1Color 1st segment color
#' @param Segment1Label 1st segment label
#' @param Segment1LabelY 1st segment label position in Y axis
#' @param XaxisLabel X axis label
#' @param YaxisLabel Y axis label
#' @param figwidth figure width
#' @param figheight figure height
#' @param FigureName figure name
#'
#' @return a figure as ggplot2 object
#' @export
#'
#' @examples
#' swimmerplot(datain = swimmerdata,
#' YaxisVar = swimmerdata$SUBJID,
#' BarVar = swimmerdata$trtedy,
#' BarColor = "grey",
#' BarWidth = 0.01,
#' BarLabel = "研究持续时间",
#' BarLabelY = 17,
#' Line1VarMin = swimmerdata$startdr,
#' Line1VarMax = swimmerdata$enddy,
#' Line1color = "blue",
#' Line1Width = 2,
#' Line1Label = "缓解持续时间",
#' Line1LabelY = 15,
#' Point1Var = swimmerdata$pddy,
#' Point1Color = "black",
#' Point1Shape = 15,
#' Point1Label = "疾病进展",
#' Point1LabelY = 13,
#' Point2Var = swimmerdata$dthdy,
#' Point2Color = "red",
#' Point2Shape = 16,
#' Point2Label = "死亡",
#' Point2LabelY = 11,
#' Segment1Var = swimmerdata$exongo,
#' Segment1Color = "brown",
#' Segment1Label = "治疗持续",
#' Segment1LabelY = 9,
#' XaxisLabel = "相对首次给药时间月",
#' YaxisLabel = "受试者编号",
#' figwidth = 9,
#' figheight = 5,
#' FigureName = "swimmerplot")
swimmerplot <- function(datain,
                        YaxisVar,
                        BarVar = NULL,
                        BarColor = NULL,
                        BarWidth = NULL,
                        BarLabel = NULL,
                        BarLabelY = NULL,
                        Line1VarMin = NULL,
                        Line1VarMax = NULL,
                        Line1color = NULL,
                        Line1Width = NULL,
                        Line1Label = NULL,
                        Line1LabelY = NULL,
                        Line2VarMin = NULL,
                        Line2VarMax = NULL,
                        Line2color = NULL,
                        Line2Width = NULL,
                        Line2Label = NULL,
                        Line2LabelY = NULL,
                        Line3VarMin = NULL,
                        Line3VarMax = NULL,
                        Line3color = NULL,
                        Line3Width = NULL,
                        Line3Label = NULL,
                        Line3LabelY = NULL,
                        Point1Var = NULL,
                        Point1Color = NULL,
                        Point1Shape = NULL,
                        Point1Label = NULL,
                        Point1LabelY = NULL,
                        Point2Var = NULL,
                        Point2Color = NULL,
                        Point2Shape = NULL,
                        Point2Label = NULL,
                        Point2LabelY = NULL,
                        Point3Var = NULL,
                        Point3Color = NULL,
                        Point3Shape = NULL,
                        Point3Label = NULL,
                        Point3LabelY = NULL,
                        Point4Var = NULL,
                        Point4Color = NULL,
                        Point4Shape = NULL,
                        Point4Label = NULL,
                        Point4LabelY = NULL,
                        Point5Var = NULL,
                        Point5Color = NULL,
                        Point5Shape = NULL,
                        Point5Label = NULL,
                        Point5LabelY = NULL,
                        Point6Var = NULL,
                        Point6Color = NULL,
                        Point6Shape = NULL,
                        Point6Label = NULL,
                        Point6LabelY = NULL,
                        Point7Var = NULL,
                        Point7Color = NULL,
                        Point7Shape = NULL,
                        Point7Label = NULL,
                        Point7LabelY = NULL,
                        Point8Var = NULL,
                        Point8Color = NULL,
                        Point8Shape = NULL,
                        Point8Label = NULL,
                        Point8LabelY = NULL,
                        Segment1Var = NULL,
                        Segment1Color = NULL,
                        Segment1Label = NULL,
                        Segment1LabelY = NULL,
                        XaxisLabel,
                        YaxisLabel,
                        figwidth,
                        figheight,
                        FigureName = "swimmerplot"
                        ){

final <- datain %>%
  mutate(Yvar = YaxisVar,
         Subj = factor(Yvar, levels = rev(Yvar)),
         xbar = BarVar,
         xline1max = Line1VarMax,
         xline2max = Line2VarMax,
         xline3max = Line3VarMax,
         xpoint1 = Point1Var,
         xpoint2 = Point2Var,
         xpoint3 = Point3Var,
         xpoint4 = Point4Var,
         xpoint5 = Point5Var,
         xpoint6 = Point6Var,
         xpoint7 = Point7Var,
         xpoint8 = Point8Var,
         )

sysfonts::font_add(family = "KT", regular = "simkai.ttf")
showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)

XMax <- ceiling(max(BarVar, Line1VarMax, Line2VarMax, Line3VarMax,
                    Point1Var, Point2Var, Point3Var, Point4Var, Point5Var, Point6Var, Point7Var, Point8Var,
                    na.rm = TRUE))

YMax <- count(final)$n

ggplot(final, aes(y = Subj)) +
  geom_col(aes(x = xbar), color = BarColor, linewidth = BarWidth, alpha = 0.5) +
  geom_linerange(aes(xmin = Line1VarMin, xmax = Line1VarMax), color = Line1color, size = Line1Width, alpha = 0.5) +
  geom_point(aes(x = Point1Var), color = Point1Color, shape = Point1Shape, size = 2) +
  geom_point(aes(x = Point2Var), color = Point2Color, shape = Point2Shape, size = 2) +
  geom_segment(aes(x = Segment1Var, xend = Segment1Var + 0.3, yend = Subj),
               color = Segment1Color, arrow = arrow(type = "open", length = unit(0.08, "in"))) +
  theme_classic()+

  annotate("segment", x = XMax +1, xend = XMax +2, y = BarLabelY, yend = BarLabelY, color = BarColor, size = 2, alpha = 0.5) +
  annotate("text", x = XMax+3, y = BarLabelY, label = BarLabel, hjust = 0, family = "KT") +

  annotate("segment", x = XMax +1, xend = XMax +2, y = Line1LabelY, yend = Line1LabelY, color = Line1color, size = 2, alpha = 0.5) +
  annotate("text", x = XMax+3, y = Line1LabelY, label = Line1Label, hjust = 0, family = "KT") +

  annotate("point", x = XMax +1.5, y = Point1LabelY, color = Point1Color, size = 2, shape = Point1Shape) +
  annotate("text", x = XMax+3, y = Point1LabelY, label = Point1Label, hjust = 0, family = "KT") +

  annotate("point", x = XMax +1.5, y = Point2LabelY, color = Point2Color, size = 2, shape = Point2Shape) +
  annotate("text", x = XMax+3, y = Point2LabelY, label = Point2Label, hjust = 0, family = "KT") +

  annotate("segment", x = XMax +1.5, xend = XMax +1.5 +0.3, y = Segment1LabelY, yend = Segment1LabelY, color = Segment1Color, arrow = arrow(type = "open", length = unit(0.08, "in"))) +
  annotate("text", x = XMax+3, y = Segment1LabelY, label = Segment1Label, hjust = 0, family = "KT") +

  scale_y_discrete() +
  labs(y = YaxisLabel, x = XaxisLabel)+
  coord_cartesian(clip = "off")+
  theme(legend.position = "right",
        legend.margin = margin(t = 0, l = 0, b = 0, r = 0),
        legend.title = element_blank(),
        legend.spacing = unit(0.001,"cm"),
        # panel.grid.major.x = element_line(),
        text = element_text(family = "KT",
                            face = "bold",
                            color = "black",
                            size = 12),
        plot.margin = unit(c(0,80,5,5),"pt")
  )

ggsave(paste0(FigureName,".png"), width = figwidth, height = figheight)
}
