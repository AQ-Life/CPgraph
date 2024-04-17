#' draw forest plot
#'
#' @param datain a dataframe or tibble
#' @param XVar X axis column
#' @param XLowVar lower limit of X axis column
#' @param XUppVar upper limit of X axis column
#' @param YOrderVar Y axis column (numeric: from 1 to N)
#' @param XLine reference line in X axis
#' @param YRefVar add background color in Y axis column
#' @param YRefWidth background color width
#' @param Anno1Var first annotation column
#' @param Anno1VarX X axis of first annotation column
#' @param Anno1VarLabel label of first annotation column
#' @param Anno2Var 2nd annotation column
#' @param Anno2VarX X axis of 2nd annotation column
#' @param Anno2VarLabel label of 2nd annotation column
#' @param Anno3Var 3rd annotation column
#' @param Anno3VarX X axis of 3rd annotation column
#' @param Anno3VarLabel label of 3rd annotation column
#' @param Anno4Var 4th annotation column
#' @param Anno4VarX X axis of 4th annotation column
#' @param Anno4VarLabel label of 4th annotation column
#' @param Anno5Var 5th annotation column
#' @param Anno5VarX X axis of 5th annotation column
#' @param Anno5VarLabel label of 5th annotation column
#' @param Anno6Var 6th annotation column
#' @param Anno6VarX X axis of 6th annotation column
#' @param Anno6VarLabel label of 6th annotation column
#' @param AnnoLineLabel label of reference line annotation column
#' @param XaxisValue axis value displayed in X axis
#' @param XaxisLabel axis label displayed in X axis
#' @param XaxisType X axis type
#' @param leftmargin reserve area in the left of figure
#' @param rightmargin reserve area in the right of figure
#' @param figwidth figure width
#' @param figheight figure height
#' @param FigureName figure name
#'
#' @return a figure as ggplot2 object
#' @export
#'
#' @examples
#' forestplot(
#' datain = final,
#' XVar = final$means,
#' XLowVar = final$yerrl,
#' XUppVar = final$yerru,
#' YOrderVar = final$yord,
#' XLine = 1,
#' YRefVar = final$ref,
#' YRefWidth = 4.9,
#' Anno1Var = final$col1,
#' Anno1VarX = 10^-3,
#' Anno1VarLabel = "D14 Subgroup",
#' Anno2Var = final$col2,
#' Anno2VarX = 3*10^-2,
#' Anno2VarLabel = "V01E GMT",
#' Anno3Var = final$col3,
#' Anno3VarX = 10^-1,
#' Anno3VarLabel = "V01E-2 GMT",
#' Anno4Var = final$col5,
#' Anno4VarX = 4*10^1,
#' Anno4VarLabel = "LS GMR 95% CI",
#' Anno5Var = NULL,
#' Anno5VarX = NULL,
#' Anno5VarLabel = NULL,
#' Anno6Var = NULL,
#' Anno6VarX = NULL,
#' Anno6VarLabel = NULL,
#' AnnoLineLabel = "LS GMR 95% CI",
#' XaxisValue = c(0.1, 1, 10),
#' XaxisLabel = c("<0.1", "1", ">=10"),
#' XaxisType = "log",
#' leftmargin = 9.5,
#' rightmargin = 4.5,
#' figwidth = 9,
#' figheight = 4,
#' FigureName = "forestplot"
#' )
forestplot <- function(datain,
                       XVar,
                       XLowVar,
                       XUppVar,
                       YOrderVar,
                       XLine,
                       YRefVar,
                       YRefWidth,
                       Anno1Var = NULL,
                       Anno1VarX = NULL,
                       Anno1VarLabel = NULL,
                       Anno2Var = NULL,
                       Anno2VarX = NULL,
                       Anno2VarLabel = NULL,
                       Anno3Var = NULL,
                       Anno3VarX = NULL,
                       Anno3VarLabel = NULL,
                       Anno4Var = NULL,
                       Anno4VarX = NULL,
                       Anno4VarLabel = NULL,
                       Anno5Var = NULL,
                       Anno5VarX = NULL,
                       Anno5VarLabel = NULL,
                       Anno6Var = NULL,
                       Anno6VarX = NULL,
                       Anno6VarLabel = NULL,
                       AnnoLineLabel,
                       XaxisValue,
                       XaxisLabel,
                       XaxisType,
                       leftmargin,
                       rightmargin,
                       figwidth,
                       figheight,
                       FigureName = "forestplot"){

  final <- datain %>%
    mutate(xv = XVar,
           xlow = XLowVar,
           xupp = XUppVar,
           ref = YRefVar,
           yv = YOrderVar,
           col1var = Anno1Var,
           col2var = Anno2Var,
           col3var = Anno3Var,
           col4var = Anno4Var,
           col5var = Anno5Var,
           col6var = Anno6Var
           )

  maxX <- max(final$yv)

  if (XaxisType == "line"){
    colorfactor <- 1.3
  } else if (XaxisType == "log"){
    colorfactor <- 2
  }

  XminLine <- min(Anno1VarX, Anno2VarX, Anno3VarX, Anno4VarX, Anno5VarX, Anno6VarX)
  XmaxLine <- max(Anno1VarX, Anno2VarX, Anno3VarX, Anno4VarX, Anno5VarX, Anno6VarX)
  # browser()
p <- ggplot(final) +
  geom_point(aes(x = xv, y = yv)) +
  geom_errorbar(aes(xmin = xlow, xmax = xupp, y = yv)) +
  geom_linerange(aes(x = XLine, ymin = 1, ymax = Inf), color = "grey", alpha = 0.01)+
  geom_linerange(aes(xmin = XminLine, xmax = colorfactor*XmaxLine, y = ref), alpha = 0.1, linewidth = YRefWidth)+
  geom_text(aes(x = XLine, y = 0, label = AnnoLineLabel), fontface = "bold")+
  theme_classic()+
  labs(y = NULL, x = NULL)+
  scale_y_reverse(breaks = seq(1,maxX, by = 1))+
  coord_cartesian(xlim = c(min(XaxisValue), max(XaxisValue)),
                  clip = "off")+
  theme(legend.position = "none",
        panel.grid = element_blank(),
        text = element_text(family = "sans",
                            face = "bold",
                            color = "black",
                            size = 12),
        axis.text.y = element_blank(),
        axis.text.x = element_text(vjust = -4),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.margin = unit(c(0,rightmargin,0.5,leftmargin),"cm"))

if (XaxisType == "log"){
  p <- p +
    scale_x_log10(breaks = XaxisValue,
                  labels = XaxisLabel,
                  expand = c(0, 0)) +
    annotation_logticks(sides = "b",
                        outside = TRUE)
} else if (XaxisType == "line"){
  p <- p +
    scale_x_continuous(breaks = XaxisValue,
                       labels = XaxisLabel,
                       expand = c(0, 0)) +
    annotation_ticks(sides = "b",
                     outside = TRUE)
}


if (length(Anno1Var)){
  p <- p + geom_text(aes(x = Anno1VarX, y = yord, label = col1var), hjust = 0)+
    geom_text(aes(x = Anno1VarX, y = 0, label = Anno1VarLabel), hjust = 0, fontface = "bold")
}
if (length(Anno2Var)){
  p <- p + geom_text(aes(x = Anno2VarX, y = yord, label = col2var))+
    geom_text(aes(x = Anno2VarX, y = 0, label = Anno2VarLabel), fontface = "bold")
}
if (length(Anno3Var)){
  p <- p + geom_text(aes(x = Anno3VarX, y = yord, label = col3var))+
    geom_text(aes(x = Anno3VarX, y = 0, label = Anno3VarLabel), fontface = "bold")
}
if (length(Anno4Var)){
  p <- p + geom_text(aes(x = Anno4VarX, y = yord, label = col4var))+
    geom_text(aes(x = Anno4VarX, y = 0, label = Anno4VarLabel), fontface = "bold")
}
if (length(Anno5Var)){
  p <- p + geom_text(aes(x = Anno5VarX, y = yord, label = col5var), hjust = 0)+
    geom_text(aes(x = Anno5VarX, y = 0, label = Anno5VarLabel), hjust = 0, fontface = "bold")
}
if (length(Anno6Var)){
  p <- p + geom_text(aes(x = Anno6VarX, y = yord, label = col6var), hjust = 0)+
    geom_text(aes(x = Anno6VarX, y = 0, label = Anno6VarLabel), hjust = 0, fontface = "bold")
}


ggsave(paste0(FigureName,".png"), width = figwidth, height = figheight)
}
