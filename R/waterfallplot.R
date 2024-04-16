#' Draw waterfall plot
#'
#' @param datain a dataframe or tibble
#' @param BORVar BOR column in dataframe
#' @param CHGVar percent column in Y axis
#' @param PercentLine1 reference line 1
#' @param PercentLine2 reference line 2
#' @param YLabel a character for Y axis label
#' @param colorSet a vector for color
#' @param FigureName a character for figure name
#'
#' @return a figure as ggplot2 object
#' @export
#'
#' @examples
#' waterfallplot(water,
#' BORVar = water$BOR,
#' CHGVar = water$chgx,
#' PercentLine1 = FALSE,
#' PercentLine2 = FALSE,
#' YLabel = "Change from Baseline",
#' colorSet = c("green", "blue", "yellow", "red", "grey"),
#' FigureName = "waterfallplot")
waterfallplot <- function(datain,
                          BORVar,
                          CHGVar,
                          PercentLine1 = FALSE,
                          PercentLine2 = FALSE,
                          YLabel,
                          colorSet = c("green", "blue", "yellow", "red", "grey"),
                          FigureName = "waterfallplot"
                          ){

  colorData <- data.frame(Bor = c("CR", "PR", "SD", "PD", "NE"),
                          colorset = colorSet)

  final <- datain %>%
    arrange(desc(CHGVar)) %>%
    mutate(Xord = row_number(),
           Bor = BORVar) %>%
    left_join(colorData, by = c("Bor"))

p1 <- ggplot(final)+
  geom_col(aes(x = Xord, y =CHGVar, fill = Bor)) +
  geom_hline(yintercept = 0) +
  theme_classic()+
  labs(x = NULL, y = YLabel) +
  scale_y_continuous(breaks=seq(-100,100,by=10), limits = c(-100,100)) +
  scale_fill_manual(breaks = distinct(final, Bor, colorset)$Bor,
                    values = distinct(final, Bor, colorset)$colorset) +
  theme(legend.position = c(0.95, 0.9),
        legend.title = element_blank(),
        legend.background = element_blank(),
        text = element_text(family = "sans",
                            face = "bold",
                            color = "black",
                            size = 10),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_line(linewidth = 0))

if (PercentLine1 != FALSE){
  p1 <- p1 + geom_hline(yintercept = PercentLine1, linetype = 2, alpha = 0.2)
}

if (PercentLine2 != FALSE){
  p1 <- p1 + geom_hline(yintercept = PercentLine2, linetype = 2, alpha = 0.2)
}

ggsave(paste0(FigureName,".png"), width = 7, height = 4)
}
