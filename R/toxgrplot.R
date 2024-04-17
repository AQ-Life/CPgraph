#' Draw AETOXGR plot
#'
#' @param dataADSL a dataframe or tibble for population
#' @param dataADAE a dataframe or tibble for events
#' @param GrpVar group by numeric Column for events
#' @param GrpADSLVar group by numeric Column for population
#' @param GrpLabel a vector for group by column label
#' @param ATOXGRNVar AETOXGR numeric column
#' @param AEDECODVar AEDECOD coding
#' @param Ylabel a character for Y axis label
#' @param LegendLabel a vector for legend label
#' @param colorSet a vector for color
#' @param FigureName a character for figure name
#'
#' @return a figure as ggplot2 object
#' @export
#'
#' @examples
#' toxgrplot(dataADSL = adsl,dataADAE = adae,GrpVar = TRTAN,GrpADSLVar = TRT01AN,GrpLabel = c("V01A", "V01B"),ATOXGRN = ATOXGRN,AEDECOD = AEDECOD,Ylabel = c("不良事件发生率（%）"),colorSet = c("red", "blue", "grey"), FigureName = "aetox")
toxgrplot <- function(dataADSL,
                        dataADAE,
                        GrpVar = TRTAN,
                        GrpADSLVar = TRT01AN,
                        GrpLabel,
                        ATOXGRNVar = ATOXGRN,
                        AEDECODVar = AEDECOD,
                        Ylabel = c("不良事件发生率（%）"),
                        LegendLabel = c("一级", "二级", "三级及以上"),
                        colorSet = c("grey", "blue", "red"),
                        FigureName = "aetoxgrplot"){

sysfonts::font_add(family = "KT", regular = "simkai.ttf")
showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)



bign <- bind_rows(mutate(dataADSL, Trtan = GrpADSLVar), mutate(dataADSL, Trtan = GrpADSLVar, Trtan = 9))%>%
  count(Trtan) %>%
  rename(bign = n)

freq1 <- bind_rows(mutate(dataADAE, Trtan = GrpVar,
                          AEDECOD = AEDECODVar,
                          ATOXGRN = ATOXGRNVar),
                   mutate(dataADAE, Trtan = GrpVar, Trtan = 9,
                          AEDECOD = AEDECODVar,
                          ATOXGRN = ATOXGRNVar)) %>%
  mutate(ATOXGRN, if_else(ATOXGRN > 3, 3, ATOXGRN),
         # AEDECOD = AEDECODVar,ATOXGRN = ATOXGRNVar
         ) %>%
  arrange(USUBJID, Trtan, AEDECOD, desc(ATOXGRN)) %>%
  distinct(USUBJID, Trtan, AEDECOD, .keep_all = TRUE) %>%
  count(Trtan, AEDECOD, ATOXGRN) %>%
  left_join(bign, by = c("Trtan")) %>%
  mutate(pct = 100*n/bign)

freqord <- freq1 %>%
  filter(Trtan == 9) %>%
  group_by(Trtan, AEDECOD) %>%
  mutate(freqn = sum(n)) %>%
  distinct(Trtan, AEDECOD, freqn) %>%
  arrange(Trtan, desc(freqn)) %>%
  group_by(Trtan) %>%
  mutate(ord = row_number()) %>%
  ungroup() %>%
  select(AEDECOD, ord)

freq2 <- left_join(freq1, freqord, by = c("AEDECOD"))

dummy <- data.frame()
for (i in seq(1,length(GrpLabel), by = 1)) {
  for (j in c(1,2,3)) {
    dummy1 <- freqord %>%
      mutate(Trtan = i,
             ATOXGRN = j)
    dummy <- bind_rows(dummy, dummy1)
  }
}

freq3 <- left_join(dummy, freq2, by = c("Trtan", "AEDECOD", "ord", "ATOXGRN")) %>%
  mutate(pct = if_else(is.na(pct), 0, pct),
         order = ord + (Trtan - 1)*(max(freqord$ord)+1))

freq3_missing <- freq3 %>%
  group_by(Trtan) %>%
  filter(row_number()==1) %>%
  ungroup() %>%
  filter(Trtan != min(Trtan)) %>%
  mutate(order = order - 1,
         pct = NA,
         AEDECOD = "")

AnnoPos <- freq3 %>%
  distinct(Trtan, ord, order) %>%
  filter(ord == floor(max(freqord$ord)/2)+1) %>%
  left_join(bign, by = c("Trtan")) %>%
  mutate(GrpLabel = GrpLabel,
         nlabel = str_c("(N = ",as.character(bign),")"))

final <- bind_rows(freq3, freq3_missing)


# browser()

colorData <- data.frame(colorvalue = colorSet,
                        Legendvalue = LegendLabel)

fig_width <- case_when(
  max(final$order) <= 9 ~ 3.5,
  max(final$order) <= 27 ~ 7,
  max(final$order) <= 36 ~ 10
)

legendPos <- case_when(
  fig_width <= 3.5 ~ c(0.2,0.95),
  fig_width <= 7 ~ c(0.09,0.95),
  fig_width <= 10 ~ c(0.06,0.95)
)

p <- ggplot() +
  geom_bar(data=final, aes(x = factor(order), y = pct, fill = factor(ATOXGRN)),
           stat = "identity",
           position = "stack") +
  geom_text(data=AnnoPos, aes(x = order, y = 80, label = GrpLabel), na.rm = TRUE, family = "sans", fontface = "bold", size = 4)+
  geom_text(data=AnnoPos, aes(x = order, y = 75, label = nlabel), na.rm = TRUE, family = "sans", fontface = "bold", size = 4)+
  theme_classic()+
  scale_y_continuous(breaks = seq(0, 100, by = 10),
                     expand = c(0, 0))+
  scale_x_discrete(labels = arrange(distinct(final,AEDECOD, order), order)$AEDECOD)+
  labs(y = Ylabel, x = NULL) +
  scale_fill_manual(values = colorData$colorvalue,
                    label = colorData$Legendvalue)+
  coord_cartesian(ylim = c(0, 100),
                  clip = "off") +
  theme(legend.title = element_blank(),
        legend.position = legendPos,
        legend.margin = margin(t = 0, l = 0, b = 0, r = 0),
        legend.key.size = unit(0.3,"cm"),
          panel.grid = element_blank(),
          text = element_text(family = "KT",
                              face = "bold",
                              color = "black",
                              size = 12),
        axis.title.y = element_text(color = "blue", face = "bold"),
          axis.text.y = element_text(family = "sans",
                                   face = "bold",
                                   color = "black",
                                   size = 12),
          axis.ticks.x = element_blank(),
        )

if (length(GrpLabel) >=2){
  p <- p +
    # max(freqord$ord) + 1
    geom_vline(xintercept = freq3_missing$order, linetype = "f8", alpha = 0.4)
}

if(max(final$order) <= 9){
  p <- p +
    theme(axis.text.x = element_text(face = "bold"))
} else if(max(final$order) <= 27){
  p <- p +
    theme(axis.text.x = element_text(angle = 45, face = "bold", hjust = 0.4, vjust = 0.5))
} else if(max(final$order) <= 36){
  p <- p +
    theme(axis.text.x = element_text(angle = 90, face = "bold", hjust = 1, vjust = 0.5))
}

ggsave(paste0(FigureName,".png"), width = fig_width, height = 3.5)
}

