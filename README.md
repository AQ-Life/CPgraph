
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gmtplot

<!-- badges: start -->
<!-- badges: end -->

The goal of this package is to draw general clinical trials graphs.

“gmtplot” function is to draw GMT (Geometric Mean Titer) plot in
immunogenicity in vaccine clinical trials. Please refer to [Generation
of Geometric Mean Titer Plot in Immunogenicity from SAS and
R](https://www.lexjansen.com/pharmasug-cn/2023/CC/Pharmasug-China-2023-CC115.pdf)
from PharmaSUG.

“gmtplot_line” function is similar to “gmtplot” function. But if the
analysis value is linear, “gmtplot_line” will be used; if the analysis
value is exponential, “gmtplot” function will be used.

“aetoxgrplot” function is to draw adverse events toxicity grade plot in
vaccine clinical trials.

## Installation

You can install the development version of gmtplot from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("AQ-Life/gmtplot")
```

## Arguments

### gmtplot

| Function Name | Arguments   | Fucntion (Requirement)                                  | Default   |
|---------------|-------------|---------------------------------------------------------|-----------|
| gmtplot       | datain      | 输入数据集（需包含USUBJID, TRTAN, AVISITN, AVAL, BASE） |           |
| gmtplot       | GrpVar      | 分组变量                                                | TRTAN     |
| gmtplot       | GrpLabel    | 分组变量展示的标签                                      |           |
| gmtplot       | AvisitnVar  | 分析访视变量（子分组变量）                              | AVISITN   |
| gmtplot       | AvisintVal  | 需要纳入的分析访视数值型结果                            |           |
| gmtplot       | AvisitLabel | 需要纳入的分析访视的标签                                |           |
| gmtplot       | Aval        | 分析值（数值型）                                        | AVAL      |
| gmtplot       | Base        | 基线值（数值型）                                        | BASE      |
| gmtplot       | YLabel      | Y轴标签                                                 |           |
| gmtplot       | LegendLabel | 分组变量的图例标签                                      |           |
| gmtplot       | colorSet    | 颜色设置（根据分组数量设置多个颜色）                    |           |
| gmtplot       | LineYN      | 是否绘制折线图                                          | FALSE     |
| gmtplot       | LegendYN    | 是否显示图例                                            | FALSE     |
| gmtplot       | FigureName  | 输出图片名称                                            | “gmtplot” |

### gmtplot_line

| Function Name | Arguments   | Fucntion (Requirement)                                  | Default   |
|---------------|-------------|---------------------------------------------------------|-----------|
| gmtplot_line  | datain      | 输入数据集（需包含USUBJID, TRTAN, AVISITN, AVAL, BASE） |           |
| gmtplot_line  | GrpVar      | 分组变量                                                | TRTAN     |
| gmtplot_line  | GrpLabel    | 分组变量展示的标签                                      |           |
| gmtplot_line  | AvisitnVar  | 分析访视变量（子分组变量）                              | AVISITN   |
| gmtplot_line  | AvisintVal  | 需要纳入的分析访视数值型结果                            |           |
| gmtplot_line  | AvisitLabel | 需要纳入的分析访视的标签                                |           |
| gmtplot_line  | Aval        | 分析值（数值型）                                        | AVAL      |
| gmtplot_line  | Base        | 基线值（数值型）                                        | BASE      |
| gmtplot_line  | YLabel      | Y轴标签                                                 |           |
| gmtplot_line  | LegendLabel | 分组变量的图例标签                                      |           |
| gmtplot_line  | colorSet    | 颜色设置（根据分组数量设置多个颜色）                    |           |
| gmtplot_line  | LineYN      | 是否绘制折线图                                          | FALSE     |
| gmtplot_line  | LegendYN    | 是否显示图例                                            | FALSE     |
| gmtplot_line  | FigureName  | 输出图片名称                                            | “gmtplot” |

### aetoxgrplot

| Function Name | Arguments  | Fucntion (Requirement)                       | Default                  |
|---------------|------------|----------------------------------------------|--------------------------|
| aetoxgrplot   | dataADSL   | 输入分析人群的数据集（ADSL）                 |                          |
| aetoxgrplot   | dataADAE   | 输入分析事件的数据集（ADAE）                 |                          |
| aetoxgrplot   | GrpVar     | 分组变量                                     | TRTAN                    |
| aetoxgrplot   | GrpADSLVar | 分析人群数据集的分组变量                     | TRT01AN                  |
| aetoxgrplot   | GrpLabel   | 分组变量展示的标签                           |                          |
| aetoxgrplot   | ATOXGRN    | 分析访视变量（子分组变量）                   | ATOXGRN                  |
| aetoxgrplot   | AEDECOD    | 需要纳入的分析访视数值型结果                 | AEDECOD                  |
| aetoxgrplot   | Ylabel     | Y轴标签                                      | c(“不良事件发生率（%）”) |
| aetoxgrplot   | colorSet   | 颜色设置（根据严重程度等级数量设置多个颜色） | c(“red”, “blue”, “grey”) |
| aetoxgrplot   | FigureName | 输出图片名称                                 | “aetoxgrplot”            |

## Example

This is a basic example which shows you how to solve a common problem:

### gmtplot

``` r
{r example}
library(gmtplot)
library(haven)

adis <- read_sas("adis.sas7bdat")

adis <- adis %>% 
  dplyr::filter(IFASFL == "Y", PARAMCD == "SAR2NAB", ANL01FL == "Y", AVISITN %in% c(0,28,42)) %>% 
  mutate(Gvar = if_else(COHORT == "Cohort 2", TRTAN+2, TRTAN),
         AVISITN = if_else(AVISITN == 42, 28, AVISITN),
         AVISITN = if_else(AVISITN == 150, 120, AVISITN),
         AVISITN = if_else(AVISITN == 178, 148, AVISITN)) %>% 
  filter(Gvar %in% c(1,2,3))

gmtplot(datain = adis,
        GrpVar = adis$Gvar,
        GrpLabel = c("a", "Group B", "C"),
        AvisitnVar = adis$AVISITN,
        AvisintVal = c(0, 28),
        AvisitLabel = c("D0", "D28"),
        Aval = adis$AVAL,
        Base = adis$BASE,
        YLabel = "PRNT50LLL",
        LegendLabel = c("V01E","V01E-2"),
        colorSet = c("grey", "blue", "red"),
        LineYN = FALSE,
        LegendYN = FALSE,
        FigureName = "gmt_plot")
```

### aetoxgrplot

``` r
{r example}
adsl <- read_sas("adsl.sas7bdat") %>%
  filter(SAFFL == "是") %>%
  mutate(TRT01AN = if_else(AGE>=60, TRT01AN+2, TRT01AN))

adae <- read_sas("adae.sas7bdat") %>%
  filter(SAFFL == "是", AECAT == "征集性不良事件", AESCAT == "征集性全身不良事件", COHORTN == 1) %>% 
  mutate(TRTAN = if_else(AGE>=60, TRTAN+2, TRTAN))

aetoxgrplot(dataADSL = adsl,
            dataADAE = adae,
            GrpVar = adae$TRTAN,
            GrpADSLVar = adsl$TRT01AN,
            GrpLabel = c("V01A","B","C"),
            ATOXGRN = adae$ATOXGRN,
            AEDECOD = adae$AEDECOD,
            Ylabel = c("不良事件发生率（%）"),
            colorSet = c("red", "blue", "grey"),
            FigureName = "aetox")
```

## Presentation

### gmtplot

![](images/IGGGE.png)

### gmtplot_line

![](images/IL4GHGL.png)

### aetoxgrplot

![](aetox.png)

## Additional Requirements

The dataset imported from .sas7bdat needs to generated per ADaM IG and
includes main variables that will be used.
