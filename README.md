
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

## Installation

You can install the development version of gmtplot from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("AQ-Life/gmtplot")
```

## Arguments

| Function Name | Arguments   | Fucntion (Requirement)                                  | Default |
|---------------|-------------|---------------------------------------------------------|---------|
| gmtplot       | datain      | 输入数据集（需包含USUBJID, TRTAN, AVISITN, AVAL, BASE） |         |
| gmtplot       | GrpVar      | 分组变量                                                | TRTAN   |
| gmtplot       | GrpLabel    | 分组变量展示的标签                                      |         |
| gmtplot       | AvisitnVar  | 分析访视变量（子分组变量）                              | AVISITN |
| gmtplot       | AvisintVal  | 需要纳入的分析访视数值型结果                            |         |
| gmtplot       | AvisitLabel | 需要纳入的分析访视的标签                                |         |
| gmtplot       | Aval        | 分析值（数值型）                                        | AVAL    |
| gmtplot       | Base        | 基线值（数值型）                                        | BASE    |
| gmtplot       | YLabel      | Y轴标签                                                 |         |
| gmtplot       | LegendLabel | 分组变量的图例标签                                      |         |
| gmtplot       | colorSet    | 颜色设置（根据分组数量设置多个颜色）                    |         |
| gmtplot       | LineYN      | 是否绘制折线图                                          | FALSE   |
| gmtplot       | LegendYN    | 是否显示图例                                            | FALSE   |
| gmtplot       | FigureName  | 输出图片名称                                            | gmtplot |
|               |             |                                                         |         |
| gmtplot_line  | datain      | 输入数据集（需包含USUBJID, TRTAN, AVISITN, AVAL, BASE） |         |
| gmtplot_line  | GrpVar      | 分组变量                                                | TRTAN   |
| gmtplot_line  | GrpLabel    | 分组变量展示的标签                                      |         |
| gmtplot_line  | AvisitnVar  | 分析访视变量（子分组变量）                              | AVISITN |
| gmtplot_line  | AvisintVal  | 需要纳入的分析访视数值型结果                            |         |
| gmtplot_line  | AvisitLabel | 需要纳入的分析访视的标签                                |         |
| gmtplot_line  | Aval        | 分析值（数值型）                                        | AVAL    |
| gmtplot_line  | Base        | 基线值（数值型）                                        | BASE    |
| gmtplot_line  | YLabel      | Y轴标签                                                 |         |
| gmtplot_line  | LegendLabel | 分组变量的图例标签                                      |         |
| gmtplot_line  | colorSet    | 颜色设置（根据分组数量设置多个颜色）                    |         |
| gmtplot_line  | LineYN      | 是否绘制折线图                                          | FALSE   |
| gmtplot_line  | LegendYN    | 是否显示图例                                            | FALSE   |
| gmtplot_line  | FigureName  | 输出图片名称                                            | gmtplot |

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(gmtplot)
#> Loading required package: tidyverse
#> ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
#> ✔ dplyr     1.1.4     ✔ readr     2.1.4
#> ✔ forcats   1.0.0     ✔ stringr   1.5.1
#> ✔ ggplot2   3.4.4     ✔ tibble    3.2.1
#> ✔ lubridate 1.9.3     ✔ tidyr     1.3.0
#> ✔ purrr     1.0.2     
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
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
#> Adding missing grouping variables: `Avisitnum`, `lineheight`
#> Warning: Removed 501 rows containing missing values (`position_stack()`).
#> Warning: Removed 504 rows containing missing values (`geom_segment()`).
#> [1] "gmt_plot.png"
```

## Additional Requirements

The parameter “datain” from “gmtplot” function needs to generated per
ADaM IG and includes main variables that will be used are listed as
below.

| Variable | Label                     |
|----------|---------------------------|
| USUBJID  | Unique Subject Identifier |
| TRTAN    | Actual Treatment (N)      |
| AVISITN  | Analysis Visit (N)        |
| AVAL     | Analysis Value            |
| BASE     | Baseline Value            |
