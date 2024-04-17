rm(list = ls())
# options(digits = 4)
# library(haven)
library(devtools)
library(ggprism)

# use_package("package" ="plyr", type = "Imports")
# use_package("package" ="tidyverse", type = "Depends", min_version = "2.0.0")
# use_package("package" ="scales", type = "Imports")

# setwd(dir = "D:/360Downloads/R/project/GMT/GMTPackage/gmtplot")
devtools::document()
load_all()

forestdata <- read_sas("forestdata.sas7bdat")

final <- forestdata %>%
  mutate(ref = if_else(yord %in% c(4,5,6,10,11,12,16,17,18,19), NA, yord))


forestplot(
  datain = final,
  XVar = final$means,
  XLowVar = final$yerrl,
  XUppVar = final$yerru,
  YOrderVar = final$yord,
  XLine = 0,
  YRefVar = final$ref,
  YRefWidth = 4,
  Anno1Var = final$col1,
  Anno1VarX = -30,
  Anno1VarLabel = "D14 Subgroup",
  Anno2Var = final$col2,
  Anno2VarX = -15,
  Anno2VarLabel = "V01E GMT",
  Anno3Var = final$col3,
  Anno3VarX = -8,
  Anno3VarLabel = "V01E-2 GMT",
  Anno4Var = final$col5,
  Anno4VarX = 15,
  Anno4VarLabel = "LS GMR 95% CI",
  AnnoLineLabel = "LS GMR 95% CI",
  XaxisValue = c(-10, 0, 10),
  XaxisLabel = c("<-10", "0", ">=10"),
  XaxisType = "line",
  leftmargin = 9.5,
  rightmargin = 4.5,
  figwidth = 9,
  figheight = 4,
  FigureName = "forestplot"
)

forestplot(
  datain = final,
  XVar = final$means,
  XLowVar = final$yerrl,
  XUppVar = final$yerru,
  YOrderVar = final$yord,
  XLine = 1,
  YRefVar = final$ref,
  YRefWidth = 4.9,
  Anno1Var = final$col1,
  Anno1VarX = 10^-3,
  Anno1VarLabel = "D14 Subgroup",
  Anno2Var = final$col2,
  Anno2VarX = 3*10^-2,
  Anno2VarLabel = "V01E GMT",
  Anno3Var = final$col3,
  Anno3VarX = 10^-1,
  Anno3VarLabel = "V01E-2 GMT",
  Anno4Var = final$col5,
  Anno4VarX = 4*10^1,
  Anno4VarLabel = "LS GMR 95% CI",
  Anno5Var = NULL,
  Anno5VarX = NULL,
  Anno5VarLabel = NULL,
  Anno6Var = NULL,
  Anno6VarX = NULL,
  Anno6VarLabel = NULL,
  AnnoLineLabel = "LS GMR 95% CI",
  XaxisValue = c(0.1, 1, 10),
  XaxisLabel = c("<0.1", "1", ">=10"),
  XaxisType = "log",
  leftmargin = 9.5,
  rightmargin = 4.5,
  figwidth = 9,
  figheight = 4,
  FigureName = "forestplot"
)

#
# devtools::build()
#
# devtools::check()
#
# use_readme_rmd()
# build_readme()


