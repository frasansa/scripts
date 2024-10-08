# ejemplo instalación en local-----------------------------------------------
# devtools::install(file.path("d:", "R_Projects", "Scripts", "Github_packages",
#                               "omopgenerics-main"))
# Sys.setenv(http_proxy="http://proxy.san.gva.es:8080/")
# options(internet.info = 0)

# Clean libraries------------------------------------------------------------
# paths = .libPaths()
# #
# # ## Try and detect bad files
# list.files(paths,
#            pattern = "^00LOCK*|*\\.rds$|*\\.RDS$",
#            full.names = TRUE)
#
# ## List files of size 0
# l = list.files(paths, full.names = TRUE)
# l[sapply(l, file.size) == 0]
# Cargar librerías-----------------------------------------------------------
library(AdhereR)
library(arrow)
library(arsenal)
library(broom)
library(broom.mixed)
library(CDMConnector)
library(codebook)
# library(collapse)
# library(conflicted)
library(cowplot)
library(data.table)
library(DBI)
# library(DescTools)
library(DiagrammeR)
library(DiagrammeRsvg)
library(doParallel)
# library(dotwhisker)
library(DrugExposureDiagnostics)
library(DT)
library(emojifont)
library(epiR)
library(extrafont)
library(fasttime)
library(foreach)
library(formattable)
library(future)
library(GGally)
library(ggfortify)
library(ggmap)
library(ggrepel)
library(ggpubr)
library(Gifi)
library(glue)
library(gmodels)
library(grid)
library(gridExtra)
library(gt)
library(haven)
library(here)
library(httr)
# library(icdcoder)
library(incidence)
library(janitor)
library(jsonlite)
library(kableExtra)
library(labelled)
library(lubridate)
library(magrittr)
# library(maps)
# library(maptools)
library(microbenchmark)
library(multidplyr)
library(naniar)
# library(plotly)
library(odbc)
library(pointblank)
library(prettydoc)
library(profvis)
library(qwraps2)
library(r2rtf)
library(ragg)
# library(rchess)
library(R2WinBUGS)
library(RColorBrewer)
library(readr)
library(readxl)
# library(rgdal)
library(rlang)
library(rlist)
library(RPostgreSQL)
library(rsvg)
library(scales)
library(skimr)
library(smoothHR)
library(snakecase)
library(sp)
# library(spdep)
library(survival)
library(survminer)
library(stringi)
library(table1)
library(tictoc)
library(tidyfast)
library(tidylog)
library(tidymodels)
# library(tidyquant)
library(tidyverse)
library(tiff)
# library(tmap)
library(unglue)
library(wesanderson)
library(writexl)
library(zeallot)
library(zip)
# conflicts
conflicted::conflict_prefer_all("dplyr")
# conflicted::conflict_prefer("distinct", "tidylog")
# conflicted::conflict_prefer("filter",   "tidylog")
# conflicted::conflict_prefer("count",   "tidylog")
conflicted::conflict_prefer("year",     "lubridate")
conflicted::conflict_prefer_all("tidyr", "tidylog")

