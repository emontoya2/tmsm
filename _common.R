set.seed(4)

 

options(dplyr.print_min = 6, dplyr.print_max = 6)

# Activate crayon output
options(
  #crayon.enabled = TRUE,
  pillar.bold = TRUE,
  stringr.html = FALSE
)

 
# packages ---------------------------------------------------------------------

suppressMessages(library(ggridges))
suppressMessages(library(gridExtra))
suppressMessages(library(knitr))
suppressMessages(library(kableExtra))
suppressMessages(library(tidyverse))
suppressMessages(library(mosaic))
suppressMessages(library(lattice))
suppressMessages(library(pwr))
suppressMessages(library(powerMediation))
suppressMessages(library(psych))
suppressMessages(library(corrplot))

  
# knitr chunk options ----------------------------------------------------------

knitr::opts_chunk$set(
  #eval = FALSE,
  comment = "#>",
  collapse = TRUE,
  message = FALSE,
  warning = FALSE,
  cache = FALSE, # only use TRUE for quick testing!
  echo = FALSE, # hide code unless otherwise noted in chunk options
  fig.align = "center",
  fig.width = 6,
  fig.asp = 0.618,  # 1 / phi
  fig.show = "hold",
  dpi = 300,
  fig.pos = "h"
)

 

if (knitr::is_html_output()) {
  knitr::opts_chunk$set(out.width = "90%")
} else if (knitr::is_latex_output()) {
  knitr::opts_chunk$set(out.width = "80%")
}

# knit options -----------------------------------------------------------------

options(knitr.kable.NA = "")

# kableExtra options -----------------------------------------------------------

options(kableExtra.html.bsTable = TRUE)

# dplyr options ----------------------------------------------------------------

options(dplyr.print_min = 6, dplyr.print_max = 6)
 
 
 