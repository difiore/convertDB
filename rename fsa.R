rm(list=ls())

library(here) # for working directory
library(tidyverse) # for dplyr, readr, stringr, tidyr, tibble, and purrr
library(lubridate) # supposedly in tidyverse but maybe not
library(readxl) # to read in xlsx files
library(lettercase)

options(tibble.width = NULL)

f <- list.files(pattern="\\.fsa", recursive=TRUE) # guess max set close to length of file

indivs <- c("Tokio", "Tanga", "Wigberto")
for (i in 1:length(f)){
g[[i]] <- str_split(f[i],"/")[2]
}

g
	indivs %in% f


	if (!require("ghit")) {
		install.packages("ghit")
	}
	# on 64-bit Windows
	ghit::install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer"), INSTALL_opts = "--no-multiarch")
	# elsewhere
	ghit::install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer"))


	library("tabulizer")
	f <- system.file("examples", "data.pdf", package = "tabulizer")
	out1 <- extract_tables(f)
	str(out1)
