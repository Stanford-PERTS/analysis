
# this script makes the companion graphs for
# https://docs.google.com/document/d/1gCooRltKcCn-9BiqTVYKGNasNwifCWSDdOxApJlIpw8/edit#

# install.packages('gsheet')
library(gsheet)
library(ggplot2)


options(stringsAsFactors = FALSE)
options(xtable.comment = FALSE)
github_base_path <- "https://raw.githubusercontent.com/PERTS/gymnast/master/"
github_local_path <- "~/Documents/Github/"


tryCatch({
    source(paste0(github_local_path,"gymnast/R/util.R"))
    gymnast_install()
    library(tidyverse)
    source(github_local_path %+% "gymnast/R/util_data_summaries.R")
    source(github_local_path %+% "gymnast/R/util_qualtrics_cleaning.R")
    source(github_local_path %+% "gymnast/R/util_graphing.R")
    source(github_local_path %+% "gymnast/R/util_scale_computation.R")
    source(github_local_path %+% "gymnast/R/util_dfc.R")
}, error = function(e){
    source(github_base_path %+% "R/util.R")
    gymnast_install()
    source(github_base_path %+% "R/util_data_summaries.R")
    source(github_base_path %+% "R/util_qualtrics_cleaning.R")
    source(github_base_path %+% "R/util_graphing.R")
    source(github_base_path %+% "R/util_scale_computation.R")
    source(github_base_path %+% "R/util_dfc.R")
})

url <- 'https://docs.google.com/spreadsheets/d/19JuV494lgDZ6ZwsIV9szdyZZrGdj7Esh-G3k6avbKX8/edit#gid=0'
raw <- gsheet::gsheet2tbl(url)
belonging <- raw[raw$Measure %in% "Social Belonging",]

ggplot2::ggplot(belonging, aes(Artifact,Mean)) +
    ggplot2::geom_bar(stat="summary",fun.y="mean", fill="#015587") +
    ggplot2::geom_errorbar(aes(x=Artifact,ymin=Lower,ymax=Upper), width=.3) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous( labels=percent ) +
    ggplot2::xlab("") + ggplot2::ylab("") +
    ug.ht +
    theme(legend.position="bottom",
          legend.text = element_text(size=7),
          legend.title = element_text(size=7),
          plot.title= element_text( color="black", angle=0, size=10 ),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_line(color="gray"),
          panel.grid.major.y = element_blank(),
          axis.text.x = element_text(hjust = .5, size=12),
          axis.text.y = element_text(hjust = 0, size=12),
    )





