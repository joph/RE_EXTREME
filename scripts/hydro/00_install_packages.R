####################################################################################################### 
# show my packages
# my_packages <- installed.packages()
####################################################################################################### 


####################################################################################################### 
#####                                       Load Data                                             #####
####################################################################################################### 
# readr   - fast functions for reading text data
# readxl  - fast functions for reading Excel spreadsheet data (xls and xlsx)
# feather - read and write feather files, a lightweight binary columnar data store designed for maximum speed.
# foreign - functions to load data files from other programs into R.

install.packages("readxl")
install.packages("readr")
install.packages("feather")
install.packages("foreign")

####################################################################################################### 
#####                                  Data manipulation                                          #####
####################################################################################################### 
# dplyr     - subsetting, summarizing, rearranging, and joining together data sets
# tidyr     - tools (gather and spread functions) for converting data sets into a tidy format
# stringr   - easy to learn tools for regular expressions and character strings.
# lubridate - tools that make working with dates and times easier.
# broom     - converts model results into a tidy data frame
# tidyverse - installs/ loads the following packages broom, DBI, dplyr, forcats, ggplot2, haven, httr, hms,
#             jsonlite, lubridate, magrittr, modelr, purrr, readr, readxl,stringr, tibble, rvest, tidyr, xml2
# zoo:      - S3 Infrastructure for Regular and Irregular Time Series (Z's Ordered Observations)
# xts       - functionalities to work on time-indexed data. xts extends zoo, 
#             another popular package for time-series analysis.

install.packages("dplyr")
install.packages("tidyr")
install.packages("stringr")
install.packages("lubridate")
install.packages("broom")

install.packages("tidyverse")
install.packages("reshape2")
install.packages("zoo")
install.packages("xts")

####################################################################################################### 
#####                         Tools for working with spatial data                                 #####
####################################################################################################### 
# sp & maptools - Tools for loading and using spatial data including shapefiles.
# rgdal         - R’s interface to the popular C/C++ spatial data processing library gdal
# rgeos         - R’s interface to the powerful vector processing library geos
# ggmap         - add maps straight from Google maps as a background in your ggplots.

install.packages("sp")
install.packages("maptools")
install.packages("rgdal")
install.packages("rgeos")
install.packages("ggmap")


####################################################################################################### 
#####                              plotting and reporting                                        #####
####################################################################################################### 
# ggplot2    - create beautiful, layered and customizable plots (using the grammar of graphics)
# plotly     - turns ggplots interactive and allows to be drawn with D3
# R Markdown - authoring framework for data science
#            - save and execute code & generate high quality reports that can be easily shared 
# Pander     - designed to provide a minimal and easy tool for rendering R objects into Pandoc’s markdown

install.packages("ggplot2")
install.packages("plotly")
install.packages("rmarkdown")
install.packages("pander")

####################################################################################################### 
#####                              other useful packages                                          #####
####################################################################################################### 
# purr      - completes R's functional programming tools with missing features from other programming languages
# magrittr  - decrease development time and improve readability and maintainability of code by using pipes %>% 
# tibble    - modern take on data frames. It encapsulates best practices for creating data frames

install.packages("purrr")
install.packages("magrittr")
install.packages("tibble")