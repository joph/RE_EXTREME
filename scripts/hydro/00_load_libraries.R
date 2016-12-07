####################################################################################################### 
# show my packages
my_packages <- installed.packages()
####################################################################################################### 


####################################################################################################### 
#####                                       Load Data                                             #####
####################################################################################################### 
# readr   - fast functions for reading text data
# readxl  - fast functions for reading Excel spreadsheet data (xls and xlsx)
# feather - read and write feather files, a lightweight binary columnar data store designed for maximum speed.
# foreign - functions to load data files from other programs into R.

library(readxl)
library(readr)
library(feather)
library(foreign)

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

library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(broom)

#library(tidyverse)
library(reshape2)

####################################################################################################### 
#####                         Tools for working with spatial data                                 #####
####################################################################################################### 
# sp & maptools - Tools for loading and using spatial data including shapefiles.
# rgdal         - R’s interface to the popular C/C++ spatial data processing library gdal
# rgeos         - R’s interface to the powerful vector processing library geos
# ggmap         - add maps straight from Google maps as a background in your ggplots.

library(sp)
library(maptools)
library(rgdal)
library(rgeos)
library(ggmap)

####################################################################################################### 
#####                              plotting and reporting                                        #####
####################################################################################################### 
# ggplot2 - create beautiful, layered and customizable plots (using the grammar of graphics)
# plotly  - turns ggplots interactive and allows to be drawn with D3

library(ggplot2)
library(plotly)

####################################################################################################### 
#####                              other useful packages                                          #####
####################################################################################################### 
# purr      - completes R's functional programming tools with missing features from other programming languages
# magrittr  - decrease development time and improve readability and maintainability of code by using pipes %>% 
# tibble    - modern take on data frames. It encapsulates best practices for creating data frames

library(purrr)
library(magrittr)
library(tibble)








