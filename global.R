## global.R ##

# Load fun, data, libs, source files
  library(shinydashboard)
  library(shinythemes)
  library(DT)
  library(ggplot2)
  library(rcdimple)
  library(dygraphs)
  library(parsetR)
  library(visNetwork)
  library(d3heatmap)
  library(dplyr)
  library(forcats)
  library(magrittr)
  library(tidyr)
  library(broom)
  library(googlesheets)
  library(plotly)
  library(xts)
  library(lubridate)
  library(RColorBrewer)
  library(car)

# Define begin & due dates
  begin <- as.Date("2014/07/01")
  due <- as.Date("2017/09/30")

# Load de-identified data
  scrub_sis <- read.csv("data/scrub_sis.csv")

# Get most recent SIS score
  most_recent <- max(as.Date(scrub_sis$sis_date)[as.Date(scrub_sis$sis_date) <= Sys.Date()])
    
# Load totals
  totals <- read.csv("data/totals.csv")

# Load needs mapping table (created by "prep/sis_mappings.R" script)
  needs <- read.csv("data/needs.csv")
  
# Load service mapping table (created by "prep/sis_mappings.R" script)
  needs_matrix <- read.csv("data/needs_matrix.csv")
  
# Load HCPCS table (created by "prep/readServices.R" script)
  codemap <- read.csv("data/codemap.csv")

# Load transformed dfs to break down TOS
  q2 <- readRDS("data/q2.rds")
  q3 <- readRDS("data/q3.rds")
    
# Add color palettes
  soft_12 <- c("#c64457","#d7532a","#ae5d35","#d0ab2c","#a69743",
               "#7ac43e","#59a653","#45bc8d","#20d8fd","#725eb3",
               "#934fd0","#c04b91")
  
    
################################################################################ 
# DEFINE FUNCTION: svs2sis (Service codes to SIS needs)

# To get the SIS needs associated with a given HCPCS code
# Assumes existence of needs_matrix df to map needs to svs
# Can enter any list of HCPCS codes and fx will return related SIS needs
  
  svs2sis <- function(hcpcs){
    
    library(dplyr); library(tidyr); library(magrittr)
    
    needs_matrix %>%
      filter(Code %in% hcpcs) %>%
      gather(item,yn,-Code) %>%
      filter(yn == T) %>%
      distinct(item) %>% #de-dup
      as.list() %>%
      return()
    
  }
  
  # Example: 
  # res_svs <- svs2sis(c("T1020","H2016"))  

