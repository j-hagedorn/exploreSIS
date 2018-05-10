  ## global.R ##
  
  # Load fun, data, libs, source files
  library(shinydashboard)
  library(shinythemes)
  library(DT)
  library(ggplot2)
  library(dygraphs) 
  library(parsetR)
  library(parcoords)
  library(visNetwork)
  library(d3heatmap)
  library(dplyr)
  library(forcats)
  library(magrittr)
  library(tidyr)
  library(broom)
  library(corrr)
  library(viridis)
  library(googlesheets)
  library(plotly)
  library(xts)
  library(lubridate)
  library(RColorBrewer)
  library(car)
  library(feather)
  # library(msir)
  
  # Define begin & due dates
  begin <- as.Date("2014/07/01")
  due <- as.Date("2017/09/30")
  
  # Load de-identified data
  scrub_sis <- read_feather("data/scrub_sis.feather")
  
  # Get most recent SIS score
  most_recent <- max(as.Date(scrub_sis$sis_date)[as.Date(scrub_sis$sis_date) <= Sys.Date()])
  
  # Define current interviewer as 
  scrub_sis %<>%
    group_by(interviewer) %>%
    mutate(
      current_int = max(sis_date) >= most_recent - 183 
    ) %>%
    ungroup()
  
  # Load totals
  totals <- read.csv("data/totals.csv")
  
  # Load needs mapping table (created by "prep/sis_mappings.R" script)
  needs <- read.csv("data/needs.csv")
  
  # Load service mapping table (created by "prep/sis_mappings.R" script)
  needs_matrix <- read.csv("data/needs_matrix.csv")
  
  # Transpose to allow joining by need item
  need_to_hcpcs <- needs_matrix
  rownames(need_to_hcpcs) <- need_to_hcpcs$Code
  need_to_hcpcs %<>% 
    select(-Code) %>% 
    t() %>% 
    as.data.frame()
  need_to_hcpcs$item <- rownames(need_to_hcpcs)
  
  # Load HCPCS table (created by "prep/readServices.R" script)
  codemap <- 
    read.csv("data/codemap.csv") %>%
    mutate(HCPCS = as.character(HCPCS))
  
  # Load transformed dfs to break down TOS
  
  q1_3 <- read_feather("data/q1_3.feather")
  # q2 <- read_feather("data/q2.feather")
  # q3 <- read_feather("data/q3.feather")
  # q2_3 <- q2 %>% bind_rows(q3)
  
  # Add color palettes
  soft_12 <- c("#c64457","#d7532a","#ae5d35","#d0ab2c","#a69743",
               "#7ac43e","#59a653","#45bc8d","#20d8fd","#725eb3",
               "#934fd0","#c04b91")
  
  
  ################################################################################ 
  # DEFINE FUNCTIONS: 
  
  # svs2sis (Service codes to SIS needs)
  
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
  
  ## to_network()
  ## Function to convert df to network data format ####
  
  to_network <- function(df){
    
    ntwk <- list()
    
    ntwk$vals <-
      df %>%
      select(from,to,val = est_hrs_per_mo)
    
    ntwk$nodes <-
      unique(c(unique(as.character(df$from)),
               unique(as.character(df$to)))) %>%
      data.frame("name_id" = .) %>%
      # Alphabetize
      arrange(name_id) %>%
      # Get values to size nodes
      left_join(ntwk$vals, by = c("name_id" = "from")) %>%
      left_join(ntwk$vals, by = c("name_id" = "to")) %>%
      mutate(
        val = ifelse(is.na(val.x) == T,val.y,val.x)
      ) %>%
      group_by(name_id) %>%
      summarize(value = sum(val)) %>%
      # Assign ids starting at 0
      mutate(
        label = name_id,
        id = row_number(name_id)-1,
        group = ifelse(name_id %in% df$from,"Needs","Services"),
        title = ifelse(
          group == "Services",
          yes = paste0(
            "Approximately ", round(value, digits = 1),
            "<br>hours of ", tolower(name_id), 
            "<br>could help me with various needs each month."
          ),
          no = paste0(
            "I could benefit from approximately ", round(value, digits = 1),
            "<br>hours of support for ", tolower(name_id), " each month."
          )
        )
      )
    
    ntwk$edges <-
      df %>%
      ungroup() %>%
      left_join(ntwk$nodes, by = c("from" = "name_id")) %>%
      rename(
        name_from = from,
        from = id
      ) %>%
      left_join(ntwk$nodes, by = c("to" = "name_id")) %>%
      rename(
        name_to = to,
        to = id,
        value = est_hrs_per_mo) %>%
      mutate(
        title = paste0(
          "Approximately ", round(value, digits = 1),
          "<br>hours of ", name_to, 
          "<br>could be used to support me with ", name_from,
          "<br>each month."
        )
      ) %>%
      droplevels()
    
    return(ntwk)
    
  }