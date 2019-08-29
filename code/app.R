rm(list=ls())

library(shiny)
library(tidyverse)
library(shinyjs)
library(shinyWidgets)
library(gtasql)
library(pool)
library(DT)
library(gtalibrary)
library(zoo)
library(data.table)
library(splitstackshape)
library(plyr)
library(stringr)
library(RMariaDB)
library(DBI)
library(RMySQL)
library(readxl)
library(shinyalert)
library(stringr)
library(stringi)

# library(RMySQL)  
# 
# killDbConnections <- function () {
#   
#   all_cons <- dbListConnections(MySQL())
#   
#   print(all_cons)
#   
#   for(con in all_cons)
#     +  dbDisconnect(con)
#   
#   print(paste(length(all_cons), " connections killed."))
#   
# }
# killDbConnections()

gta_setwd()
app.path<<-'17 Shiny/6 delta app/code/'
source(paste0(app.path,'server.R'))
source(paste0(app.path,'ui.R'), local=T)
source(paste0(app.path,'functions/gta_delta_confirm_xlsx.R'))
source(paste0(app.path,'functions/gta_delta_input_check.R'))

shinyApp(ui,
         server,
         onStart = function() {
           gta_sql_pool_open(table.prefix = 'delta_',got.keyring = F)
           cat("Opening SQL connection\n")
           onStop(function() {
             gta_sql_pool_close()
             cat("Closing SQL connection\n")
           })
         }
)
