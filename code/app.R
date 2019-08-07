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
library(xlsx)

#lapply(dbListConnections(MySQL()), dbDisconnect)

gta_setwd()
app.path='0 dev/delta-app-ks/'
source(paste0(app.path,'server.R'))
source(paste0(app.path,'ui.R'), local=T)

shinyApp(ui,
         server,
         onStart = function() {
           gta_sql_pool_open()
           cat("Opening SQL connection\n")
           onStop(function() {
             gta_sql_pool_close()
             cat("Closing SQL connection\n")
           })
         }
)
