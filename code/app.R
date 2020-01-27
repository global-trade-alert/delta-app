# gtasql::gta_sql_kill_connections()
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
library(openxlsx)

#2gb file allowed§
options(shiny.maxRequestSize=2000*1024^2)

# setwd("/home/rstudio/Dropbox/GTA cloud")
gta_setwd()
app.path<<-'0 dev/delta-app-ks/code/'
source(paste0(app.path,'server.R'))
source(paste0(app.path,'ui.R'), local=T)
source(paste0(app.path,'functions/gta_delta_get_jurisdiction_id.R'))
source(paste0(app.path,'functions/gta_delta_confirm_xlsx.R'))
source(paste0(app.path,'functions/gta_delta_input_check.R'))
source(paste0(app.path,'functions/gta_delta_input_ids.R'))
source(paste0(app.path,'functions/gta_delta_input_upload.R'))
source(paste0(app.path,'functions/gta_delta_query.R'))


shinyApp(ui,
         server,
         onStart = function() {
           gta_sql_pool_open(table.prefix = 'delta_',
                             db.title="ricardodev",
                             db.host = gta_pwd("ricardodev")$host,
                             db.name = gta_pwd("ricardodev")$name,
                             db.user = gta_pwd("ricardodev")$user,
                             db.password = gta_pwd("ricardodev")$password)
           cat("Opening SQL connection\n")
           onStop(function() {
             gta_sql_pool_close()
             cat("Closing SQL connection\n")
           })
         }
)
