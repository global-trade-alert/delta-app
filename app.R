library(shiny)
library(tidyverse)
library(shinyjs)
library(shinyWidgets)
library(gtasql)
library(pool)
library(DT)

lapply(dbListConnections(MySQL()), dbDisconnect)

gta_setwd()
app.path='17 Shiny/6 Tariff database/app'
source('17 Shiny/6 Tariff database/app/server.R')
source('17 Shiny/6 Tariff database/app/ui.R', local=T)

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
