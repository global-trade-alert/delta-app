app.path='/home/rstudio/Dropbox/GTA cloud/17 Shiny/6 delta app/code/'
copy.path='/home/rstudio/ShinyApps/tariff/'

##### DON'T USE THIS UNLESS YOU KNOW WHAT YOU ARE DOING!
file.copy(paste0(app.path,"app.R"),
          paste0(copy.path,"app.R"), overwrite = T)

file.copy(paste0(app.path,"ui.R"),
          paste0(copy.path,"ui.R"), overwrite = T)

file.copy(paste0(app.path,"server.R"),
          paste0(copy.path,"server.R"), overwrite = T)

file.copy(paste0(app.path,"functions/table_filter.R"),
          paste0(copy.path,"functions/table_filter.R"), overwrite = T)

file.copy(paste0(app.path,"functions/xlsx_query.R"),
          paste0(copy.path,"functions/xlsx_query.R"), overwrite = T)

file.copy(paste0(app.path,"www/gta logo-white.svg"),
          paste0(copy.path,"www/gta logo-white.svg"), overwrite = T)

file.copy(paste0(app.path,"www/loading.svg"),
          paste0(copy.path,"www/loading.svg"), overwrite = T)

file.copy(paste0(app.path,"www/style.css"),
          paste0(copy.path,"www/style.css"), overwrite = T)

file.copy(paste0(app.path,"www/tipped.css"),
          paste0(copy.path,"www/tipped.css"), overwrite = T)

file.copy(paste0(app.path,"www/tipped.js"),
          paste0(copy.path,"www/tipped.js"), overwrite = T)

file.copy(paste0(app.path,"www/tooltips.js"),
                 paste0(copy.path,"www/tooltips.js"), overwrite = T)