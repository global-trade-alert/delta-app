app.path='/home/rstudio/Dropbox/GTA cloud/17 Shiny/6 delta app/code/'
copy.path='/home/rstudio/ShinyApps/delta/'

##### DON'T USE THIS UNLESS YOU KNOW WHAT YOU ARE DOING!
file.copy(paste0(app.path,"app.R"),
          paste0(copy.path,"app.R"), overwrite = T)


