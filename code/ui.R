gta_sql_pool_open(table.prefix = 'delta_',
                  db.title="ricardodev",
                  db.host = gta_pwd("ricardodev")$host,
                  db.name = gta_pwd("ricardodev")$name,
                  db.user = gta_pwd("ricardodev")$user,
                  db.password = gta_pwd("ricardodev")$password)

addResourcePath(prefix = 'www', directoryPath = paste0(app.path,'www'))

ui <- fluidPage(
  useShinyjs(),
  theme = "www/style.css",
  tags$head(tags$link(rel="stylesheet", type="text/css", href="www/tipped.css")),
  tags$head(tags$script(src="www/tipped.js")),
  
  ######## DATA COUNTS #########
  tags$div(id="loading",
           tags$div(class="loading-background"),
           tags$div(class="img-holder",
                    tags$img(src="www/loading.svg"))),
  tags$div(class="header",
           tags$div(class="username",
                    selectInput("users",
                                label=NULL,
                                choices = c("Choose User"="Select",
                                            gta_sql_get_value("SELECT DISTINCT `user_login` FROM `gta_user_log`")),
                                multiple = F)),
           tags$div(class="logo",
                    img(src="www/gta logo-white.svg"))),
  
  tags$div(class="overall-wrap",
           tags$div(class = "settings",
                    tags$div(class = "settings-inner",
                             tags$div(class="app-switcher",
                                      tags$div(class="tab-nav-wrapper",  
                                               tags$ul(class="tab-list nav",
                                                       tags$li(class="active check-suggestions",
                                                               HTML("<a data-toggle='tab' href='#search'>Tariff<br/>Search Settings</a>")
                                                       ),
                                                       tags$li(class="queryxlsx",
                                                               HTML("<a data-toggle='tab' href='#queryxlsx'> Excel Query </a>")
                                                       ),
                                                       tags$li(class="upload",
                                                               HTML("<a data-toggle='tab' href='#upload'> Upload data </a>")
                                                       )))),
                             tags$div(class="settings-bottom",
                                      # tags$div(class="scroll",
                                      tags$div(class="tab-pane active fade in", id="search",
                                               column(width=12,
                                                      column(width=6,
                                                             tags$div(class="suggestions",
                                                                      textAreaInput("treatment.codes",
                                                                                    label = "Treatment code input (csv).",
                                                                                    value = '',
                                                                                    width = '200px',
                                                                                    rows = 2))),
                                                      column(width=5,
                                                             tags$div(class="suggestions",
                                                                      selectInput("treatment.code.type",
                                                                                  "Treatment area",
                                                                                  choices=c('hs','cpc'),
                                                                                  selected='hs',
                                                                                  width='70px'))),
                                                      column(width=12,
                                                             column(6,
                                                             tags$div(class="suggestions",
                                                                      textAreaInput("new.treatment.value",
                                                                                    label = "(Optional) new treatment value",
                                                                                    width = '50px',
                                                                                    rows = 1))),
                                                             column(width=6,
                                                             tags$div(class="suggestions",
                                                                      selectInput("new.treatment.unit",
                                                                                  "(Optional) new treatment unit",
                                                                                  choices=c(NA,gta_sql_get_value('select distinct `level_unit` from gta_unit_list')),
                                                                                  selected=NA,
                                                                                  width='165px')))),
                                                      column(width=12,
                                                             tags$div(class="suggestions",
                                                                      multiInput("implementing.jurisdiction",
                                                                                 "Restrict implementers",
                                                                                 choices = gta_sql_get_value(paste('select distinct `jurisdiction_name` from gta_jurisdiction_list union',
                                                                                                                   'select distinct `jurisdiction_group_name` from gta_jurisdiction_group_list')),
                                                                                 selected = c('Argentina')))),
                                                      column(width=12,
                                                             tags$div(class="suggestions",
                                                                      multiInput("affected.jurisdiction",
                                                                                 "Restrict affected countries",
                                                                                 choices = c('MFN'), #,gta_sql_get_value(paste('select distinct `jurisdiction_name` from gta_jurisdiction_list union',
                                                                                                   #                        'select distinct `jurisdiction_group_name` from gta_jurisdiction_group_list'
                                                                                                                           
                                                                                 selected = 'MFN'))),
                                                      column(width=12,
                                                             column(width=6,
                                                                    tags$div(class="suggestions",
                                                                             dateInput("date",
                                                                                       "Date",
                                                                                       format='yyyy-mm-dd',
                                                                                       value=NA,
                                                                                       width='300px'))),
                                                             column(width=6,
                                                                    tags$div(class="suggestions",
                                                                             selectInput("treatment.area",
                                                                                         "Treatment area",
                                                                                         choices=gta_sql_get_value('select distinct `treatment_area_name` from delta_treatment_area_list'),
                                                                                         selected='tariff',
                                                                                         width='165px'))),
                                                             hidden(actionButton('clear.xlsx', 'Clear xlsx input'))
                                                      ),
                                                      hidden(
                                                      column(width=5,
                                                             tags$div(class="suggestions",
                                                                      selectInput("affected.flow",
                                                                                  "Affected flow",
                                                                                  choices=gta_sql_get_value('select distinct `affected_flow` from gta_affected_flow_list'),
                                                                                  selected=NULL,
                                                                                  multiple=T,
                                                                                  width='165px')))
                                                      )
                                               )
                                      ),
                                      tags$div(class="tab-pane fade", id="queryxlsx",
                                               column(width=12,
                                                      tags$div(class="suggestions",
                                                               tags$div(class="queryxlsx",
                                                                        fileInput('excel.query', 'Choose xlsx file to use to query.',
                                                                                  accept = c(".xlsx",".csv"))
                                                               )
                                                      ),
                                                      hidden(actionButton('clear.query.xlsx', 'Clear xlsx input')),
                                                      column(width=12,
                                                             tags$div(class="suggestions",
                                                                      tags$div(class="upload",
                                                      downloadButton('dl.xlsx.query.template', 'Download xlsx query template')
                                                                      )))
                                                      )
                                               
                                      ),
                                      tags$div(class="tab-pane fade", id="upload",
                                               column(width=12,
                                                      tags$div(class="suggestions",
                                                               tags$div(class="upload",
                                                                        fileInput('excel.upload', 'Choose xlsx or csv file (max 2gb)',
                                                                                  accept = c(".xlsx"))
                                                               )
                                                      )),
                                               column(width=12,
                                                      tags$div(class="suggestions",
                                                               tags$div(class="upload",
                                                                        textInput('upload.name', 'Choose a name for the upload')
                                                               )
                                                      )),
                                               column(width=12,
                                                      tags$div(class="suggestions",
                                                               tags$div(class="upload",
                                               downloadButton('dl.upload.template', 'Download upload template')
                                                               )))
                                               
                                               
                                      ),
                                      
                                      tags$div(class="continue-button",
                                               column(width=12,
                                                      actionButton("submit.query",
                                                                   "Submit query")))
                                      
                                      
                                      
                             ))),
           
           
           tags$div(class="content",
                    tags$div(class="results",
                             useShinyalert(),
                             downloadButton("dl", "Download"),
                             uiOutput('sorted.display'),
                             uiOutput('unsorted.display'),
                             dataTableOutput('unrecognized.variables')
                    )
           )
           
           
  ),
  
  tags$script(src="tooltips.js")
  
  
)


gta_sql_pool_close()




























