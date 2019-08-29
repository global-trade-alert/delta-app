gta_delta_input_check=function(
  input.df=NULL
){
  # Setup
  
  library(lubridate)
  library(zoo)
  library(stringr)
  library(gtalibrary)
  library(gtasql)
  library(pool)
  library(RMariaDB)
  library(splitstackshape)
  
  # gta_sql_pool_open(table.prefix = "delta_" )
  #### basic checks that need to hold up before touching the SQL datbase
  
  input=input.df
  
  input=cSplit(input, which(colnames(input)=="implementing.jurisdiction"), direction="long", sep=";")
  input=cSplit(input, which(colnames(input)=="affected.country"), direction="long", sep=";")
  input=cSplit(input, which(colnames(input)=="affected.code"), direction="long", sep=";")
  input$implementing.jurisdiction=str_trim(input$implementing.jurisdiction)
  input$affected.country=str_trim(input$affected.country)
  input$affected.code=str_trim(input$affected.code)
  
  input=as.data.frame(input)
  for (i in colnames(input)) {
    assign(i, input[, i])
  }
  
  fatal=F
  error.msg=NULL
  new.error=F
  #dangerous cpc which are same in 2digit or 3digit
  dangerous.cpc=cpc.names[duplicated(cpc.names$cpc),]$cpc

  date.error=function(date.variable.name='', NA.permitted=F){
    if(NA.permitted==F){
      return(paste('The field', date.variable.name, 'was rejected. Please enter a valid date format (yyyy-mm-dd: 2019-02-30).',
                   'All dates recognized by excel work, regardless of their cell-format.',
                   'Ensure that all dates are in either string, or date format.'))
    } else {
      return(paste('The field', date.variable.name, 'was rejected. Please enter a valid date format (yyyy-mm-dd: 2019-02-30).',
                   'All dates recognized by excel work, regardless of their cell-format.',
                   'Ensure that all dates are in either string, or date format.',
                   'NA values are permissible and must be communicated as a blank cell in Excel.'))
    }
  }
  ### Parameter completeness check
  ## TBA write check that ensures required fields are filled
  ## if not, all non-specified fields are reported in the same error message
  ## required fields are 
  ## treatment.area, source, source.official, author, 
  ## intervention.type, affected.flow, implementation.level, eligible.firms,
  ## implementing.jurisdiction, affected.code, affected.code.type, treatment.value, treatment.unit, treatment.code.official
  ## date.implemented
  
  #not necessary anymore
  #missing fields are checked in the confirm_xlsx right now
  #if we allow in the future users to go through the database with the gui perhaps this could be useful? 

  # if(any(sapply(
  #   list(treatment.area, source, source.official, author, intervention.type, affected.flow, implementation.level, eligible.firms,
  #        implementing.jurisdiction, affected.code, affected.code.type, treatment.value, treatment.unit, treatment.code.official, date.implemented),
  #   is.null
  # ))){
  #   req.fields=list(treatment.area=treatment.area, source=source, source.official=source.official, author=author,
  #                   intervention.type=intervention.type, affected.flow=affected.flow, implementation.level=implementation.level, eligible.firms=eligible.firms,
  #                   implementing.jurisdiction=implementing.jurisdiction, affected.code=affected.code, affected.code.type=affected.code.type, treatment.value=treatment.value,
  #                   treatment.unit=treatment.unit, treatment.code.official=treatment.code.official, date.implemented=date.implemented)
  #   stop(paste('The following parameters are required and are currently missing:', paste(names(req.fields[sapply(req.fields,is.null)]),collapse='; ')))
  # }
  
  ### Parameter correctness check
  ## Length checks
  # again done in the confirm_xlsx
  
  ## Interpretability checks
  
  # valid treatment.area
  if(!all(tolower(treatment.area) %in% tolower(c(gta_sql_get_value("SELECT DISTINCT `treatment_area_id` FROM `delta_treatment_area_list`"),
                                gta_sql_get_value("SELECT DISTINCT `treatment_area_name` FROM `delta_treatment_area_list`"))))){
    fatal=T
    error.msg=paste(error.msg,
                    "Your treatment areas should be one of the following: ", 
                    paste(c(gta_sql_get_value("SELECT DISTINCT `treatment_area_id` FROM `delta_treatment_area_list`"),
                            gta_sql_get_value("SELECT DISTINCT `treatment_area_name` FROM `delta_treatment_area_list`")),
                          collapse="; "),", respectively.", sep="")
  }
  
  
  # valid date.announced
  tryCatch({date=format(as.Date(x=date.announced),"%Y-%m-%d")},
           error=function(e){
             fatal<<-T
             new.error<<-T
           },
           finally={
             if(!all(is.na(date) | str_sub(as.character(date),1,4) %in% as.character(2000:2024))){
               fatal=T
               new.error=T
             }
             
           })
  if(new.error==T){
    error.msg=paste(error.msg,date.error('date.announced',NA.permitted = T),sep='<br>')
    new.error=F
  }             
  
  # valid date.implemented
  tryCatch({date=format(as.Date(x=date.implemented),"%Y-%m-%d")},
           error=function(e){
             fatal<<-T
             new.error<<-T
           },
           finally={
             if(any(is.na(date))){
               fatal=T
               new.error=T
             } 
             
             if(!all(str_sub(as.character(date),1,4) %in% as.character(2005:2024))){
               fatal=T
               new.error=T
             }
             
             
           })
  if(new.error==T){
    error.msg=paste(error.msg,date.error('date.implemented'),sep='<br>')
    new.error=F
  }
  
  #no restriction to source yet
  # state.act.source
  
  # source.official
  if(!all(tolower(source.official) %in% c('yes','no','true','false','t','f'))){
    fatal=T
    error.msg=paste(error.msg,
                    "The field source official was rejected. Permissible values are True, T, False, F, Yes, No",
                    sep='<br>')
  }
  
  # author
  if(!all(tolower(author) %in% tolower(c(gta_sql_get_value("SELECT DISTINCT `user_login` FROM `gta_user_log`"),
                        gta_sql_get_value("SELECT DISTINCT `user_id` FROM `gta_user_log`")))) | length(unique(author))!=1){
    fatal=T
    error.msg=paste(error.msg,
                    "Author identification is invalid, please enter either your login name (firstname.surname), or your user identification number. Note: one author maximum per excel entry is authorized",
                    sep='<br>')
  }
  
  #dont think this is necessary any more, i have confirmed that the author is in the list, then i can just use this.author.id=author 
  ## this.author.id
  # sql <- "SELECT user_id FROM user_log WHERE user_login = ?fromwhom;"
  # query <- sqlInterpolate(pool, 
  #                         sql, 
  #                         fromwhom = author)
  # 
  #   this.author.id=gta_sql_get_value(query)
  #   rm(query)
  
  # intervention.type
  if(!all(tolower(intervention.type) %in% tolower(c(gta_sql_get_value("SELECT DISTINCT `intervention_type_id` FROM `gta_intervention_type_list`"),
                                   gta_sql_get_value("SELECT DISTINCT `intervention_type` FROM `gta_intervention_type_list`"))))){
    fatal=T
    error.msg=paste(error.msg,
                    "Intervention type is invalid.",
                    sep='<br>')
    
  }
  
  # affected.flow
  if(!all(tolower(affected.flow) %in% tolower(c(gta_sql_get_value("SELECT DISTINCT `affected_flow_id` FROM `gta_affected_flow_list`"),
                               gta_sql_get_value("SELECT DISTINCT `affected_flow` FROM `gta_affected_flow_list`"))))){
    fatal=T
    error=paste0("Either the id or the full name are permissibles for affected.flows: ",
                 paste(gta_sql_get_value("SELECT DISTINCT `affected_flow_id` FROM `gta_affected_flow_list`"),collapse='; '),
                 ' or ',
                 paste(gta_sql_get_value("SELECT DISTINCT `affected_flow` FROM `gta_affected_flow_list`"),collapse='; '),
                 ' respectively.'
    )
    error.msg=paste(error.msg,
                    error,
                    sep='<br>')
    rm(error)
  }
  
  # implementing jurisdiction
  if(!all(tolower(implementing.jurisdiction) %in% tolower(c(gta_sql_get_value("SELECT DISTINCT `un_code` FROM `gta_jurisdiction_list`"),
                                           gta_sql_get_value("SELECT DISTINCT `jurisdiction_name` FROM `gta_jurisdiction_list`"))))){
    fatal=T
    error.msg=paste(error.msg,
                    "Implementing jurisdiction was rejected. Please enter the implementing.jurisdiction's un code or its name. Multiple entries in a single cell are permissible, but must be separated by a semi-colon (;)",
                    
                    sep='<br>')
  }
  
  # optional implementer.end.date
  tryCatch({date=format(as.Date(x=implementer.end.date),"%Y-%m-%d")},
           error=function(e){
             fatal<<-T
             new.error<<-T
           },
           finally={
             if(!all(is.na(date) | str_sub(as.character(date),1,4) %in% as.character(2000:2024))){
               fatal=T
               new.error=T
             }
             
           })
  if(new.error==T){
    error.msg=paste(error.msg,date.error('implementer.end.date',NA.permitted = T),sep='<br>')
    new.error=F
  } 
  
  # implementation.level
  if(!all(tolower(implementation.level) %in% tolower(c(gta_sql_get_value("SELECT DISTINCT `implementation_level_id` FROM `gta_implementation_level_list`"),
                                      gta_sql_get_value("SELECT DISTINCT `implementation_level_name` FROM `gta_implementation_level_list`"))))){
    fatal=T
    error=paste0("Either the id or the full name are permissibles for implementation level: ",
                 paste(gta_sql_get_value("SELECT DISTINCT `implementation_level_id` FROM `gta_implementation_level_list`"),collapse='; '),
                 ' or ',
                 paste(gta_sql_get_value("SELECT DISTINCT `implementation_level_name` FROM `gta_implementation_level_list`"),collapse='; '),
                 ' ,respectively.')
    error.msg=paste(error.msg,
                    error,
                    sep='<br>')
    rm(error)
  }
  
  # eligible.firms
  if(!all(tolower(eligible.firms) %in% tolower(c(gta_sql_get_value("SELECT DISTINCT `eligible_firms_id` FROM `gta_eligible_firms_list`"),
                                gta_sql_get_value("SELECT DISTINCT `eligible_firms_name` FROM `gta_eligible_firms_list`"))))){
    fatal=T
    error=paste0("Either the id or the full name are permissibles for eligible.firms: ",
                 paste(gta_sql_get_value("SELECT DISTINCT `eligible_firms_id` FROM `gta_eligible_firms_list`"),collapse='; '),
                 ' or',
                 paste(gta_sql_get_value("SELECT DISTINCT `eligible_firms_name` FROM `gta_eligible_firms_list`"),collapse='; '),
                 ' ,respectively.')
    error.msg=paste(error.msg,
                    error,
                    sep='<br>')
    rm(error)
  }
  
  
  # affected.code
  if(any(is.na(as.numeric(gsub('\\D','',affected.code))))){
    fatal=T
    error='Only codes are permissible values for affected.code, either in character or numeric format, 1234.24 or 123424 are permissible'
    error.msg=paste(error.msg,
                    error,
                    sep='<br>')
    rm(error)
  } 
  
  # I don't allow for ids here, the added flexibility doesn't serve much 
  # affected.code.type
  if(!all(tolower(affected.code.type) %in% tolower(gta_sql_get_value("SELECT DISTINCT `code_type_name` FROM `delta_code_type_list`")))){
    fatal=T
    error=paste0("Please enter the affected.code.type's name: ",
                 paste(gta_sql_get_value("SELECT DISTINCT `code_type_name` FROM `delta_code_type_list`"), collapse='; '),
                 '.')
    error.msg=paste(error.msg,
                    error,
                    sep='<br>')
    rm(error)
  }
  
  df.affected.code=data.frame(affected.code=as.numeric(gsub('\\D','',affected.code)),
                              affected.code.type=affected.code.type)
  #is there any way to make sure that the user entered 3 digit cpc?
  # in gtalibrary::cpc.codes for example '11' is considered 3 digit? 
  if(nrow(subset(df.affected.code, !nchar(affected.code) %in% c(2,3) & affected.code.type=='cpc'))>0){
    fatal=T
    error.msg=paste(error.msg,
                    "Cpc codes must be given in a three digit format. Multiple entries in a single cell are permissible, but must be separated by a semi-colon (;)",
                    sep='<br>')
  }
  
  if(nrow(subset(df.affected.code, (nchar(affected.code)<3 | nchar(affected.code)>6) & affected.code.type=='hs'))>0){
    fatal=T
    error.msg=paste(error.msg,
                    "Hs codes must be between 4(3) and 6(5) digits long. Multiple entries in a single cell are permissible, but must be separated by a semi-colon (;)",
                    sep='<br>')
  }
  
  #disregarding this for now
  ## TBA affected.code
  # is it numeric/integer of a length that makes sense? 
  # also, please expand all HS codes with less than 6-digits
  ######## and/or cpc codes with less than 3-digit: We decided only 3 digits allowed as entry
  # After this section, affected.code should only include the correct HS/CPC digit length (6/3)

  hs.codes=subset(df.affected.code, affected.code.type=='hs')$affected.code
  if(is.null(gta_hs_code_check(hs.codes))){
    fatal=T
    error.msg <- capture.output({
      gta_hs_code_check(hs.codes)
    })
  }  
  
  if(new.error==T){
    error=paste0('The following hs code(s) returned no match: ', 
                 paste(unique(hs.nomatch),collapse='; '),
                 '. Multiple entries in a single cell are permissible, but must be separated by a semi-colon (;)')
    error.msg=paste(error.msg,
                    error,
                    sep='<br>')
    new.error=F
    rm(error)
    
  } 
  
  cpc.nomatch=NULL
  for(code in subset(df.affected.code, affected.code.type=='cpc')$affected.code){
    if(is.null(gta_cpc_code_check(code))|is.factor(gta_cpc_code_check(code))){
      fatal=T
      new.error=T
      cpc.nomatch=c(cpc.nomatch,code)
    }
  }  
  if(new.error==T){
    error=paste0('The following cpc code(s) returned no match: ', 
                 paste(unique(cpc.nomatch),collapse='; '),
                 '. Multiple entries in a single cell are permissible, but must be separated by a semi-colon (;)')
    error.msg=paste(error.msg,
                    error,
                    sep='<br>')
    new.error=F
    rm(error)
    
  } 
  
  if(any(dangerous.cpc %in% unique(subset(df.affected.code, affected.code.type=='cpc')$affected.code))){
    error=paste(unique(subset(df.affected.code, affected.code %in% dangerous.cpc & affected.code.type=='cpc')$affected.code),
                collapse = '; ')
    error=paste('The following cpc codes are ambiguously matched on both a two digit and three digit level: ',
                error,
                'As a reminder, only three digit codes are permissible, please ensure that they were intended as a three digit cpc code!')

    error.msg=paste(error.msg,
                    error,
                    sep='<br>')
    rm(error)
  }
  
  
  # optional affected.code.end.date
  tryCatch({date=format(as.Date(x=affected.code.end.date),"%Y-%m-%d")},
           error=function(e){
             fatal<<-T
             new.error<<-T
           },
           finally={
             if(!all(is.na(date) | str_sub(as.character(date),1,4) %in% as.character(2000:2024))){
               fatal=T
               new.error=T
             }
             
           })
  if(new.error==T){
    error.msg=paste(error.msg,date.error('affected.code.end.date',NA.permitted = T),sep='<br>')
    new.error=F
  } 
  
  ## SHOULD I ADD RESTRICTIONS HERE? PERCENT BETWEEN 0-100 ETC?
  # treatment.value
  if(any(is.na(as.numeric(treatment.value)))){
    fatal=T
    error.msg=paste(error.msg,
                    "Only numeric input is permissible for treatment.values. Decimals must be deliniated by periods (.).",
                    sep='<br>')
  }
  
  # treatment.unit
  if(!all(tolower(treatment.unit) %in% tolower(c(gta_sql_get_value("SELECT DISTINCT `level_unit_id` FROM `gta_unit_list`"),
                                gta_sql_get_value("SELECT DISTINCT `level_unit` FROM `gta_unit_list`"))))){
    fatal=T
    error=paste("Treatment unit was rejected. Permissible values are unit ids, or their names: ", 
                paste(gta_sql_get_value("SELECT DISTINCT `level_unit` FROM `gta_unit_list`"), collapse='; '))
    error.msg=paste(error.msg,
                    error,
                    sep='<br>')
    rm(error)
  }
  
  # treatment.code.official
  if(!all(tolower(treatment.code.official) %in% c('yes','no','true','false','t','f'))){
    fatal=T
    error.msg=paste(error.msg,
                    "The field treatment code official was rejected. Permissible values are True, T, False, F, Yes, No",
                    sep='<br>')
  }
  
  
  # affected.country
  if(!all(is.na(affected.country) | tolower(affected.country) %in% 
          tolower(c(gta_sql_get_value("SELECT DISTINCT `un_code` FROM `gta_jurisdiction_list`"),
            gta_sql_get_value("SELECT DISTINCT `jurisdiction_name` FROM `gta_jurisdiction_list`"))))){
    fatal=T
    error.msg=paste(error.msg,
                    "Affected country was rejected. Please enter the affected.country's un code or it's name",
                    sep='<br>')
  }
  
  # optional affected.country.end.date
  tryCatch({date=format(as.Date(x=affected.country.end.date),"%Y-%m-%d")},
           error=function(e){
             fatal<<-T
             new.error<<-T
           },
           finally={
             if(!all(is.na(date) | str_sub(as.character(date),1,4) %in% as.character(2000:2024))){
               fatal=T
               new.error=T
             }
             
           })
  if(new.error==T){
    error.msg=paste(error.msg,date.error('affected.country.end.date',NA.permitted = T),sep='<br>')
    new.error=F
  } 
  
  # framework,name
  if(!all(is.na(framework.name) | tolower(framework.name) %in% 
          tolower(c(gta_sql_get_value("SELECT DISTINCT `framework_id` FROM `gta_framework_log`"),
            gta_sql_get_value("SELECT DISTINCT `framework_name` FROM `gta_framework_log`"))))){
    fatal=T
    error.msg=paste(error.msg,
                    "Please enter a valid framework(regime) name or id.",
                    sep='<br>')
  }
  
  # please create
  # this.int.type.id
  # this.flow.id
  # this.imp.level.id
  # this.firms.id
  # this.implementer.id 
  # this.treatment.unit.id
  # this.affected.country.id (if specified)
  
  # NOTE 2 things for the country IDs
  # 1) We want to use the GTA IDs in this database (the primary key on jurisdiction_list reflects that).
  # 2) We want to support all user-defined country groups. That raises a question and a general point. 
  #    The general point is that this is the time to partially integrate with the GTA's main database. Do this based on the elsewhere-defined country groups by uploading from GTA cloud/data/database replica into ricardodev using the filename as the table name. files: gta_jurisdiction_group, gta_jurisdiction_group_member. It's pobably a good idea to delete delta_jurisdiction_list and replace it with gta_jurisdiction (on which it is based anyway).
  #    The question is how to treat those groups in the database: 
  #    Are they their own jurisdiction ID (and would thus have to be added to gta_jurisdiction on ricardodev)? If so, is this the only or the n+1'th jurisdiction ID that is recorded on the relevant table?
  #    Or are they only recorded in disaggregated form (e.g. all EU members as implementers, but not the EU itself).
  #    We use the "disaggregated approach" on the main site. I guess we should do it here (but can be convinced otherwise).
  
  
  
  
  
  
  ## TBA treatment.value
  ## is it numeric/integer of a length that makes sense? 
  
  # TBA treatment.code.official
  # source.official must be T or F
  
  ## TBA affected.country.end.date (if specified, default is NA)
  ## note that his could be a vector with NA's (! not NULL's) plus dates e.g. a preferential regime for China and India that is still in force for one of them but expired for the other.
  
  ## TBA affected.code.end.date (if specified, default is NA)
  ## same note as for affected.country.end.date
  
  ## TBA implementer.end.date (if specified, default is NA)
  ## we may want to worry about this one last
  ## same note as for affected.country.end.date
  ## Also, this cannot contain a date for all implementers of the intervention.
  
  ## regime.name & regime.new.to.db
  ## First check that either of these is not NULL, if so check that the name is (not) in the system and append regime.log, if appropriate.
  ## you can store the regime id already here since we will need it further down. 
  ## There is a new get.id parameter in gta_sql_append_table that helps you do it e.g gta_sql_append_table(get.id="regime.id")
  
  
  
  
  #decided against this for the time being
  # if(regime.new.to.db){
  #   
  #   # create new regime ID
  #   regime.log.update=data.frame(regime.id=NA,
  #                                regime.name=regime.name,
  #                                user.id=this.author.id,
  #                                stringsAsFactors = F)
  #   
  #   this.regime.id=gta_sql_append_table(append.table = "regime.log",
  #                                       append.by.df = "regime.log.update",
  #                                       get.id = "regime.id")
  #   rm(regime.log.update)
  # } 
  
  #remove html break if at end or start
  error.msg=sub("^<br>", "", error.msg)
  error.msg=sub("<br>$", "", error.msg)
  return(list(fatal=fatal,error.msg=error.msg))
}

# rm(list=ls())

# gta_sql_pool_open(table.prefix = 'delta_',got.keyring = F)
# gtalibrary::gta_setwd()
# source("17 Shiny/6 delta app/code/functions/gta_delta_confirm_xlsx.R")
# xlsx.input <- gta_delta_confirm_xlsx(readxl::read_excel("17 Shiny/6 delta app/testqueries/full query.xlsx"))
# xlsx.input[[1]]
# xlsx.input[[2]]
# input.df=xlsx.input[[3]]
# gta_delta_input_check(input.df = input.df)
