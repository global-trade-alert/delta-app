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
  
  # gta_sql_pool_open(table.prefix = 'delta_',got.keyring = F)
  # #### basic checks that need to hold up before touching the SQL datbase
  # gta_setwd()
  # input.df=read.csv('17 Shiny/6 delta app/data/delta data sample.csv')
  # 
  # app.path<<-'17 Shiny/6 delta app/code/'
  # source(paste0(app.path,'functions/gta_delta_confirm_xlsx.R'))
  # 
  # input.df=gta_delta_confirm_xlsx(input.df)[[3]]
  
  
  input=as.data.frame(input.df)
  
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
                   'Ensure that all dates are in either string, or date format. \n\n'))
    } else {
      return(paste('The field', date.variable.name, 'was rejected. Please enter a valid date format (yyyy-mm-dd: 2019-02-30).',
                   'All dates recognized by excel work, regardless of their cell-format.',
                   'Ensure that all dates are in either string, or date format.',
                   'NA values are permissible and must be communicated as a blank cell in Excel. \n\n'))
    }
  }
 
  ## Interpretability checks
  
  # valid treatment.area
  unrec.treatment.area=NULL
  if(length(unique(treatment.area))>1 | !all(tolower(treatment.area) %in% tolower(c(gta_sql_get_value("SELECT DISTINCT `treatment_area_id` FROM `delta_treatment_area_list`"),
                                gta_sql_get_value("SELECT DISTINCT `treatment_area_name` FROM `delta_treatment_area_list`"))))){
    fatal=T
    unrec.treatment.area=treatment.area[which(!tolower(treatment.area) %in% tolower(c(gta_sql_get_value("SELECT DISTINCT `treatment_area_id` FROM `delta_treatment_area_list`"),
                                                                                      gta_sql_get_value("SELECT DISTINCT `treatment_area_name` FROM `delta_treatment_area_list`"))))]
    unrec.treatment.area=unique(unrec.treatment.area)
    error.msg=paste(error.msg,
                    "Your treatment areas should be one of the following: ", 
                    paste(c(gta_sql_get_value("SELECT DISTINCT `treatment_area_id` FROM `delta_treatment_area_list`"),
                            gta_sql_get_value("SELECT DISTINCT `treatment_area_name` FROM `delta_treatment_area_list`")),
                          collapse="; "),", respectively. And only one treatment area per excel file is allowed. \n\n", sep="")
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
    error.msg=paste(error.msg,date.error('date.announced',NA.permitted = T),sep='')
    new.error=F
  }             
  
  # valid announced.removal.date
  tryCatch({date=format(as.Date(x=announced.removal.date),"%Y-%m-%d")},
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
    error.msg=paste(error.msg,date.error('announced.removal.date',NA.permitted = T),sep='')
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
    error.msg=paste(error.msg,date.error('date.implemented'),sep='')
    new.error=F
  }
  
  #no restriction to source yet
  # state.act.source
  
  # source.official
  unrec.source.off=NULL
  if(!all(tolower(source.official) %in% c('yes','no','true','false','t','f'))){
    fatal=T
    unrec.source.off=source.official[which(!tolower(source.official) %in% c('yes','no','true','false','t','f'))]
    unrec.source.off=unique(unrec.source.off)
    error.msg=paste(error.msg,
                    "The field source official was rejected. Permissible values are True, T, False, F, Yes, No",
                    sep='')
  }
  
  # author
  unrec.author=NULL
  if(!all(tolower(author) %in% tolower(c(gta_sql_get_value("SELECT DISTINCT `user_login` FROM `gta_user_log`"),
                        gta_sql_get_value("SELECT DISTINCT `user_id` FROM `gta_user_log`")))) | length(unique(author))!=1){
    fatal=T
    unrec.author=author[which(!tolower(author) %in% tolower(c(gta_sql_get_value("SELECT DISTINCT `user_login` FROM `gta_user_log`"),
                                                              gta_sql_get_value("SELECT DISTINCT `user_id` FROM `gta_user_log`"))))]
    unrec.author=unique(unrec.author)
    error.msg=paste(error.msg,
                    "Author identification is invalid, please enter either your login name (firstname.surname), or your user identification number. Note: one author maximum per excel entry is authorized \n\n",
                    sep='')
  }

  # intervention.type
  unrec.int.type=NULL
  if(!all(tolower(intervention.type) %in% tolower(c(gta_sql_get_value("SELECT DISTINCT `intervention_type_id` FROM `gta_intervention_type_list`"),
                                   gta_sql_get_value("SELECT DISTINCT `intervention_type` FROM `gta_intervention_type_list`"))))){
    fatal=T
    unrec.int.type=intervention.type[which(!tolower(intervention.type) %in% tolower(c(gta_sql_get_value("SELECT DISTINCT `intervention_type_id` FROM `gta_intervention_type_list`"),
                                                                                      gta_sql_get_value("SELECT DISTINCT `intervention_type` FROM `gta_intervention_type_list`"))))]
    unrec.int.type=unique(unrec.int.type)
    error.msg=paste(error.msg,
                    "Intervention type is invalid. \n\n",
                    sep='')
    
  }
  
  # affected.flow
  unrec.aff.flow=NULL
  if(!all(tolower(affected.flow) %in% tolower(c(gta_sql_get_value("SELECT DISTINCT `affected_flow_id` FROM `gta_affected_flow_list`"),
                               gta_sql_get_value("SELECT DISTINCT `affected_flow` FROM `gta_affected_flow_list`"))))){
    fatal=T
    unrec.aff.flow=affected.flow[which(!tolower(affected.flow) %in% tolower(c(gta_sql_get_value("SELECT DISTINCT `affected_flow_id` FROM `gta_affected_flow_list`"),
                                                                              gta_sql_get_value("SELECT DISTINCT `affected_flow` FROM `gta_affected_flow_list`"))))]
    unrec.aff.flow=unique(unrec.aff.flow)
    error=paste0("Either the id or the full name are permissibles for affected.flows: ",
                 paste(gta_sql_get_value("SELECT DISTINCT `affected_flow_id` FROM `gta_affected_flow_list`"),collapse='; '),
                 ' or ',
                 paste(gta_sql_get_value("SELECT DISTINCT `affected_flow` FROM `gta_affected_flow_list`"),collapse='; '),
                 ' respectively. \n\n'
    )
    error.msg=paste(error.msg,
                    error,
                    sep='')
    rm(error)
  }
  
  # implementing jurisdiction
  unrec.impl=NULL
  if(!all(tolower(implementing.jurisdiction) %in% tolower(c(gta_sql_get_value("SELECT DISTINCT `un_code` FROM `gta_jurisdiction_list`"),
                                           gta_sql_get_value("SELECT DISTINCT `jurisdiction_name` FROM `gta_jurisdiction_list`"),
                                           gta_sql_get_value("SELECT DISTINCT `jurisdiction_group_name` FROM `gta_jurisdiction_group_list`"))))){
    fatal=T
    unrec.impl=implementing.jurisdiction[which(!tolower(implementing.jurisdiction) %in% tolower(c(gta_sql_get_value("SELECT DISTINCT `un_code` FROM `gta_jurisdiction_list`"),
                                                                                    gta_sql_get_value("SELECT DISTINCT `jurisdiction_name` FROM `gta_jurisdiction_list`"),
                                                                                    gta_sql_get_value("SELECT DISTINCT `jurisdiction_group_name` FROM `gta_jurisdiction_group_list`"))))]
    unrec.impl=unique(unrec.impl)
    error.msg=paste(error.msg,
                    "Implementing jurisdiction was rejected. Please enter the implementing.jurisdiction's un code or its name. Country groups are only accepted with their name. Multiple entries in a single cell are permissible, but must be separated by a semi-colon (;)",
                    # 'The following went unrecognized: ', paste(unique(unrec.impl), collapse = '; '),
                    sep='')
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
    error.msg=paste(error.msg,date.error('implementer.end.date',NA.permitted = T),sep='')
    new.error=F
  } 
  
  # implementation.level
  unrec.impl.level=NULL
  if(!all(tolower(implementation.level) %in% tolower(c(gta_sql_get_value("SELECT DISTINCT `implementation_level_id` FROM `gta_implementation_level_list`"),
                                      gta_sql_get_value("SELECT DISTINCT `implementation_level_name` FROM `gta_implementation_level_list`"))))){
    fatal=T
    unrec.impl.level=implementation.level[which(!tolower(implementation.level) %in% tolower(c(gta_sql_get_value("SELECT DISTINCT `implementation_level_id` FROM `gta_implementation_level_list`"),
                                                                                              gta_sql_get_value("SELECT DISTINCT `implementation_level_name` FROM `gta_implementation_level_list`"))))]
    unrec.impl.level=unique(unrec.impl.level)
    error=paste0("Either the id or the full name are permissibles for implementation level: ",
                 paste(gta_sql_get_value("SELECT DISTINCT `implementation_level_id` FROM `gta_implementation_level_list`"),collapse='; '),
                 ' or ',
                 paste(gta_sql_get_value("SELECT DISTINCT `implementation_level_name` FROM `gta_implementation_level_list`"),collapse='; '),
                 ' ,respectively. \n\n')
    error.msg=paste(error.msg,
                    error,
                    sep='')
    rm(error)
  }
  
  # eligible.firms
  unrec.elig.firms=NULL
  if(!all(tolower(eligible.firms) %in% tolower(c(gta_sql_get_value("SELECT DISTINCT `eligible_firms_id` FROM `gta_eligible_firms_list`"),
                                gta_sql_get_value("SELECT DISTINCT `eligible_firms_name` FROM `gta_eligible_firms_list`"))))){
    fatal=T
    unrec.elig.firms=eligible.firms[which(!tolower(eligible.firms) %in% tolower(c(gta_sql_get_value("SELECT DISTINCT `eligible_firms_id` FROM `gta_eligible_firms_list`"),
                                                                                  gta_sql_get_value("SELECT DISTINCT `eligible_firms_name` FROM `gta_eligible_firms_list`"))))]
    unrec.elig.firms=unique(unrec.elig.firms)
    error=paste0("Either the id or the full name are permissibles for eligible.firms: ",
                 paste(gta_sql_get_value("SELECT DISTINCT `eligible_firms_id` FROM `gta_eligible_firms_list`"),collapse='; '),
                 ' or',
                 paste(gta_sql_get_value("SELECT DISTINCT `eligible_firms_name` FROM `gta_eligible_firms_list`"),collapse='; '),
                 ' ,respectively.')
    error.msg=paste(error.msg,
                    error,
                    sep='')
    rm(error)
  }
  
  unrec.aff.code=c()
  # affected.code
  if(any(is.na(as.numeric(gsub('\\D','',affected.code))))){
    fatal=T
    unrec.aff.code=as.numeric(gsub('\\D','',affected.code))
    unrec.aff.code=unrec.aff.code[is.na(unrec.aff.code)]
    error='Only codes are permissible values for affected.code, either in character or numeric format, 1234.24 or 123424 are permissible. \n\n'
    error.msg=paste(error.msg,
                    error,
                    sep='')
    rm(error)
  } 
  
  # I don't allow for ids here, the added flexibility doesn't serve much 
  # affected.code.type
  if(!all(tolower(affected.code.type) %in% tolower(gta_sql_get_value("SELECT DISTINCT `code_type_name` FROM `delta_code_type_list`")))){
    fatal=T
    unrec.aff.code.type=affected.code.type[which(!tolower(affected.code.type) %in% tolower(gta_sql_get_value("SELECT DISTINCT `code_type_name` FROM `delta_code_type_list`")))]
    error=paste0("Please enter the affected.code.type's name: ",
                 paste(gta_sql_get_value("SELECT DISTINCT `code_type_name` FROM `delta_code_type_list`"), collapse='; '),
                 '. \n\n')
    error.msg=paste(error.msg,
                    error,
                    sep='')
    rm(error)
  }
  
  df.affected.code=data.frame(affected.code=as.numeric(gsub('\\D','',affected.code)),
                              affected.code.type=affected.code.type)

  if(nrow(subset(df.affected.code, !nchar(affected.code) %in% c(2,3) & affected.code.type=='cpc'))>0){
    fatal=T
    unrec.aff.code=c(unrec.aff.code,subset(df.affected.code, !nchar(affected.code) %in% c(2,3) & affected.code.type=='cpc')$affected.code)
    error.msg=paste(error.msg,
                    "Cpc codes must be given in a three digit format. Multiple entries in a single cell are permissible, but must be separated by a semi-colon (;). \n\n",
                    sep='')
  }
  
  if(nrow(subset(df.affected.code, (nchar(affected.code)<3 | nchar(affected.code)>6) & affected.code.type=='hs'))>0){
    fatal=T
    unrec.aff.code=c(unrec.aff.code,subset(df.affected.code, (nchar(affected.code)<3 | nchar(affected.code)>6) & affected.code.type=='hs')$affected.code)
    error.msg=paste(error.msg,
                    "Hs codes must be between 4(3) and 6(5) digits long. Multiple entries in a single cell are permissible, but must be separated by a semi-colon (;). \n\n",
                    sep='')
  }
  

  hs.codes=subset(df.affected.code, affected.code.type=='hs')$affected.code
  if(is.null(gta_hs_code_check(hs.codes))){
    fatal=T
    error <- capture.output({
      gta_hs_code_check(hs.codes)
    })
    error=sub("^[^:]*", "", error)  
    error=gsub(':|"|\\\\','',error)
    unrec.aff.code=c(unrec.aff.code,str_trim(unlist(str_split(error,','))))
    error=paste0('Affected.code was rejected, some hs code(s) returned no match',
                 '. Multiple entries in a single cell are permissible, but must be separated by a semi-colon (;). \n\n')
    error.msg=paste(error.msg,
                    error,
                    sep='')
    rm(error)
  }  
  
  cpc.codes=subset(df.affected.code, affected.code.type=='cpc')$affected.code
  codes.checked=gta_cpc_code_check(cpc.codes)
  if(is.null(codes.checked)|is.factor(codes.checked)){
    fatal=T
    error <- capture.output({
      gta_cpc_code_check(cpc.codes)
    })
    error=sub("^[^:]*", "", error[1])  
    error=gsub(':|"|\\\\','',error)
    unrec.aff.code=c(unrec.aff.code,str_trim(unlist(str_split(error,','))))
    error=paste0('Affected.code was rejected, some cpc code(s) returned no match',
                 '. Multiple entries in a single cell are permissible, but must be separated by a semi-colon (;). \n\n')
    error.msg=paste(error.msg,
                    error,
                    sep='')
    rm(error)
  }  
  
  unrec.aff.code=unique(unrec.aff.code)
  
  if(any(dangerous.cpc %in% unique(subset(df.affected.code, affected.code.type=='cpc')$affected.code))){
    error=paste(unique(subset(df.affected.code, affected.code %in% dangerous.cpc & affected.code.type=='cpc')$affected.code),
                collapse = '; ')
    error=paste('The following cpc codes are ambiguously matched on both a two digit and three digit level: ',
                error,
                'As a reminder, only three digit codes are permissible, please ensure that they were intended as a three digit cpc code! \n\n')

    error.msg=paste(error.msg,
                    error,
                    sep='')
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
    error.msg=paste(error.msg,date.error('affected.code.end.date',NA.permitted = T),sep='')
    new.error=F
  } 
  
  ## SHOULD I ADD RESTRICTIONS HERE? PERCENT BETWEEN 0-100 ETC?
  # treatment.value
  unrec.treatment.value=NULL
  if(any(is.na(as.numeric(treatment.value)))){
    fatal=T
    unrec.treatment.value=as.numeric(treatment.value)
    unrec.treatment.value=unrec.treatment.value[is.na(unrec.treatment.value)]
    unrec.treatment.value=unique(unrec.treatment.value)
    error.msg=paste(error.msg,
                    "Only numeric input is permissible for treatment.values. Decimals must be deliniated by periods (.). \n\n",
                    sep='')
  }
  
  # treatment.unit
  unrec.treatment.unit=NULL
  if(!all(tolower(treatment.unit) %in% tolower(c(gta_sql_get_value("SELECT DISTINCT `level_unit_id` FROM `gta_unit_list`"),
                                gta_sql_get_value("SELECT DISTINCT `level_unit` FROM `gta_unit_list`"))))){
    fatal=T
    unrec.treatment.unit=treatment.unit[which(!tolower(treatment.unit) %in% tolower(c(gta_sql_get_value("SELECT DISTINCT `level_unit_id` FROM `gta_unit_list`"),
                                                                                      gta_sql_get_value("SELECT DISTINCT `level_unit` FROM `gta_unit_list`"))))]
    unrec.treatment.unit=unique(unrec.treatment.unit)
    error=paste("Treatment unit was rejected. Permissible values are unit ids, or their names: ", 
                paste(gta_sql_get_value("SELECT DISTINCT `level_unit` FROM `gta_unit_list`"), collapse='; '),
                "\n\n")
    error.msg=paste(error.msg,
                    error,
                    sep='')
    rm(error)
  }
  
  # treatment.code.official
  unrec.treatment.code.off=NULL
  if(!all(tolower(treatment.code.official) %in% c('yes','no','true','false','t','f'))){
    fatal=T
    unrec.treatment.code.off=treatment.code.official[which(!tolower(treatment.code.official) %in% c('yes','no','true','false','t','f'))]
    unrec.treatment.code.off=unique(unrec.treatment.code.off)
    error.msg=paste(error.msg,
                    "The field treatment code official was rejected. Permissible values are True, T, False, F, Yes, No. \n\n",
                    sep='')
  }
  
  
  # affected.country
  unrec.aff.cty=NULL
  if(!all(is.na(affected.country) | tolower(affected.country) %in% 
          tolower(c(gta_sql_get_value("SELECT DISTINCT `un_code` FROM `gta_jurisdiction_list`"),
            gta_sql_get_value("SELECT DISTINCT `jurisdiction_name` FROM `gta_jurisdiction_list`"),
            gta_sql_get_value("SELECT DISTINCT `jurisdiction_group_name` FROM `gta_jurisdiction_group_list`"))))){
    fatal=T
    unrec.aff.cty=affected.country[which(!tolower(affected.country) %in% tolower(c(gta_sql_get_value("SELECT DISTINCT `un_code` FROM `gta_jurisdiction_list`"),
                                                                                   gta_sql_get_value("SELECT DISTINCT `jurisdiction_name` FROM `gta_jurisdiction_list`"),
                                                                                   gta_sql_get_value("SELECT DISTINCT `jurisdiction_group_name` FROM `gta_jurisdiction_group_list`"))))]
    unrec.aff.cty=unique(unrec.aff.cty)
    error.msg=paste(error.msg,
                    "Affected country was rejected. Please enter the affected.country's un code or it's name. \n\n",
                    sep='')
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
    error.msg=paste(error.msg,date.error('affected.country.end.date',NA.permitted = T),sep='')
    new.error=F
  } 
  
  # framework,name
  unrec.framework=NULL
  if(!all(is.na(framework.name) | tolower(framework.name) %in% 
          tolower(c(gta_sql_get_value("SELECT DISTINCT `framework_id` FROM `gta_framework_log`"),
            gta_sql_get_value("SELECT DISTINCT `framework_name` FROM `gta_framework_log`"))))){
    fatal=T
    unrec.framework=framework.name[which(!tolower(framework.name) %in% tolower(c(gta_sql_get_value("SELECT DISTINCT `framework_id` FROM `gta_framework_log`"),
                                                                                 gta_sql_get_value("SELECT DISTINCT `framework_name` FROM `gta_framework_log`"))))]
    unrec.framework=unique(unrec.framework)
    error.msg=paste(error.msg,
                    "Please enter a valid framework(regime) name or id. \n\n",
                    sep='')
  }
  

  unrecognized.variables=list(unrec.aff.code,
                             unrec.aff.cty,
                             unrec.aff.flow,
                             unrec.author,
                             unrec.elig.firms,
                             unrec.framework,
                             unrec.impl,
                             unrec.impl.level,
                             unrec.int.type,
                             unrec.source.off,
                             unrec.treatment.code.off,
                             unrec.treatment.unit,
                             unrec.treatment.value,
                             unrec.treatment.area)
  if(any(!sapply(unrecognized.variables,is.null))){
    unrecognized.variables =data.frame(processing.id=1:max(unlist(rapply(unrecognized.variables, length, how="list"))))
    
    unrecognized.variables$implementing.jurisdiction=unrec.impl
    unrecognized.variables$affected.country=unrec.aff.cty
    unrecognized.variables$affected.code=unrec.aff.code
    unrecognized.variables$affected.flow=unrec.aff.flow
    unrecognized.variables$eligible.firms=unrec.elig.firms
    unrecognized.variables$implementation.level=unrec.impl.level
    unrecognized.variables$intervention.type=unrec.int.type
    unrecognized.variables$framework.name=unrec.framework
    unrecognized.variables$source.official=unrec.source.off
    unrecognized.variables$treatment.code.official=unrec.treatment.code.off
    unrecognized.variables$treatment.unit=unrec.treatment.unit
    unrecognized.variables$treatment.value=unrec.treatment.value
    unrecognized.variables$author=unrec.author
    unrecognized.variables$treatment.area=unrec.treatment.area
    unrecognized.variables$processing.id=NULL
  }
  
  #remove html break if at end or start
  error.msg=sub("^\n\n", "", error.msg)
  error.msg=sub("\n\n$", "", error.msg)
  return(list(fatal=fatal,error.msg=error.msg,unrecognized.variables=unrecognized.variables))
}


