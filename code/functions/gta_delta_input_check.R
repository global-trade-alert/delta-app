gta_delta_add_state_act=function(
  treatment.area=NULL,
  date.announced=NULL,
  date.implemented=NULL,
  source=NULL,
  source.official=NULL,
  author=NULL,
  intervention.type=NULL,
  implementing.jurisdiction=NULL,
  implementer.end.date=NA,
  affected.code=NULL,
  affected.code.type=NULL,
  affected.code.end.date=NA,
  treatment.value=NULL, 
  treatment.unit=NULL, 
  treatment.code.official=NULL,
  affected.country=NULL,
  affected.country.end.date=NA,
  regime.name=NULL,
  regime.new.to.db=NULL
){
  # Setup
  treatment.tables=c("tariff","subsidy","investment","migration")
  
  library(lubridate)
  library(stringr)
  #### basic checks that need to hold up before touching the SQL datbase
  
  ### Parameter completeness check
  ## TBA write check that ensures required fields are filled
  ## if not, all non-specified fields are reported in the same error message
  ## required fields are 
  ## treatment.area, source, source.official, author, treatment.area, 
  ## intervention.type, affected.flow, implementation.level, eligible firms,
  ## implementing.jurisdiction, affected.code, affected.code.type, treatment.value, treatment.unit, treatment.code.official
  ## date.implemented
  if(any(sapply(
    list(treatment.area, source, source.official, author, treatment.area, intervention.type, 
         implementing.jurisdiction, affected.code, affected.code.type, treatment.value, treatment.unit, treatment.code.official, date.implemented),
    is.null
  ))){
    req.fields=list(treatment.area=treatment.area, source=source, source.official=source.official, author=author, treatment.area=treatment.area, 
                    intervention.type=intervention.type, implementing.jurisdiction=implementing.jurisdiction, affected.code=affected.code, affected.code.type=affected.code.type,
                    treatment.value=treatment.value, treatment.unit=treatment.unit, treatment.code.official=treatment.code.official, date.implemented=date.implemented)
    stop(paste('The following parameters are required and are currently missing:', paste(names(req.fields[sapply(req.fields,is.null)]),collapse='; ')))
  }
  
  ### Parameter correctness check
  ## These checks ensure that the specified values are interpretable
  
  # valid treatment.area
  if(!all(treatment.area %in% treatment.tables)){
    stop(paste("Your treatment areas should be one of the following: ", paste(treatment.tables, collapse="; "),".", sep=""))
  }
  
  # valid date.announced
  tryCatch({format(as.Date(x=date.announced, "%Y-%m-%d"),"%Y-%m-%d")},
           error=function(e){stop('Please enter a valid date format (yyyy-mm-dd: 2019-02-30)')},
           finally={
             if(!str_sub(as.character(format(as.Date(x=date.announced, "%Y-%m-%d"),"%Y-%m-%d")),1,4) %in% as.character(1900:2100)){
               stop('Please enter a valid date format (yyyy-mm-dd: 2019-02-30)')
             }
             
             if(is.na(as.Date(x=date.announced, "%Y-%m-%d"))){
               stop('Please enter a valid date format (yyyy-mm-dd: 2019-02-30)')
             } 
           })
  
  
  # valid date.implemented
  tryCatch({format(as.Date(x=date.implemented, "%Y-%m-%d"),"%Y-%m-%d")},
           error=function(e){stop('Please enter a valid date format (yyyy-mm-dd: 2019-02-30)')},
           finally={
             if(!str_sub(as.character(format(as.Date(x=date.implemented, "%Y-%m-%d"),"%Y-%m-%d")),1,4) %in% as.character(1900:2100)){
               stop('Please enter a valid date format (yyyy-mm-dd: 2019-02-30)')
             }
             
             if(is.na(as.Date(x=date.implemented, "%Y-%m-%d"))){
               stop('Please enter a valid date format (yyyy-mm-dd: 2019-02-30)')
             } 
           })
  
  # source.official
  if(!all(tolower(source.official) %in% c('y','n','yes','no','t','f','true','false'))){
    stop(paste("You treatment areas should be one of the following: ", paste(treatment.tables, collapse="; "),".", sep=""))
  }
  
  # author
  if(!author %in% c(gta_sql_get_value("SELECT DISTINCT `user_login` FROM `gta_user_log`")$user.login,
                    gta_sql_get_value("SELECT DISTINCT `user_id` FROM `gta_user_log`")$user.id)){
    stop("Author identification is invalid, please enter either your login name (firstname.surname), or your user identification number")
  }
  
  if(!author %in% c(gta_sql_get_value("SELECT DISTINCT `user_login` FROM `gta_user_log`")$user.login,
                    gta_sql_get_value("SELECT DISTINCT `user_id` FROM `gta_user_log`")$user.id)){
    stop("Author identification is invalid, please enter either your login name (firstname.surname), or your user identification number")
  }
  
  
}