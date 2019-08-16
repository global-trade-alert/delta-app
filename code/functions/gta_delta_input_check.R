gta_delta_add_state_act=function(
  treatment.area=NULL,
  date.announced=NULL,
  date.implemented=NULL,
  source=NULL,
  source.official=NULL,
  author=NULL,
  intervention.type=NULL,
  affected.flow=NULL,
  implementing.jurisdiction=NULL,
  implementer.end.date=NA,
  implementation.level=NULL,
  eligible.firms=NULL,
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
  ## intervention.type, affected.flow, implementation.level, eligible.firms,
  ## implementing.jurisdiction, affected.code, affected.code.type, treatment.value, treatment.unit, treatment.code.official
  ## date.implemented
  if(any(sapply(
    list(treatment.area, source, source.official, author, treatment.area, intervention.type, affected.flow, implementation.level, eligible.firms, 
         implementing.jurisdiction, affected.code, affected.code.type, treatment.value, treatment.unit, treatment.code.official, date.implemented),
    is.null
  ))){
    req.fields=list(treatment.area=treatment.area, source=source, source.official=source.official, author=author, treatment.area=treatment.area, 
                    intervention.type=intervention.type, affected.flow=affected.flow, implementation.level=implementation.level, eligible.firms=eligible.firms, 
                    implementing.jurisdiction=implementing.jurisdiction, affected.code=affected.code, affected.code.type=affected.code.type, treatment.value=treatment.value,
                    treatment.unit=treatment.unit, treatment.code.official=treatment.code.official, date.implemented=date.implemented)
    stop(paste('The following parameters are required and are currently missing:', paste(names(req.fields[sapply(req.fields,is.null)]),collapse='; ')))
  }
  
  ### Parameter correctness check
  ## Length checks
  
  
  ## Interpretability checks

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
    stop("Permissible values are True, T, False, F, Yes, No")
  }
  
  # author
  if(!author %in% c(gta_sql_get_value("SELECT DISTINCT `user_login` FROM `gta_user_log`")$user.login,
                    gta_sql_get_value("SELECT DISTINCT `user_id` FROM `gta_user_log`")$user.id)){
    stop("Author identification is invalid, please enter either your login name (firstname.surname), or your user identification number")
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
  if(!all(intervention.type %in% c(gta_sql_get_value("SELECT DISTINCT `intervention_type_id` FROM `gta_intervention_type_list`")$intervention.type.id,
                                   gta_sql_get_value("SELECT DISTINCT `intervention_type` FROM `gta_intervention_type_list`")$intervention.type))){
    stop(paste0("Either the id number or the full name are permissibles values for intervention types: ",
                paste(gta_sql_get_value("SELECT DISTINCT `intervention_type` FROM `gta_intervention_type_list`")$intervention.type,collapse='; ')
    ))
  }
  
  # affected.flow
  if(!all(affected.flow %in% c(gta_sql_get_value("SELECT DISTINCT `affected_flow_id` FROM `gta_affected_flow_list`")$affected.flow.id,
                    gta_sql_get_value("SELECT DISTINCT `affected_flow` FROM `gta_affected_flow_list`")$affected.flow))){
    stop(paste0("Either the id or the full name are permissibles for affected.flows: ",
                paste(gta_sql_get_value("SELECT DISTINCT `affected_flow_id` FROM `gta_affected_flow_list`")$affected.flow.id,collapse='; '),
                'or',
                paste(gta_sql_get_value("SELECT DISTINCT `affected_flow` FROM `gta_affected_flow_list`")$affected.flow,collapse='; '),
                'respectively.'
                ))
  }
  
  # implementing jurisdiction
  if(!all(implementing.jurisdiction %in% c(gta_sql_get_value("SELECT DISTINCT `un_code` FROM `gta_jurisdiction_list`")$un.code,
                                   gta_sql_get_value("SELECT DISTINCT `name` FROM `gta_jurisdiction_list`")$name))){
    stop("Please enter the implementing.jurisdiction's un_code or it's name")
  }
  
  # optional implementer.end.date
  if(!is.na(implementer.end.date)){
    
    tryCatch({format(as.Date(x=implementer.end.date, "%Y-%m-%d"),"%Y-%m-%d")},
             error=function(e){stop('Please enter a valid date format (yyyy-mm-dd: 2019-02-30)')},
             finally={
               if(!str_sub(as.character(format(as.Date(x=implementer.end.date, "%Y-%m-%d"),"%Y-%m-%d")),1,4) %in% as.character(1900:2100)){
                 stop('Please enter a valid date format (yyyy-mm-dd: 2019-02-30)')
               }
               
               if(is.na(as.Date(x=implementer.end.date, "%Y-%m-%d"))){
                 stop('Please enter a valid date format (yyyy-mm-dd: 2019-02-30)')
               } 
             })
  }
  
  # implementation.level
  if(!all(implementation.level %in% c(gta_sql_get_value("SELECT DISTINCT `gta_implementation_level_id` FROM `gta_implementation_level_list`")$gta.implementation.level.id,
                               gta_sql_get_value("SELECT DISTINCT `gta_implementation_level_name` FROM `gta_implementation_level_list`")$gta.implementation.level.name))){
    stop(paste0("Either the id or the full name are permissibles for affected.flows: ",
                paste(gta_sql_get_value("SELECT DISTINCT `gta_implementation_level_id` FROM `gta_implementation_level_list`")$gta.implementation.level.id,collapse='; '),
                'or',
                paste(gta_sql_get_value("SELECT DISTINCT `gta_implementation_level_name` FROM `gta_implementation_level_list`")$gta.implementation.level.name,collapse='; '),
                'respectively.'
    ))
  }
  
  # eligible.firms
  if(!all(eligible.firms.name %in% c(gta_sql_get_value("SELECT DISTINCT `eligible_firms_id` FROM `gta_eligible_firms_list`")$eligible.firms.id,
                                     gta_sql_get_value("SELECT DISTINCT `eligible_firms_name` FROM `gta_eligible_firms_list`")$eligible.firms.name))){
    stop(paste0("Either the id or the full name are permissibles for eligible.firms: ",
                paste(gta_sql_get_value("SELECT DISTINCT `eligible_firms_id` FROM `gta_eligible_firms_list`")$eligible.firms.id,collapse='; '),
                'or',
                paste(gta_sql_get_value("SELECT DISTINCT `eligible_firms_name` FROM `gta_eligible_firms_list`")$eligible.firms.name,collapse='; '),
                'respectively.'
    ))
  }
  
  # affected.code
  if(any(is.na(as.numeric(gsub('\\D','',affected.code))))){
    stop('Only codes are permissible values for affected.code, either in character or numeric format, 1234.24 or 123424 are permissible')
  } 
  
  # affected.code.type
  if(!all(affected.code.type %in% c(gta_sql_get_value("SELECT DISTINCT `code_type_id` FROM `delta_code_type_list`")$code.type.id,
                                    gta_sql_get_value("SELECT DISTINCT `code_type_name` FROM `delta_code_type_list`")$code.type.name))){
    stop("Please enter the affected.code.type's name or its id: 1 or hs; 2 or cpc")
  }

  affected.code=c(323,'4.323',2342,256343)
  affected.code.type=c('cpc','cpc','hs','hs')
  id=1:4
  df.affected.code=data.frame(affected.code=as.numeric(gsub('\\D','',affected.code)),
                              affected.code.type=affected.code.type,
                              id=id)
  
  if(nrow(subset(df.affected.code, nchar(affected.code)!=3 & affected.code.type=='cpc'))>0){
    stop("Cpc codes must be given in a three digit format")
  }
  
  if(nrow(subset(df.affected.code, nchar(affected.code)<4 & nchar(affected.code)>6 & affected.code.type=='hs'))>0){
    stop("Hs codes must be between 4(3) and 6(5) digits long")
  }
   
  ## TBA affected.code
  # is it numeric/integer of a length that makes sense? 
  # also, please expand all HS codes with less than 6-digits
  ######## and/or cpc codes with less than 3-digit: We decided only 3 digits allowed as entry
  # After this section, affected.code should only include the correct HS/CPC digit length (6/3)

  #if someone enters 8 digits hs? We throw away the information completely right?  
  
  if(nrow(subset(df.affected.code, nchar(affected.code) < 6 & affected.code.type=='hs'))>0){
    
    coarse.idx=which(nchar(df.affected.code$affected.code) < 6 & df.affected.code$affected.code.type=='hs')
    coarse.code.update=data.frame(coarse.code=df.affected.code$affected.code[coarse.idx],
                                  coarse.code.type=df.affected.code$affected.code.type[coarse.idx],
                                  stringsAsFactors = F)

    rm(coarse.idx)
  } else {
    coarse.code.update=data.frame()
  }
  
  df.affected.code$affected.code=str_sub(df.affected.code$affected.code,1,6)
  
  
  
  # optional affected.code.end.date
  if(!is.na(affected.code.end.date)){
    
    tryCatch({format(as.Date(x=affected.code.end.date, "%Y-%m-%d"),"%Y-%m-%d")},
             error=function(e){stop('Please enter a valid date format (yyyy-mm-dd: 2019-02-30)')},
             finally={
               if(!str_sub(as.character(format(as.Date(x=affected.code.end.date, "%Y-%m-%d"),"%Y-%m-%d")),1,4) %in% as.character(1900:2100)){
                 stop('Please enter a valid date format (yyyy-mm-dd: 2019-02-30)')
               }
               
               if(is.na(as.Date(x=affected.code.end.date, "%Y-%m-%d"))){
                 stop('Please enter a valid date format (yyyy-mm-dd: 2019-02-30)')
               } 
             })
  }
  
  
  # treatment.values
  if(any(is.na(as.numeric(treatment.values)))){
    stop("Only numeric input is permissible for treatment.values")
  }
  
  # treatment.unit
  if(!all(treatment.unit %in% c(gta_sql_get_value("SELECT DISTINCT `level_unit_id` FROM `gta_unit_list`")$level.unit.id,
                                gta_sql_get_value("SELECT DISTINCT `level_unit` FROM `gta_unit_list`")$level.unit))){
    stop("Permissible values for treatment.unit are unit ids, or their names: ", 
         paste(gta_sql_get_value("SELECT DISTINCT `level_unit` FROM `gta_unit_list`")$level.unit, collapse='; '))
  }
  
  # treatment.code.official
  if(!all(tolower(treatment.code.official) %in% c('y','n','yes','no','t','f','true','false'))){
    stop("Permissible values are True, T, False, F, Yes, y, No, n")
  }
  
  
  
  # affected.country
  if(!all(affected.country %in% c(gta_sql_get_value("SELECT DISTINCT `un_code` FROM `gta_jurisdiction_list`")$un.code,
                                   gta_sql_get_value("SELECT DISTINCT `name` FROM `gta_jurisdiction_list`")$name))){
    stop("Please enter the affected.country's un code or it's name")
  }
  
  # optional affected.country.end.date
  if(!is.na(affected.country.end.date)){
    
    tryCatch({format(as.Date(x=affected.country.end.date, "%Y-%m-%d"),"%Y-%m-%d")},
             error=function(e){stop('Please enter a valid date format (yyyy-mm-dd: 2019-02-30)')},
             finally={
               if(!str_sub(as.character(format(as.Date(x=affected.country.end.date, "%Y-%m-%d"),"%Y-%m-%d")),1,4) %in% as.character(1900:2100)){
                 stop('Please enter a valid date format (yyyy-mm-dd: 2019-02-30)')
               }
               
               if(is.na(as.Date(x=affected.country.end.date, "%Y-%m-%d"))){
                 stop('Please enter a valid date format (yyyy-mm-dd: 2019-02-30)')
               } 
             })
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
  
  # regime.name
  if(!all(affected.country %in% c(gta_sql_get_value("SELECT DISTINCT `un_code` FROM `gta_jurisdiction_list`")$un.code,
                                  gta_sql_get_value("SELECT DISTINCT `name` FROM `gta_jurisdiction_list`")$name))){
    stop("Please enter the affected.country's un code or it's name")
  }
  
  
  if(regime.new.to.db){
    
    # create new regime ID
    regime.log.update=data.frame(regime.id=NA,
                                 regime.name=regime.name,
                                 user.id=this.author.id,
                                 stringsAsFactors = F)
    
    this.regime.id=gta_sql_append_table(append.table = "regime.log",
                                        append.by.df = "regime.log.update",
                                        get.id = "regime.id")
    rm(regime.log.update)
  } 
  
  
}