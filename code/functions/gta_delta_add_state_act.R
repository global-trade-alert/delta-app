
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
  
  
  #### basic checks that need to hold up before touching the SQL datbase

  ### Parameter completeness check
  ## TBA write check that ensures required fields are filled
  ## if not, all non-specified fields are reported in the same error message
  ## required fields are 
  ## treatment.area, source, source.official, author, treatment.area, 
  ## intervention.type, affected.flow, implementation.level, eligible firms,
  ## implementing.jurisdiction, affected.code, affected.code.type, treatment.value, treatment.unit, treatment.code.official
  ## date.implemented
  
  
  ### Parameter correctness check
  ## These checks ensure that the specified values are interpretable
  
  # valid treatment.area
  if(! treatment.area %in% treatment.tables){
    stop(paste("You treatment areas should be one of the following: ", paste(treatment.tables, collapse="; "),".", sep=""))
  }
  
  ## NOTE
  ## These parameters will be vectors in many cases.
  
  # date.announced (won't be a vector since I will only feed the function 1 state act at a time)
  if(is.na(as.Date(date.announced, "%Y-%m-%d"))){
    stop("Please specify the announcement date in YYY-mm-dd.")
  }
  
  # TBA source.official
  # source.official must be T or F

  ## TBA author, intervention.type, affected.flow, implementation.level, eligible firms, implementing.jurisdiction, affected.code.type, treatment.unit, affected.country (if specified)
  ## check the phrases against those in the relevent log/list-df
  ## store the ID using the following example
  ## this.author.id
  sql <- "SELECT user_id FROM user_log WHERE user_login = ?fromwhom;"
  query <- sqlInterpolate(pool, 
                          sql, 
                          fromwhom = author)
  
  this.author.id=gta_sql_get_value(query)
  rm(query)
  
  # please create
  # this.int.type.id
  # this.flow.id
  # this.impl.level.id
  # this.firms.id
  # this.implementer.id 
  # this.treatment.unit.id
  # this.affected.country.id (if specified)
  
  
  ## date.implemented
  ## could be a vector, check that all values are specified & interpretable as a date.
  
  ## TBA affected.code
  # is it numeric/integer of a length that makes sense? 
  # also, please expand all HS codes with less than 6-digits and/or cpc codes with less than 3-digit
  # After this section, affected.code should only include the correct HS/CPC digit length (6/3)
  # If you have to expand, please create
  if(there.is.at.least.one.coarse.code){
    coarse.code.update=data.frame(coarse.code=the.coarse.code.vector,
                                  coarse.code.type=the.coarse.code.type.vector,
                                  stringsAsFactors = F)
    
  } else {
    coarse.code.update=data.frame()
  }
  
  
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
  
   
  
  #### Writing to the database
  
  ## creating state act

  state.act.update=data.frame(date.announced=date.announced,
                              state.act.source=source,
                              is.source.official=source.official,
                              author=this.author.id,
                              creation.date=Sys.Date(),
                              stringsAsFactors = F)
  
  this.sa.id=gta_sql_append_table(append.table = "state.act.log",
                       append.by.df = "state.act.update",
                       get.id="state.act.id")
  
  rm(state.act.update)
  
  # intervention.log
  
  intervention.log.update=data.frame(state.act.id=this.sa.id,
                                     intervention.type.id=this.int.type.id,
                                     affected.flow.id=this.flow.id,
                                     implementation.level.id=this.impl.level.id,
                                     eligible.firms.id=this.firms.id,
                                     is.mfn=is.null(affected.country),
                              stringsAsFactors = F)
  
  this.intervention.id=gta_sql_append_table(append.table = "intervention.log",
                                  append.by.df = "intervention.log.update",
                                  get.id="intervention.id")
  
  rm(intervention.log.update)
  
  # intervention.implementer
  intervention.implementer.update=data.frame(intervention.id=this.intervention.id,
                                             implementing.jurisdiction.id=this.implementer.id,
                                             stringsAsFactors = F)
  
  gta_sql_append_table(append.table = "intervention.implementer",
                       append.by.df = "intervention.implementer.update")
  
  rm(intervention.implementer.update)
  
  # treatment.log
  treatment.log.update=data.frame(intervention.id=this.intervention.id,
                                  date.implemented=date.implemented,
                                  treatment.code=affected.code,
                                  treatment.code.type=affected.code.type,
                                  treatment.value=treatment.value,
                                  treatment.unit.id=this.treatment.unit.id,
                                  treatment.code.official=treatment.code.official,
                                  stringsAsFactors = F)
    
  gta_sql_append_table(append.table = paste(treatment.area,".log",sep=""),
                       append.by.df = "treatment.log.update")
  
  rm(treatment.log.update)
  
  # deviation.affected
  if(! is.null(affected.country)){
    deviation.affected.update=data.frame(intervention.id=this.intervention.id,
                                         affected.jurisdiction.id=this.affected.country.id,
                                         deviation.affected.end.date=deviation.affected.end.date,
                                         stringsAsFactors = F)
    
    gta_sql_append_table(append.table = "deviation.affected",
                         append.by.df = "deviation.affected.update")
    
    rm(deviation.affected.update)
  
  }
  
  # deviation.code
  
  if(affected.code.end.date[is.na(affected.code.end.date)==F]>0){
    
    end.code.position=which(is.na(affected.code.end.date)==F)
    
    end.dev.codes=affected.code[end.code.position]
    end.dev.code.type=affected.code.type[end.code.position]
    end.dev.code.date=affected.code.end.date[end.code.position]
    
    deviation.affected.update=data.frame(intervention.id=this.intervention.id,
                                         deviation.code=end.dev.codes,
                                         deviation.code.type=end.dev.code.type,
                                         deviation.code.end.date=end.dev.code.date,
                                         stringsAsFactors = F)
    
    gta_sql_append_table(append.table = "deviation.affected",
                         append.by.df = "deviation.affected.update")
    
    rm(deviation.affected.update,
       end.code.position,
       end.dev.codes,
       end.dev.code.type,
       end.dev.code.date)
    
  }
  
  # deviation.implementer
  
  if(implementer.end.date[is.na(implementer.end.date)==F]>0){
    
    end.imp.position=which(is.na(implementer.end.date)==F)
    
    end.dev.imp=this.implementer.id[end.imp.position]
    end.dev.imp.date=implementer.end.date[end.imp.position]
    
    deviation.implenenter.update=data.frame(intervention.id=this.intervention.id,
                                            deviation.implementer.id=end.dev.imp,
                                            deviation.implementer.end.date=end.dev.imp.date,
                                            stringsAsFactors = F)
    
    gta_sql_append_table(append.table = "deviation.implenenter",
                         append.by.df = "deviation.implenenter.update")
    
    rm(deviation.implenenter.update,
       end.imp.position,
       end.dev.imp,
       end.dev.imp.date)
    
  }
  

  ## Updating regime, if specified (regime.log & intervention.regime)
  if(!is.null(regime.name)){
    
    # add current intervention id's to intervention.regime.
    intervention.regime.update=data.frame(intervention.id=this.intervention.id,
                                          regime.id=this.regime.id,
                                          stringsAsFactors = F)
    
    
    gta_sql_append_table(append.table = "intervention.regime",
                         append.by.df = "intervention.regime.update")
    rm(intervention.regime.update)
  }
  
}  

