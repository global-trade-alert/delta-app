gta_delta_input_ids=function(
  input.df=NULL
){
  
  
  
  
  
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
  
  library(gtalibrary)
  library(gtasql)
  library(pool)
  library(RMariaDB)
  library(plyr)
  library(zoo)
  library(plyr)
  #gta_sql_pool_open(table.prefix = "delta_" )
  
  output=data.frame(processing.id=1:nrow(input))
  
  ##implementing.jurisdiction.id
  output$implementing.jurisdiction=implementing.jurisdiction
  # sql=do.call('sprintf', as.list(c("SELECT jurisdiction_id, jurisdiction_name, un_code FROM gta_jurisdiction_list WHERE (un_code IN (%s) OR lower(jurisdiction_name) IN (%s))", 
  #             rep(toString(sprintf("'%s'",tolower(implementing.jurisdiction))), 2))))
  # temp=gta_sql_get_value(sql)
  # output$implementing.jurisdiction.id=mapvalues(tolower(implementing.jurisdiction),
  #                                               tolower(c(temp$un.code,temp$jurisdiction.name)),
  #                                               c(temp$jurisdiction.id,temp$jurisdiction.id))
   
  ##treatment.value
  output$treatment.value=as.numeric(treatment.value)
  
  ##treatment.code
  output$treatment.code=as.numeric(gsub('\\D','',affected.code))
  
  ##date.announced
  output$date.announced=as.Date(date.announced)
  
  ##announced.removal.date
  output$announced.removal.date=as.Date(announced.removal.date)
  
  ##date.implemented
  output$date.implemented=as.Date(date.implemented)
  
  ##treatment.unit.id
  sql=do.call('sprintf', as.list(c("SELECT * FROM gta_unit_list WHERE (lower(level_unit) IN (%s) OR level_unit_id IN (%s))", 
                                   rep(toString(sprintf("'%s'",tolower(unique(treatment.unit)))), 2))))
  temp=gta_sql_get_value(sql)
  output$treatment.unit.id=as.numeric(mapvalues(tolower(treatment.unit),
                                     tolower(temp$level.unit),
                                     temp$level.unit.id))

  ##treatment.code.official
  output$treatment.code.official=as.logical(mapvalues(tolower(treatment.code.official),c('yes','no','t','f','true','false'),c('True','False','True','False','True','False')))
  
  ##treatment.area
  sql=do.call('sprintf', as.list(c("SELECT * FROM delta_treatment_area_list WHERE (treatment_area_id IN (%s) OR lower(treatment_area_name) IN (%s))", 
                                   rep(toString(sprintf("'%s'",tolower(unique(treatment.area)))), 2))))
  temp=gta_sql_get_value(sql)
  output$treatment.area=mapvalues(tolower(treatment.area),
                                  temp$treatment.area.id,
                                  tolower(temp$treatment.area.name))
  
  ##treatment.code.type
  output$treatment.code.type=tolower(affected.code.type)
  
  ##intervention.type.id
  sql=do.call('sprintf', as.list(c("SELECT intervention_type, intervention_type_id FROM gta_intervention_type_list WHERE (lower(intervention_type) IN (%s) OR intervention_type_id IN (%s))", 
                                   rep(toString(sprintf("'%s'",tolower(unique(intervention.type)))), 2))))
  temp=gta_sql_get_value(sql)
  output$intervention.type.id=as.numeric(mapvalues(tolower(intervention.type),
                                  tolower(temp$intervention.type),
                                  temp$intervention.type.id))

  ##state.act.source
  output$state.act.source=as.character(source)
  
  ##is.source.official
  output$is.source.official=as.logical(mapvalues(tolower(source.official),c('yes','no','t','f','true','false'),c('True','False','True','False','True','False')))
  
  ##author.id
  sql=do.call('sprintf', as.list(c("SELECT * FROM gta_user_log WHERE (lower(user_login) IN (%s) OR user_id IN (%s))", 
                                   rep(toString(sprintf("'%s'",tolower(unique(author)))), 2))))
  temp=gta_sql_get_value(sql)
  output$author.id=mapvalues(tolower(author),
                             tolower(temp$user.login),
                             temp$user.id)
  

  ##affected.flow.id
  sql=do.call('sprintf', as.list(c("SELECT * FROM gta_affected_flow_list WHERE (lower(affected_flow) IN (%s) OR affected_flow_id IN (%s))", 
                                   rep(toString(sprintf("'%s'",tolower(unique(affected.flow)))), 2))))
  temp=gta_sql_get_value(sql)
  output$affected.flow.id=as.numeric(mapvalues(tolower(affected.flow),
                                    tolower(temp$affected.flow),
                                    temp$affected.flow.id))
  ##implementation.level.id
  sql=do.call('sprintf', as.list(c("SELECT * FROM gta_implementation_level_list WHERE (lower(implementation_level_name) IN (%s) OR implementation_level_id IN (%s))", 
                                   rep(toString(sprintf("'%s'",tolower(unique(implementation.level)))), 2))))
  temp=gta_sql_get_value(sql)
  output$implementation.level.id=as.numeric(mapvalues(tolower(implementation.level),
                                           tolower(temp$implementation.level.name),
                                           temp$implementation.level.id))
  ##eligible.firms.id
  sql=do.call('sprintf', as.list(c("SELECT * FROM gta_eligible_firms_list WHERE (eligible_firms_id IN (%s) OR lower(eligible_firms_name) IN (%s))", 
                                   rep(toString(sprintf("'%s'",tolower(unique(eligible.firms)))), 2))))
  temp=gta_sql_get_value(sql)
  output$eligible.firms.id=as.numeric(mapvalues(tolower(eligible.firms),
                                     tolower(temp$eligible.firms.name),
                                     temp$eligible.firms.id))
  ##implementer.end.date
  output$implementer.end.date=as.Date(implementer.end.date)
  
  ##treatment.code.end.date
  output$treatment.code.end.date=as.Date(affected.code.end.date)
  
  ##nonmfn.affected.id
  output$nonmfn.affected=affected.country
  # sql=do.call('sprintf', as.list(c("SELECT jurisdiction_id, jurisdiction_name, un_code FROM gta_jurisdiction_list WHERE (un_code IN (%s) OR lower(jurisdiction_name) IN (%s))", 
  #                                  rep(toString(sprintf("'%s'",tolower(affected.country))), 2))))
  # temp=gta_sql_get_value(sql)
  # output$nonmfn.affected.id=mapvalues(tolower(affected.country),
  #                                     tolower(c(temp$un.code,temp$jurisdiction.name)),
  #                                     c(temp$jurisdiction.id,temp$jurisdiction.id))
  ##nonmfn.affected.end.date
  output$nonmfn.affected.end.date=as.Date(affected.country.end.date)
  
  ##framework.id
  sql=do.call('sprintf', as.list(c("SELECT framework_id, framework_name FROM gta_framework_log WHERE (lower(framework_name) IN (%s) OR framework_id IN (%s))", 
                                   rep(toString(sprintf("'%s'",tolower(unique(framework.name)))), 2))))
  temp=gta_sql_get_value(sql)
  
  if(nrow(temp)!=0){
    
    
    output$framework.id=mapvalues(tolower(framework.name),
                                  tolower(temp$framework.name),
                                  temp$framework.id )
    
  } else {
    output$framework.id=NA
  }
  
  ##framework.new.to.db
  # currently no "green light" has been implemented for this, I thought we wanted to not pursue this option for the time being?
  # framework.new.to.db=as.logical(mapvalues(tolower(framework.new.to.db),c('yes','no'),c('True','False')))
  
  output$processing.id=NULL

  return(output)
}  