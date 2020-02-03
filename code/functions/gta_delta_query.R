# rm(list=ls())
# 
# library(gtalibrary)
# library(gtasql)
# library(RMariaDB)
# library(DBI)
# library(RMySQL)
# library(pool)
# library(stringr)
# 
# gta_setwd()
# source('17 Shiny/6 delta app/code/functions/gta_delta_get_jurisdiction_id.R')
# gta_setwd()
# 
# gta_sql_pool_open(table.prefix = 'delta_',
#                   db.title="ricardodev",
#                   db.host = gta_pwd("ricardodev")$host,
#                   db.name = gta_pwd("ricardodev")$name,
#                   db.user = gta_pwd("ricardodev")$user,
#                   db.password = gta_pwd("ricardodev")$password)
# 
# 
# input=readxl::read_excel('17 Shiny/6 delta app/data/query sample.xlsx')#[1:5,]
# 
# expand=F
# implementer=input$implementing.jurisdiction
# # cutoff.date=input$cutoff.date
# # cutoff.date[1:5]=NA
# cutoff.date=NA
# new.treatment.value=input$new.treatment.value#rep(NA,10)
# new.treatment.unit=input$new.treatment.unit#rep(NA,10)
# treatment.area=input$treatment.area
# treatment.code=875250
# treatment.code=as.numeric(plyr::mapvalues(treatment.code,10121,101))
# treatment.code.type=input$treatment.code.type
# affected.country=input$affected.jurisdiction
# affected.country[6:9]='MFN'
# db.connection='pool'
# user=29


gta_delta_query=function(
  expand=T,
  implementer=NULL,
  cutoff.date=NULL,
  new.treatment.value=NA,
  new.treatment.unit=NA,
  treatment.area=NULL,
  treatment.code=NULL,
  treatment.code.type=NULL,
  affected.country=NULL,
  db.connection='pool',
  user=NULL
){

  library(gtasql)
  library(plyr)
  library(data.table)

  
  treatment.code=as.numeric(treatment.code)
  req.var=list(implementer=implementer, 
               cutoff.date=cutoff.date,
               treatment.area=treatment.area, 
               treatment.code=treatment.code,
               treatment.code.type=treatment.code.type,
               affected.country=affected.country,
               db.connection=db.connection,
               user=user)
  
  
  if(any(sapply(req.var, is.null))){
    return(data.frame(temp.error=paste0('The following mandatory input is missing: ', paste0(names(req.var[sapply(req.var, is.null)]), collapse='; '))))
  }

  affected.country=mapvalues(affected.country, 'MFN',NA)
  
  if(all(is.na(new.treatment.value)) | length(new.treatment.value)==0){
    new.treatment.value=NA
    new.value.provided=F
  }
  
  if(all(is.na(new.treatment.unit)) | all(new.treatment.unit=='NA')){
    new.treatment.unit=NA
    new.value.provided=F
  }
  
  if(expand==T){
    query.table=expand.grid(implementer=implementer, 
                            cutoff.date=cutoff.date,
                            new.treatment.value=new.treatment.value,
                            new.treatment.unit=new.treatment.unit,
                            treatment.area=treatment.area, 
                            treatment.code=treatment.code,
                            treatment.code.type=treatment.code.type,
                            affected.country=affected.country)
  } else {
    query.table=data.frame(implementer=implementer, 
                            cutoff.date=cutoff.date,
                            new.treatment.value=new.treatment.value,
                            new.treatment.unit=new.treatment.unit,
                            treatment.area=treatment.area, 
                            treatment.code=treatment.code,
                            treatment.code.type=treatment.code.type,
                            affected.country=affected.country)  
  }
  
  #map NA dates to Sys.Date()
  query.table$cutoff.date=mapvalues(query.table$cutoff.date,NA,Sys.Date())
  query.table$cutoff.date=zoo::as.Date(query.table$cutoff.date)
  
  implementer.ids=gta_delta_get_jurisdiction_id(jurisdiction.name=unique(query.table$implementer),
                                                db.connection=db.connection)
  
  names(implementer.ids)=c("implementing.jurisdiction.id","implementer")
  query.table=merge(query.table, implementer.ids, by="implementer", all.x=T)  
  
  query.table$affected.country.id=NA
  got.aj=subset(query.table, is.na(affected.country)==F)
  
  if(nrow(got.aj)>0){
    
    got.aj$affected.country.id=NULL
    affected.ids=gta_delta_get_jurisdiction_id(jurisdiction.name=unique(got.aj$affected.country),
                                               db.connection=db.connection)
    
    names(affected.ids)=c("affected.country.id","affected.country")
    
    got.aj=merge(got.aj, affected.ids, by="affected.country", all.x=T)
    
    query.table=rbind(subset(query.table, is.na(affected.country)),
                     got.aj)
    
    rm(affected.ids)
    
  } else {
    
  }
  rm(got.aj)
  
  #expand coarse code inputs
  coarse=subset(query.table, (nchar(as.character(query.table$treatment.code)) < 5 & query.table$treatment.code.type=='hs'))
  
  if(nrow(coarse)>0){
    
    coarse$processing.id=1:nrow(coarse)
    
    expanded.output=data.frame()
    for(i in 1:nrow(coarse)){
      expanded.codes=gta_hs_code_check(coarse$treatment.code[i])
      if(length(expanded.codes)==0) expanded.codes =  coarse$treatment.code[i] # temporary fix, later will show popup with nonexistent provided codes
      expanded.output=rbind(expanded.output,
                            data.frame(processing.id=coarse$processing.id[i],
                                       treatment.code=as.double(expanded.codes),
                                       treatment.code.type="hs"))
    }
    
    coarse$treatment.code <- NULL
    coarse$treatment.code.type <- NULL
    
    coarse=merge(coarse, expanded.output, by="processing.id", all.x=T)
    coarse$processing.id <- NULL
    
    query.table=rbind(subset(query.table, !(nchar(as.character(query.table$treatment.code)) < 5 & query.table$treatment.code.type=='hs')),
                     subset(coarse, select=names(query.table)))
    
    rm(expanded.output, expanded.codes)
    
  }
  rm(coarse)
  
  #vintage conversion
  vintage=subset(query.table, (nchar(as.character(query.table$treatment.code)) < 7 & query.table$treatment.code.type=='hs'))
  
  if(nrow(vintage)>0){
    
    vintage$processing.id=1:nrow(vintage)
    
    expanded.output=data.frame()
    for(i in 1:nrow(vintage)){
      expanded.codes=gta_hs_vintage_converter(vintage$treatment.code[i], origin='any')
      if(length(expanded.codes)==0) expanded.codes =  vintage$treatment.code[i] # temporary fix, later will show popup with nonexistent provided codes
      expanded.output=rbind(expanded.output,
                            data.frame(processing.id=vintage$processing.id[i],
                                       treatment.code=as.double(expanded.codes),
                                       treatment.code.type="hs"))
    }
    
    vintage$treatment.code <- NULL
    vintage$treatment.code.type <- NULL
    
    vintage=merge(vintage, expanded.output, by="processing.id", all.x=T)
    vintage$processing.id <- NULL
    
    query.table=rbind(subset(query.table, !(nchar(as.character(query.table$treatment.code)) < 7 & query.table$treatment.code.type=='hs')),
                      subset(vintage, select=names(query.table)))
    
    rm(expanded.output, expanded.codes)
    
  }
  rm(vintage)
  
  
  query.codes=unique(query.table$treatment.code)
  #longer than 6 digits: create extra row for 6 digit code which will be filtered later depending on if the longer than 6 digit returned a result
  long.codes=subset(query.table, nchar(treatment.code)>6)
  long.codes$treatment.code[which(nchar(long.codes$treatment.code) %% 2 == 0)]=str_sub(long.codes$treatment.code[which(nchar(long.codes$treatment.code) %% 2 == 0)],1,6)
  long.codes$treatment.code[which(nchar(long.codes$treatment.code) %% 2 == 1)]=str_sub(long.codes$treatment.code[which(nchar(long.codes$treatment.code) %% 2 == 1)],1,5)
  
  query.table=rbind(query.table,long.codes)
  
  query.table=query.table[order(query.table$treatment.code),]
  
  
  query.name=paste0('query_',user)
  assign(query.name, query.table, envir = globalenv())
  gta_sql_get_value(paste0('DROP TABLE IF EXISTS delta_',query.name,';'),db.connection=db.connection)
  gta_sql_create_table(write.df=query.name,
                       contains.data = T,
                       append.existing = F)
  
  
  sql.query = paste0(
    "
    SELECT user_query.implementer Implementing_jurisdiction, user_query.affected_country Affected_jurisdiction,  
    user_query.treatment_code Affected_code, user_query.treatment_code_type Affected_code_type, user_query.treatment_area Policy_area,
    user_query.cutoff_date New_date, user_query.new_treatment_value New_value, user_query.new_treatment_unit New_unit,
    delta_tariff_log.treatment_value Prior_value, gta_unit_list.level_unit Prior_unit, MAX(delta_tariff_log.date_implemented) Prior_date, 
    (CASE WHEN user_query.desired_nonmfn_state IS NULL THEN 'Yes' ELSE 'No' END) AS Is_mfn
    FROM delta_record_linkage, delta_tariff_log, gta_unit_list,
    ( # sub query to determine whether to report mfn or nonmfn value UNION those cases which specifically demand nonmfn
    SELECT user_query.*, MAX(state_log.nonmfn_state) desired_nonmfn_state
    FROM 
    ( # add linkage_id to user query
    SELECT delta_query_",user,".*, delta_linkage_log.linkage_id, delta_root_log.root_id, delta_linkage_log.linkage_affected_country_id FROM delta_query_",user,", delta_root_log, delta_linkage_log
    WHERE delta_query_",user,".implementing_jurisdiction_id = delta_root_log.linkage_implementer_id
    AND delta_query_",user,".treatment_code = delta_root_log.linkage_code
    AND delta_query_",user,".treatment_code_type = delta_root_log.linkage_code_type
    AND delta_root_log.root_id = delta_linkage_log.root_id 
    AND delta_query_",user,".affected_country_id IS NOT NULL
    AND (delta_query_",user,".affected_country_id = delta_linkage_log.linkage_affected_country_id OR delta_linkage_log.linkage_affected_country_id IS NULL)) user_query 
    LEFT JOIN ( # sub query max date per linkage_id below the cutoff date from the user's query
    SELECT delta_nonmfn_state_log.* FROM delta_nonmfn_state_log,
    (SELECT MAX(nonmfn_state_date) nonmfn_state_date, nonmfn_state, linkage_id FROM delta_nonmfn_state_log, delta_query_",user," 
    WHERE nonmfn_state_date <= delta_query_",user,".cutoff_date
    GROUP BY linkage_id) max_dates
    WHERE delta_nonmfn_state_log.linkage_id = max_dates.linkage_id
    AND delta_nonmfn_state_log.nonmfn_state_date = max_dates.nonmfn_state_date
    ) state_log
    ON user_query.linkage_id = state_log.linkage_id
    WHERE (state_log.nonmfn_state > 0 OR state_log.nonmfn_state IS NULL)
    GROUP BY user_query.root_id
    UNION
    SELECT delta_query_",user,".*, delta_linkage_log.linkage_id, delta_root_log.root_id, 
    delta_linkage_log.linkage_affected_country_id, NULL as desired_nonmfn_state 
    FROM delta_query_",user,", delta_root_log, delta_linkage_log
    WHERE delta_query_",user,".implementing_jurisdiction_id = delta_root_log.linkage_implementer_id
    AND delta_query_",user,".treatment_code = delta_root_log.linkage_code
    AND delta_query_",user,".treatment_code_type = delta_root_log.linkage_code_type
    AND delta_root_log.root_id = delta_linkage_log.root_id 
    AND delta_query_",user,".affected_country_id IS NULL 
    AND delta_linkage_log.linkage_affected_country_id IS NULL
    ) user_query
    WHERE user_query.linkage_id = delta_record_linkage.linkage_id
    AND delta_record_linkage.record_id = delta_tariff_log.record_id
    AND user_query.cutoff_date >= delta_tariff_log.date_implemented
    AND user_query.treatment_code = delta_tariff_log.treatment_code 
    AND user_query.treatment_code_type = delta_tariff_log.treatment_code_type
    AND gta_unit_list.level_unit_id = delta_tariff_log.treatment_unit_id
    GROUP BY user_query.linkage_id
    ;
    

    
    "
  )
  
  #cat(sql.query)
  
  remote=gta_sql_get_value(sql.query, db.connection=db.connection)
  gta_sql_get_value(paste0('DROP TABLE IF EXISTS delta_',query.name,';'),db.connection=db.connection)
  
  if(nrow(remote)>0) {
  remote$Match.precision='6 digit'
  keep.cols=names(remote)
  
  add.entries=subset(remote, !Affected.code %in% query.codes)
  setnames(add.entries,c('Prior.value','Prior.unit','Prior.date'),c('six.Prior.value','six.Prior.unit','six.Prior.date'))
  remote=subset(remote, Affected.code %in% query.codes)
  remote$six.digit.code=remote$Affected.code
  remote$six.digit.code[which(nchar(remote$six.digit.code) %% 2 == 0)]=str_sub(remote$six.digit.code[which(nchar(remote$six.digit.code) %% 2 == 0)],1,6)
  remote$six.digit.code[which(nchar(remote$six.digit.code) %% 2 == 1)]=str_sub(remote$six.digit.code[which(nchar(remote$six.digit.code) %% 2 == 1)],1,5)
  remote=merge(remote, add.entries[,c('Affected.code','Affected.code.type','Implementing.jurisdiction','Affected.jurisdiction','six.Prior.value','six.Prior.unit','six.Prior.date')], 
               by.x=c('six.digit.code','Implementing.jurisdiction','Affected.jurisdiction','Affected.code.type'),by.y=c('Affected.code','Implementing.jurisdiction','Affected.jurisdiction','Affected.code.type'),all.x=T)
  
  remote$Match.precision[which(nchar(remote$Affected.code)>6&remote$Affected.code.type=='hs'&!is.na(remote$Prior.value))]=paste(ceiling(nchar(remote$Affected.code[which(nchar(remote$Affected.code)>6&remote$Affected.code.type=='hs'&!is.na(remote$Prior.value))])/2)*2,'digit')
  remote$Prior.value[which(nchar(remote$Affected.code)>6&remote$Affected.code.type=='hs'&is.na(remote$Prior.value))]=remote$six.Prior.value[which(nchar(remote$Affected.code)>6&remote$Affected.code.type=='hs'&is.na(remote$Prior.value))]
  remote$Match.precision[which(!is.na(remote$Prior.value)&is.na(remote$Prior.unit))]='6 digit'
  remote$Prior.unit[which(nchar(remote$Affected.code)>6&remote$Affected.code.type=='hs'&is.na(remote$Prior.unit))]=remote$six.Prior.unit[which(nchar(remote$Affected.code)>6&remote$Affected.code.type=='hs'&is.na(remote$Prior.unit))]
  remote$Prior.date[which(nchar(remote$Affected.code)>6&remote$Affected.code.type=='hs'&is.na(remote$Prior.date))]=remote$six.Prior.date[which(nchar(remote$Affected.code)>6&remote$Affected.code.type=='hs'&is.na(remote$Prior.date))]
  remote$Match.precision[which(nchar(remote$Affected.code)>6&remote$Affected.code.type=='hs'&is.na(remote$Prior.value))]=NA
  remote=subset(remote, select=keep.cols)
  
  remote$sort.result='Unclear'
  remote$sort.result[which(remote$New.value < remote$Prior.value & remote$New.unit == remote$Prior.unit)]='Decrease'
  remote$sort.result[which(remote$New.value > remote$Prior.value & remote$New.unit == remote$Prior.unit)]='Increase'
  remote$sort.result[which(remote$New.value == remote$Prior.value & remote$New.unit == remote$Prior.unit)]='Unchanged'
  colnames(remote)[names(remote)!='sort.result']=gsub('[.]',' ',colnames(remote)[names(remote)!='sort.result'])
  } else {
    
    remote=setNames(data.frame(matrix(ncol = 14, nrow = 0)), 
                    c("Implementing jurisdiction","Affected jurisdiction","Affected code","Affected code type","Policy area","New date","New value","New unit",
                      "Prior value","Prior unit","Prior date","Is mfn","Match precision","sort.result"))

  }
  
  return(remote)
}


# a =gta_delta_query(expand=T,
#                 implementer=c('Angola','Argentina','Algeria','Albania'),
#                 cutoff.date=as.Date('2014-01-01'),
#                 new.treatment.value=NA,
#                 new.treatment.unit='percent',
#                 treatment.area='tariff',
#                 treatment.code=str_trim(unlist(str_split('900630,90240,101,12072158',','))),
#                 # treatment.code='',
#                 treatment.code.type='hs',
#                 affected.country=c('MFN'),
#                 db.connection='pool',
#                 user=29)

