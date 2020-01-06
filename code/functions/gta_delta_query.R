# rm(list=ls())
# 
# library(gtalibrary)
# library(gtasql)
# library(RMariaDB)
# library(DBI)
# library(RMySQL)
# library(pool)
# library(stringr)
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
# input=readxl::read_excel('17 Shiny/6 delta app/data/query sample.xlsx')[c(1:5,8:12),]
# 
# expand=F
# implementer=input$implementing.jurisdiction
# # cutoff.date=input$cutoff.date
# # cutoff.date[1:5]=NA
# cutoff.date=NA
# new.treatment.value=rep(NA,10)
# new.treatment.unit=rep(NA,10)
# treatment.area=input$treatment.area
# treatment.code=input$treatment.code
# treatment.code=as.numeric(plyr::mapvalues(treatment.code,10121,101))
# treatment.code.type=input$treatment.code.type
# affected.country=input$affected.jurisdiction
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

  library(gtalibrary)
  library(gtasql)
  library(plyr)
  library(data.table)

  gta_setwd()
  source('17 Shiny/6 delta app/code/functions/gta_delta_get_jurisdiction_id.R')
  
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
  
  #map NA dates to Sys.Date()'
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
    ALTER TABLE delta_query_",user," ADD COLUMN IF NOT EXISTS new_treatment_value tinyint(1) DEFAULT NULL;
    ALTER TABLE delta_query_",user," ADD COLUMN IF NOT EXISTS new_treatment_unit_id tinyint(1) DEFAULT NULL;
    
    UPDATE delta_query_",user," 
    LEFT JOIN gta_unit_list
    ON delta_query_",user,".new_treatment_unit = gta_unit_list.level_unit
    SET delta_query_",user,".new_treatment_unit_id = gta_unit_list.level_unit_id;
    
    
    SELECT impl_jur.jurisdiction_name Implementing_jurisdiction, (CASE WHEN aff_jur.jurisdiction_name IS NULL THEN 'MFN' ELSE aff_jur.jurisdiction_name END) Affected_jurisdiction,
    query.treatment_code Affected_code, query.treatment_code_type Affected_code_type, query.treatment_area Policy_area, query.cutoff_date New_date, query.new_treatment_value New_value, new_unit_list.level_unit New_unit, 
    tar_log.treatment_value Prior_value, prior_unit_list.level_unit Prior_unit,
    IF(COUNT(*)=COUNT(date_implemented),MAX(date_implemented),NULL) Prior_date 
    FROM delta_query_",user," query
    LEFT JOIN (SELECT delta_tariff_log.*, delta_record_implementer.implementing_jurisdiction_id
    FROM delta_tariff_log
    LEFT JOIN delta_record_implementer 
    ON delta_tariff_log.record_id = delta_record_implementer.record_id) tar_log
    ON query.implementing_jurisdiction_id = tar_log.implementing_jurisdiction_id
    AND query.cutoff_date > tar_log.date_implemented
    AND query.treatment_code = tar_log.treatment_code
    AND query.treatment_code_type = tar_log.treatment_code_type
    LEFT JOIN gta_jurisdiction_list impl_jur
    ON query.implementing_jurisdiction_id = impl_jur.jurisdiction_id
    LEFT JOIN gta_jurisdiction_list aff_jur
    ON query.affected_country_id = aff_jur.jurisdiction_id
    LEFT JOIN gta_unit_list new_unit_list
    ON query.new_treatment_unit_id = new_unit_list.level_unit_id 
    LEFT JOIN gta_unit_list prior_unit_list
    ON tar_log.treatment_unit_id = prior_unit_list.level_unit_id 
    GROUP BY impl_jur.jurisdiction_name, cutoff_date, query.treatment_code, query.treatment_code_type 
    ;

    
    "
  )
  
  #cat(sql.query)
  
  remote=gta_sql_multiple_queries(query.string=sql.query, output.queries = 4)
  gta_sql_get_value(paste0('DROP TABLE IF EXISTS delta_',query.name,';'),db.connection=db.connection)
  
  # remote$id=1:nrow(remote)
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

