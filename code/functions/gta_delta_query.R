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
# treatment.code=input$treatment.code
# treatment.code=as.numeric(plyr::mapvalues(treatment.code,10121,101))
# treatment.code.type=input$treatment.code.type
# affected.country=input$affected.jurisdiction
# affected.country[7:9]='MFN'
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
    SELECT query.implementer Implementing_jurisdiction, query.affected_country Affected_jurisdiction, query.treatment_code Affected_code, query.treatment_code_type Affected_code_type, query.treatment_area Policy_area,
    query.cutoff_date New_date, query.new_treatment_value New_value, query.new_treatment_unit New_unit, tar_log.treatment_value Prior_value, gta_unit_list.level_unit Prior_unit, MAX(tar_log.date_implemented) Prior_date,
    (CASE WHEN max_nonmfn.nonmfn_state = 1 THEN 'No' ELSE 'Yes' END) AS Is_mfn, src_log.state_act_source Source
    FROM delta_query_",user," query
    LEFT JOIN delta_root_log root_log
    ON root_log.linkage_implementer_id = query.implementing_jurisdiction_id AND root_log.linkage_code = query.treatment_code AND root_log.linkage_code_type = query.treatment_code_type
    LEFT JOIN delta_linkage_log mfn_link_log
    ON root_log.root_id = mfn_link_log.root_id AND mfn_link_log.linkage_affected_country_id IS NULL
    LEFT JOIN delta_linkage_log nonmfn_link_log
    ON root_log.root_id = nonmfn_link_log.root_id AND query.affected_country_id = nonmfn_link_log.linkage_affected_country_id
    LEFT JOIN (SELECT query.*, nonmfn_log.linkage_id, nonmfn_log.nonmfn_state, MAX(nonmfn_log.nonmfn_state_date) 
    		   FROM delta_query_",user," query
    		   JOIN delta_root_log root_log
    		   ON root_log.linkage_implementer_id = query.implementing_jurisdiction_id AND root_log.linkage_code = query.treatment_code AND root_log.linkage_code_type = query.treatment_code_type
    	       JOIN delta_linkage_log nonmfn_link_log ON root_log.root_id = nonmfn_link_log.root_id AND nonmfn_link_log.linkage_affected_country_id = query.affected_country_id
    		   JOIN delta_nonmfn_state_log nonmfn_log ON nonmfn_link_log.linkage_id = nonmfn_log.linkage_id 
    	       AND nonmfn_log.nonmfn_state_date <= query.cutoff_date 
    		   GROUP BY nonmfn_log.linkage_id) max_nonmfn ON nonmfn_link_log.linkage_id = max_nonmfn.linkage_id
    JOIN delta_treatment_linkage treat_link ON (CASE WHEN max_nonmfn.nonmfn_state = 1 THEN nonmfn_link_log.linkage_id ELSE mfn_link_log.linkage_id END) = treat_link.linkage_id
    JOIN delta_tariff_log tar_log ON tar_log.treat_id = treat_link.treat_id AND query.cutoff_date >= tar_log.date_implemented
    JOIN delta_treatment_record treat_rec ON treat_rec.treat_id = treat_link.treat_id JOIN delta_record_source rec_src ON treat_rec.record_id = rec_src.record_id JOIN delta_source_log src_log ON rec_src.source_id = src_log.source_id
    JOIN gta_unit_list ON tar_log.treatment_unit_id = gta_unit_list.level_unit_id
    GROUP BY query.implementer, query.cutoff_date, query.new_treatment_value, query.new_treatment_unit, query.treatment_area, query.treatment_code, query.treatment_code, query.affected_country, rec_src.source_id;
    		   
    "
  )
  
  #cat(sql.query)
  
  remote=gta_sql_get_value(sql.query, db.connection=db.connection)
  gta_sql_get_value(paste0('DROP TABLE IF EXISTS delta_',query.name,';'),db.connection=db.connection)
  
  if(nrow(remote)>0) {
  remote$Match.precision='6 digit'
  keep.cols=names(remote)
  remote$New.date = as.Date(remote$New.date)
  remote = merge(remote, query.table, all.y = T,
               by.x=c("Implementing.jurisdiction","New.date","New.value","New.unit","Policy.area","Affected.code","Affected.code.type","Affected.jurisdiction"),
               by.y=c("implementer","cutoff.date","new.treatment.value","new.treatment.unit","treatment.area","treatment.code","treatment.code.type","affected.country")
               )
  
  
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
    
    remote=setNames(data.frame(matrix(ncol = 15, nrow = 0)), 
                    c("Implementing jurisdiction","Affected jurisdiction","Affected code","Affected code type","Policy area","New date","New value","New unit",
                      "Prior value","Prior unit","Prior date","Is mfn","Source","Match precision","sort.result"))

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

