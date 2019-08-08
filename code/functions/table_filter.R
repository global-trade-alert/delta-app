# library(gtalibrary)
# library(zoo)
# library(data.table)
# library(splitstackshape)
# library(plyr)
# library(stringr)
# library(RMariaDB)
# library(pool)
# library(DBI)
# library(gtasql)
# library(RMySQL)
# 
# gta_sql_pool_open()
# 
# hs.codes='20110,020120,201,410419211,71021,71022,71029,71039,51110,50800'
# # cpc.codes=NULL
# cpc.codes='21,22,23,24,60,61,62,63,64,40,41,42,43,44,10,11,12,13'
# implementing.jurisdiction=c('Afghanistan','Andorra','Armenia','Solomon Islands',
#                             'Croatia','Jamaica','Norfolk Island')
# affected.jurisdiction='French Polynesia'
# # implementing.jurisdiction='Croatia'
# # affected.jurisdiction='Any'
# start.date=as.Date('2013-01-01')
# end.date=as.Date('2019-02-01')
# regime.types='Any'
# regime.names='Any'
# tariff.unit='Any'
# show.previous=F
# sort=T

table_filter=function(
  hs.codes=NULL,
  cpc.codes='deactivated',
  implementing.jurisdiction=NULL,
  affected.jurisdiction=NULL,
  start.date=NULL,
  end.date=NULL,
  regime.types='Any',
  regime.names='Any',
  tariff.unit='Any',
  show.previous=F,
  sort=T
){
  
  ## check inputs for completeness
  if((is.null(hs.codes) & is.null(cpc.codes))|is.null(implementing.jurisdiction)|is.null(affected.jurisdiction)){
    stop("Complete input requires at least one implementer, date and HS or CPC code.")
  }
  
  start_time <-Sys.time()
  
  sql="SELECT * FROM prior_new WHERE 1=1"
  
  #code filter (cpc and hs)
  if(!is.null(hs.codes)){
    codes=unlist(strsplit(as.numeric(as.character(hs.codes)),','))
    code=gta_hs_code_check(codes)
  } else {
    codes=character()
  }
  
  if(!is.null(cpc.codes)){
    # I will take this out for now, since it complicates things and is not relevant at this stage.
    # What complicates things is that the user could supply 2 or 3 digit CPC codes 
    # and some of these are indistinguishable e.g. "13" could be the 3-digit "013" or the 2-digit "13".
    # codes=c(codes,subset(cpc.to.hs,cpc %in% unlist(strsplit(as.character(cpc.codes),',')))$hs)
  }
  
  
  codes[which(nchar(codes)==5)]=paste0('0',codes[which(nchar(codes)==5)])
  
  codes=toString(sprintf("'%s'",codes))
  sql=paste(sql,
            sprintf("AND (hs4 IN (%s) OR",codes),
            sprintf("hs6 IN (%s) OR",codes),
            sprintf("hs IN (%s))",codes),
            sep=' ')
  
  sql=paste(sql,sprintf('AND (date_implemented BETWEEN %s',toString(sprintf("'%s'",start.date))))
  sql=paste(sql,sprintf('AND %s)',toString(sprintf("'%s'",end.date))))
  
  if(!'Any' %in% tariff.unit){
    tariff.unit=toString(sprintf("'%s'", tariff.unit))
    sql=paste(sql,sprintf("AND (applied_value_unit in (%s))",tariff.unit),sep=' ')
  }  
  
  if(!'Any' %in% regime.names & !'Any' %in% regime.types){
    regime.names=toString(sprintf("'%s'",regime.names))
    sql=paste(sql,sprintf("AND (regime_id in (SELECT regime_id FROM regime_list WHERE (regime_name IN (%s)))",regime.names))
    regime.types=toString(sprintf("'%s'",regime.types))
    sql=paste(sql, sprintf("OR regime_id IN (SELECT regime_id FROM regime_list WHERE (regime_type_id IN (SELECT regime_type_id FROM regime_type_list WHERE(regime_type_name IN (%s))))))",
                           regime.types),sep=' ')
    
  } else if('Any' %in% regime.names & !'Any' %in% regime.types) {
    regime.types=toString(sprintf("'%s'",regime.types))
    sql=paste(sql, sprintf("AND regime_id IN (SELECT regime_id FROM regime_list WHERE (regime_type_id IN (SELECT regime_type_id FROM regime_type_list WHERE(regime_type_name IN (%s)))))",
                           regime.types),sep=' ')
  } else if(!'Any' %in% regime.names & 'Any' %in% regime.types) {
    regime.names=toString(sprintf("'%s'",regime.names))
    sql=paste(sql,sprintf("AND regime_id in (SELECT regime_id FROM regime_list WHERE (regime_name IN (%s)))",regime.names))
  }

  base<-gta_sql_get_value(sql)
  
  base$match.precision=4
  base$match.precision[which(base$hs6 %in% codes)]=6
  base$match.precision[which(base$hs %in% codes)]=nchar(base$hs[which(base$hs %in% codes)])
  base$match.precision=paste0(base$match.precision,' Digits')
  base$cpc.code=mapvalues(as.numeric(base$hs6),cpc.to.hs$hs,cpc.to.hs$cpc)

  base=subset(base,select=c('intervention.id','regime.id','cpc.code','hs.code','applied.value','applied.value.unit','date.implemented','match.precision'))

  jurisdiction.list=gta_sql_load_table('jurisdiction.list')
  intervention.log=gta_sql_load_table('intervention.log')
  regime.affected=gta_sql_load_table('regime.affected')
  regime.implementer=gta_sql_load_table('regime.implementer')
  regime.list=gta_sql_load_table('regime.list')
  state.act.log=gta_sql_load_table('state.act.log')
  
  if(!'Any' %in% implementing.jurisdiction){
    implementing.jurisdiction.ids=subset(jurisdiction.list,country.name%in%implementing.jurisdiction)$jurisdiction.id
    intervention.ids=subset(intervention.log,jurisdiction.id %in% implementing.jurisdiction.ids)$intervention.id
    base=subset(base, intervention.id %in% intervention.ids)
  } else {implementing.jurisdiction.ids=jurisdiction.list$jurisdiction.id}
    
  #determine between which countries there is an MFN
  mfn=expand.grid(i.id=implementing.jurisdiction.ids,a.id=jurisdiction.list$jurisdiction.id)
  nonmfn=merge(regime.implementer,regime.affected,by=c('regime.id'),all=T,allow.cartesian=T)
  nonmfn$mfn=0
  setnames(nonmfn,names(nonmfn),c('regime.id','i.id','a.id','mfn'))
  mfn=merge(mfn,subset(nonmfn,select=names(nonmfn)!='regime.id'),by=c('i.id','a.id'),all=T)
  mfn$mfn[is.na(mfn$mfn)]=1
  mfn=aggregate(a.id~i.id,subset(mfn,mfn==1&i.id!=a.id),function(x) paste(x,collapse=';'))
  
  base=merge(base,unique(subset(intervention.log,intervention.id %in% base$intervention.id,select=c('intervention.id','jurisdiction.id'))),
             by='intervention.id')
  
  #locate current tariff level and most recent
  base$date.implemented=as.Date(base$date.implemented)
  base$id=1:nrow(base)
  
  first.max = tryCatch({
    first.max=merge(aggregate(date.implemented~jurisdiction.id+regime.id+hs.code+applied.value+applied.value.unit,base,max),base,
            by=c('date.implemented','jurisdiction.id','regime.id','hs.code','applied.value','applied.value.unit'))
  },error=function(e) {
    first.max=base[0,]
    return(first.max)
  })
  
  second.max = tryCatch({
    second.max=subset(base, !base$id %in% first.max$id)
    second.max=merge(aggregate(date.implemented~jurisdiction.id+regime.id+hs.code+applied.value+applied.value.unit,second.max,max),second.max,
                    by=c('date.implemented','jurisdiction.id','regime.id','hs.code','applied.value','applied.value.unit'))
    second.max=subset(second.max, select=c('jurisdiction.id','regime.id','hs.code','applied.value','applied.value.unit'))
  },error=function(e) {
    second.max=base[0,c('jurisdiction.id','regime.id','hs.code','applied.value','applied.value.unit')]
    return(second.max)
  })
  
  if(nrow(first.max)!=0){
  #fix something here, not sure why with the default query there is 4004 first.max rows but suddenly when i merge there is 4560, 
  #there are no duplicates
  base=merge(first.max,second.max,by=c('jurisdiction.id','regime.id','hs.code'),all.x=T)

  #add partner countries and csplit
  base=merge(base,subset(regime.affected,select=c('regime.id','jurisdiction.id')),
             by='regime.id',all.x=T)
  base$jurisdiction.id.y[base$regime.id==1]=mapvalues(base$jurisdiction.id.x[base$regime.id==1],mfn$i.id,mfn$a.id)
  base=cSplit(base,which(colnames(base)=='jurisdiction.id.y'), direction = "long", sep = ";",drop=TRUE)
  base$jurisdiction.id.x=mapvalues(base$jurisdiction.id.x,jurisdiction.list$jurisdiction.id,jurisdiction.list$country.name)
  base$jurisdiction.id.y=mapvalues(base$jurisdiction.id.y,jurisdiction.list$jurisdiction.id,jurisdiction.list$country.name)
  base$regime.id=mapvalues(base$regime.id,regime.list$regime.id,as.character(regime.list$regime.name))

  if(affected.jurisdiction!='Any'){
    base=subset(base, jurisdiction.id.y %in% affected.jurisdiction)
  }
  
  #add source from intervention.id>act.id>act.source
  base$source=mapvalues(mapvalues(base$intervention.id,intervention.log$intervention.id,intervention.log$state.act.id),
                 state.act.log$state.act.id,state.act.log$act.source)
  
  #Not sure how to handle cases like this: https://gyazo.com/81640744c734dd608c55d250ba651123
  #For future purposes (with new tariff, also what if the newest value is 8 mean , but previous was 5 max,
  #how do i compare different units? Do i just not?)
  base$eval='Amber'
  base$eval[which(base$previous.value<base$applied.value)]='Restricted'
  base$eval[which(base$previous.value>base$applied.value)]='Liberalised'
  
  base=subset(base,select=c('intervention.id','jurisdiction.id.x','jurisdiction.id.y','hs.code','cpc.code',
                            'date.implemented','applied.value.x','applied.value.unit.x','applied.value.y','applied.value.unit.y','regime.id','source','match.precision','eval'))
  setnames(base,names(base),c('Intervention Id','Implementing Jurisdiction','Affected Jurisdiction','HS Code', 'Cpc sector',
                              'Date implemented','Tariff applied','Tariff unit','Previous tariff applied','Previous tariff unit','Regime of Tariff','Source','Match Precision','Evaluation'))
  
  if(any(sort %in% c(T,'yes','Yes','YES'))){
    display.cols=c(#'Intervention Id',
      'Implementing Jurisdiction',
      'Affected Jurisdiction',
      'HS Code', 
      'Cpc sector',
      'Date implemented',
      'Tariff applied',
      'Tariff unit',
      'Previous tariff applied',
      'Previous tariff unit',
      'Source',
      'Match Precision',
      'Regime of Tariff',
      'Evaluation')}else if(any(show.previous %in% c(T,'yes','Yes','YES'))){
        display.cols=c(#'Intervention Id',
          'Implementing Jurisdiction',
          'Affected Jurisdiction',
          'HS Code', 
          'Cpc sector',
          'Date implemented',
          'Tariff applied',
          'Tariff unit',
          'Previous tariff applied',
          'Previous tariff unit',
          'Source',
          'Match Precision',
          'Regime of Tariff')} else {
            display.cols=c(#'Intervention Id',
              'Implementing Jurisdiction',
              'Affected Jurisdiction',
              'HS Code', 
              'Cpc sector',
              'Date implemented',
              'Tariff applied',
              'Tariff unit',
              'Source',
              'Match Precision',
              'Regime of Tariff')}
  
  base=subset(base,select=display.cols)
  
  } else {base=data.frame(nothing.remains='Nothing Remains')}
  
  end_time <- Sys.time()
  print(end_time - start_time)
  
  return(list(unique(base)))
}  

