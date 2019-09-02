## This function parses input data into state acts & interventions.

gta_delta_input_parser=function(
  delta.data,
  input.name=NULL,
  user.id=NULL,
  db.connection="pool"
){
  
  library(gtasql)
  library(gtalibrary)
  library(data.table)
  
  ## Check for having all variables
  necessary.variables=c("implementing.jurisdiction", "treatment.value", "treatment.code", 
                        "date.announced","date.implemented", "announced.removal.date", "treatment.unit.id",
                        "treatment.code.official", "treatment.area", "treatment.code.type",
                        "intervention.type.id", "state.act.source", "is.source.official", 
                        "author.id", "affected.flow.id", "implementation.level.id",
                        "eligible.firms.id","implementer.end.date","treatment.code.end.date", 
                        "nonmfn.affected","nonmfn.affected.end.date", "framework.id")
  
  
  got.all.vars=setdiff(necessary.variables, names(delta.data))
  
  if(length(got.all.vars)>0){
    stop(paste("The following column names are missing:",paste(got.all.vars, collapse=";")))
  }
  
  
  ############ PARSER
  ## creating input ID
  if(is.null(input.name)|is.null(user.id)|length(input.name)>1|length(user.id)>1){
    
    stop("Please assign 1 input name and specify 1 user ID.")
    
  } else {
    
    
    input.id.query  <- paste("SELECT input_id 
                              FROM delta_input_log 
                              WHERE user_id = ",user.id,"
                              AND input_name = '",input.name,"';", sep="")
    
    this.input.id=gta_sql_get_value(query=input.id.query,
                                            db.connection=db.connection)
    
    rm(input.id.query)
    
    if(is.na(this.input.id)){
      
      input.log.update<<-data.frame(user.id=user.id,
                                  input.date=Sys.Date(),
                                  input.name=input.name,
                                  stringsAsFactors = F)
      
      this.input.id=gta_sql_append_table(append.table = "input.log",
                                          append.by.df = "input.log.update",
                                          get.id = "input.id",
                                         db.connection=db.connection)
    }
    
    
  }
  
  ## expanding coarse.codes
  delta.data$coarse.code=NA
  delta.data$coarse.code.type=NA
  
  coarse=subset(delta.data, (nchar(delta.data$treatment.code) < 5 & delta.data$treatment.code.type=='hs'))
  
  if(nrow(coarse)>0){
    
    coarse$processing.id=1:nrow(coarse)
    
    expanded.output=data.frame()
    for(i in 1:nrow(coarse)){
      expanded.codes=gta_hs_code_check(coarse$treatment.code[i])
      expanded.output=rbind(expanded.output,
                            data.frame(processing.id=coarse$processing.id[i],
                                       treatment.code=as.double(expanded.codes),
                                       coarse.code=coarse$treatment.code[i],
                                       coarse.code.type="hs"))
    }
    
    coarse$treatment.code=NULL
    coarse$coarse.code=NA
    coarse$coarse.code.type=NA
    coarse=merge(coarse, expanded.output, by="processing.id", all.x=T)
    
    delta.data=rbind(subset(delta.data, !(nchar(delta.data$treatment.code) < 5 & delta.data$treatment.code.type=='hs')),
                     expanded.output)
    
    rm(expanded.output, expanded.codes)
    
  }
  rm(coarse)
  
  
  ## adding jurisdiction.ids
  
  #### Implementers
  implementer.ids=gta_delta_get_jurisdiction_id(jurisdiction.name=unique(delta.data$implementing.jurisdiction),
                                                db.connection=db.connection)
  
  names(implementer.ids)=c("implementing.jurisdiction.id","implementing.jurisdiction")
  
  delta.data=merge(delta.data, implementer.ids, by="implementing.jurisdiction", all.x=T)
  
  rm(implementer.ids)
  
  #### Affected jurisdictions
  delta.data$nonmfn.affected.id=NA
  got.aj=subset(delta.data, is.na(nonmfn.affected)==F)
  
  if(nrow(got.aj)>0){
    
    got.aj$nonmfn.affected.id=NULL
    affected.ids=gta_delta_get_jurisdiction_id(jurisdiction.name=unique(got.aj$nonmfn.affected),
                                                  db.connection=db.connection)
    
    names(affected.ids)=c("nonmfn.affected.id","nonmfn.affected")
    
    got.aj=merge(got.aj, affected.ids, by="nonmfn.affected", all.x=T)
    
    delta.data=rbind(subset(delta.data, is.na(nonmfn.affected)),
                     got.aj)
    
    rm(affected.ids)
    
  } else {
    
  }
  rm(got.aj)

  

  
  ## checking the linkages
  
  linkages=unique(delta.data[,c("implementing.jurisdiction.id","affected.flow.id","treatment.code", "treatment.code.type","nonmfn.affected.id")])
  linkages$linkage.id=-1:-nrow(linkages) # negative values avoid duplication with existing linkage.id's in the remote database
  
  delta.data=merge(delta.data, linkages, by=c("implementing.jurisdiction.id","affected.flow.id","treatment.code", "treatment.code.type","nonmfn.affected.id"), all.x=T)
  
  ## now have to compare the local and the database values for date.implemented-treatment.value-treatement.unit.type.id for each linkage
  added.recs=0
  for(link in unique(linkages$linkage.id)){
    # link=-456
    
    ## generating list of local announcements for this link
    link.local.data=subset(delta.data, linkage.id==link)[,c("implementing.jurisdiction.id","affected.flow.id","treatment.code", "treatment.code.type","nonmfn.affected.id", "treatment.area","date.implemented", "treatment.value" , "treatment.unit.id")]
    link.local.data$record.id=-1:-nrow(link.local.data)
    
    ## processing treatment area by treatment area
    for(link.area in unique(link.local.data$treatment.area)){
      # link.area="tariff" 
      
    
      
      ## Do we have a remote record on this linkage?
      ## If so, extract all records.
      this.linkage.id=NA
      link.remote.data=gta_delta_get_relevant_records(implementer.id=subset(linkages, linkage.id==link)$implementing.jurisdiction.id,
                                                      treatment.area=link.area,
                                                      affected.flow.id=subset(linkages, linkage.id==link)$affected.flow.id,
                                                      treatment.code=subset(linkages, linkage.id==link)$treatment.code,
                                                      treatment.code.type=subset(linkages, linkage.id==link)$treatment.code.type,
                                                      affected.country.id=subset(linkages, linkage.id==link)$nonmfn.affected.id,
                                                      excl.mfn=T,
                                                      excl.prolongation=F,
                                                      db.connection=db.connection)
      
      
      this.linkage.id=unique(link.remote.data$linkage.id)
      
      
    
      ## Check consistency of overlapping entries
      # for every date in my local data I have to check whether it also exists in remote and if so, whether the values match.
      # If they match, I delete the local entry. If they do not match, I record the discrepancy.
      
      if(length(intersect(link.remote.data$date.implemented, link.local.data$date.implemented))>0){
        
        for(common.date in intersect(link.remote.data$date.implemented, link.local.data$date.implemented)){
          common.date=as.Date(common.date, origin="1970-01-01")
          
          for(common.area in subset(link.local.data, date.implemented==common.date)$treatment.area){
            
            local.overlap=subset(link.local.data, date.implemented==common.date & treatment.area==common.area)
            remote.overlap=subset(link.remote.data, date.implemented==common.date & treatment.area==common.area)
            
            if(nrow(remote.overlap)>0){
              
              consistent= ((local.overlap$treatment.value==remote.overlap$treatment.value) &
                             (local.overlap$treatment.unit.id==remote.overlap$treatment.unit.id))
              
              if(! consistent){
                # document discrepancy for subsequent resolution by user
                
                # source.id
                this.source.id=gta_delta_source_id(source.description=delta.data$state.act.source[delta.data$linkage.id==link & delta.data$treatment.area==common.area & delta.data$date.implemented==common.date],
                                                    source.official=delta.data$is.source.official[delta.data$linkage.id==link & delta.data$treatment.area==common.area & delta.data$date.implemented==common.date],
                                                    create.source=T,
                                                    db.connection=db.connection)
                
                local.vars=names(local.overlap)[names(link.local.data) %in% names(delta.data)]
                local.overlap=merge(local.overlap, subset(delta.data, linkage.id==link)[,c(local.vars,"treatment.code.official")], by=local.vars,
                                  all.x=T)
                
                input.discrepancy.log.update<<-data.frame(input.id=this.input.id,
                                                        record.id=remote.overlap$record.id,	
                                                        discrepancy.date=common.date,	
                                                        discrepancy.value=local.overlap$treatment.value,	
                                                        discrepancy.value.unit.id=local.overlap$treatment.unit.id,
                                                        discrepancy.code.official=local.overlap$treatment.code.official,
                                                        discrepancy.source.id=this.source.id,
                                                        stringsAsFactors = F)
                
                gta_sql_append_table(append.table = "input.discrepancy.log",
                                                    append.by.df = "input.discrepancy.log.update",
                                                    db.connection=db.connection)
                
              } 
              
              # remove local entry, regardless of consistency (discrepancies are resolved separately)
              link.local.data=subset(link.local.data, ! (date.implemented==common.date & treatment.area==common.area))
              
              
            }
            
            rm(local.overlap,remote.overlap)
            
          }
          
        }
      }
      
      
      ## Uploading into the remote database
      if(nrow(link.local.data)>0){
        
        upload.data=merge(link.local.data, subset(delta.data, linkage.id==link), by=names(link.local.data)[names(link.local.data) %in% names(delta.data)],
                          all.x=T)
        
        if(is.null(this.linkage.id)==F){
          upload.data$linkage.id=this.linkage.id
        }
        
        gta_delta_input_upload(upload.data,
                               input.id=this.input.id,
                               input.name=input.name,
                               db.connection=db.connection)
        rm(upload.data)
        
        added.recs=added.recs+1
        print(paste("Number of added records:", added.recs))
        }
      }
    }
  
  print("Data import complete.")
}  

