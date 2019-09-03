gta_delta_get_all_records=function(link.implementer.id=NULL,
                                   link.treatment.area=NULL,
                                   link.affected.flow.id=NULL,
                                   link.code=NULL,
                                   link.code.type=NULL,
                                   link.affected.country.id=NULL,
                                   db.connection="pool"){
  
  ## all there?
  if(length(c(link.implementer.id,link.treatment.area,link.affected.flow.id,link.code, link.code.type))!=5){
    stop("Please specify all required parameters (link.implementer.id,link.affected.flow.id,link.code, link.code.type).")
  }
  
  link.remote.data=data.frame()
  
  ### MFN case
  ### need it anyways to potentially complete non-MFN queries, too.
  
  link.query <- paste("SELECT linkage_id 
                   FROM delta_linkage_log 
                   WHERE linkage_implementer_id IN (",paste(link.implementer.id, collapse=','),")
                   AND linkage_affected_flow_id IN (",paste(link.affected.flow.id, collapse=','),")
                   AND linkage_code IN (",paste(link.code, collapse=','),")
                   AND linkage_code_type IN ('",link.code.type,"')
                   AND linkage_affected_country_id IS NULL;", sep="")
  
  this.linkage.id=gta_sql_get_value(query=link.query,
                                   db.connection=db.connection)
  rm(link.query)
  
  
  if(is.na(this.linkage.id)==F){
    
    if(length(this.linkage.id)>1){
      stop(paste("Somehow I have more than one linkage ID here. Impossible. They are: ", paste(this.linkage.id, collapse=";")))
    }
    
    ## Get the records.
    record.query <- paste("SELECT record_id 
                          FROM delta_record_linkage 
                          WHERE linkage_id = ",this.linkage.id,";", sep="")
    
    l.record.ids=gta_sql_get_value(query=record.query,
                                   db.connection=db.connection)
    rm(record.query)
    
    if(any(is.na(l.record.ids)==F)){
      
      link.data.query <- paste("SELECT * 
                                FROM delta_",link.treatment.area,"_log 
                                WHERE record_id IN (",paste(l.record.ids, collapse=","),")
                                AND treatment_code IN (",paste(link.code, collapse=','),");", sep="")
      
      link.remote.data=gta_sql_get_value(query=link.data.query,
                                         db.connection=db.connection)
      
      rm(link.data.query)
      
      if(nrow(link.remote.data)==0){
        link.remote.data=data.frame()
        
      } else {
        
        link.remote.data$treatment.area=link.treatment.area
        link.remote.data$linkage.id=this.linkage.id
        link.remote.data$is.mfn=T
        
        
        link.remote.data=link.remote.data[,c("record.id","linkage.id", "treatment.area","date.implemented", "treatment.value" , "treatment.unit.id","is.intervention","is.mfn","was.enforced","announced.as.temporary")]
        
        
      }

    }
    rm(l.record.ids)
  
    
  } else {
    print("There are no MFN records in the dataset.")
  }
  
  
  
  ### non-MFN case
  if(is.na(link.affected.country.id)==F){
    ### non-MFN case
    link.query <- paste("SELECT linkage_id 
                         FROM delta_linkage_log 
                         WHERE linkage_implementer_id IN (",paste(link.implementer.id, collapse=','),")
                         AND linkage_affected_flow_id IN (",paste(link.affected.flow.id, collapse=','),")
                         AND linkage_code IN (",paste(link.code, collapse=','),")
                         AND linkage_code_type_id IN (",paste(link.code.type, collapse=','),")
                         AND linkage_affected_country_id IN (",paste(link.affected.country.id, collapse=','),");", sep="")
          
    
    this.linkage.id=gta_sql_get_value(query=link.query,
                                      db.connection=db.connection)
    rm(link.query)
    
    if(is.na(this.linkage.id)==F){
      
      if(length(this.linkage.id)>1){
        stop(paste("Somehow I have more than one linkage ID here. Impossible. They are: ", paste(this.linkage.id, collapse=";")))
      }
      
      ## Get the records.
      record.query <- paste("SELECT record_id 
                             FROM delta_record_linkage 
                             WHERE linkage_id = ",this.linkage.id,";", sep="")
      
      l.record.ids=gta_sql_get_value(query=record.query,
                                     db.connection=db.connection)
      rm(record.query)
      
      if(any(is.na(l.record.ids)==F)){
        l.record.ids=unique(l.record.ids$record.id)
        
        link.data.query <- paste("SELECT * 
                                  FROM delta_",link.treatment.area,"_log 
                                  WHERE record_id IN (",paste(l.record.ids, collapse=","),")
                                  AND treatment_code IN (",paste(link.code, collapse=','),");", sep="")
        
        link.remote.nmfn.data=gta_sql_get_value(query=link.data.query,
                                           db.connection=db.connection)
        
        rm(link.data.query)
        
        if(nrow(link.remote.nmfn.data)==0){
          link.remote.nmfn.data=data.frame()
        } else {
          
          link.remote.nmfn.data$treatment.area=link.treatment.area
          link.remote.nmfn.data$linkage.id=this.linkage.id
          link.remote.nmfn.data$is.mfn=F
          
          
          link.remote.nmfn.data=link.remote.nmfn.data[,c("record.id","linkage.id", "treatment.area","date.implemented", "treatment.value" , "treatment.unit.id","is.intervention","is.mfn","was.enforced","announced.as.temporary")]
          
          
        }

        
        ### combining with link.remote.data to complete the series
        ### Querying by linkage as above will only return the entries where non-MFN status was TRUE
        
        ### Need 2 checks:
        ### non-MFN check 1: Have to check for periods where non-MFN state is FALSE
        
        if(nrow(link.remote.nmfn.data)>0){
          
          link.status.query <- paste("SELECT * 
                                    FROM delta_",link.treatment.area,"_nonmfn_state_log 
                                     WHERE linkage_id IN (",paste(this.linkage.id, collapse=","),")
                                     AND treatment_area IN (",paste(link.treatment.area, collapse=','),");", sep="")
          
          link.status=gta_sql_get_value(query=link.status.query,
                                        db.connection=db.connection)
          
          
          if(is.na(link.status)==F & (F %in% link.status$nonmfn.state)){
            
            for(mfn.date in subset(link.status, nonmfn.state==F)$nonmfn.state.date){
              
              start.mfn=as.Date(as.numeric(mfn.date), origin="1970-01-01")
              
              if(subset(link.status, nonmfn.state==T & nonmfn.state.date>start.mfn)){
                
                end.mfn=as.Date(as.numeric(min(subset(link.status, nonmfn.state==T & nonmfn.state.date>start.mfn)$nonmfn.state.date)), origin="1970-01-01") - 1 # substracting a day to avoid same-day entries for non-MFN and MFN
                
              } else {
                end.mfn=as.Date(as.numeric(max(link.remote.data$date.implemented)), origin="1970-01-01")
              }
              
              
              ## first entry
              if(nrow(subset(link.remote.data, date.implemented<=start.mfn))>0){
                
                mfn.valid=subset(link.remote.data, date.implemented<=start.mfn)
                
                mfn.valid=subset(mfn.valid, date.implemented==max(mfn.valid$date.implemented))
                
                mfn.valid$date.implemented=start.mfn
                
                link.remote.nmfn.data=rbind(link.remote.nmfn.data,
                                            mfn.valid)
                
                rm(mfn.valid)
              } 
              
              ## remainder entries
              if(nrow(subset(link.remote.data, date.implemented>start.mfn & date.implemented<=end.mfn))>0){
                
                mfn.valid=subset(link.remote.data, date.implemented>start.mfn & date.implemented<=end.mfn)
                
                mfn.valid$date.implemented[mfn.valid$date.implemented==max(mfn.valid$date.implemented)]=end.mfn
                
                link.remote.nmfn.data=rbind(link.remote.nmfn.data,
                                            mfn.valid)
                
                rm(mfn.valid)
              } 
              
              
            }
            
          }
          rm(link.status)
          
          
          ### non-MFN check 2: whether min(nonmnf.state.date<=tariff.log$date.implemeneted for MFN state
          if(nrow(subset(link.remote.data, date.implemented < min(link.remote.nmfn.data$date.implemented)))>0){
            
            link.remote.nmfn.data=rbind(link.remote.nmfn.data,
                                        subset(link.remote.data, date.implemented < min(link.remote.nmfn.data$date.implemented))
            )
          }
          
        }
        
        ## overwriting MFN-only data frame
        link.remote.data=link.remote.nmfn.data
        
        
      }
      rm(l.record.ids)
    } else {
      print("No non-MFN records for this query.")
    }
    
  } 
  
  ## return data
  return(link.remote.data)
}
