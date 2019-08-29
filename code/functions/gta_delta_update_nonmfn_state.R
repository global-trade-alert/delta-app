gta_delta_update_nonmfn_state=function(linkage.id=NULL,
                                       treatment.area=NULL,
                                       local.start.date=NULL,
                                       local.end.date.affected.cty=NULL,
                                       local.end.date.code=NULL,
                                       local.end.date.implementer=NULL,
                                       db.connection="pool"){
  

  # extracting nonmfn.state.log
  nonmfn.query=paste("SELECT *
                     FROM delta_nonmfn_log
                     WHERE linkage_id = ",linkage.id,"
                     AND treatment_area = '",treatment.area,"'
                     ;", sep="")
  
  nonmfn.state.log=gta_sql_get_value(query=nonmfn.query,
                                     db.connection=db.connection)
  
  rm(record.query)
  
  # getting the local nonmfn.dates
  
  local.end.date=min(c(local.end.date.affected.cty,
                       local.end.date.code, 
                       local.end.date.implementer), na.rm = T)
  
  
  if(nrow(nonmfn.state.log)==0){
    
    ## Easy case: no remote record
    nonmfn.state.log.update=data.frame(linkage.id=upload.this.type$linkage.id,
                                       treatment.area=upload.this.type$treatment.area,
                                       nonmfn.state.date=as.character(upload.this.type$date.implemented),
                                       nonmfn.state=T,
                                       source.id=this.source.id,
                                       state.redundant=F,
                                       stringsAsFactors = F)
    
    gta_sql_append_table(append.table = "nonmfn.state.log.update",
                         append.by.df = "nonmfn.state.log",
                         db.connection=db.connection)
    
    rm(nonmfn.state.log.update)
    
    if(local.end.date!=Inf){
      
      nonmfn.state.log.update=data.frame(linkage.id=upload.this.type$linkage.id,
                                         treatment.area=upload.this.type$treatment.area,
                                         nonmfn.state.date=as.character(end.date),
                                         nonmfn.state=F,
                                         source.id=this.source.id,
                                         state.redundant=F,
                                         stringsAsFactors = F)
      
      gta_sql_append_table(append.table = "nonmfn.state.log.update",
                           append.by.df = "nonmfn.state.log",
                           db.connection=db.connection)
      
      rm(nonmfn.state.log.update)
      
    }
    
  } else {
    
    ## Are we already in nonmfn.state before this?
    
    if(local.end.date!=Inf){
      local.states=list(date=c(local.start.date, local.end.date),
                        state=c(T,F))
      
    } else {
      local.states=list(date=local.start.date,
                        state=T)
    }
    
    
    
    for(i in 1:length(local.states$date)){
      local.date=local.states$date[i]
      local.state=local.states$state[i]
      
      neighbouring.dates=gta_delta_get_closest_date(benchmark.date = local.date,
                                                    candidate.date=nonmfn.state.log$nonmfn.state.date,
                                                    get.closest.before=T,
                                                    get.closest.after=T)
      remote.before.date=neighbouring.dates[1]
      remote.before.state=nonmfn.state.log$nonmfn.state[nonmfn.state.log$nonmfn.state.date==remote.before.date]
      
      remote.after.date=neighbouring.dates[2]
      remote.before.state=nonmfn.state.log$nonmfn.state[nonmfn.state.log$nonmfn.state.date==remote.after.date]
      
      
      
      redundant.remote.before=F
      redundant.remote.after=F
      upload.local.state.redundant=F
      
      ## figuring out the updates to nonmfn.state.log
      if(is.na(remote.before.date)){
        
        if(local.state){
          
          # if prior state does not exists, add local state as non-redundant
          # and declare the one following it redundant, if it exists
          upload.local.state.redundant=F
          
          if(is.na(remote.after.date)==F){
            redundant.remote.after=T
          }
          
          
        } else {
          
          stop("You are adding a terminating a non-MFN state that has never started.")
        }
        
        
        
      } else {
        
        if(remote.before.date==local.date){
          
          if(local.state==remote.before.state){
            
            # (1) if prior date is same and state is same, then add local state as redundant
            upload.local.state.redundant=T
            
          } else {
            
            
            if(local.state){
              
              # (2) start date: if prior date is same date but different state, make remote state redundant & 
              # make the one following it redundant, if it exists
              # upload local state as redundant
              
              upload.local.state.redundant=T
              
              redundant.remote.before=T
              
              if(is.na(remote.after.date)==F){
                redundant.remote.after=T
              }
              
              
            } else {
              
              ## (2) end date: if prior date is same date but different state,
              ## remove remote date, 
              ## upload local date as redundant
              
              redundant.remote.before=T
              upload.local.state.redundant=T
            }
            
            
            
          }
          
          
          
        } else {
          
          
          
          if(remote.before.state==local.state){
            
            if(local.state){
              
              # Start date: if prior state is earlier and is same state as local, upload it as redundant
              upload.local.state.redundant=T
              
            } else {
              
              # End date: if prior state is earlier and is same as local, then remove prior state and upload local state as non-redundant
              redundant.remote.before=T
              upload.local.state.redundant=F
              
            }
            
          } else {
            
            # Start Date and end date: if prior state is earlier and is different from current state,
            # add current state and 
            # make redundant following state, if exists
            
            upload.local.state.redundant=F
            
            if(is.na(remote.after.date)==F){
              redundant.remote.after=T
            }
            
          }
          
        }
        
      }
      
      ## processing updates
      if(redundant.remote.before){
        
        rm.query=paste("UPDATE delta_nonmfn_state_log 
                       SET state_redundant = 0
                       WHERE linkage_id = ",this.linkage.id,"
                       AND treatment_area = '",upload.this.type$treatment.area,"'
                       AND nonmfn_state_date = ",remote.before.date,"';", sep="")
        
        gta_sql_update_table(rm.query,
                             db.connection=db.connection)
        
        rm(rm.query)
        
      }
      
      if(redundant.remote.after){
        
        rm.query=paste("UPDATE delta_nonmfn_state_log 
                                SET state_redundant = 0 
                               WHERE linkage_id = ",this.linkage.id,"
                               AND treatment_area = '",upload.this.type$treatment.area,"'
                               AND nonmfn_state_date = ",remote.after.date,"';", sep="")
        
        gta_sql_update_table(rm.query,
                             db.connection=db.connection)
        
        rm(rm.query)
        
        
      }
      
      
      ## adding local state.date
      
      nonmfn.state.log.update=data.frame(linkage.id=upload.this.type$linkage.id,
                                         treatment.area=upload.this.type$treatment.area,
                                         nonmfn.state.date=local.date,
                                         nonmfn.state=local.state,
                                         source.id=this.source.id,
                                         state.redundant=upload.local.state.redundant,
                                         stringsAsFactors = F)
      
      gta_sql_append_table(append.table = "nonmfn.state.log.update",
                           append.by.df = "nonmfn.state.log",
                           db.connection=db.connection)
      
      rm(nonmfn.state.log.update)
    }
  }
  
}
