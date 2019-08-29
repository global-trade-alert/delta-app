gta_delta_get_relevant_records=function(implementer.id=NULL,
                                        treatment.area=NULL,
                                        affected.flow.id=NULL,
                                        treatment.code=NULL,
                                        treatment.code.type=NULL,
                                        affected.country.id=NULL,
                                        excl.mfn=F,
                                        excl.prolongation=F,
                                        cut.off.date=NULL,
                                        incl.prior.record=T,
                                        incl.same.date.record=T,
                                        incl.subsequent.record=F,
                                        db.connection="pool"){

  
  ## initialise
  
  
  link.imp=implementer.id
  link.treatment.area=treatment.area
  link.flow=affected.flow.id
  link.code=treatment.code
  link.code.type=treatment.code.type
  link.country=affected.country.id
  
  
  if(is.null(cut.off.date)==F){
    
    if(is.na(as.Date(as.numeric(cut.off.date), origin="1970-01-01"))==F){
      
      cut.off.date=as.Date(as.numeric(cut.off.date), origin="1970-01-01")
      
      } else {
      
        stop("The cut.off.date is not in the correct format ('%Y-%m-%d').")
        
    }
    
  }
  
  
  
  #### RETRIEVING THE REMOTE DATA
  
  
  ## Extracting records
  link.remote.data=gta_delta_get_all_records(link.implementer.id=link.imp,
                                             link.treatment.area=link.treatment.area,
                                             link.affected.flow.id=link.flow,
                                             link.code=link.code,
                                             link.code.type.id=link.code.type,
                                             link.affected.country.id=link.country,
                                             db.connection=db.connection)
  
  
  #### PROCESSING THE REMOTE DATA
  
  ## Excluding MFN entries, if called for
  if(is.null(link.affected.country.id)==F  & excl.mfn){
    link.remote.data=subset(link.remote.data, is.mfn==F)
  }
  
  ## Excluding prolongations, if called for
  if(excl.prolongation){
    link.remote.data=subset(link.remote.data, is.intervention==T)
  }
  
  ## Restricting time series, if called for
  if(is.null(cut.off.date)==F & nrow(link.remote.data)>0){
    
    link.cut.base=link.remote.data
    link.cut.base$distance=link.cut.base$date.implemented - cut.off.date
    
    link.remote.data=data.frame()
    
    if(incl.prior.record){
      
      lrd.cut=subset(link.cut.base, distance<0)
      
      if(nrow(lrd.cut)>0){
        link.remote.data=rbind(link.remote.data,
                               subset(lrd.cut, distance==max(lrd.cut$distance)))
      } 
      
      rm(lrd.cut)
      
    }
    
    if(incl.same.date.record){
      
      lrd.cut=subset(link.cut.base, distance==0)
      
      if(nrow(lrd.cut)>0){
        link.remote.data=rbind(link.remote.data,
                               lrd.cut)
      } 
      
      rm(lrd.cut)
      
    }
    
    
    if(incl.subsequent.record){
      
      lrd.cut=subset(link.cut.base, distance>0)
      
      if(nrow(lrd.cut)>0){
        link.remote.data=rbind(link.remote.data,
                               subset(lrd.cut, distance==min(lrd.cut$distance)))
      } 
      
      rm(lrd.cut)
      
    }
    
    if(nrow(link.remote.data)>0){
      link.remote.data$distance=NULL
    }
    
    rm(link.cut.base)
    
  }
  
  return(link.remote.data)
  
}