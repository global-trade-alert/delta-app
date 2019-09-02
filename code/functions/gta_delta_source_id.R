gta_delta_source_id=function(source.description=NULL,
                    source.official=NULL,
                    create.source=F,
                    db.connection=db.connection){
  
  
  if(is.null(source.description)|is.null(source.official)|is.na(as.logical(source.official))){
    stop("Please specify a proper source and whether it is official (T/F).")
  }
  
  query=paste("SELECT source_id 
              FROM delta_source_log 
              WHERE state_act_source='",
              source.description,"';", sep="")
  
  this.source.id=gta_sql_get_value(query)
  
  if(is.na(this.source.id) & create.source==T){
    
    source.log.update<<-data.frame(state.act.source=source.description,	
                                 is.source.official=source.official,
                                 stringsAsFactors = F)
    
    this.source.id=gta_sql_append_table(append.table = "source.log",
                                        append.by.df = "source.log.update",
                                        get.id = "source.id",
                                        db.connection=db.connection)
    
    rm(source.log.update)
    
  }
  
  return(this.source.id)
}

