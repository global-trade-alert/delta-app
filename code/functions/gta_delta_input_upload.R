
gta_delta_input_upload=function(send.to.remote,
                                  input.id=NULL,
                                  input.name=NULL,
                                  db.connection="pool"){
  
  # send.to.remote=upload.data
  # input.id=this.input.id
  # input.name=input.name
  # db.connection=db.connection
  
  ## input check
  
  
  if(is.null(input.id)|is.null(input.name)){
    stop("Input Upload: Please specify input.id and input.name.")
  }
  
  ## load data
  upload.data=send.to.remote
  
  ## NO linkage in current db
  if(unique(upload.data$linkage.id)<0){
    
    
    if(is.na(unique(upload.data$nonmfn.affected.id))){
      aj=NA
    } else {
      aj=unique(upload.data$nonmfn.affected.id)
    }
    
    linkage.log.update<<-data.frame(linkage.implementer.id=unique(upload.data$implementing.jurisdiction.id),
                                  linkage.affected.flow.id=unique(upload.data$affected.flow.id),
                                  linkage.code=unique(upload.data$treatment.code),
                                  linkage.code.type=unique(upload.data$treatment.code.type),
                                  linkage.affected.country.id=aj,
                                  stringsAsFactors = F)
    
    this.linkage.id=gta_sql_append_table(append.table = "linkage.log",
                                         append.by.df = "linkage.log.update",
                                         get.id = "linkage.id",
                                         db.connection=db.connection)
    
    
    rm(aj)
    
    upload.data$linkage.id=this.linkage.id
    
  } else {
    this.linkage.id=unique(upload.data$linkage.id)
  }
  
  

  ## organise into records 
  ## A record = same source, treatement.area, affected.flow, intervention.type, IJ and AJ on annnouncement day

  # Upload new records
  for(source in unique(upload.data$state.act.source)){
    #source=unique(upload.data$state.act.source)
    
    upload.this=subset(upload.data, state.act.source==source)
    
    for(date in unique(upload.this$date.announced)){ 
      #date=unique(upload.this$date.announced)
      
      upload.this.date=subset(upload.this, date.announced==as.Date(as.numeric(date), origin="1970-01-01"))
      
      for(int.type.id in unique(upload.this$intervention.type.id)){
        # int.type.id=unique(upload.this$intervention.type.id)
        
        # upload.this.type=upload.data
        # int.type.id=upload.data$intervention.type.id
        # date=upload.this.type$date.announced
        
        upload.this.type=subset(upload.this.date, intervention.type.id==int.type.id)
        
        ## is there a record already?
        
        # get source
        this.source.id=gta_delta_source_id(source.description=unique(upload.this.type$state.act.source),
                                           source.official=unique(upload.this.type$is.source.official),
                                           create.source=T,
                                           db.connection=db.connection)
        
        
        # get treatment.area.id
        query=paste("SELECT treatment_area_id
                     FROM delta_treatment_area_list
                     WHERE treatment_area_name = '",
                     upload.this.type$treatment.area,"';", sep="")
        
        
        this.area.id=gta_sql_get_value(query=query,
                                        db.connection=db.connection)
        rm(query)

        # this.area.id=1
        
     
        if(is.na(this.area.id)){stop("Treatment area could not be identified.")}
        
        
        #### record.log
        record.query=paste("SELECT reclog.record_id 
                           FROM delta_record_log reclog
                           JOIN delta_record_area recarea                           
                           ON reclog.record_id = recarea.record_id
                           JOIN delta_record_source recsrc                           
                           ON recarea.record_id = recsrc.record_id
                           WHERE recsrc.source_id = ",this.source.id,"
                           AND reclog.intervention_type_id = ",upload.this.type$intervention.type.id,"
                           AND reclog.record_date_announced = '",as.character(as.Date(as.numeric(date), origin="1970-01-01")),"'
                           AND recarea.treatment_area_id = ",this.area.id,";", sep="")
       
        this.record.id=gta_sql_get_value(query=record.query,
                                         db.connection=db.connection)
        
        rm(record.query)
        
        if(length(this.record.id)>1){stop("Multiple records where there should only be one.")}
        
        if(is.na(this.record.id)){
          ## If there is not: create new records in the relevant sheets

          record.log.update<<-data.frame(intervention.type.id=int.type.id,
                                       affected.flow.id=upload.this.type$affected.flow.id,
                                       implementation.level.id=upload.this.type$implementation.level.id ,
                                       eligible.firms.id=upload.this.type$eligible.firms.id ,
                                       is.mfn=is.na(upload.this.type$nonmfn.affected.id),
                                       record.date.created=Sys.Date(),
                                       record.date.announced=as.Date(as.numeric(date), origin="1970-01-01"),
                                       stringsAsFactors = F)
          
          this.record.id=gta_sql_append_table(append.table = "record.log",
                                              append.by.df = "record.log.update",
                                              get.id = "record.id",
                                              db.connection=db.connection)
          
          rm(record.log.update)
        } 
        
        #### record.linkage
        query=paste("SELECT * 
                    FROM delta_record_linkage 
                    WHERE record_id = ", this.record.id,"
                    AND linkage_id = ", this.linkage.id,";", sep="")
        
        got.rec.link=gta_sql_get_value(query=query,
                                      db.connection=db.connection)
        
        if(nrow(got.rec.link)==0){
        
          record.linkage.update<<-data.frame(record.id=this.record.id,
                                           linkage.id=this.linkage.id,
                                           stringsAsFactors = F)
          
          gta_sql_append_table(append.table = "record.linkage",
                               append.by.df = "record.linkage.update",
                               db.connection=db.connection)
          
          rm(record.linkage.update)
          
        }
        rm(query,got.rec.link)
        
        #### record.area
        query=paste("SELECT * 
                    FROM delta_record_area 
                    WHERE record_id = ", this.record.id,"
                    AND treatment_area_id = ", this.area.id,";", sep="")
        
        got.rec.area=gta_sql_get_value(query=query,
                                       db.connection=db.connection)
        
        if(nrow(got.rec.area)==0){
          
          record.area.update<<-data.frame(record.id=this.record.id,
                                        treatment.area.id=this.area.id,
                                        stringsAsFactors = F)
          
          gta_sql_append_table(append.table = "record.area",
                               append.by.df = "record.area.update",
                               db.connection=db.connection)
          
          rm(record.area.update)
          
        }
        rm(query,got.rec.area)
        
        
        
        
        #### record.implementer
        query=paste("SELECT * 
                    FROM delta_record_implementer
                    WHERE record_id = ", this.record.id,"
                    AND implementing_jurisdiction_id = ", upload.this.type$implementing.jurisdiction.id,";", sep="")
        
        got.rec.imp=gta_sql_get_value(query=query,
                                       db.connection=db.connection)
        
        if(nrow(got.rec.imp)==0){
          
          record.imp.update<<-data.frame(record.id=this.record.id,
                                       implementing.jurisdiction.id=upload.this.type$implementing.jurisdiction.id,
                                       stringsAsFactors = F)
          
          gta_sql_append_table(append.table = "record.implementer",
                               append.by.df = "record.imp.update",
                               db.connection=db.connection)
          
          rm(record.imp.update)
          
        }
        rm(query,got.rec.imp)
        
        
        
        #### record.source
        query=paste("SELECT * 
                    FROM delta_record_source 
                    WHERE record_id = ", this.record.id,"
                    AND source_id = ", this.source.id,";", sep="")
        
        got.rec.src=gta_sql_get_value(query=query,
                                      db.connection=db.connection)
        rm(query)
        
        if(nrow(got.rec.src)==0){
          
          record.src.update<<-data.frame(record.id=this.record.id,
                                       source.id=this.source.id,
                                       stringsAsFactors = F)
          
          gta_sql_append_table(append.table = "record.source",
                                              append.by.df = "record.src.update",
                                              db.connection=db.connection)
          
          rm(record.src.update)
          
        }
        
        
        #### treatment.log
        
        ## is.intervention?
        link.remote.data=gta_delta_get_relevant_records(implementer.id=upload.this.type$implementing.jurisdiction.id,
                                                        treatment.area=upload.this.type$treatment.area,
                                                        affected.flow.id=upload.this.type$affected.flow.id,
                                                        treatment.code=upload.this.type$treatment.code,
                                                        treatment.code.type=upload.this.type$treatment.code.type,
                                                        affected.country.id=upload.this.type$nonmfn.affected.id,
                                                        excl.mfn=F,
                                                        excl.prolongation=F,
                                                        cut.off.date=upload.this.type$date.implemented,
                                                        incl.prior.record=T,
                                                        incl.same.date.record=F,
                                                        incl.subsequent.record=F,
                                                        db.connection=db.connection)
        
        if(nrow(link.remote.data)>0){
          
          pre.value=link.remote.data$treatment.value
          pre.unit=link.remote.data$treatment.unit.id
          
          is.int=(pre.value==upload.this.type$treatment.value & pre.unit==upload.this.type$treatment.unit.id)==F
          
          
        } else {
          is.int=T
        }
      
        ## announced as temporary
        if(any(c(is.na(upload.this.type$announced.removal.date),
                 is.na(upload.this.type$implementer.end.date),
                 is.na(upload.this.type$nonmfn.affected.end.date),
                 is.na(upload.this.type$treatment.code.end.date))==F)){
          this.is.temporary=T
        } else {
          this.is.temporary=F
        }
        
        
        # update treatment.log
        record.treatment.update<<-data.frame(record.id=this.record.id,
                                           date.implemented=upload.this.type$date.implemented,
                                           treatment.code=upload.this.type$treatment.code,
                                           treatment.code.type=upload.this.type$treatment.code.type,
                                           treatment.value=upload.this.type$treatment.value,
                                           treatment.unit.id=upload.this.type$treatment.unit.id,
                                           treatment.code.official=upload.this.type$treatment.code.official,
                                           is.intervention=is.int,
                                           was.enforced=T,
                                           announced.as.temporary=this.is.temporary,
                                           stringsAsFactors = F)
        
        gta_sql_append_table(append.table = paste(upload.this.type$treatment.area,".log",sep=""),
                             append.by.df = "record.treatment.update",
                             db.connection=db.connection)
        
        rm(record.treatment.update)

        ## adding the removal date on an MFN announcement
        
        if(is.na(upload.this.type$announced.removal.date)==F){
          
          if(nrow(link.remote.data)>0){
            # re-instating the prior status using link.remote.data
            record.treatment.update<<-data.frame(record.id=this.record.id,
                                               date.implemented=upload.this.type$announced.removal.date,
                                               treatment.code=upload.this.type$treatment.code,
                                               treatment.code.type=upload.this.type$treatment.code.type,
                                               treatment.value=link.remote.data$treatment.value,
                                               treatment.unit.id=link.remote.data$treatment.unit.id,
                                               treatment.code.official=link.remote.data$treatment.code.official,
                                               is.intervention=T,
                                               was.enforced=T,
                                               announced.as.temporary=F,
                                               stringsAsFactors = F)
            
          } else {
            # log a discrepancy
            
            input.discrepancy.log.update<<-data.frame(input.id=this.input.id,
                                                    record.id=this.record.id,	
                                                    discrepancy.date=upload.this.type$announced.removal.date,	
                                                    discrepancy.value=NA,	
                                                    discrepancy.value.unit.id=upload.this.type$treatment.unit.id,
                                                    discrepancy.code.official=upload.this.type$treatment.code.official,
                                                    discrepancy.source.id=this.source.id,
                                                    discrepancy.description="Not prior value for temporary record",
                                                    stringsAsFactors = F)
            
            gta_sql_append_table(append.table = "input.discrepancy.log",
                                                append.by.df = "input.discrepancy.log.update",
                                                db.connection=db.connection)
            
            
          }

        }
        
        rm(link.remote.data)
        
        ### possibly
        ## updating treatment.log is.intervention/was.enforced
        
        link.remote.data=gta_delta_get_relevant_records(implementer.id=upload.this.type$implementing.jurisdiction.id,
                                                        treatment.area=upload.this.type$treatment.area,
                                                        affected.flow.id=upload.this.type$affected.flow.id,
                                                        treatment.code=upload.this.type$treatment.code,
                                                        treatment.code.type=upload.this.type$treatment.code.type,
                                                        affected.country.id=upload.this.type$nonmfn.affected.id,
                                                        excl.mfn=T,
                                                        excl.prolongation=F,
                                                        cut.off.date=upload.this.type$date.implemented,
                                                        incl.prior.record=F,
                                                        incl.same.date.record=F,
                                                        incl.subsequent.record=T,
                                                        db.connection=db.connection)
        
        if(nrow(link.remote.data)>0){
          
          ## is.intervention
          post.value=link.remote.data$treatment.value
          post.unit=link.remote.data$treatment.unit.id
          
          is.int=(post.value==upload.this.type$treatment.value & post.unit==upload.this.type$treatment.unit.id)==F
          
          ## is.intervention: Update existing entry to become a prolongation
          if(is.int!=link.remote.data$is.intervention){
            
            int.query=paste("UPDATE delta_",upload.this.type$treatment.area,"_log 
                            SET is_intervention = ",as.numeric(is.int),"
                            WHERE record_id = ",this.record.id,"
                            AND date_implemented = '",as.character(upload.this.type$date.implemented),"'
                            AND treatment_code = ",upload.this.type$treatment.code,";", sep="")
            
            gta_sql_update_table(int.query,
                                 db.connection=db.connection)
            rm(int.query)
            
            
          }
          
          ## was.enforced
          record.query=paste("SELECT treatlog.record_id, was_enforced
                             FROM delta_",upload.this.type$treatment.area,"_log treatlog
                             JOIN delta_record_log reclog
                             ON treatlog.record_id = reclog.record_id
                             JOIN delta_record_linkage reclink
                             ON reclog.record_id = reclink.record_id 
                             WHERE reclink.linkage_id = ",this.linkage.id,"
                             AND reclog.intervention_type_id = ",upload.this.type$intervention.type.id,"
                             AND reclog.record_date_announced < '",as.character(upload.this.type$date.announced),"'
                             AND treatlog.treatment_code = ",upload.this.type$treatment.code,"
                             AND treatlog.was_enforced = 1
                             AND treatlog.date_implemented BETWEEN '",as.character(upload.this.type$date.announced),"' AND '",as.character(upload.this.type$date.implemented),"';", sep="")
          
          was.not.enforced=gta_sql_get_value(query=record.query,
                                           db.connection=db.connection)
          rm(record.query)
          
          if(nrow(was.not.enforced)>0){
            
            update.recs=unique(was.not.enforced$record.id)
            
            
            was.query=paste("UPDATE delta_",upload.this.type$treatment.area,"_log 
                            SET was_enforced = 0
                            WHERE record_id IN (",paste(update.recs, collapse=","),")
                            AND treatment_code = ",upload.this.type$treatment.code,";", sep="")
            
            gta_sql_update_table(was.query,
                                 db.connection=db.connection)
            
            rm(was.query,update.recs)
            
          }
          
          
        }
        
        #### nonmfn.state.log updates
        if(is.na(upload.this.type$nonmfn.affected.id)==F){
          
     
          gta_delta_update_nonmfn_state(linkage.id=this.linkage.id,
                                        treatment.area=upload.this.type$treatment.area,
                                        local.start.date=upload.this.type$date.implemented,
                                        local.end.date.affected.cty=upload.this.type$nonmfn.affected.end.date,
                                        local.end.date.code=upload.this.type$treatment.code.end.date, 
                                        local.end.date.implementer=upload.this.type$implementer.end.date,
                                        db.connection=db.connection)
          
          
        }
        
        
        #### coarse.code.log
        if(is.na(upload.this.type$coarse.code)==F){
          
          coarse.code.log.update<<-data.frame(record.id=this.record.id,
                                            coarse.code=upload.this.type$coarse.code,
                                            coarse.code.type=upload.this.type$coarse.code.type,
                                            stringsAsFactors = F)
          
          gta_sql_append_table(append.table = "coarse.code.log",
                               append.by.df = "coarse.code.log.update",
                               db.connection=db.connection)
          
          rm(coarse.code.log.update)
        }
        
        #### framework.log & record.framework
        
        if(is.na(upload.this.type$framework.id)==F){
          ###### framework.log
          
          ###### record.framework
          
        }
        
    
        #### rollback.log
        
      }
      
      }
    }

}

# gta_delta_input_upload(send.to.remote=upload.data,
#                        input.id=this.input.id,
#                        input.name=input.name,
#                        db.connection=db.connection)