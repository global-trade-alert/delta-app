gta_delta_resolve_discrepancy=function(
  discrepancy.ids=NULL,
  keep.discrepancy.value = NULL,
  user.id = NULL,
  db.connection='pool'
){
  
  if(length(discrepancy.ids)<1) stop('non positive length of discrepancy ids provided!!')
  if(!all(is.integer(as.integer(discrepancy.ids)) & !is.na(discrepancy.ids))) stop('only numeric input accepted')
  
  if(length(keep.discrepancy.value)!=1 & !is.logical(keep.discrepancy.value)) stop('keep.discrepancy.value must be True or False and of length 1')

  discrepancy.ids = data.frame(discrepancy.id = discrepancy.ids)
  resolution.name=paste0('resolution_',user.id)
  assign(resolution.name, discrepancy.ids, envir = globalenv())
  gta_sql_get_value(paste0('DROP TABLE IF EXISTS delta_',resolution.name,';'),db.connection=db.connection)
  gta_sql_create_table(write.df=resolution.name,
                       contains.data = T,
                       append.existing = F)
  
  
  if(keep.discrepancy.value==F){
    sql.resolve.discr = paste0("DELETE delta_input_discrepancy_log
                                FROM delta_input_discrepancy_log 
                                JOIN delta_",resolution.name," ON delta_input_discrepancy_log.discrepancy_id = delta_",resolution.name,".discrepancy_id
                                WHERE 1 = 1;
                               
                                DROP TABLE IF EXISTS delta_",resolution.name,";"
                               )
    gta_sql_multiple_queries(sql.resolve.discr,output.queries = 1, show.time = T, db.connection = db.connection)
  }
  
  if(keep.discrepancy.value==T){
    sql.resolve.discr = paste0("UPDATE delta_tariff_log 
                                JOIN (SELECT discr_log.discrepancy_value, discr_log.treat_id FROM delta_input_discrepancy_log discr_log
                                      JOIN delta_",resolution.name," ON discr_log.discrepancy_id = delta_",resolution.name,".discrepancy_id) discrepancies
                                ON delta_tariff_log.treat_id = discrepancies.treat_id
                                SET delta_tariff_log.treatment_value = discrepancies.discrepancy_value;
                                
                                DELETE delta_treatment_record
                                FROM delta_treatment_record
                                JOIN (SELECT discr_log.treat_id FROM delta_input_discrepancy_log discr_log
                                      JOIN delta_",resolution.name," ON discr_log.discrepancy_id = delta_",resolution.name,".discrepancy_id) discrepancies
                                ON discrepancies.treat_id = delta_treatment_record.treat_id 
                                WHERE 1 = 1;
                                
                                INSERT INTO delta_treatment_record(treat_id, record_id, input_id)
                                SELECT treat_id, record_id, input_id FROM delta_input_discrepancy_log
                                JOIN delta_",resolution.name," ON delta_input_discrepancy_log.discrepancy_id = delta_",resolution.name,".discrepancy_id;
                                
                                DELETE delta_treatment_linkage
                                FROM delta_treatment_linkage
                                JOIN (SELECT discr_log.treat_id FROM delta_input_discrepancy_log discr_log
                                      JOIN delta_",resolution.name," ON discr_log.discrepancy_id = delta_",resolution.name,".discrepancy_id) discrepancies
                                ON discrepancies.treat_id = delta_treatment_linkage.treat_id 
                                WHERE 1 = 1;
                                
                                INSERT INTO delta_treatment_linkage(treat_id, linkage_id, input_id)
                                SELECT treat_id, linkage_id, input_id FROM delta_input_discrepancy_log
                                JOIN delta_",resolution.name," ON delta_input_discrepancy_log.discrepancy_id = delta_",resolution.name,".discrepancy_id;
                               
                                DELETE delta_input_discrepancy_log
                                FROM delta_input_discrepancy_log
                                JOIN delta_",resolution.name," ON delta_",resolution.name,".discrepancy_id = delta_input_discrepancy_log.discrepancy_id
                                WHERE 1 = 1;
                                
                                DROP TABLE IF EXISTS delta_",resolution.name,";"
                               
                               )
    
    
    gta_sql_multiple_queries(sql.resolve.discr,output.queries = 1, show.time = T, db.connection = db.connection)
  }
    
  
}

