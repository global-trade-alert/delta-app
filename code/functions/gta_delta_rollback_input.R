gta_delta_rollback_input=function(
  input.id=NULL,
  db.connection='pool'
){
  
  if(length(input.id)!=1) stop('only single input ids are currently accepted')
  if(!is.integer(as.integer(input.id)) & !is.na(input.id)) stop('only numeric input ids accepted')
  
  sql.rollback = paste0("DELETE delta_record_implementer, delta_record_source, delta_record_framework, delta_record_log, delta_treatment_record, delta_nonmfn_state_records, delta_input_discrepancy_log
                          FROM delta_record_implementer
                          JOIN (SELECT record_id FROM 
                          		(SELECT DISTINCT delta_treatment_record.record_id, delta_treatment_record.input_id 
                          		FROM delta_treatment_record 
                          		JOIN (SELECT record_id FROM delta_treatment_record WHERE input_id = ",input.id,") input_recs ON delta_treatment_record.record_id = input_recs.record_id) treat_rec
                          		GROUP BY treat_rec.record_id
                          	  HAVING COUNT(*) = 1) del_recs ON del_recs.record_id = delta_record_implementer.record_id 
                          JOIN delta_record_framework ON del_recs.record_id = delta_record_framework.record_id
                          JOIN delta_record_source ON del_recs.record_id = delta_record_source.record_id
                          JOIN delta_record_log ON del_recs.record_id = delta_record_log.record_id 
                          JOIN delta_treatment_record ON del_recs.record_id = delta_treatment_record.record_id 
                          LEFT JOIN delta_nonmfn_state_records ON del_recs.record_id = delta_nonmfn_state_records.record_id
                          LEFT JOIN delta_input_discrepancy_log ON del_recs.record_id = delta_input_discrepancy_log.record_id
                          WHERE 1 = 1;
                          
                          DELETE FROM delta_nonmfn_state_log
                          WHERE NOT EXISTS (SELECT NULL FROM delta_nonmfn_state_records del_states WHERE del_states.status_id = delta_nonmfn_state_log.status_id); 
                          
                          DELETE delta_tariff_log, delta_treatment_linkage, delta_treatment_area, delta_coarse_code_log, delta_input_discrepancy_log
                          FROM delta_treatment_linkage
                          JOIN (SELECT treat_id FROM 
                          		(SELECT DISTINCT delta_treatment_linkage.treat_id, delta_treatment_linkage.input_id
                          		FROM delta_treatment_linkage 
                          		JOIN (SELECT treat_id FROM delta_treatment_linkage WHERE input_id = ",input.id,") input_treats ON delta_treatment_linkage.treat_id = input_treats.treat_id) treat_link
                          		GROUP BY treat_link.treat_id
                          	HAVING COUNT(*) = 1) del_treat ON delta_treatment_linkage.treat_id = del_treat.treat_id 
                          JOIN delta_tariff_log ON delta_tariff_log.treat_id = del_treat.treat_id
                          JOIN delta_treatment_area ON del_treat.treat_id = delta_treatment_area.treat_id
                          LEFT JOIN delta_coarse_code_log ON delta_coarse_code_log.treat_id = del_treat.treat_id
                          LEFT JOIN delta_input_discrepancy_log ON delta_input_discrepancy_log.treat_id = del_treat.treat_id
                          WHERE 1 = 1;
                          	 
                          DELETE FROM delta_treatment_record
                          WHERE input_id = ",input.id,";
                          
                          DELETE FROM delta_treatment_linkage
                          WHERE input_id = ",input.id,";
                          
                          DELETE FROM delta_input_discrepancy_log
                          WHERE input_id = ",input.id,";
                          
                          DELETE FROM delta_input_log
                          WHERE input_id = ",input.id,";
                          "
                        )
  #cat(sql.rollback)
  gta_sql_multiple_queries(sql.rollback,output.queries = 1, show.time = T, db.connection = db.connection)
}





