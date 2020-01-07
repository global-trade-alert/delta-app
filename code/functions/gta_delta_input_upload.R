gta_delta_upload=function(
  delta.data=data,
  input.name=NULL,
  user.id=NULL,
  db.connection='pool'
){
  
  source('17 Shiny/6 delta app/code/functions/gta_delta_get_jurisdiction_id.R')
  
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
  
  treatment.area=unique(delta.data$treatment.area)
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
  
  linkages=unique(delta.data[,c("implementing.jurisdiction.id","affected.flow.id","treatment.code", "treatment.code.type","nonmfn.affected.id")])
  linkages$linkage.id=-1:-nrow(linkages) # negative values avoid duplication with existing linkage.id's in the remote database
  
  delta.data=merge(delta.data, linkages, by=c("implementing.jurisdiction.id","affected.flow.id","treatment.code", "treatment.code.type","nonmfn.affected.id"), all.x=T)
  
  ## now have to compare the local and the database values for date.implemented-treatment.value-treatement.unit.type.id for each linkage
  
  temp.up.name=paste0("temp.upload.data.",user.id)
  assign(temp.up.name,delta.data,envir=globalenv())
  
  gta_sql_get_value(paste0("DROP TABLE IF EXISTS delta_",gsub('\\.','_',temp.up.name),";"),db.connection = 'pool')
  gta_sql_create_table(write.df=temp.up.name,
                       append.existing = F)
  
  
  sql.statement=paste0(
    "
    /* ADD NEW SOURCES */
    INSERT INTO delta_source_log (state_act_source, is_source_official)
    SELECT DISTINCT state_act_source, is_source_official
    FROM delta_temp_upload_data_",user.id,"
    WHERE state_act_source NOT IN (SELECT DISTINCT state_act_source FROM delta_source_log);
    
    /* ADD NEW LINKAGES */
    INSERT INTO delta_root_log (linkage_implementer_id, linkage_affected_flow_id, linkage_code, linkage_code_type) 
    SELECT DISTINCT implementing_jurisdiction_id linkage_implementer_id, affected_flow_id linkage_affected_flow_id, 
    treatment_code linkage_code, treatment_code_type linkage_code_type
    FROM delta_temp_upload_data_",user.id," up_data
    WHERE NOT EXISTS
    (SELECT 1
    FROM delta_root_log root_log
    WHERE root_log.linkage_implementer_id = up_data.implementing_jurisdiction_id
    AND root_log.linkage_affected_flow_id = up_data.affected_flow_id
    AND root_log.linkage_code = up_data.treatment_code
    AND root_log.linkage_code_type = up_data.treatment_code_type
    );
    
    ALTER TABLE delta_temp_upload_data_",user.id,"
    ADD root_id DOUBLE NULL;
    
    /* ATTRIBUTE ROOT IDS */
    UPDATE delta_temp_upload_data_",user.id," up_data, delta_root_log
    SET up_data.root_id = delta_root_log.root_id
    WHERE up_data.implementing_jurisdiction_id = delta_root_log.linkage_implementer_id
    AND up_data.treatment_code = delta_root_log.linkage_code
    AND up_data.affected_flow_id = delta_root_log.linkage_affected_flow_id
    AND up_data.treatment_code_type = delta_root_log.linkage_code_type;
    
    /* INSERT NEW LINKAGES */
    INSERT INTO delta_linkage_log (root_id, linkage_affected_country_id) 
    SELECT DISTINCT root_id, nonmfn_affected_id linkage_affected_country_id
    FROM delta_temp_upload_data_",user.id," up_data
    WHERE NOT EXISTS
    (SELECT 1
    FROM delta_linkage_log link_log
    WHERE link_log.root_id = up_data.root_id
    AND (link_log.linkage_affected_country_id = up_data.nonmfn_affected_id OR (
    link_log.linkage_affected_country_id IS NULL AND up_data.nonmfn_affected_id IS NULL))
    );
    
    /* ATTRIBUTE LINKAGES IDS */
    UPDATE delta_temp_upload_data_",user.id," up_data, delta_linkage_log
    SET up_data.linkage_id = delta_linkage_log.linkage_id
    WHERE up_data.root_id = delta_linkage_log.root_id
    AND (up_data.nonmfn_affected_id = delta_linkage_log.linkage_affected_country_id OR (
    up_data.nonmfn_affected_id IS NULL AND delta_linkage_log.linkage_affected_country_id IS NULL));
    
    ALTER TABLE delta_temp_upload_data_",user.id,"
    ADD live_treatment_value DOUBLE NULL,
    ADD live_treatment_unit_id INT NULL;
    
    /* MERGE TREATMENT VALUES WITH SAME DATE/LINK AND DROP THOSE WITH IDENTICAL INFORMATION AS IN DATABASE */
    UPDATE delta_temp_upload_data_",user.id," up_data, delta_record_linkage, delta_tariff_log
    SET up_data.live_treatment_value = delta_tariff_log.treatment_value,
    up_data.live_treatment_unit_id = delta_tariff_log.treatment_unit_id
    WHERE up_data.linkage_id = delta_record_linkage.linkage_id
    AND delta_record_linkage.record_id = delta_tariff_log.record_id
    AND up_data.date_implemented = delta_tariff_log.date_implemented 
    AND up_data.treatment_code = delta_tariff_log.treatment_code
    AND up_data.treatment_code_type = delta_tariff_log.treatment_code_type;
    
    /* find which ids are duplicates and then remove those ids */
    DELETE FROM delta_temp_upload_data_",user.id,"
    WHERE treatment_value = live_treatment_value
    AND treatment_unit_id = live_treatment_unit_id;
    
    ALTER TABLE delta_temp_upload_data_",user.id,"
    ADD COLUMN source_id INT NULL,
    ADD treatment_area_id INT NULL;
    
    UPDATE delta_temp_upload_data_",user.id," up_data, delta_source_log, delta_treatment_area_list
    SET up_data.treatment_area_id = delta_treatment_area_list.treatment_area_id,
    up_data.source_id = delta_source_log.source_id
    WHERE up_data.state_act_source = delta_source_log.state_act_source
    AND up_data.treatment_area = delta_treatment_area_list.treatment_area_name;
    
    DROP TABLE IF EXISTS delta_temp_records_29 ;
    
    /* ADD NEW RECORD IDS */
    CREATE TABLE delta_temp_records_29  AS
    SELECT DISTINCT up_data.affected_flow_id, up_data.eligible_firms_id, up_data.implementation_level_id, up_data.intervention_type_id,
    (CASE WHEN up_data.nonmfn_affected IS NULL THEN 1 ELSE 0 END) AS is_mfn, up_data.date_announced, up_data.implementing_jurisdiction_id, up_data.treatment_area_id, up_data.source_id, up_data.framework_id,
    DATE(NOW()) as date_created
    FROM delta_temp_upload_data_",user.id," up_data, delta_source_log
    WHERE NOT EXISTS
    (SELECT 1 FROM
    (SELECT reconstructed_rec_log.affected_flow_id, reconstructed_rec_log.eligible_firms_id, reconstructed_rec_log.implementation_level_id, reconstructed_rec_log.intervention_type_id,
    reconstructed_rec_log.is_mfn, reconstructed_rec_log.date_announced date_announced, reconstructed_rec_log.implementing_jurisdiction_id, reconstructed_rec_log.source_id, reconstructed_rec_log.treatment_area_id, delta_record_framework.framework_id
    FROM (SELECT DISTINCT rec_log.record_id, rec_log.affected_flow_id, rec_log.eligible_firms_id, rec_log.implementation_level_id, rec_log.intervention_type_id,
    rec_log.is_mfn, rec_log.record_date_announced date_announced, rec_impl.implementing_jurisdiction_id, rec_source.source_id, rec_area.treatment_area_id
    FROM delta_record_log rec_log, delta_record_implementer rec_impl, delta_record_area rec_area, delta_record_source rec_source
    WHERE rec_log.record_id = rec_impl.record_id
    AND rec_log.record_id = rec_area.record_id
    AND rec_log.record_id = rec_source.record_id
    ) reconstructed_rec_log
    LEFT JOIN delta_record_framework
    ON reconstructed_rec_log.record_id = delta_record_framework.record_id
    ) existing_records
    WHERE existing_records.affected_flow_id = up_data.affected_flow_id
    AND existing_records.eligible_firms_id = up_data.eligible_firms_id
    AND existing_records.implementation_level_id = up_data.implementation_level_id
    AND existing_records.intervention_type_id = up_data.intervention_type_id
    AND existing_records.is_mfn = (CASE WHEN up_data.nonmfn_affected IS NULL THEN 1 ELSE 0 END)
    AND existing_records.date_announced = up_data.date_announced
    AND existing_records.implementing_jurisdiction_id = up_data.implementing_jurisdiction_id
    AND existing_records.source_id = up_data.source_id
    AND existing_records.treatment_area_id = up_data.treatment_area_id
    AND ((existing_records.framework_id = up_data.framework_id) OR (existing_records.framework_id IS NULL AND up_data.framework_id IS NULL))
    );
    
    ALTER TABLE delta_temp_records_29 ADD COLUMN record_id INT NOT NULL AUTO_INCREMENT KEY;
    
    UPDATE delta_temp_records_29
    SET record_id=record_id+(SELECT AUTO_INCREMENT FROM information_schema.tables WHERE table_name='delta_record_log') - 1;
    
    /* ADD NEW RECORDS */
    INSERT INTO delta_record_log (intervention_type_id, affected_flow_id, implementation_level_id, eligible_firms_id, is_mfn, source_id, record_date_announced, record_date_created) 
    SELECT intervention_type_id, affected_flow_id, implementation_level_id, eligible_firms_id,
    is_mfn, source_id, date_announced record_date_created, date_created AS record_date_created
    FROM delta_temp_records_29
    ORDER BY record_id;
    
    /* ADD RECORD IMPLEMENTER */
    INSERT INTO delta_record_implementer (record_id, implementing_jurisdiction_id)
    SELECT DISTINCT record_id, implementing_jurisdiction_id
    FROM delta_temp_records_29 ;
    
    /* ADD RECORD SOURCE */
    INSERT INTO delta_record_source (record_id, source_id)
    SELECT DISTINCT record_id, source_id
    FROM delta_temp_records_29 ;
    
    /* ADD RECORD TREATMENT AREA */
    INSERT INTO delta_record_area (record_id, treatment_area_id)
    SELECT DISTINCT record_id, treatment_area_id
    FROM delta_temp_records_29 ;
    
    /* ADD RECORD FRAMEWORK */
    INSERT INTO delta_record_framework (record_id, framework_id)
    SELECT DISTINCT record_id, framework_id
    FROM delta_temp_records_29;
    
    DROP TABLE IF EXISTS delta_temp_records_29 ;
    
    ALTER TABLE delta_temp_upload_data_",user.id,"
    ADD record_id INT NULL;
    
    UPDATE delta_temp_upload_data_",user.id," up_data, delta_record_log, delta_record_area, delta_record_implementer , delta_record_framework
    SET up_data.record_id = delta_record_log.record_id
    WHERE up_data.treatment_area_id = delta_record_area.treatment_area_id
    AND up_data.implementing_jurisdiction_id = delta_record_implementer.implementing_jurisdiction_id
    AND ((up_data.framework_id = delta_record_framework.framework_id) OR (up_data.framework_id IS NULL AND delta_record_framework.framework_id IS NULL))
    AND delta_record_log.record_id = delta_record_area.record_id 
    AND delta_record_log.record_id = delta_record_implementer.record_id 
    AND delta_record_log.record_id = delta_record_framework.record_id 
    AND up_data.affected_flow_id = delta_record_log.affected_flow_id
    AND up_data.eligible_firms_id = delta_record_log.eligible_firms_id
    AND up_data.implementation_level_id = delta_record_log.implementation_level_id
    AND up_data.intervention_type_id = delta_record_log.intervention_type_id
    AND (CASE WHEN up_data.nonmfn_affected IS NULL THEN 1 ELSE 0 END) = delta_record_log.is_mfn
    AND up_data.date_announced = delta_record_log.record_date_announced
    AND up_data.source_id = delta_record_log.source_id
    ;
    
    /* ADD RECORD LINKAGES */
    INSERT INTO delta_record_linkage (record_id, linkage_id)
    SELECT DISTINCT record_id, linkage_id
    FROM delta_temp_upload_data_",user.id," ;
    
    /* ADD COARSE RECORDS */
    INSERT INTO delta_coarse_code_log (record_id, coarse_code, coarse_code_type)
    SELECT DISTINCT record_id, coarse_code, coarse_code_type
    FROM delta_temp_upload_data_",user.id,"
    WHERE coarse_code IS NOT NULL;
    
    /* ADD INTO INPUT DISCREPANCY LOG */
    INSERT INTO delta_input_discrepancy_log (input_id, record_id, discrepancy_date, discrepancy_value, discrepancy_value_unit_id, discrepancy_code_official, discrepancy_source_id, discrepancy_description)
    SELECT DISTINCT 1 AS input_id, record_id, date_implemented discrepancy_date,
    treatment_value discrepancy_value, treatment_unit_id discrepancy_value_unit_id, treatment_code_official, b.source_id discrepancy_source_id,
    'Not prior value for temporary record' AS discrepancy_description
    FROM delta_temp_upload_data_",user.id," a, delta_source_log b
    WHERE treatment_value != live_treatment_value
    AND treatment_unit_id = live_treatment_unit_id
    AND a.source_id = b.source_id;
    
    /* DELETE DISCREPANCIES */
    DELETE FROM delta_temp_upload_data_",user.id,"
    WHERE treatment_value != live_treatment_value
    AND treatment_unit_id = live_treatment_unit_id;
    
    /* ADD INTO NONMFN STATE LOG */
    /* FIND OUT HOW TO GET STATE REDUNDANT, FOR TIME BEING JUST ASSUMED TO BE 0 */
    INSERT INTO delta_nonmfn_state_log (linkage_id, treatment_area, nonmfn_state_date, nonmfn_state, source_id, state_redundant)
    SELECT linkage_id, treatment_area, date_implemented nonmfn_state_date, 1 AS nonmfn_state, source_id, 0 AS state_redundant
    FROM delta_temp_upload_data_",user.id,"  a
    WHERE nonmfn_affected IS NOT NULL
    AND nonmfn_affected_end_date IS NULL
    UNION
    SELECT linkage_id, treatment_area, nonmfn_affected_end_date nonmfn_state_date, 0 AS nonmfn_state, source_id, 0 AS state_redundant
    FROM delta_temp_upload_data_",user.id,"  a
    WHERE nonmfn_affected IS NOT NULL
    AND nonmfn_affected_end_date IS NOT NULL;
    
    /* ADD INTO LOG OF APPROPRIATE AREA*/
    INSERT INTO delta_tariff_log (record_id, date_implemented, treatment_code, treatment_code_type, treatment_value, treatment_unit_id, treatment_code_official, announced_as_temporary)
    SELECT DISTINCT record_id, date_implemented, treatment_code, treatment_code_type, treatment_value, treatment_unit_id, treatment_code_official,
    (CASE WHEN nonmfn_affected_end_date IS NOT NULL THEN 1 ELSE 0 END) AS announced_as_temporary
    FROM delta_temp_upload_data_",user.id,";
    
    DROP TABLE delta_temp_upload_data_",user.id," ;



    "
  )

  #return(cat(sql.statement))
  gta_sql_multiple_queries(sql.statement,output.queries = 1, show.time = T)
  
  
}