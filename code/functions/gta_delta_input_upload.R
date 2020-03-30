gta_delta_upload=function(
  delta.data=data,
  input.name=NULL,
  user.id=NULL,
  db.connection='pool'
){
  
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
    

    input.log.update<<-data.frame(user.id=user.id,
                                  input.date=substr(as.POSIXct(format(Sys.time()),tz="CET"),1,19), #format time as CET
                                  input.name=input.name,
                                  input.state='incomplete',
                                  stringsAsFactors = F)
    
    this.input.id=gta_sql_append_table(append.table = "input.log",
                                       append.by.df = "input.log.update",
                                       get.id = "input.id",
                                       db.connection=db.connection)
      
    
    
    delta.data$input.id=this.input.id
       
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
    
    coarse=merge(subset(coarse, select=names(coarse)[!names(coarse) %in% c('treatment.code','coarse.code','coarse.code.type')]),
                 expanded.output, by="processing.id", all.x=T)
    coarse$processing.id=NULL 
    delta.data=rbind(subset(delta.data, !(nchar(delta.data$treatment.code) < 5 & delta.data$treatment.code.type=='hs')),
                     subset(coarse, select=names(delta.data)))
    
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

  delta.data=merge(delta.data, linkages, by=c("implementing.jurisdiction.id","affected.flow.id","treatment.code", "treatment.code.type","nonmfn.affected.id"), all.x=T)
  delta.data = cbind(processing.id = 1:nrow(delta.data), delta.data)
  ## now have to compare the local and the database values for date.implemented-treatment.value-treatement.unit.type.id for each linkage
  
  temp.up.name=paste0("temp.upload.data.",user.id)
  assign(temp.up.name,delta.data,envir=globalenv())
  
  gta_sql_get_value(paste0("DROP TABLE IF EXISTS delta_",gsub('\\.','_',temp.up.name),";"),db.connection = 'pool')
  gta_sql_create_table(write.df=temp.up.name,
                       append.existing = F,
                       create.primary.key =  "processing.id",
                       create.primary.auto.incr = T)
  
  
  sql.statement=paste0(
    "
    /* REMAININGS THINGS TO IMPLEMENT: 
     * 1. TRANSFORM DELTA_TREATMENT_RECORDS TO START.ID END.ID ( i dont like this approach, slow and doesn't use sql tree structure )
     * 
     * */
    
    /* ADD COLUMNS FOR LATER USE */
    ALTER TABLE delta_temp_upload_data_",user.id,"
    ADD source_id INT NULL,
    ADD treatment_area_id INT NULL,
    ADD nonmfn_status_id INT NULL,
    ADD mfn_status_id INT NULL,
    ADD record_id INT NULL,
    ADD linkage_id INT NULL AFTER implementing_jurisdiction_id,
    ADD discrepancy_treat_id INT NULL AFTER implementing_jurisdiction_id,
    ADD announced_as_temporary INT NULL AFTER implementing_jurisdiction_id,
    ADD new_treat_id INT NULL AFTER implementing_jurisdiction_id,
    ADD existing_treat_id INT NULL AFTER implementing_jurisdiction_id;
    
    CREATE INDEX index_linkage ON delta_temp_upload_data_",user.id,"(implementing_jurisdiction_id, affected_flow_id, treatment_code_type(3), treatment_code);
    
    /* ADD NEW SOURCES */
    INSERT INTO delta_source_log (state_act_source, is_source_official)
    SELECT DISTINCT up_data.state_act_source, up_data.is_source_official
    FROM delta_temp_upload_data_",user.id," up_data
    WHERE NOT EXISTS 
    (SELECT 1 FROM delta_source_log src_log
    WHERE up_data.state_act_source = src_log.state_act_source);
    
    /* ATTACH SOURCE AND TREATMENT AREA ID */
    UPDATE delta_temp_upload_data_",user.id," up_data
    JOIN delta_source_log
    ON up_data.state_act_source = delta_source_log.state_act_source
    JOIN delta_treatment_area_list
    ON up_data.treatment_area = delta_treatment_area_list.treatment_area_name
    SET up_data.treatment_area_id = delta_treatment_area_list.treatment_area_id,
    	up_data.source_id = delta_source_log.source_id;
    
    /* ADD NEW ROOTS */
    INSERT INTO delta_root_log (linkage_implementer_id, linkage_affected_flow_id, linkage_code, linkage_code_type) 
    SELECT * FROM 
    (SELECT DISTINCT implementing_jurisdiction_id, affected_flow_id, treatment_code, treatment_code_type
    FROM delta_temp_upload_data_",user.id," up_data) new_unique_roots
    WHERE NOT EXISTS
    (SELECT 1
    FROM delta_root_log root_log
    WHERE root_log.linkage_implementer_id = new_unique_roots.implementing_jurisdiction_id
    AND root_log.linkage_affected_flow_id = new_unique_roots.affected_flow_id
    AND root_log.linkage_code = new_unique_roots.treatment_code
    AND root_log.linkage_code_type = new_unique_roots.treatment_code_type
    );
    
    /* INSERT NEW LINKAGES */
    INSERT INTO delta_linkage_log (root_id, linkage_affected_country_id) 
    SELECT root_log.root_id, unique_links.nonmfn_affected_id FROM
    (SELECT DISTINCT delta_temp_upload_data_",user.id,".implementing_jurisdiction_id, delta_temp_upload_data_",user.id,".affected_flow_id, delta_temp_upload_data_",user.id,".treatment_code, delta_temp_upload_data_",user.id,".treatment_code_type, delta_temp_upload_data_",user.id,".nonmfn_affected_id
    FROM delta_temp_upload_data_",user.id,") unique_links 
    JOIN delta_root_log root_log 
    ON unique_links.implementing_jurisdiction_id = root_log.linkage_implementer_id
    AND unique_links.treatment_code = root_log.linkage_code
    AND unique_links.affected_flow_id = root_log.linkage_affected_flow_id
    AND unique_links.treatment_code_type = root_log.linkage_code_type
    LEFT JOIN delta_linkage_log ON delta_linkage_log.root_id = root_log.root_id AND unique_links.nonmfn_affected_id <=> delta_linkage_log.linkage_affected_country_id
    WHERE delta_linkage_log.linkage_id IS NULL;
    
    /* ATTRIBUTE LINKAGES IDS */
    UPDATE delta_temp_upload_data_",user.id," up_data
    JOIN delta_root_log root_log
    ON root_log.linkage_implementer_id = up_data.implementing_jurisdiction_id AND root_log.linkage_affected_flow_id = up_data.affected_flow_id AND root_log.linkage_code = up_data.treatment_code AND root_log.linkage_code_type = up_data.treatment_code_type
    JOIN delta_linkage_log link_log
    ON root_log.root_id = link_log.root_id
    AND up_data.nonmfn_affected_id <=> link_log.linkage_affected_country_id
    SET up_data.linkage_id = link_log.linkage_id;
    
    CREATE INDEX index_rec ON delta_temp_upload_data_",user.id,"(affected_flow_id, implementation_level_id, eligible_firms_id, intervention_type_id, date_announced);
    
    /* add to nonmfn state log only those states not already in it */ 
    INSERT INTO delta_nonmfn_state_log (linkage_id, treatment_area, nonmfn_state_date, nonmfn_state, state_redundant) 
    SELECT * FROM 
    (SELECT DISTINCT * FROM 
    (SELECT linkage_id, treatment_area, date_implemented nonmfn_state_date, 1 AS nonmfn_state, 0 AS state_redundant
    FROM delta_temp_upload_data_",user.id," 
    WHERE nonmfn_affected IS NOT NULL
    UNION
    SELECT linkage_id, treatment_area, nonmfn_affected_end_date nonmfn_state_date, 0 AS nonmfn_state, 0 AS state_redundant
    FROM delta_temp_upload_data_",user.id," 
    WHERE nonmfn_affected IS NOT NULL
    AND nonmfn_affected_end_date IS NOT NULL
    ) a) new_states
    WHERE NOT EXISTS
    (SELECT 1
    FROM delta_nonmfn_state_log existing_states
    WHERE existing_states.linkage_id = new_states.linkage_id
    AND existing_states.nonmfn_state = new_states.nonmfn_state
    AND existing_states.nonmfn_state_date = new_states.nonmfn_state_date
    AND existing_states.treatment_area = new_states.treatment_area
    );
    
    /* ADD STATUS INTO NONMFNSTATE RECORDS*/
    UPDATE delta_temp_upload_data_",user.id," up_data
    JOIN delta_nonmfn_state_log state_log
    ON up_data.date_implemented = state_log.nonmfn_state_date
    AND state_log.nonmfn_state = 1
    AND up_data.nonmfn_affected IS NOT NULL
    AND up_data.linkage_id = state_log.linkage_id
    AND up_data.treatment_area = state_log.treatment_area
    SET up_data.nonmfn_status_id = state_log.status_id;
    
    UPDATE delta_temp_upload_data_",user.id," up_data
    JOIN delta_nonmfn_state_log state_log
    ON up_data.nonmfn_affected_end_date = state_log.nonmfn_state_date
    AND state_log.nonmfn_state = 0
    AND up_data.nonmfn_affected IS NOT NULL
    AND up_data.linkage_id = state_log.linkage_id
    AND up_data.treatment_area = state_log.treatment_area
    SET up_data.mfn_status_id = state_log.status_id;
    
    DROP TABLE IF EXISTS delta_temp_records_",user.id,";
    
    /* ADD NEW RECORD IDS */
    CREATE TABLE delta_temp_records_",user.id," AS 
    SELECT * FROM 
    (SELECT DISTINCT up_data.affected_flow_id, up_data.eligible_firms_id, up_data.implementation_level_id, up_data.intervention_type_id, up_data.input_id,
    (CASE WHEN up_data.nonmfn_affected IS NULL THEN 1 ELSE 0 END) AS is_mfn, up_data.date_announced, up_data.implementing_jurisdiction_id, up_data.source_id, up_data.framework_id,
    DATE(NOW()) as date_created
    FROM delta_temp_upload_data_",user.id," up_data) new_rec
    WHERE NOT EXISTS
    (SELECT 1 FROM delta_record_log rec_log, delta_record_implementer rec_impl, delta_record_source rec_src, delta_record_framework rec_frame
    WHERE new_rec.source_id = rec_src.source_id AND rec_impl.implementing_jurisdiction_id = new_rec.implementing_jurisdiction_id
    AND rec_frame.framework_id <=> new_rec.framework_id 
    AND rec_log.affected_flow_id = new_rec.affected_flow_id
    AND rec_log.eligible_firms_id = new_rec.eligible_firms_id
    AND rec_log.implementation_level_id = new_rec.implementation_level_id
    AND rec_log.intervention_type_id = new_rec.intervention_type_id
    AND rec_log.is_mfn = new_rec.is_mfn
    AND rec_log.record_date_announced = new_rec.date_announced
    );
    
    
    ALTER TABLE delta_temp_records_",user.id," ADD COLUMN record_id INT NOT NULL AUTO_INCREMENT KEY;
    
    UPDATE delta_temp_records_",user.id,"
    SET record_id=record_id+(SELECT AUTO_INCREMENT FROM information_schema.tables WHERE table_name='delta_record_log') - 1;
    
    /* ADD NEW RECORDS */
    INSERT INTO delta_record_log (intervention_type_id, affected_flow_id, implementation_level_id, eligible_firms_id, is_mfn, source_id, record_date_announced, record_date_created) 
    SELECT intervention_type_id, affected_flow_id, implementation_level_id, eligible_firms_id,
    is_mfn, source_id, date_announced record_date_created, date_created AS record_date_created
    FROM delta_temp_records_",user.id,";
    
    /* ADD RECORD IMPLEMENTER */
    INSERT INTO delta_record_implementer (record_id, implementing_jurisdiction_id)
    SELECT DISTINCT record_id, implementing_jurisdiction_id
    FROM delta_temp_records_",user.id,";
    
    /* ADD RECORD SOURCE */
    INSERT INTO delta_record_source (record_id, source_id)
    SELECT DISTINCT record_id, source_id
    FROM delta_temp_records_",user.id,";
    
    /* ADD RECORD FRAMEWORK */
    INSERT INTO delta_record_framework (record_id, framework_id)
    SELECT DISTINCT record_id, framework_id
    FROM delta_temp_records_",user.id,";
    
    DROP TABLE IF EXISTS delta_temp_records_",user.id,";
    
    /* ATTRIBUTE RECORD ID */
    UPDATE delta_temp_upload_data_",user.id," up_data
    JOIN delta_record_source rec_src ON up_data.source_id = rec_src.source_id
    JOIN delta_record_implementer rec_impl ON rec_src.record_id = rec_impl.record_id AND rec_impl.implementing_jurisdiction_id = up_data.implementing_jurisdiction_id
    JOIN delta_record_framework rec_frame ON rec_frame.record_id = rec_src.record_id AND rec_frame.framework_id <=> up_data.framework_id
    JOIN delta_record_log rec_log ON rec_log.record_id = rec_src.record_id
    AND up_data.affected_flow_id = rec_log.affected_flow_id
    AND up_data.eligible_firms_id = rec_log.eligible_firms_id
    AND up_data.implementation_level_id = rec_log.implementation_level_id
    AND up_data.intervention_type_id = rec_log.intervention_type_id
    AND up_data.date_announced = rec_log.record_date_announced
    AND (CASE WHEN up_data.nonmfn_affected IS NULL THEN 1 ELSE 0 END) = rec_log.is_mfn
    SET up_data.record_id = rec_log.record_id;
    
    INSERT INTO delta_nonmfn_state_records(status_id, record_id)
    SELECT DISTINCT * FROM 
    (SELECT nonmfn_status_id AS status_id, record_id 
    FROM delta_temp_upload_data_",user.id,"
    WHERE nonmfn_status_id IS NOT NULL
    UNION
    SELECT mfn_status_id  AS status_id, record_id
    FROM delta_temp_upload_data_",user.id,"
    WHERE mfn_status_id IS NOT NULL) states;
    
    UPDATE delta_temp_upload_data_",user.id,"
    SET delta_temp_upload_data_",user.id,".announced_as_temporary = (CASE WHEN delta_temp_upload_data_",user.id,".nonmfn_affected_end_date IS NULL THEN 0 ELSE 1 END);
    
    /* FIND WHETHER THERE EXISTS A DISCREPANCY OR EXISTING ID */
    UPDATE delta_temp_upload_data_",user.id," up_data
    INNER JOIN delta_treatment_linkage ON up_data.linkage_id = delta_treatment_linkage.linkage_id
    INNER JOIN delta_treatment_record ON delta_treatment_record.treat_id = delta_treatment_linkage.treat_id
    INNER JOIN delta_record_implementer ON delta_record_implementer.record_id = delta_treatment_record.record_id AND delta_record_implementer.implementing_jurisdiction_id = up_data.implementing_jurisdiction_id
    INNER JOIN delta_record_log rec_log ON delta_record_implementer.record_id = rec_log.record_id
    AND up_data.affected_flow_id = rec_log.affected_flow_id 
    AND up_data.eligible_firms_id = rec_log.eligible_firms_id
    AND up_data.implementation_level_id = rec_log.implementation_level_id
    AND up_data.intervention_type_id = rec_log.intervention_type_id
    AND up_data.date_announced = rec_log.record_date_announced
    AND (CASE WHEN up_data.nonmfn_affected IS NULL THEN 1 ELSE 0 END) = rec_log.is_mfn
    LEFT JOIN delta_coarse_code_log ON delta_coarse_code_log.treat_id = delta_treatment_linkage.treat_id AND delta_coarse_code_log.coarse_code <=> up_data.coarse_code AND delta_coarse_code_log.coarse_code_type <=> up_data.coarse_code_type
    INNER JOIN delta_tariff_log ON delta_tariff_log.treat_id = delta_treatment_linkage.treat_id
    AND up_data.date_implemented = delta_tariff_log.date_implemented
    AND up_data.announced_as_temporary = delta_tariff_log.announced_as_temporary
    AND up_data.treatment_unit_id = delta_tariff_log.treatment_unit_id
    SET up_data.discrepancy_treat_id = (CASE WHEN up_data.treatment_value <=> delta_tariff_log.treatment_value THEN NULL ELSE delta_tariff_log.treat_id END),
    	up_data.existing_treat_id = (CASE WHEN up_data.treatment_value = delta_tariff_log.treatment_value THEN delta_tariff_log.treat_id ELSE NULL END);
    
    /* INSERT DISCREPANCIES */
    INSERT INTO delta_input_discrepancy_log(input_id, record_id, treat_id, linkage_id, discrepancy_value, discrepancy_description)
    SELECT up_data.input_id, up_data.record_id, up_data.discrepancy_treat_id AS treat_id, up_data.linkage_id, up_data.treatment_value, 'value conflict' AS discrepancy_description
    FROM delta_temp_upload_data_",user.id," up_data
    WHERE up_data.discrepancy_treat_id IS NOT NULL; 
    
    UPDATE delta_temp_upload_data_",user.id," up_data
    INNER JOIN (
        SELECT processing_id, (DENSE_RANK() OVER(ORDER BY date_implemented, treatment_code, treatment_code_type, coarse_code, coarse_code_type, treatment_value, treatment_unit_id, 
    	treatment_code_official, treatment_area_id, announced_as_temporary) + (SELECT AUTO_INCREMENT FROM information_schema.tables WHERE table_name='delta_treatment_area') - 1) new_treat_id
    	FROM delta_temp_upload_data_",user.id,"
        WHERE delta_temp_upload_data_",user.id,".discrepancy_treat_id IS NULL AND delta_temp_upload_data_",user.id,".existing_treat_id IS NULL 
    ) r on r.processing_id = up_data.processing_id
    SET up_data.new_treat_id = r.new_treat_id;
    
    /* ADD NEW TREATMENT IDS */ 
    INSERT INTO delta_treatment_area (treat_id, treatment_area_id)
    SELECT DISTINCT new_treat_id AS treat_id, treatment_area_id  
    FROM delta_temp_upload_data_",user.id,"
    WHERE delta_temp_upload_data_",user.id,".new_treat_id IS NOT NULL;
    
    /* ONLY NEW TREATMENTS NEED TO BE ADDED TO THE TARIFF LOG */
    INSERT INTO delta_tariff_log (treat_id, date_implemented, treatment_code, treatment_code_type, treatment_value, treatment_unit_id, treatment_code_official, announced_as_temporary)
    SELECT DISTINCT new_treat_id AS treat_id, date_implemented, treatment_code, treatment_code_type, treatment_value, treatment_unit_id, treatment_code_official, announced_as_temporary
    FROM delta_temp_upload_data_",user.id,"
    WHERE delta_temp_upload_data_",user.id,".new_treat_id IS NOT NULL;
    
    /* INSERT NEW TREAT LINKAGES */
    INSERT INTO delta_treatment_linkage (treat_id, linkage_id, input_id)
    SELECT DISTINCT treat_id, linkage_id, input_id FROM
    (SELECT new_treat_id AS treat_id, linkage_id, input_id FROM delta_temp_upload_data_",user.id," WHERE new_treat_id IS NOT NULL 
    UNION 
    SELECT existing_treat_id AS treat_id, linkage_id, input_id FROM delta_temp_upload_data_",user.id," WHERE existing_treat_id IS NOT NULL) new_link_treats
    WHERE NOT EXISTS
    (SELECT 1
    FROM delta_treatment_linkage treat_link
    WHERE treat_link.treat_id = new_link_treats.treat_id
    AND treat_link.linkage_id = new_link_treats.linkage_id
    );
    
    /* NEW TREATMENT COARSE CODES */ 
    INSERT INTO delta_coarse_code_log (treat_id, coarse_code, coarse_code_type)
    SELECT DISTINCT new_treat_id AS treat_id, coarse_code, coarse_code_type
    FROM delta_temp_upload_data_",user.id,"
    WHERE delta_temp_upload_data_",user.id,".new_treat_id IS NOT NULL AND delta_temp_upload_data_",user.id,".coarse_code IS NOT NULL AND delta_temp_upload_data_",user.id,".coarse_code_type IS NOT NULL;
    
    /* INSERT NEW AND EXISTING TREATMENTS TO TREATMENT RECORDS */ 
    /* SUFFICIENT LINKS ARE PROVIDED TO LATER ADD RESOLVED DISCREPANCIES */
    INSERT INTO delta_treatment_record(treat_id, record_id, input_id)
    SELECT DISTINCT treat_id, record_id, input_id FROM
    (SELECT new_treat_id AS treat_id, record_id, input_id FROM delta_temp_upload_data_",user.id," WHERE new_treat_id IS NOT NULL 
    UNION 
    SELECT existing_treat_id AS treat_id, record_id, input_id FROM delta_temp_upload_data_",user.id," WHERE existing_treat_id IS NOT NULL) new_rec_treats
    WHERE NOT EXISTS
    (SELECT 1
    FROM delta_treatment_record treat_rec
    WHERE treat_rec.treat_id = new_rec_treats.treat_id
    AND treat_rec.record_id = new_rec_treats.record_id
    );
    
    UPDATE delta_input_log
    SET input_state = 'complete'
    WHERE input_id = ",this.input.id,";
    
    DROP TABLE delta_temp_upload_data_",user.id,";
    
    "
  )

  #return(cat(sql.statement))
  gta_sql_multiple_queries(sql.statement,output.queries = 1, show.time = T, db.connection = db.connection)
  
  
}