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
                       contains.data = T,
                       # create.foreign.key = c('implementing.jurisdiction.id','affected.flow.id','nonmfn.affected.id'),
                       # foreign.key.parent.table = c('jurisdiction.list','affected.flow.list','jurisdiction.list'),
                       # foreign.key.parent.name = c('jurisdiction.id','affected.flow.id','jurisdiction.id'),
                       # foreign.key.parent.table.prefix = c('gta_','gta_','gta_'),
                       append.existing = F)
  
  
  sql.statement=paste0(
    "
    DROP TABLE IF EXISTS check_linkage_",user.id,"; 

    /* IDENTIFY WHICH LINKAGES ARE ALREADY IN DATABASE */ 
    CREATE TABLE check_linkage_",user.id," AS
    SELECT newlink.*, linklog.linkage_id
    FROM (SELECT DISTINCT implementing_jurisdiction_id, affected_flow_id, treatment_code, treatment_code_type, nonmfn_affected_id
    FROM delta_temp_upload_data_",user.id,") AS newlink
    LEFT JOIN delta_linkage_log linklog
    ON newlink.implementing_jurisdiction_id = linklog.linkage_implementer_id
    AND newlink.affected_flow_id = linklog.linkage_affected_flow_id
    AND newlink.treatment_code=linklog.linkage_code
    AND newlink.treatment_code_type=linklog.linkage_code_type
    AND newlink.nonmfn_affected_id=linklog.linkage_affected_country_id;
    
    /* ADD MISSING LINKAGES TO DATABASE */ 
    INSERT INTO delta_linkage_log (linkage_implementer_id, linkage_affected_flow_id, linkage_code, linkage_code_type, linkage_affected_country_id)
    SELECT DISTINCT implementing_jurisdiction_id linkage_implementer_id, affected_flow_id linkage_affected_flow_id, treatment_code linkage_code, treatment_code_type linkage_code_type, nonmfn_affected_id linkage_affected_country_id
    FROM check_linkage_",user.id," 
    WHERE linkage_id IS NULL;

    DROP TABLE IF EXISTS check_linkage_",user.id,"; 

    /* ADD NEW SOURCES */ 
    INSERT INTO delta_source_log (state_act_source, is_source_official) 
    SELECT DISTINCT state_act_source, is_source_official
    FROM delta_temp_upload_data_",user.id," 
    WHERE state_act_source NOT IN (SELECT DISTINCT state_act_source FROM delta_source_log);
    
    /* DROP AND RE-JOIN WITH NEWLY ADDED LINK IDS */
    DROP TABLE IF EXISTS added_links_",user.id,"; 

    CREATE TABLE added_links_",user.id," AS 
    SELECT a.*, c.source_id, b.linkage_id live_link 
    FROM delta_temp_upload_data_",user.id," a
    LEFT JOIN delta_source_log c
    ON a.state_act_source=c.state_act_source
    LEFT JOIN delta_linkage_log b
    ON a.implementing_jurisdiction_id=b.linkage_implementer_id
    AND a.affected_flow_id=b.linkage_affected_flow_id
    AND a.treatment_code=b.linkage_code
    AND a.treatment_code_type=b.linkage_code_type
    AND (a.nonmfn_affected_id=b.linkage_affected_country_id OR (a.nonmfn_affected_id IS NULL AND b.linkage_affected_country_id IS NULL));
    
    /* MERGE TREATMENT VALUES WITH SAME DATE/CODE/LINK AND DROP THOSE WITH IDENTICAL INFORMATION AS IN DATABASE */
    DROP TABLE delta_temp_upload_data_",user.id,";
    CREATE TABLE delta_temp_upload_data_",user.id," AS
    SELECT a.*, d.treatment_value live_treatment_value, d.treatment_unit_id live_treatment_unit_id 
    FROM added_links_",user.id," a
    LEFT JOIN (SELECT b.linkage_id ,c.* 
    FROM added_links_",user.id," a, delta_record_linkage b, delta_",treatment.area,"_log c
    WHERE a.live_link = b.linkage_id
    AND b.record_id = c.record_id
    AND a.date_implemented = c.date_implemented
    AND a.treatment_code = c.treatment_code
    AND a.treatment_code_type = c.treatment_code_type
    
    ) d
    ON a.live_link = d.linkage_id
    AND a.date_implemented = d.date_implemented
    AND a.treatment_code = d.treatment_code
    AND a.treatment_code_type = d.treatment_code_type;       
    
    DROP TABLE IF EXISTS added_links_",user.id,";
    
    DELETE FROM delta_temp_upload_data_",user.id,"
    WHERE treatment_value = live_treatment_value
    AND treatment_unit_id = live_treatment_unit_id;
    
    DROP TABLE IF EXISTS delta_temp_records_",user.id,";
    
    /* ADD NEW RECORD IDS */
    /* BE CAREFUL WITH imp_jur which has eu-28.. do we want 1 record per country in the eu-28 or just one record for eu-28?  */
    /* CURRENTLY THIS IS DONE as one record for EACH COUNTRY IN eu-28 (no choice due to how we have not set up jurisdiction ids for country-groups) */
    /* IF WANTED, CHANGE THIS IN THE DENSE_RANK (and the group by in the insert into delta_record_log) WHERE NEW RECORD IDS ARE ATTRIBUTED! */
    /* I ALSO ADDED UNIQUE IMPLEMENTATION LEVEL PER RECORD, WHICH IS NOT THE CASE IN THE GTA_DELTA_INPUT_UPLOAD FUNCTION IS THIS CORRECT? */
    
    /* ADD NEW RECORDS AND ORDER BY RECORD_ID TO ENSURE LINK IS CORRECTLY MADE */ 
    /* FIRST CHECK WHETHER RECORD ALREADY EXISTS */
    CREATE TABLE delta_temp_records_",user.id," AS 
    SELECT a.source_id, a.treatment_area, a.affected_flow_id, a.intervention_type_id, a.implementing_jurisdiction, a.implementing_jurisdiction_id, a.nonmfn_affected,
    (CASE WHEN a.nonmfn_affected IS NULL THEN 1 ELSE 0 END) AS is_mfn, a.date_announced, a.treatment_code, a.treatment_code_type, a.coarse_code, a.coarse_code_type, a.date_implemented, a.treatment_value, a.treatment_unit_id, a.treatment_code_official, 
    a.live_treatment_value, a.live_treatment_unit_id, a.nonmfn_affected_end_date, a.eligible_firms_id, a.implementation_level_id, DATE(NOW()) as date_created, a.live_link, b.record_id
    FROM delta_temp_upload_data_",user.id," a
    LEFT JOIN delta_record_log b
    ON a.intervention_type_id=b.intervention_type_id
    AND a.affected_flow_id = b.affected_flow_id
    AND a.implementation_level_id=b.implementation_level_id
    AND a.eligible_firms_id=b.eligible_firms_id
    AND (CASE WHEN a.nonmfn_affected IS NULL THEN 1 ELSE 0 END)=b.is_mfn
    AND a.source_id = b.source_id
    AND a.date_announced=b.record_date_announced;

    /* UPLOAD NEW RECORDS */
    INSERT INTO delta_record_log (intervention_type_id, affected_flow_id, implementation_level_id, eligible_firms_id, is_mfn, source_id, record_date_announced, record_date_created)
    SELECT DISTINCT intervention_type_id, affected_flow_id, implementation_level_id, eligible_firms_id, 
    is_mfn, source_id, date_announced record_date_created, DATE(NOW()) as record_date_created
    FROM delta_temp_records_",user.id,"
    WHERE record_id IS NULL
    GROUP BY source_id, treatment_area, affected_flow_id, intervention_type_id, implementing_jurisdiction_id, is_mfn, date_announced, eligible_firms_id
    ORDER BY date_announced ASC;

    /* RECREATE TEMP */
    DROP TABLE IF EXISTS delta_temp_records_",user.id,";
    CREATE TABLE delta_temp_records_",user.id," AS 
    SELECT a.source_id, a.treatment_area, a.affected_flow_id, a.intervention_type_id, a.implementing_jurisdiction, a.implementing_jurisdiction_id, a.nonmfn_affected,
    (CASE WHEN a.nonmfn_affected IS NULL THEN 1 ELSE 0 END) AS is_mfn, a.date_announced, a.treatment_code, a.treatment_code_type, a.coarse_code, a.coarse_code_type, a.date_implemented, a.treatment_value, a.treatment_unit_id, a.treatment_code_official, 
    a.live_treatment_value, a.live_treatment_unit_id, a.nonmfn_affected_end_date, a.eligible_firms_id, a.implementation_level_id, DATE(NOW()) as date_created, a.live_link, b.record_id
    FROM delta_temp_upload_data_",user.id," a
    LEFT JOIN delta_record_log b
    ON a.intervention_type_id=b.intervention_type_id
    AND a.affected_flow_id = b.affected_flow_id
    AND a.implementation_level_id=b.implementation_level_id
    AND a.eligible_firms_id=b.eligible_firms_id
    AND (CASE WHEN a.nonmfn_affected IS NULL THEN 1 ELSE 0 END)=b.is_mfn
    AND a.source_id = b.source_id
    AND a.date_announced=b.record_date_announced;
    
    /* ADD RECORD LINKAGES */ 
    INSERT INTO delta_record_linkage (record_id, linkage_id)
    SELECT DISTINCT record_id, live_link linkage_id
    FROM delta_temp_records_",user.id,";
    
    /* ADD RECORD IMPLEMENTER */ 
    INSERT INTO delta_record_implementer (record_id, implementing_jurisdiction_id)
    SELECT DISTINCT record_id, implementing_jurisdiction_id 
    FROM delta_temp_records_",user.id,";
    
    /* ADD RECORD TREATMENT AREA */ 
    INSERT INTO delta_record_area (record_id, treatment_area_id)
    SELECT DISTINCT record_id, b.treatment_area_id 
    FROM delta_temp_records_",user.id," a, delta_treatment_area_list b
    WHERE a.treatment_area = b.treatment_area_name;
    
    /* ADD COARSE RECORDS */ 
    INSERT INTO delta_coarse_code_log (record_id, coarse_code, coarse_code_type) 
    SELECT DISTINCT record_id, coarse_code, coarse_code_type
    FROM delta_temp_records_",user.id,"
    WHERE coarse_code IS NOT NULL;
    
    /* ADD INTO INPUT DISCREPANCY LOG */ 
    INSERT INTO delta_input_discrepancy_log (input_id, record_id, discrepancy_date, discrepancy_value, discrepancy_value_unit_id, discrepancy_code_official, discrepancy_source_id, discrepancy_description) 
    SELECT DISTINCT (SELECT AUTO_INCREMENT FROM information_schema.tables WHERE table_name='delta_input_log')-1 input_id, record_id, date_implemented discrepancy_date, 
    treatment_value discrepancy_value, treatment_unit_id discrepancy_value_unit_id, treatment_code_official, b.source_id discrepancy_source_id, 
    'Not prior value for temporary record' AS discrepancy_description
    FROM delta_temp_records_",user.id," a, delta_source_log b
    WHERE treatment_value != live_treatment_value
    AND treatment_unit_id = live_treatment_unit_id
    AND a.source_id = b.source_id;
    
    /* ADD INTO NONMFN STATE LOG */
    /* FIND OUT HOW TO GET STATE REDUNDANT, FOR TIME BEING JUST ASSUMED TO BE 0 */
    INSERT INTO delta_nonmfn_state_log (linkage_id, treatment_area, nonmfn_state_date, nonmfn_state, source_id, state_redundant) 
    SELECT live_link linkage_id, treatment_area, date_implemented nonmfn_state_date, 1 AS nonmfn_state, b.source_id, 0 AS state_redundant  
    FROM delta_temp_records_",user.id," a, delta_source_log b
    WHERE nonmfn_affected IS NOT NULL
    AND nonmfn_affected_end_date IS NULL 
    AND a.source_id = b.source_id
    UNION 
    SELECT live_link linkage_id, treatment_area, nonmfn_affected_end_date nonmfn_state_date, 0 AS nonmfn_state, b.source_id, 0 AS state_redundant  
    FROM delta_temp_records_",user.id," a, delta_source_log b
    WHERE nonmfn_affected IS NOT NULL
    AND nonmfn_affected_end_date IS NOT NULL 
    AND a.source_id = b.source_id;
    
    /* ADD INTO LOG OF APPROPRIATE AREA*/ 
    INSERT INTO delta_",treatment.area,"_log (record_id, date_implemented, treatment_code, treatment_code_type, treatment_value, treatment_unit_id, treatment_code_official, announced_as_temporary) 
    SELECT DISTINCT record_id, date_implemented, treatment_code, treatment_code_type, treatment_value, treatment_unit_id, treatment_code_official,
    (CASE WHEN nonmfn_affected_end_date IS NOT NULL THEN 1 ELSE 0 END) AS announced_as_temporary
    FROM delta_temp_records_",user.id,"
    WHERE live_treatment_value IS NULL;
    
    
    DROP TABLE delta_temp_records_",user.id,";
    DROP TABLE delta_temp_upload_data_",user.id,";


        
          
          
  "
  )
  
  # return(cat(sql.statement))
  gta_sql_multiple_queries(sql.statement,output.queries = 1)
  
  
}
  