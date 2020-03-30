gta_delta_delete_input=function(
  delete.input.ids = c(),
  treatment.area='tariff',
  db.connection='pool'
){
  
  if(!is.null(delete.input.ids)) 
  
  del.inputs.query = paste0("DELETE FROM delta_",treatment.area,"_log")
  # If provided vector of inputs to pull records for is not empty then pull record ids for specific input_ids 
  if(!is.null(pull.input.id.records)){
    pull.input.records.query = paste0(pull.input.records.query, " WHERE input_id IN (%s);")
    pull.input.records.query = sprintf(pull.input.records.query, toString(sprintf("'%s'",unique(pull.input.id.records))))
  }
  
  if(pull.inputs) query = pull.inputs.query else query = pull.input.records.query
  
  remote = gta_sql_get_value(query = query,db.connection=db.connection)
  
  return(remote)
  
}