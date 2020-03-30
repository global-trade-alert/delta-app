gta_delta_input_query=function(
  pull.inputs=F,
  pull.input.records=F,
  pull.input.id.records = c(),
  regex.field=NULL,
  treatment.area='tariff',
  db.connection='pool'
){
  
  if(is.null(regex.field)) regex.field = '.'
  if(!xor(pull.inputs, pull.input.records)) stop('A unique pull parameter must be TRUE!')
 
  pull.inputs.query = paste0("SELECT * FROM delta_input_log WHERE delta_input_log.input_name REGEXP '",regex.field,"';")
  
  # Can elaborate the record query, not sure what information would be pulled
  pull.input.records.query = paste0("SELECT * FROM delta_",treatment.area,"_log")
  # If provided vector of inputs to pull records for is not empty then pull record ids for specific input_ids 
  if(!is.null(pull.input.id.records)){
    pull.input.records.query = paste0(pull.input.records.query, " WHERE input_id IN (%s);")
    pull.input.records.query = sprintf(pull.input.records.query, toString(sprintf("'%s'",unique(pull.input.id.records))))
  }

  if(pull.inputs) query = pull.inputs.query else query = pull.input.records.query
  
  remote = gta_sql_get_value(query = query,db.connection=db.connection)
  
  return(remote)
  
}




# library(gtalibrary)
# library(gtasql)
# library(RMariaDB)
# library(DBI)
# library(RMySQL)
# library(pool)
# library(stringr)
# 
# gta_setwd()
# 
# gta_sql_pool_open(table.prefix = 'delta_',
#                   db.title="ricardodev",
#                   db.host = gta_pwd("ricardodev")$host,
#                   db.name = gta_pwd("ricardodev")$name,
#                   db.user = gta_pwd("ricardodev")$user,
#                   db.password = gta_pwd("ricardodev")$password)
# 
# db.connection = 'pool'
# pull.inputs=T
# pull.input.records=F
# regex.field=NULL
# pull.input.id.records=c()
# result = gta_input_query(pull.inputs=pull.inputs,
#                           pull.input.records=pull.input.records,
#                           pull.input.id.records = pull.input.id.records,
#                           regex.field=regex.field,
#                           db.connection=db.connection)$input.id
# 
# gta_input_query(pull.inputs=F,
#                 pull.input.records=T,
#                 pull.input.id.records = result,
#                 regex.field=regex.field,
#                 db.connection=db.connection)
