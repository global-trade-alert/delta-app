gta_delta_get_jurisdiction_id=function(jurisdiction.name=NULL,
                                 db.connection="pool"){
  
  
  
  
  ij.id.query  <- paste("SELECT jurisdiction_id, jurisdiction_name 
                        FROM gta_jurisdiction_list
                        WHERE jurisdiction_name IN (",paste(paste('"',jurisdiction.name,'"', sep=""),collapse=","),
                        ");", sep="")
  
  ij.ids=gta_sql_get_value(query=ij.id.query,
                           db.connection=db.connection)
  
  rm(ij.id.query)
  
  if(length(setdiff(jurisdiction.name,ij.ids$jurisdiction.name))>0){
    
    ij.groups=setdiff(jurisdiction.name,ij.ids$jurisdiction.name)
    
    ij.group.id.query  <- paste("SELECT jgm.jurisdiction_id, jurisdiction_name,jurgroup.jurisdiction_group_name
                         FROM gta_jurisdiction_list jurlist
                         JOIN gta_jurisdiction_group_member_list jgm
                         ON jurlist.jurisdiction_id = jgm.jurisdiction_id
                         JOIN gta_jurisdiction_group_list jurgroup
                         ON jgm.jurisdiction_group_id = jurgroup.jurisdiction_group_id 
                         WHERE jurgroup.jurisdiction_group_name IN (",paste(paste('"',ij.groups,'"', sep=""),collapse=","),
                                ");", sep="")
    
    
    
    ij.group.ids=gta_sql_get_value(query=ij.group.id.query,
                                   db.connection=db.connection)
    
    if(any(is.na(ij.group.ids)==F)){
      
      ij.group.ids$jurisdiction.name=ij.group.ids$jurisdiction.group.name
      ij.group.ids$jurisdiction.group.name=NULL
      ij.ids= rbind(ij.ids,ij.group.ids)
      
      jurisdiction.name=jurisdiction.name[! jurisdiction.name %in% unique(ij.group.ids$jurisdiction.group.name)]
      if(length(setdiff(jurisdiction.name,ij.ids$jurisdiction.name))>0){
        stop(paste("Cannot convert the jurisdictions: ", paste(setdiff(jurisdiction.name,ij.ids$jurisdiction.name), collapse=";"), sep=""))
      }
    }
    
  }
  return(ij.ids) 
}