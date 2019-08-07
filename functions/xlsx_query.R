xlsx_query=function(input.query,sort=T){
  
  base=list()
  for (i in 1:nrow(input.query)){
  
  if(any(str_detect(tolower(names(input.query)),'hs'))){
    col=names(input.query)[str_detect(tolower(names(input.query)),'hs')]
    if(!is.na(input.query[[col]][i])){
      hs.codes=input.query[[col]][i]}
  }else{hs.codes=''}
  
  if(any(str_detect(tolower(names(input.query)),'cpc'))){
    col=names(input.query)[str_detect(tolower(names(input.query)),'cpc')]
    if(!is.na(input.query[[col]][i])){
      cpc.codes=input.query[[col]][i]}
  }else{cpc.codes=''}
    
  if(any(str_detect(tolower(names(input.query)),'implement'))){
    col=names(input.query)[str_detect(tolower(names(input.query)),'implement')]
    if(!is.na(input.query[[col]][i])){
    implementing.jurisdiction=input.query[[col]][i]}
    }else{implementing.jurisdiction='Any'}
  
  if(any(str_detect(tolower(names(input.query)),'affect'))){
    col=names(input.query)[str_detect(tolower(names(input.query)),'affect')]
    if(!is.na(input.query[[col]][i])){
    affected.jurisdiction=input.query[[col]][i]}
    }else{affected.jurisdiction='Any'}
  
  if(any(str_detect(tolower(names(input.query)),'start'))){
    col=names(input.query)[str_detect(tolower(names(input.query)),'start')]
    if(!is.na(input.query[[col]][i])){
    start.date=input.query[[col]][i]}
    }else{start.date=as.Date('1950-01-01')}
  
  if(any(str_detect(tolower(names(input.query)),'end'))){
    col=names(input.query)[str_detect(tolower(names(input.query)),'end')]
    if(!is.na(input.query[[col]][i])){
      end.date=input.query[[col]][i]}
    }else{end.date=Sys.Date()}
  
  if(any(str_detect(tolower(names(input.query)),'type'))){
    col=names(input.query)[str_detect(tolower(names(input.query)),'type')]
    if(!is.na(input.query[[col]][i])){
    regime.type=input.query[[col]][i]}
    }else{regime.type='Any'}
  
  if(any(str_detect(tolower(names(input.query)),'name'))){
    col=names(input.query)[str_detect(tolower(names(input.query)),'name')]
    if(!is.na(input.query[[col]][i])){
    regime.name=input.query[[col]][i]}
    }else{regime.name='Any'}
  
  if(any(str_detect(tolower(names(input.query)),'unit'))){
    col=names(input.query)[str_detect(tolower(names(input.query)),'unit')]
    if(!is.na(input.query[[col]][i])){
      tariff.unit=input.query[[col]][i]}
  }else{tariff.unit='Any'}
    
  if(any(str_detect(tolower(names(input.query)),'previous'))){
    col=names(input.query)[str_detect(tolower(names(input.query)),'previous')]
    if(!is.na(input.query[[col]][i])){
      show.previous=input.query[[col]][i]}
  }else{show.previous=T}  
    
  base[[i]]=table_filter(hs.codes,
                    cpc.codes,
                    implementing.jurisdiction,
                    affected.jurisdiction,
                    start.date,
                    end.date,
                    regime.type,
                    regime.name,
                    tariff.unit,
                    show.previous,
                    sort)  
    
    
  }  
  return(base)
}


