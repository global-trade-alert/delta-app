gta_delta_confirm_xlsx=function(xlsx.input){
  
  library(stringr)
  
  input=xlsx.input
  clean.string=function(string){
    return(gsub("x","", tolower(str_replace_all(string, "[^[a-zA-Z]]", ""))))
  }
  names(input)=clean.string(names(input))
  
  req.cols=c('treatment.area', 'source','source.official', 'author','intervention.type', 'affected.flow', 'implementation.level', 'eligible.firms',
             'implementing.jurisdiction', 'affected.code', 'affected.code.type', 'treatment.value','treatment.unit', 'treatment.code.official', 'date.implemented')
  opt.cols=c('date.announced','implementer.end.date','affected.code.end.date','affected.country.end.date','affected.country','framework.name')
  one.value.permitted=c('treatment.area','source','source.official','author','date.implemented','date.announced')
  
  #tracks whether upload was successful or not
  #successful fatal=NULL, successful but optional columns missing fatal=F, unsuccessful fatal=T
  fatal=NULL
  
  if(!all(clean.string(opt.cols) %in% names(input))){
    fatal=F
    opt.cols.error=paste0('However, the following optional columns are missing: ',
                          paste(opt.cols[!clean.string(opt.cols) %in% names(input)], collapse = ';'),
                          '. Please ensure this is intended before proceeding!',
                          '<br>')
  } else {opt.cols.error=NULL}
  
  if(!all(clean.string(req.cols) %in% names(input))){
    fatal=T
    req.cols.error=paste0('The following mandatory columns are missing: ',
                          paste(req.cols[!clean.string(req.cols) %in% names(input)], collapse = ';'),
                          '.<br>')
  } else {req.cols.error=NULL}
  
  ##check length is suitable and assemble input
  output=setNames(data.frame(matrix(ncol = length(req.cols)+length(opt.cols), nrow = nrow(input))), c(req.cols,opt.cols))
  for(col in c(req.cols,opt.cols)[clean.string(c(req.cols,opt.cols)) %in% names(input)]){
    output[[col]]=input[[clean.string(col)]]
  }
  
  
  for(col in one.value.permitted){
    if(length(output[[col]][!is.na(output[[col]])])==1){
      output[[col]]=rep(output[[col]][1],nrow(output))
    }
  }
  
  # notify user of incorrect length
  # incorrect length if missing elements in vector (unless part of one.value.permitted dictionary)
  incomplete.vec=sapply(output, function(x) sum(is.na(x)))
  incomplete.vec=incomplete.vec[incomplete.vec!=0]
  incomplete.vec=incomplete.vec[incomplete.vec==nrow(output) | !names(incomplete.vec) %in% opt.cols]
  if (length(incomplete.vec)>0){
    if (any(names(incomplete.vec) %in% req.cols)){
      fatal=T
      length.error=paste0('The following required columns are incomplete: ',
                          paste(names(incomplete.vec), collapse = '; '),
                          '. Only the following columns may be reported either as single values or as same-length vectors as other cell-entries: ',
                          paste(one.value.permitted, collapse = '; '))
    } else {
      fatal=F
      length.error=paste0('The following optional columns are empty: ',
                          paste(names(incomplete.vec), collapse = '; '),
                          '. Please ensure this is intended.')
    }
    
  } else {length.error=NULL}
  
  rm(incomplete.vec)
  
  
  if(is.null(fatal)|isFALSE(fatal)){
    error.msg=paste0('Successful Upload. <br>', opt.cols.error, length.error)
    error.msg=sub("^<br>", "", error.msg)
    error.msg=sub("<br>$", "", error.msg)
    output=list(fatal=fatal,error.msg=error.msg,output=output)
    
  } else {
    error.msg=paste0('Unsuccessful Upload. ',
                     req.cols.error,
                     opt.cols.error,
                     length.error)
    error.msg=sub("^<br>", "", error.msg)
    error.msg=sub("<br>$", "", error.msg)
    output=list(fatal=fatal,error.msg=error.msg,output=output)
    
  }
  
  return(output)
}


