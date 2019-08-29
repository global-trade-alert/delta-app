gta_delta_get_closest_date=function(benchmark.date=NULL,
                                    candidate.dates=NULL,
                                    get.closest.before=T,
                                    get.closest.after=T){
  

  find.date=as.Date(as.numeric(benchmark.date), origin="1970-01-01")
  if(is.na(find.date)|find.date<"2000-01-01"|find.date>"2030-01-01"){
    stop("Please specify the benchmark date in R format (YYYY-MM-DD).")
  }
  
  date.pool=as.Date(as.numeric(candidate.dates), origin="1970-01-01")
  
  if(any(is.na(date.pool)|date.pool<"2000-01-01"|date.pool>"2030-01-01")){
    stop("Please specify the candidate dates in R format (YYYY-MM-DD).")
  }
  
  closest.dates=c()
  date.pool.before=date.pool[date.pool<=find.date]
  date.pool.after=date.pool[date.pool>find.date]
  
  if(get.closest.before){
    
    if(length(date.pool.before)>0){
      closest.dates=c(closest.dates,
                      date.pool.before[which(date.pool.before-find.date==max(date.pool.before-find.date))])
    } else {
      closest.dates=c(closest.dates,NA)
    }
    
  }
  
  if(get.closest.after){
    
    if(length(date.pool.after)>0){
      closest.dates=c(closest.dates,
                      date.pool.after[which(date.pool.after-find.date==min(date.pool.after-find.date))])
    } else {
      closest.dates=c(closest.dates,NA)
      }

  }
  
  
  return(as.Date(as.numeric(closest.dates),origin="1970-01-01"))
  
}
