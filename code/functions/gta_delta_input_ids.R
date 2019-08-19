gta_delta_input_ids=function(
  treatment.area=NULL,
  date.announced=NULL,
  date.implemented=NULL,
  source=NULL,
  source.official=NULL,
  author=NULL,
  intervention.type=NULL,
  affected.flow=NULL,
  implementing.jurisdiction=NULL,
  implementer.end.date=NA,
  implementation.level=NULL,
  eligible.firms=NULL,
  affected.code=NULL,
  affected.code.type=NULL,
  affected.code.end.date=NA,
  treatment.value=NULL, 
  treatment.unit=NULL, 
  treatment.code.official=NULL,
  affected.country=NULL,
  affected.country.end.date=NA,
  framework.name=NULL,
  framework.new.to.db=NULL
){
  
  #create ids for following if given as character
  
  a=c("implementing.jurisdiction.id", "treatment.value", "treatment.code",
     "date.announced","date.implemented", "treatment.unit.id",
     "treatment.code.official", "treatment.area", "treatment.code.type.id",
     "intervention.type.id", "state.act.source", "is.source.official",
     "author.id", "affected.flow.id", "implementation.level.id",
     "eligible.firms.id","implementer.end.date","treatment.code.end.date",
     "nonmfn.affected.id","nonmfn.affected.end.date", "framework.id",
     "framework.new.to.db")
  
  
  
   
  
}  