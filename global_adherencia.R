mostrejar_id_farmacs<-function(set_seed=9){
  set.seed(set_seed)
  prescrits_id<-dt_prescrits %>% semi_join(dt_ids %>% sample_n(1),by="idp")
  facturats_id<-dt_facturats %>% semi_join(prescrits_id,by="idp")
  list(prescrits=prescrits_id,facturats=facturats_id)
}  

# Convertir facturats --> prescrits a 30 dies per envas 
formatar_facturats<-function(dt=dt_facturats) {
  dt<-dt %>%
    mutate(dat=lubridate::ymd(paste0(as.character(dat),"15")),dbaixa=dat+(30*env),
           dat=data.to.string(dat) %>% as.numeric(),
           dbaixa=data.to.string(dbaixa) %>% as.numeric())
  dt
}


formatar_prescrits<-function(dades=dt_prescrits) {
  agregar_solapaments_gaps(dt=dades,id="idp",datainici =
                             "dat",datafinal = "dbaixa",gap=0)
}


# Actualitza fitrxer de facturacions a factor de conversió
# Llanço fitxer de facturacions , amb envasos i datainici i retorno data fi segons nou FC 
formatar_factor_conversio<-function (dades=dt_facturats_formatat,dt_factor=conductor_facmacs) {
  dades %>% 
    dplyr::left_join(dt_factor, by="cod") %>% 
    mutate(env=env*as.numeric(factor_conversio_farmacs)) %>% 
    select(-factor_conversio_farmacs)%>% 
    mutate(dat=lubridate::ymd(dat),
           dbaixa=dat+(30*env),
           dat=data.to.string(dat) %>% as.numeric(),dbaixa=data.to.string(dbaixa) %>% as.numeric())
}


### Retorna un fitxer historic de farmacs truncant en finestra de dates. 
# Historic amb idp, dat, dbaixa + data_inferior + data_superior
# Retorna idp, i dates acttualitzades dins finestra dels limits temporals
truncament_farmacs<-function(dt_historic=mostra_facturats_post2,data_inferior="dtindex",data_superior="datafi_seguiment"){
  
  limit_inf<-rlang::sym(data_inferior)
  limit_sup<-rlang::sym(data_superior)
  
  # Si no son dates convertir a dates
  dt_historic %>% mutate(dat=lubridate::ymd(dat),dbaixa=lubridate::ymd(dbaixa),
                         !!limit_inf:=lubridate::ymd(!!limit_inf),!!limit_sup:=lubridate::ymd(!!limit_sup))
  
  dt_historic %>% group_by(idp) %>% 
    mutate (min=min(dat), max=max(dbaixa)) %>% ungroup() %>%  # Afagar data mínima i data maxima de cada idp 
    mutate (dat=(ifelse(min<!!limit_inf,!!limit_inf,dat))) %>%              # Si dataminima<dtindex dat= limi inferior   
    mutate (dbaixa=(ifelse(max>!!limit_sup,!!limit_sup,dbaixa))) %>%  # Si datamaxima>datafiOT databaixa = limitsuperior
    mutate (dat=data_convert_numeric(dat),dbaixa=data_convert_numeric(dbaixa)) %>% 
    select(-min,-max)
}




