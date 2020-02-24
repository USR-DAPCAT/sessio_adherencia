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

generar_mapa<-function(gap=1,set_seed=9,dadesprescripcio=dt_prescrits) {
  # gap<-0 # gap: gap=45
  # set_seed<-3  # set_seed=12
  
  # 0. Mostrejar id's
  set.seed(set_seed)
  
  prescrits_id<-dadesprescripcio %>% semi_join(dt_ids %>% sample_n(1),by="idp")
  facturats_id<-dt_facturats %>% semi_join(prescrits_id,by="idp")
  
  # 2.agregar solapaments tant de prescrits com facturats
  prescrits_id<-agregar_solapaments_gaps(dt=prescrits_id,id="idp",datainici =
                                           "dat",datafinal = "dbaixa",gap=gap)
  facturats_id<-facturats_id %>%
    mutate(dat=lubridate::ymd(paste0(as.character(dat),"15")),dbaixa=dat+(30*env)) %>%
    agregar_solapaments_gaps(id="idp",datainici = "dat",datafinal = "dbaixa",gap=gap)
  # 2.2. Juntar prescrits + dispensats
  total_id<- mutate(facturats_id,tipus="Facturats") %>% rbind(mutate(prescrits_id,tipus="Prescrits"))
  total_id<-total_id %>% unite(id_kk,c(idp,tipus),remove = F)
  # Mapejar
  mapa<-MAP_ggplot(dades=total_id,datainicial = "dat",datafinal =
                     "dbaixa",id="id_kk",grup_linea = "tipus",grup_color = "tipus")
  
  # sumar temps acumulats
  temps_per_id<-total_id %>% mutate(temps=dbaixa-dat) %>% group_by(idp,tipus) %>%
    summarise(Temps=sum(temps))
  # Generar taulaMPR
  
  # Si no existeix dispensat = 0
  temps_dispensat<-temps_per_id$Temps[1] %>% as.numeric()
  temps_prescrit<-temps_per_id$Temps[2] %>% as.numeric()
  
  if (is.na(temps_prescrit)) {
    temps_prescrit<-temps_dispensat
    temps_dispensat<-0}
  
  tableMPR<-temps_dispensat/temps_prescrit %>%
    as_tibble() %>%
    select(MPR=value)
  # 3. Posar-ho en una taula
  tableMPR<-tableMPR %>%
    mutate(D=temps_dispensat,
           P=temps_prescrit,
           Adherence=if_else(MPR>0.8,"Good: >=0.8","Low: <0.8")) %>%
    transmute(D,P,MPR=round(MPR,3),Adherence)
  
  subtitul<-paste0("MPR: ",round(tableMPR$MPR,3)," del subjecte: ",total_id$idp[1])
  
  
  mapa+labs(x="Temps",
            y="Subjecte",
            title="Seguiment fàrmacs",
            subtitle=subtitul)+
    annotation_custom(gridExtra::tableGrob(tableMPR))
}


