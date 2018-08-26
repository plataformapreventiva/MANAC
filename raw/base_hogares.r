library(foreign)
library(tidyverse)

rm(list = ls())
ruta_trabajo <- setwd('C:/Users/H11765/Documents/Proyecto')

# hogares
hogares <- read.dbf('R_2010/Bases de datos/hogares.dbf')
concentrado <- read.dbf('R_2010/Bases de datos/concentrado.dbf')
hoga_conce <- full_join(hogares, concentrado, by = c('PROYECTO', 'FOLIOVIV', 'FOLIOHOG'))
rm('hogares', 'concentrado')

hoga_conce <- hoga_conce %>%
  mutate(hogar_id = paste0(PROYECTO, '-', FOLIOVIV, '-', FOLIOHOG),
         # tamaño de localidad
         tam_loc = ifelse(is.na(TAM_LOC.x), 0, TAM_LOC.x),
         # tenencia de la vivienda
         tenen = ifelse(is.na(TENEN), 0, 
                 ifelse(TENEN %in% c('3', '4'), 1, 
                 ifelse(TENEN == '1', 2, 
                 ifelse(TENEN %in% c('2', '5', '6'), 3, 0)))),
         # indicador de disponibilidad de sistema de drenaje
         drenaje_1 = ifelse(is.na(DRENAJE), 0, ifelse(DRENAJE %in% c('1', '2', '3', '4'), 1, 0)),
         # indicador de disponibilidad de electricidad
         elect_1 = ifelse(is.na(ELECT), 0, ifelse(ELECT %in% c('1', '2', '3', '4'), 1, 0)),
         # tipo de combustible para cocinar
         combus = ifelse(is.na(COMBUS), 0,
                  ifelse(COMBUS %in% c('1', '2'), 1, 
                  ifelse(COMBUS %in% c('3', '4'), 2,
                  ifelse(COMBUS %in% c('5', '6'), 3, 0)))),
         # forma en que desechan la basura
         eli_ba_1 = ifelse(is.na(ELI_BA), 0, ifelse(ELI_BA %in% c('1', '2', '3'), 1, 0)),
         # indicador de disponibilidad de cuarto para cocinar    
         cua_coc_1 = ifelse(is.na(CUA_COC), 0, ifelse(CUA_COC == '1', 1, 0)),
         # indicador de disponibilidad de sanitario        
         excus_1 = ifelse(is.na(EXCUS), 0, ifelse(EXCUS == '1', 1, 0)),
         # indicador automóvil en el hogar
         vehi1_1 = ifelse(is.na(VEHI1_N), 0, ifelse(VEHI1_N > 0, 1, 0)),
         # indicador camioneta cerrada en el hogar
         vehi2_1 = ifelse(is.na(VEHI2_N), 0, ifelse(VEHI2_N > 0, 1, 0)),
         # indicador camioneta de caja en el hogar
         vehi3_1 = ifelse(is.na(VEHI3_N), 0, ifelse(VEHI3_N > 0, 1, 0)),
         # indicador de posesión de automóvil en el hogar
         vehi_1 = ifelse(vehi1_1 == 1 | vehi2_1 == 1 | vehi3_1 == 1, 1, 0),
         # disponibilidad de servicio de línea telefónica en el hogar
         telefono_1 = ifelse(is.na(SERV_1), 0, ifelse(SERV_1 == '1', 1, 0)),
         # disponibilidad de servicio de teléfono móvil en el hogar
         celular_1 = ifelse(is.na(SERV_2), 0, ifelse(SERV_2 == '1', 1, 0)),
         # disponibilidad de internet en el hogar
         internet_1 = ifelse(is.na(SERV_4), 0, ifelse(SERV_4 == '1', 1, 0)),
         # indicador microondas en el hogar
         micro_1 = ifelse(is.na(EQH9_N), 0, ifelse(EQH9_N > 0, 1, 0)),
         # indicador lavadora en el hogar
         lavad_1 = ifelse(is.na(EQH12_N), 0, ifelse(EQH12_N > 0, 1, 0)),
         # indicador computadora en el hogar
         compu_1 = ifelse(is.na(EQH17_N), 0, ifelse(EQH17_N > 0, 1, 0)),
         # indicador estéreo en el hogar
         eqh1_1 = ifelse(is.na(EQH1_N), 0, ifelse(EQH1_N > 0, 1, 0)),
         # indicador radiograbadora en el hogar
         eqh2_1 = ifelse(is.na(EQH2_N), 0, ifelse(EQH2_N > 0, 1, 0)),
         # indicador radio en el hogar
         eqh3_1 = ifelse(is.na(EQH3_N), 0, ifelse(EQH3_N > 0, 1, 0)),
         # indicador equipo de radio en el hogar
         radio_1 = ifelse(eqh1_1 == 1 | eqh2_1 == 1 | eqh3_1 == 1, 1, 0),
         # indicador televisor en el hogar
         tv_1 = ifelse(is.na(EQH4_N), 0, ifelse(EQH4_N > 0,  1, 0)),
         # indicador refrigerador en el hogar
         refri_1 = ifelse(is.na(EQH10_N), 0, ifelse(EQH10_N > 0, 1, 0)),
         # indicador regadera en el hogar
         regad_1 = ifelse(is.na(REGAD), 0, ifelse(REGAD == '1', 1, 0)),
         # indicador tinaco en el hogar
         tin_azo_1 = ifelse(is.na(TIN_AZO), 0, ifelse(TIN_AZO == '1', 1, 0)),
         # indicador cisterna en el hogar
         ciste_1 = ifelse(is.na(CISTE), 0, ifelse(CISTE == '1', 1, 0)),
         # indicador calentador de agua en el hogar
         calen_1 = ifelse(is.na(CALEN), 0, ifelse(CALEN == '1', 1, 0)),
         # indicador bomba de agua en el hogar
         bom_ag_1 = ifelse(is.na(BOM_AG), 0, ifelse(BOM_AG == '1', 1, 0)),
         # indicador aire acondicionado en el hogar
         aire_aco_1 = ifelse(is.na(AIRE_ACO), 0, ifelse(AIRE_ACO == '1', 1, 0)),
         # número de servicios de comunicación en el hogar (línea telefónica, celular, intenet)
         serv_comu_n = telefono_1 + celular_1 + internet_1,
         # número de activos de mayor valor en el hogar (microondas, lavadora y computadora) 
         act_mayvalor_n = micro_1 + lavad_1 + compu_1,
         # número de activos normales en el hogar (radio, tv y refrigerador) 
         act_normal_n = radio_1 + tv_1 + refri_1,
         # número de bienes de equipamiento (regadera, tinaco, cisterna, calentador de agua, bomba de agua, aire acondicionado)
         equipa_n = regad_1 + tin_azo_1 + ciste_1 + calen_1 + bom_ag_1 + aire_aco_1,
         # número de cuartos del hogar
         cuart_n = ifelse(is.na(CUART), 0, CUART),
         # número de integrantes del hogar        
         tam_hog_n = ifelse(is.na(TAM_HOG), 0, TAM_HOG),
         # promedio de habitantes por cuarto
         hab_cuar_prom = ifelse(cuart_n > 0, tam_hog_n / cuart_n, 0),
         # número de hombres en el hogar
         hombres_n = ifelse(is.na(HOMBRES), 0, HOMBRES),
         # número de mujeres  en el hogar
         mujeres_n = ifelse(is.na(MUJERES), 0, MUJERES),
         # número de menores de 12 años en el hogar
         menores_n = ifelse(is.na(MENORES), 0, MENORES),
         # número de personas entre 12 y 64 años en el hogar 
         p12_64_n = ifelse(is.na(P12_64), 0, P12_64),
         # número de mayores de 64 años en el hogar 
         p65mas_n = ifelse(is.na(P65MAS), 0, P65MAS),
         # número de ocupados en el hogar     
         ocup_n = ifelse(is.na(N_OCUP), 0, N_OCUP),
         # número de perceptores de ingresos en el hogar     
         pering_n = ifelse(is.na(PERING), 0, PERING),
         # condición de recepción de ingresos por apoyo de otro hogar
         donativo_1 = ifelse(is.na(DONATIVO), 0, ifelse(DONATIVO > 0, 1, 0)),
         # condición de recepción de ingresos por remesas en el hogar      
         remesas_1 = ifelse(is.na(REMESA), 0, ifelse(REMESA > 0, 1, 0)),
         # condición de recepción de ingresos por programas de gobierno en el hogar    
         bene_gob_1 = ifelse(is.na(BENE_GOB), 0, ifelse(BENE_GOB > 0, 1, 0)),
         # sexo del jefe del hogar
         j_sexo = ifelse(is.na(SEXO), 0, SEXO),
         # edad del jefe del hogar
         j_edad = ifelse(is.na(EDAD), 0, EDAD),
         # educación formal del jefe del hogar
         j_ed_formal = ifelse(is.na(ED_FORMAL), 0, ED_FORMAL)) %>%
  select(hogar_id,
         # ingreso corriente del hogar
         ingcor = INGCOR,
         tam_loc,
         # ubicación geográfica
         ubica_geo = UBICA_GEO.x,
         tenen,
         drenaje_1,
         elect_1,
         combus,
         eli_ba_1,
         cua_coc_1,
         excus_1,
         vehi_1,
         serv_comu_n,
         act_mayvalor_n,
         act_normal_n,
         equipa_n,
         cuart_n,
         tam_hog_n,
         hab_cuar_prom,
         hombres_n,
         mujeres_n,
         menores_n,
         p12_64_n,
         p65mas_n,
         ocup_n,
         pering_n,
         donativo_1,
         remesas_1,
         bene_gob_1,
         j_sexo,
         j_edad,
         j_ed_formal)

# población, carencias y trabajos

poblacion <- read.dbf('R_2010/Bases de datos/poblacion.dbf') 
trabajos <- read.dbf('R_2010/Bases de datos/trabajos.dbf')

pobla_trab <- full_join(poblacion, trabajos, by = c('PROYECTO', 'FOLIOVIV', 'FOLIOHOG', 'NUMREN'))
rm('poblacion', 'trabajos')

pobla_trab <- pobla_trab %>%
  mutate(parentesco1 = substr(PARENTESCO, 1, 1)) %>%
  # no se incluye a trabajadores domésticos ni huéspedes
  filter(parentesco1 == '1' | parentesco1 == '2' | parentesco1 == '3' | parentesco1 == '5' | parentesco1 == '6')

carencias <- read.dbf("R_2010/Base final/pobreza_10.dbf")

carencias <- carencias %>%
  mutate(PROYECTO = factor(PROYECTO),
         FOLIOVIV = factor(str_pad(FOLIOVIV, width = 6, side = 'left', pad = '0')),
         FOLIOHOG = factor(FOLIOHOG),
         NUMREN = factor(ifelse(NUMREN < 10, paste0('0', NUMREN), NUMREN))) %>%
  select(PROYECTO, 
         FOLIOVIV, 
         FOLIOHOG, 
         NUMREN, 
         ic_rezedu, 
         ic_cv, ic_sbv, 
         ic_asalud, 
         ic_segsoc, 
         ic_ali, 
         ins_ali)

pobla_trab_caren <- full_join(pobla_trab, carencias, by = c('PROYECTO', 'FOLIOVIV', 'FOLIOHOG', 'NUMREN'))
rm('pobla_trab', 'carencias') 
   
pobla_trab_caren <- pobla_trab_caren %>%
  mutate(hogar_id = paste0(PROYECTO, '-', FOLIOVIV, '-', FOLIOHOG),
         # educación formal 
         edu_formal = ifelse(is.na(NIVELAPROB), 0,
                      ifelse(NIVELAPROB == '0', 1, 
                      ifelse(NIVELAPROB == '1', 2, 
                      ifelse(NIVELAPROB == '2' & GRADOAPROB %in% c('0', '1', '2', '3', '4', '5'), 3, 
                      ifelse(NIVELAPROB == '2' & GRADOAPROB == '6', 4,
                      ifelse(NIVELAPROB %in% c('5','6') & ANTEC_ESC == '1', 4,
                      ifelse(NIVELAPROB == '3' & GRADOAPROB %in% c('0', '1', '2'), 5,
                      ifelse(NIVELAPROB == '3' & GRADOAPROB == '3', 6,
                      ifelse(NIVELAPROB %in% c('5','6') & ANTEC_ESC == '2', 6,
                      ifelse(NIVELAPROB == '4' & GRADOAPROB %in% c('0', '1', '2'), 7,
                      ifelse(NIVELAPROB == '4' & GRADOAPROB == '3', 8,
                      ifelse(NIVELAPROB %in% c('5','6') & ANTEC_ESC == '3', 8,
                      ifelse(NIVELAPROB == '7' & GRADOAPROB %in% c('0', '1', '2', '3'), 9,
                      ifelse(NIVELAPROB == '7' & GRADOAPROB %in% c('4', '5', '6'), 10,
                      ifelse(NIVELAPROB %in% c('5','6') & ANTEC_ESC == '4', 10,
                      ifelse(NIVELAPROB %in% c('8','9'), 11,
                      ifelse(NIVELAPROB %in% c('5','6') & ANTEC_ESC == '5', 11, 0))))))))))))))))),
         # personas menores de 16 años
         p16_menos = ifelse(is.na(EDAD), 0, ifelse(EDAD < 16, 1, 0)),
         # personas entre 16 y 64 años
         p16_64 = ifelse(is.na(EDAD), 0, ifelse(EDAD > 15 & EDAD < 65, 1, 0)),
         # personas mayores de 64 años
         p65mas = ifelse(is.na(EDAD), 0, ifelse(EDAD > 64, 1, 0)),
         # hijos nacidos vivos
         hijos_viv = ifelse(is.na(HIJOS_VIV), 0, HIJOS_VIV),
         # hijos fallecidos
         hijos_mue = ifelse(is.na(HIJOS_MUE), 0, HIJOS_MUE),
         # hablante de lengua indígena           
         hablaind_1 = ifelse(is.na(HABLAIND), 0, ifelse(HABLAIND == '1', 1, 0)),
         # personas que saben leer y escribir           
         alfabe_1 = ifelse(is.na(ALFABE), 0, ifelse(ALFABE == '1', 1, 0)),
         # personas con carencia en servicios de salud
         ic_asalud_1 = ifelse(is.na(ic_asalud), 0, ic_asalud),
         # personas mayores de 16 años con rezago educativo
         rezedu_may16_1 = ifelse(is.na(ic_rezedu), 0, ifelse(ic_rezedu == 1 & EDAD > 16, 1, 0)),
         # escala mexicana de seguridad alimentaria
         j_ins_ali = ifelse(is.na(ins_ali), 0, ifelse(parentesco1 == '1', ins_ali, 0)),
         # indicador de carencia por calidad y espacios de la vivienda
         j_ic_cv = ifelse(is.na(ic_cv), 0, ifelse(ic_cv == 1 & parentesco1 == '1', 1, 0)),
         # indicador de carencia por servicios básicos de la vivienda
         j_ic_sbv = ifelse(is.na(ic_sbv), 0, ifelse(ic_sbv == 1 & parentesco1 == '1', 1, 0)),
         # condición de ocupación del jefe del hogar
         j_ocup = ifelse(is.na(NUMTRAB), 0, ifelse(NUMTRAB == '1' & parentesco1 == '1', 1, 0)),
         # jefe con rezago educativo            
         j_rezedu = ifelse(is.na(ic_rezedu), 0, ifelse(ic_rezedu == 1 & parentesco1 == '1', 1, 0)),
         # jefe hablante de lengua indígena           
         j_hablaind = ifelse(is.na(HABLAIND), 0, ifelse(HABLAIND == '1' & parentesco1 == '1', 1, 0)),
         # jefe con discapacidad
         j_disc = ifelse(is.na(DISC1), 0, ifelse(DISC1 %in% c('1', '2', '3', '4', '5', '6', '7') & parentesco1 == '1', 1, 0)),
         # jefe con posición laboral independiente
         j_ind = ifelse(is.na(INDEP), 0, ifelse(INDEP == '1' & parentesco1 == '1', 1, 0)),
         # jefe con posición laboral subordinado
         j_sub = ifelse(is.na(SUBOR), 0, ifelse(SUBOR == '1' & parentesco1 == '1', 1, 0)),
         # prestaciones laborales del jefe servicios médicos
         j_sermed = ifelse(is.na(PRES_1), 0, ifelse(PRES_1 == '01' & parentesco1 == '1', 1, 0)),
         # prestaciones laborales del jefe aguinaldo  
         j_agui = ifelse(is.na(PRES_2), 0, ifelse(PRES_2 == '02' & parentesco1 == '1', 1, 0)),
         # prestaciones laborales del jefe vacaciones           
         j_vac = ifelse(is.na(PRES_3), 0, ifelse(PRES_3 == '03' & parentesco1 == '1', 1, 0)),
         # prestaciones laborales del jefe utilidades           
         j_utili = ifelse(is.na(PRES_4), 0, ifelse(PRES_4 == '04' & parentesco1 == '1', 1, 0)),
         # prestaciones laborales del jefe afore 
         j_afore = ifelse(is.na(PRES_8), 0, ifelse(PRES_8 == '08' & parentesco1 == '1', 1, 0)),
         # sector económico del jefe del hogar          
         j_sect = ifelse(is.na(NUMTRAB) | is.na(TIPOACT), 0, ifelse(NUMTRAB == '1' & parentesco1 == '1', TIPOACT, 0)),
         # sexo del cónyugue
         cony_sexo = ifelse(is.na(SEXO), 0, ifelse(parentesco1 == '2', SEXO, 0)),
         # edad del cónyugue
         cony_edad = ifelse(is.na(EDAD), 0, ifelse(parentesco1 == '2', EDAD, 0)),
         # educación formal del cónyugue
         cony_ed_formal = ifelse(parentesco1 == '2', edu_formal, 0), 
         # condición de ocupación del cónyugue
         cony_ocup = ifelse(is.na(NUMTRAB), 0, ifelse(NUMTRAB == '1' & parentesco1 == '2', 1, 0)),
         # cónyuge con rezago educativo            
         cony_rezedu = ifelse(is.na(ic_rezedu), 0, ifelse(ic_rezedu == 1 & parentesco1 == '2', 1, 0)),
         # cónyuge hablante de lengua indígena           
         cony_hablaind = ifelse(is.na(HABLAIND), 0, ifelse(HABLAIND == '1' & parentesco1 == '2', 1, 0)),
         # cónyuge con discapacidad
         cony_disc = ifelse(is.na(DISC1), 0, ifelse(DISC1 %in% c('1', '2', '3', '4', '5', '6', '7') & parentesco1 == '2', 1, 0)),
         # cónyuge con posición laboral independiente
         cony_ind = ifelse(is.na(INDEP), 0, ifelse(INDEP == '1' & parentesco1 == '2', 1, 0)),
         # cónyuge con posición laboral subordinado
         cony_sub = ifelse(is.na(SUBOR), 0, ifelse(SUBOR == '1' & parentesco1 == '2', 1, 0)),
         # prestaciones laborales del cónyuge servicios médicos
         cony_sermed = ifelse(is.na(PRES_1), 0, ifelse(PRES_1 == '01' & parentesco1 == '2', 1, 0)),
         # prestaciones laborales del cónyuge aguinaldo           
         cony_agui = ifelse(is.na(PRES_2), 0, ifelse(PRES_2 == '02' & parentesco1 == '2', 1, 0)),
         # prestaciones laborales del cónyuge vacaciones           
         cony_vac = ifelse(is.na(PRES_3), 0, ifelse(PRES_3 == '03' & parentesco1 == '2', 1, 0)),
         # prestaciones laborales del cónyuge utilidades           
         cony_utili = ifelse(is.na(PRES_4), 0, ifelse(PRES_4 == '04' & parentesco1 == '2', 1, 0)),
         # prestaciones laborales del cónyuge afore           
         cony_afore = ifelse(is.na(PRES_8), 0, ifelse(PRES_8 == '08' & parentesco1 == '2', 1, 0)),
         # sector económico del cónyuge del hogar          
         cony_sect = ifelse(is.na(NUMTRAB) | is.na(TIPOACT), 0, ifelse(NUMTRAB == '1' & parentesco1 == '2', TIPOACT, 0)),
         # indicador de presencia de menor trabajador
         p16_menos_trab = ifelse(is.na(TRABAJO), 0, ifelse(TRABAJO == '1' & EDAD < 16, 1, 0)),
         # indicador de presencia de trabajador entre 16 y 64 años
         p16_64_trab = ifelse(is.na(TRABAJO), 0, ifelse(TRABAJO == '1' & EDAD > 16 & EDAD < 65, 1, 0)),
         # indicador de presencia de mayor de 64 años trabajador
         p65mas_trab = ifelse(is.na(TRABAJO), 0, ifelse(TRABAJO == '1' & EDAD > 64, 1, 0)),
         # indicador se personas jubiladas o pensionadas en el hogar 
         jub_pen = ifelse(is.na(BUSTRAB_3), 0, ifelse(BUSTRAB_3 == '3', 1, 0))) %>%
  select(hogar_id,
         edu_formal,
         p16_menos,
         p16_64,
         p65mas,
         hijos_viv,
         hijos_mue,
         hablaind_1,
         alfabe_1,
         ic_asalud_1,
         rezedu_may16_1,
         j_ins_ali,
         j_ic_cv,
         j_ic_sbv,
         j_ocup,
         j_rezedu,
         j_hablaind,
         j_disc,
         j_ind,
         j_sub,
         j_sermed,
         j_agui,
         j_vac,
         j_utili,
         j_afore,
         j_sect,
         cony_sexo,
         cony_edad,
         cony_ed_formal, 
         cony_ocup,
         cony_rezedu,
         cony_hablaind,
         cony_disc,
         cony_ind,
         cony_sub,
         cony_sermed,
         cony_agui,
         cony_vac,
         cony_utili,
         cony_afore,
         cony_sect,
         p16_menos_trab,
         p16_64_trab,
         p65mas_trab,
         jub_pen)

pobla_trab_caren <- pobla_trab_caren %>%
  group_by(hogar_id) %>%
  summarise(max_nved_h = max(edu_formal),
            p16_menos_n = sum(p16_menos),
            p16_64_n = sum(p16_64),
            p65mas_n = sum(p65mas),
            hijos_viv_n = sum(hijos_viv),
            hijos_mue_n = sum(hijos_mue),
            hablaind_n = sum(hablaind_1),
            alfabe_n = sum(alfabe_1),
            ic_asalud_n = sum(ic_asalud_1),
            rezedu_may16_n = sum(rezedu_may16_1),
            ins_ali_h = sum(j_ins_ali),
            ic_cv_h = sum(j_ic_cv),
            ic_sbv_h = sum(j_ic_sbv),
            j_ocup = sum(j_ocup),
            j_rezedu = sum(j_rezedu),
            j_hablaind = sum(j_hablaind),
            j_disc = sum(j_disc),
            j_ind = sum(j_ind),
            j_sub = sum(j_sub),
            j_sermed = sum(j_sermed),
            j_agui = sum(j_agui),
            j_vac = sum(j_vac),
            j_utili = sum(j_utili),
            j_afore = sum(j_afore),
            j_sect = sum(j_sect),
            cony_sexo = sum(cony_sexo),
            cony_edad = sum(cony_edad),
            cony_ed_formal = sum(cony_ed_formal), 
            cony_ocup = sum(cony_ocup),
            cony_rezedu = sum(cony_rezedu),
            cony_hablaind = sum(cony_hablaind),
            cony_disc = sum(cony_disc),
            cony_ind = sum(cony_ind),
            cony_sub = sum(cony_sub),
            cony_sermed = sum(cony_sermed),
            cony_agui = sum(cony_agui),
            cony_vac = sum(cony_vac),
            cony_utili = sum(cony_utili),
            cony_afore = sum(cony_afore),
            cony_sect = sum(cony_sect),
            p16_menos_trab_n = sum(p16_menos_trab),
            p16_64_trab_n = sum(p16_64_trab),
            p65mas_trab_n = sum(p65mas_trab),
            jub_pen_n = sum(jub_pen)) %>%
  mutate(# relación de dependencia
         rel_depen = ifelse(p16_64_n > 0, (p16_menos_n + p65mas_n)/p16_64_n, p16_menos_n + p65mas_n),
         # tasa de mortalidad
         tasa_mort = ifelse(hijos_viv_n > 0, hijos_mue_n/hijos_viv_n, 0)) %>%
  select(hogar_id, 
         max_nved_h,
         hablaind_n, 
         alfabe_n,
         ic_asalud_n,
         rezedu_may16_n,
         ins_ali_h,
         ic_cv_h,
         ic_sbv_h,
         rel_depen,
         tasa_mort,
         j_ocup,
         j_rezedu,
         j_hablaind,
         j_disc,
         j_ind,
         j_sub,
         j_sermed,
         j_agui,
         j_vac,
         j_utili,
         j_afore,
         j_sect,
         cony_sexo,
         cony_edad,
         cony_ed_formal, 
         cony_ocup,
         cony_rezedu,
         cony_hablaind,
         cony_disc,
         cony_ind,
         cony_sub,
         cony_sermed,
         cony_agui,
         cony_vac,
         cony_utili,
         cony_afore,
         cony_sect,
         p16_menos_trab_n,
         p16_64_trab_n,
         p65mas_trab_n,
         jub_pen_n)

base_ingreso <- full_join(hoga_conce, pobla_trab_caren, by = c('hogar_id'))
rm('hoga_conce', 'pobla_trab_caren')

# verificando faltantes
nas = rep(0, ncol(base_ingreso)) 
for (i in 1:ncol(base_ingreso)){
  x <- i
  nas[x] = sum(is.na(base_ingreso[,x]))
}
sum(nas)
rm('i', 'x')

# verificando la estructura de la base
str(base_ingreso)

# guardando base final
write_csv(x = base_ingreso, path = 'modelo_ingreso/base_ingreso.csv')

