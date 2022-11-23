
rm(list = ls())

rm(amostra)
# Instalando e lendo pacotes ------------------------------------------------------
install.packages("RODBC")
install.packages("DBI")
install.packages("survey")
install.packages("srvyr")
install.packages("tidyverse")
install.packages("data.table")

library(data.table)
library(RODBC)  # ConexÃ£o com servidor de banco de dados
library(DBI)    # ConexÃ£o com servidor de banco de dados
library(survey) # Desenho da amostra complexa da Pdad
library(srvyr)  # Desenho da amostra complexa da Pdad
library(tidyverse)  # NecessÃ¡ria para transformar variÃ¡veis
library(clipr)
library(rlang)

# Baixando a base de dados------------------------------------------------------



pdad_pes<- read_delim("pdad2021_moradores.csv",";",
                      escape_double = FALSE, locale = locale(decimal_mark = ","),
                      trim_ws = TRUE)
pdad_dom<- read_delim("pdad2021_domicilios.csv",";",
                      escape_double = FALSE, locale = locale(decimal_mark = ","),
                      trim_ws = TRUE)


# junção das duas bases
x <- which((names(pdad_dom)%in%names(pdad_pes)))
names(pdad_dom)[x]


pdad_2021 <- pdad_pes %>% 
  dplyr::left_join(pdad_dom,
                   by=c("A01ra"="A01ra",
                        "A01nficha"="A01nficha",
                        "A01setor" = "A01setor")) 



# Criando variáveis gerais-------------------------------------------------------


pdad_2021 <- pdad_2021 %>% mutate(RA=factor(A01ra, levels = c(1,2,3,4,5,6,7,8,
                                                              9,10,11,12,13,14,15,16,17,18,
                                                              19,20,21,22,23,24,25,26,27,28,
                                                              29,30,31,32,33),
                                            labels = c("Plano Piloto", "Gama", "Taguatinga",
                                                       "Brazlândia", "Sobradinho", "Planaltina", "Paranoá",
                                                       "Núcleo Bandeirante", "Ceilândia", "Guará",
                                                       "Cruzeiro", "Samambaia", "Santa Maria", "São Sebastião",
                                                       "Recanto das Emas", "Lago Sul", "Riacho Fundo", "Lago Norte",
                                                       "Candangolândia", "Águas Claras", "Riacho Fundo II", "Sudoeste/Octogonal",
                                                       "Varjão", "Parkway", "Scia/Estrutural", "Sobradinho II", "Jardim Botânico",
                                                       "Itapoã", "SIA*", "Vicente Pires", "Fercal", "Sol Nascente/Pôr do Sol",
                                                       "Arniqueira")),
                                  
                                  #Grupos de rendimento - PED
                                  gruposrenda_ped=case_when(A01ra==1|A01ra==16|A01ra==18|A01ra==22|A01ra==24|A01ra==27 ~ 1,
                                                            A01ra==2|A01ra==3|A01ra==5|A01ra==8|A01ra==10|A01ra==11|A01ra==19|A01ra==20|A01ra==26|A01ra==30|A01ra==33 ~ 2,
                                                            A01ra==4|A01ra==6|A01ra==9|A01ra==12|A01ra==13|A01ra==14|A01ra==17|A01ra==21|A01ra==29|A01ra==32 ~ 3,
                                                            A01ra==7|A01ra==15|A01ra==23|A01ra==25|A01ra==28|A01ra==31 ~ 4),
                                  
                                  #Grupos de rendimento - PDAD 2021
                                  gruposrenda_pdad=case_when(A01ra==20|A01ra==27|A01ra==18|A01ra==16|A01ra==24|A01ra==1|A01ra==22 ~1 ,
                                                             A01ra==33|A01ra==19|A01ra==11|A01ra==10|A01ra==8|A01ra==29|A01ra==5|A01ra==3|A01ra==30 ~ 2,
                                                             A01ra==9|A01ra==2|A01ra==17|A01ra==12|A01ra==13|A01ra==26 ~3,
                                                             A01ra==4|A01ra==31|A01ra==28|A01ra==7|A01ra==6|A01ra==15|
                                                               A01ra==21|A01ra==14|A01ra==25|A01ra==32|A01ra==23 ~4),
                                  #Regiões de saúde
                                  regioesdesaude=factor(case_when(A01ra==1|A01ra==11|A01ra==22|A01ra==16|A01ra==18|A01ra==23 ~"1. Central",
                                                                  A01ra==4|A01ra==9|A01ra==32 ~"2. Oeste",
                                                                  A01ra==29|A01ra==25|A01ra==8|A01ra==17|A01ra==10|A01ra==21|A01ra==24|A01ra==19 ~"3. Centro-Sul",
                                                                  A01ra==3|A01ra==12|A01ra==20|A01ra==30|A01ra==33|A01ra==15 ~"4. Sudoeste",
                                                                  A01ra==5|A01ra==26|A01ra==6|A01ra==31 ~"5. Norte",
                                                                  A01ra==7|A01ra==14|A01ra==27|A01ra==28 ~"6. Leste",
                                                                  A01ra==2|A01ra==13 ~"7. Sul")),
                                  #Criando a variável sexo
                                  sexo = factor(case_when(E04==1~"Homens",
                                                          E04==2~"Mulheres")),
                                  #Criando a variável raça/cor
                                  raca_cor = factor(case_when(E06==2|E06==4 ~"1.Negros",
                                                              E06==1|E06==3|E06==5 ~"2.Não negros")),
                                  
                                  
                                  #Criando as faixas etárias 
                                  faixa_etaria_quinquenal=cut(idade,
                                                              breaks=c(-Inf,seq(4,84, by=5), Inf),
                                                              labels=c("0 a 4 anos", "5 a 9 anos",
                                                                       "10 a 14 anos","15 a 19 anos",
                                                                       "20 a 24 anos", "25 a 29 anos",
                                                                       "30 a 34 anos", "35 a 39 anos",
                                                                       "40 a 44 anos", "45 a 49 anos",
                                                                       "50 a 54 anos","55 a 59 anos",
                                                                       "60 a 64 anos", "65 a 69 anos",
                                                                       "70 a 74 anos", "75 a 19 anos",
                                                                       "80 a 84 anos", "85 anos ou mais"),
                                                              ordered_result=TRUE))



pdad_2021$gruposrenda_ped <- factor(pdad_2021$gruposrenda_ped, labels = c("1.Alta", "2.Média-alta", "3.Média-baixa", "4.Baixa"))
pdad_2021$gruposrenda_pdad <- factor(pdad_2021$gruposrenda_pdad, labels = c("1.Alta", "2.Média-alta", "3.Média-baixa", "4.Baixa"))


pdad_2021$criterio_brasil<-factor (pdad_2021$criterio_brasil, labels = c("Classe A", "Classe B1", "Classe B2", "Classe C1", "Classe C2", "Classe DE", "Sem Classificação"))



pdad_2021 <- pdad_2021 %>%
  mutate(
    
    def_visual = case_when(E09 %in% c(1, 2) ~ 1,
                           E09 %in% c(3, 4) ~ 0),
    
    def_auditiva = case_when(E10 %in% c(1, 2) ~ 1,
                             E10 %in% c(3, 4) ~ 0),
    
    def_intelectual = case_when(E12 %in% c(1, 2) ~ 1,
                                E12 %in% c(3, 4) ~ 0),
    
    def_fisica = case_when(E11 %in% c(1,2) | E13 %in% c(1,2)~1,
                           E11 %in% c(3,4) & E13 %in% c(3,4)~0),
    
    possui_def = ifelse(def_visual == 1 | def_auditiva == 1 |
                          def_intelectual == 1 | def_fisica == 1, 1,0),
    
    def_multipla = ifelse(def_visual+def_fisica+def_auditiva+def_intelectual > 1,1,0)
  ) %>%
  

  
  mutate(
    tipo_defic = case_when(
      def_visual == 1 ~ "Visual",
      def_auditiva == 1 ~ "Auditiva",
      def_intelectual == 1 ~ "Intelectual",
      def_fisica == 1 ~ "Fisica"
    ),
    tipo_defic = ifelse(def_multipla == 1, "Multipla", tipo_defic)
  )


#Raça e Gênero

pdad_2021 <- pdad_2021 %>%
  mutate(raca_gen=case_when(sexo == "Homens" & raca_cor=="1.Negros" ~ "3.Homem negro",
                            sexo == "Homens" & raca_cor=="2.Não negros" ~ "4.Homem não negro",
                            sexo == "Mulheres" & raca_cor=="1.Negros" ~ "1.Mulher negra",
                            sexo == "Mulheres" & raca_cor=="2.Não negros" ~ "2.Mulher não negra"))
#Etapa de Ensino 

pdad_2021 <- pdad_2021 %>%
  mutate(etapa_ens = factor(case_when(H07 == 3 ~ "Alfabetização de jovens e adultos",
                                      H07==4 & H08 %in% c(1,2,3,4,5) ~ "Ensino Fundamental - Anos iniciais",
                                      H07==4 & H08 %in% c(6,7,8,9) ~ "Ensino Fundamental - Anos finais",
                                      H07 %in% c(5,6) ~ "Ensino Médio",
                                      H07 == 9 ~ "Ensino Superior",
                                      H07==10 ~ "Especialização",
                                      H07 %in% c(11,12) ~ "Mestrado/doutorado",
                                      H07 == 7 ~ "EJA Ensino Fundamental",
                                      H07 == 8 ~ "EJA Ensino Médio",
                                      H07 == 88 ~ "Não sabe")))




# Densidade cômodo 

pdad_2021 <- pdad_2021 %>%
  mutate(densidade = A01npessoas/B11)


# Idades 
pdad_2021 <- pdad_2021 %>%
mutate(
  cca_0a5=ifelse(idade<6,1,0),
  cca_0a6=ifelse(idade<7,1,0),
  cca_0a11=ifelse(idade<12,1,0),
  pes_menor18=ifelse(idade<18,1,0))

table(pdad_2021$cca_0a5)
table(pdad_2021$cca_0a6)
table(pdad_2021$cca_0a11)
table(pdad_2021$pes_menor18)

# Variáveis domicílio 
pdad_2021_carac_dom <- dplyr::select(pdad_2021,A01nficha,cca_0a5,cca_0a6,cca_0a11,pes_menor18,possui_def) %>%
  # Agrupar por domicílio
  dplyr::group_by(A01nficha) %>%
  # Somatório de cada condição por domicílio
  dplyr::summarise(                
    cca_0a5_sum=sum(cca_0a5),
    cca_0a6_sum=sum(cca_0a6),
    cca_0a11_sum=sum(cca_0a11),
    pes_menor18_sum=sum(pes_menor18),
    possui_def_sum=sum(possui_def))%>%

  dplyr::mutate(
    cca_0a5_bin=ifelse(cca_0a5_sum>0,1,0),
    cca_0a6_bin=ifelse(cca_0a6_sum>0,1,0),
    cca_0a11_bin=ifelse(cca_0a11_sum>0,1,0),
    pes_menor18_bin=ifelse(pes_menor18_sum>0,1,0),
    possui_def_bin=ifelse(possui_def_sum>0,1,0))


# Junção com cenários

pdad_2021 <- pdad_2021 %>% 
  dplyr::left_join(pdad_2021_carac_dom, by="A01nficha")


#Segurança alimentar 

pdad_2021 <- pdad_2021 %>%

mutate(inseg_alimentar=factor(inseg_alimentar, labels = c("Segurança",
                                                         "Insegurança leve",
                                                         "Insegurança moderada",
                                                         "Insegurança grave",
                                                         "Sem Classificação")))                                                                                                                                  

# Declarando o desenho amostral da pesquisa -------------------------------

sample.pdad <- 
  survey::svydesign(id = ~A01nficha, # Identificador único da unidade amostrada
                    strata = ~A01setor, # Identificação do estrato
                    weights = ~PESO_PRE, # Inverso da fração amostral
                    nest=TRUE, # Parâmetro de tratamento para os IDs dos estratos
                    data=pdad_2021 # Declarar a base a ser utilizada
  )

# Criar um objeto para pós estrato
post.pop <- pdad_2021 %>%
  dplyr::group_by(POS_ESTRATO) %>% # Agrupar por pós-estrato
  dplyr::summarise(Freq=max(POP_AJUSTADA_PROJ)) # Capturar o total da população

# Declarar o objeto de pós-estrato
# Estamos dizendo nesse passo qual é a população alvo para cada
# pós-estrato considerado
sample.pdad <- survey::postStratify(sample.pdad,~POS_ESTRATO,post.pop)

# Criar objeto para calcular os erros por bootstrap (Rao and Wu’s(n − 1) bootstrap)
# J. N. K. Rao and C. F. J. Wu - Journal of the American Statistical Association
# Vol. 83, No. 401 (Mar., 1988), pp. 231-241
amostra <- survey::as.svrepdesign(sample.pdad, type = "subbootstrap")

# Ajustar para tratamento de estratos com apenas uma UPA (adjust=centered)
options(survey.lonely.psu = "adjust")

# Ajustar objeto de amostra, para uso com o pacote srvyr (como tibble)
amostra <- srvyr::as_survey(amostra)


sample.pdad <- survey::postStratify(sample.pdad,~POS_ESTRATO,post.pop)

# Criar objeto para calcular os erros por bootstrap 
# J. N. K. Rao and C. F. J. Wu - Journal of the American Statistical Association
# Vol. 83, No. 401 (Mar., 1988), pp. 231-241

amostra <- survey::as.svrepdesign(sample.pdad, type = "subbootstrap")

# Ajustar estratos com apenas uma UPA (adjust=centered)

options( survey.lonely.psu = "adjust")

# Ajustar objeto de amostra, para uso com o pacote srvyr -

amostra <- srvyr::as_survey(amostra)    


# VARIÁVEIS INTERMEDIÁRIAS -------------------------------

# 1) Esgotamento Sanitário 



tab_1 <- amostra %>% 
  
  srvyr::mutate(esgotamento= ifelse(B14_1 ==1 |B14_2 == 1, "Tem esgotamento", "Não tem esgotamento"))    %>%
  
  
  srvyr::group_by(esgotamento) %>% 
  srvyr::summarise(n=survey_total(vartype = "cv"),
                   pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
  mutate(territorio="DF") %>% 
  select(territorio, esgotamento, n, n_cv, pct, pct_cv) %>% 
  pivot_wider(names_from = "esgotamento", values_from = c("n", "n_cv", "pct", "pct_cv"))

write_clip(tab_1, dec=",")

tab_1.1 <- amostra %>% 
  
  
  srvyr::mutate(esgotamento= ifelse(B14_1 ==1 |B14_2 == 1, "Tem esgotamento", "Não tem esgotamento"))    %>%
  
  
  srvyr::group_by(inseg_alimentar, esgotamento) %>% 
  srvyr::summarise(n=survey_total(vartype = "cv"),
                   pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
  
  select(inseg_alimentar, esgotamento, n, n_cv, pct, pct_cv) %>% 
  pivot_wider(names_from = "esgotamento", values_from = c("n", "n_cv", "pct", "pct_cv"))

write_clip(tab_1.1, dec=",")


# 2) Número de psssoas no domicílio 


tab_2 <-  amostra %>%

  srvyr::summarise(media = survey_mean(A01npessoas,na.rm=T, vartype = "cv"))

write_clip(tab_2, dec =",")



tab_2.1 <-  amostra %>%
  
  srvyr::group_by(inseg_alimentar) %>% 
  
  
  srvyr::summarise(media = survey_mean(A01npessoas,na.rm=T, vartype = "cv"))

write_clip(tab_2.1, dec =",")


  
  

# 3) Densidade 



tab_3 <-  amostra %>%
  
  srvyr::filter(densidade!=0 & densidade<1000) %>% 
  
  srvyr::group_by(inseg_alimentar) %>% 
  srvyr::summarise(media = survey_mean(densidade,na.rm=T, vartype = "cv"))

write_clip(tab_3, dec =",")

tab_3.1 <-  amostra %>%
  srvyr::filter(idade>=0&idade<=11) %>%
  
  srvyr::filter(densidade!=0 & densidade<1000) %>% 
  
  
  srvyr::summarise(media = survey_mean(densidade,na.rm=T, vartype = "cv"))

write_clip(tab_3.1, dec =",")


#8) Criança com menos de 6 anos



tab_8 <- amostra %>% 
  
  srvyr::mutate(criança5= ifelse( cca_0a5==1, "Tem criança com menos de 6 anos ", "Não tem  criança com menos de 6 anos"))    %>%
  
  
  srvyr::group_by(criança5) %>% 
  srvyr::summarise(n=survey_total(vartype = "cv"),
                   pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
  mutate(territorio="DF") %>% 
  select(territorio, criança5, n, n_cv, pct, pct_cv) %>% 
  pivot_wider(names_from = "criança5", values_from = c("n", "n_cv", "pct", "pct_cv"))

write_clip(tab_8, dec=",")


tab_8.1 <- amostra %>% 
  
  srvyr::mutate(criança5= ifelse( cca_0a5==1, "Tem criança com menos de 6 anos ", "Não tem  criança com menos de 6 anos"))    %>%
  
  
  srvyr::group_by(inseg_alimentar, criança5) %>% 
  srvyr::summarise(n=survey_total(vartype = "cv"),
                   pct=survey_mean(vartype = "cv")) %>% na.omit %>% 

  select(inseg_alimentar, criança5, n, n_cv, pct, pct_cv) %>% 
  pivot_wider(names_from = "criança5", values_from = c("n", "n_cv", "pct", "pct_cv"))

write_clip(tab_8.1, dec=",")


table(pdad_2021$cca_0a6)


#9) Número de crianças na casa 


tab_9 <-  amostra %>%
  
  srvyr::summarise(ncrianças = survey_mean(cca_0a11_sum,na.rm=T, vartype = "cv"))

write_clip(tab_9, dec =",")


tab_9.1 <-  amostra %>%
  
  srvyr::group_by(inseg_alimentar) %>% 
  
  srvyr::summarise(ncrianças = survey_mean(cca_0a11_sum,na.rm=T, vartype = "cv"))

write_clip(tab_9.1, dec =",")


#10) Domicílios com crianças 


tab_10<- amostra %>% 
  


  srvyr::mutate (dom=factor(case_when(pes_menor18_sum ==0 ~ "Só adultos",
                                      pes_menor18_sum == 1 ~ "1 menor de 18",
                                      pes_menor18_sum == 2 ~"2 menor que 18",
                                      pes_menor18_sum > 2 ~" 3 ou mais menor que 18" 
                                      
                                             )))  %>%


  srvyr::group_by(dom) %>% 
  srvyr::summarise(n=survey_total(vartype = "cv"),
                   pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
  mutate(territorio="DF") %>% 
  select(territorio, dom, n, n_cv, pct, pct_cv) %>% 
  pivot_wider(names_from = "dom", values_from = c("n", "n_cv", "pct", "pct_cv"))


write_clip(tab_10, dec=",")







tab_10.1 <- amostra %>% 
  
  
  
  srvyr::mutate (dom=factor(case_when(pes_menor18_sum ==0 ~ "Só adultos",
                                      pes_menor18_sum == 1 ~ "1 menor de 18",
                                      pes_menor18_sum == 2 ~"2 menor que 18",
                                      pes_menor18_sum > 2 ~" 3 ou mais menor que 18" 
                                      
  )))  %>%
  
  
  srvyr::group_by(inseg_alimentar, dom) %>% 
  srvyr::summarise(n=survey_total(vartype = "cv"),
                   pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
 
  select(inseg_alimentar, dom, n, n_cv, pct, pct_cv) %>% 
  pivot_wider(names_from = "dom", values_from = c("n", "n_cv", "pct", "pct_cv"))


write_clip(tab_10.1, dec=",")




# 11) Pessoa com deficiência 

tab_11<- amostra %>% 
  
  
  
  srvyr::mutate (def=factor(case_when(possui_def_bin ==1 ~ " Possui Pess. com def",
                                      pes_menor18_sum == 0 ~ "Não possui Pess. com def",
                                     
  )))  %>%
  
  
  srvyr::group_by(def) %>% 
  srvyr::summarise(n=survey_total(vartype = "cv"),
                   pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
  mutate(territorio="DF") %>% 
  select(territorio, def, n, n_cv, pct, pct_cv) %>% 
  pivot_wider(names_from = "def", values_from = c("n", "n_cv", "pct", "pct_cv"))


write_clip(tab_11, dec=",")



tab_11.1<- amostra %>% 
  
  
  
  srvyr::mutate (def=factor(case_when(possui_def_bin ==1 ~ " Possui Pess. com def",
                                      pes_menor18_sum == 0 ~ "Não possui Pess. com def",
                                      
  )))  %>%
  
  
  srvyr::group_by(inseg_alimentar, def) %>% 
  srvyr::summarise(n=survey_total(vartype = "cv"),
                   pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
  
  select(inseg_alimentar, def, n, n_cv, pct, pct_cv) %>% 
  pivot_wider(names_from = "def", values_from = c("n", "n_cv", "pct", "pct_cv"))


write_clip(tab_11.1, dec=",")






# VARIÁVEIS PROXIMAIS -------------------------------

# 4)  Faixa etária chefe 


tab_4 <-  amostra %>%
  
srvyr::filter(E05==1)  %>%


srvyr::mutate (idade_chefe=factor(case_when(idade>=0&idade<=60 ~ "Maior de 60",
                            TRUE ~"Outras idades")))  %>%

srvyr::group_by(idade_chefe) %>% 
  srvyr::summarise(n=survey_total(vartype = "cv"),
                   pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
  mutate(territorio="DF") %>% 
  select(territorio, idade_chefe, n, n_cv, pct, pct_cv) %>% 
  pivot_wider(names_from = "idade_chefe", values_from = c("n", "n_cv", "pct", "pct_cv"))

write_clip(tab_4, dec=",")



tab_4.1 <-  amostra %>%
  
  srvyr::filter(E05==1)  %>%
  
  
  srvyr::mutate (idade_chefe=factor(case_when(idade>=0&idade<=60 ~ "Maior de 60",
                                              TRUE ~"Outras idades")))  %>%

  srvyr::group_by(inseg_alimentar, idade_chefe) %>% 
  srvyr::summarise(n=survey_total(vartype = "cv"),
                   pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
  
  select(inseg_alimentar, idade_chefe, n, n_cv, pct, pct_cv) %>% 
  pivot_wider(names_from = "idade_chefe", values_from = c("n", "n_cv", "pct", "pct_cv"))

write_clip(tab_4.1, dec=",")





# 5)  Ocupação chefe


tab_5 <-  amostra %>%
  
  srvyr::filter(E05==1)  %>%
  
  srvyr::mutate (ocupado_chefe=factor(case_when(I05==1 ~"Ocupado",
                            I05==2 ~"Não ocupado"))) %>% 
  
  srvyr::group_by(ocupado_chefe) %>% 
  srvyr::summarise(n=survey_total(vartype = "cv"),
                   pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
  mutate(territorio="DF") %>% 
  select(territorio, ocupado_chefe, n, n_cv, pct, pct_cv) %>% 
  pivot_wider(names_from = "ocupado_chefe", values_from = c("n", "n_cv", "pct", "pct_cv"))

write_clip(tab_5, dec=",")



tab_5.1 <-  amostra %>%
  
  srvyr::filter(E05==1)  %>%
  
  srvyr::mutate (ocupado_chefe=factor(case_when(I05==1 ~"Ocupado",
                                                I05==2 ~"Não ocupado"))) %>% 
  
  srvyr::group_by(inseg_alimentar, ocupado_chefe) %>% 
  srvyr::summarise(n=survey_total(vartype = "cv"),
                   pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
  
  select(inseg_alimentar, ocupado_chefe, n, n_cv, pct, pct_cv) %>% 
  pivot_wider(names_from = "ocupado_chefe", values_from = c("n", "n_cv", "pct", "pct_cv"))

write_clip(tab_5.1, dec=",")


#6)Formalização

tab_6 <-  amostra %>%

srvyr::filter(E05==1)  %>%
  
  
  srvyr::mutate (formal=factor(case_when((I12==1|I12==2)&(I14==1|I14==2|I14==3|I14==4)~"Formal",
                        (I12==3|I12==4|I12==14|I12==88888)&(I17==1)~"Formal",
                        (I12==7|I12==8|I12==9|I12==10|I12==11)&(I16==1)~"Formal",
                        (I12==3|I12==4|I12==14|I12==88888)&(I17==2)~"Informal",
                        (I12==7|I12==8|I12==9|I12==10|I12==11)&(I16==2)~"Informal",
                        I12==13~"Informal",
                        TRUE ~"Sem classificação"))) %>% 

  
  srvyr::group_by(formal) %>% 
  srvyr::summarise(n=survey_total(vartype = "cv"),
                   pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
  mutate(territorio="DF") %>% 
  select(territorio, formal, n, n_cv, pct, pct_cv) %>% 
  pivot_wider(names_from = "formal", values_from = c("n", "n_cv", "pct", "pct_cv"))


write_clip(tab_6, dec=",")




tab_6.1 <-  amostra %>%
  
  srvyr::filter(E05==1)  %>%
  
  
  srvyr::mutate (formal=factor(case_when((I12==1|I12==2)&(I14==1|I14==2|I14==3|I14==4)~"Formal",
                                         (I12==3|I12==4|I12==14|I12==88888)&(I17==1)~"Formal",
                                         (I12==7|I12==8|I12==9|I12==10|I12==11)&(I16==1)~"Formal",
                                         (I12==3|I12==4|I12==14|I12==88888)&(I17==2)~"Informal",
                                         (I12==7|I12==8|I12==9|I12==10|I12==11)&(I16==2)~"Informal",
                                         I12==13~"Informal",
                                         TRUE ~"Sem classificação"))) %>% 
  
  
  srvyr::group_by(inseg_alimentar, formal) %>% 
  srvyr::summarise(n=survey_total(vartype = "cv"),
                   pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
 
  select(inseg_alimentar, formal, n, n_cv, pct, pct_cv) %>% 
  pivot_wider(names_from = "formal", values_from = c("n", "n_cv", "pct", "pct_cv"))


write_clip(tab_6.1, dec=",")


#7) Escolaridade do chefe 


tab_7 <-  amostra %>%
  
  srvyr::filter(E05==1)  %>%
  
  
  srvyr::mutate(escolaridade_certa2 = factor(case_when(
  escolaridade == 1 ~ "Sem instrução",
  escolaridade == 2 ~ "Fundamental incompleto",
  escolaridade %in% c(3,4) ~ "Fundamental completo",
  escolaridade %in% c(5,6,7) ~ "Médio + Superior completo",
    escolaridade == 8 ~ "Sem classificação"))) %>% 

srvyr::group_by(escolaridade_certa2) %>% 
  srvyr::summarise(n=survey_total(vartype = "cv"),
                   pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
  mutate(territorio="DF") %>% 
  select(territorio, escolaridade_certa2, n, n_cv, pct, pct_cv) %>% 
  pivot_wider(names_from = "escolaridade_certa2", values_from = c("n", "n_cv", "pct", "pct_cv"))


write_clip(tab_7, dec=",")



tab_7 <-  amostra %>%
  
  srvyr::filter(E05==1)  %>%
  
  
  srvyr::mutate(escolaridade_certa2 = factor(case_when(
    escolaridade == 1 ~ "Sem instrução",
    escolaridade == 2 ~ "Fundamental incompleto",
    escolaridade %in% c(3,4) ~ "Fundamental completo",
    escolaridade %in% c(5,6,7) ~ "Médio + Superior completo"
    ))) %>% 
  
  srvyr::group_by(escolaridade_certa2) %>% 
  srvyr::summarise(n=survey_total(vartype = "cv"),
                   pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
  mutate(territorio="DF") %>% 
  select(territorio, escolaridade_certa2, n, n_cv, pct, pct_cv) %>% 
  pivot_wider(names_from = "escolaridade_certa2", values_from = c("n", "n_cv", "pct", "pct_cv"))


write_clip(tab_7, dec=",")

tab_7.1 <-  amostra %>%
  
  srvyr::filter(E05==1)  %>%
  
  
  srvyr::mutate(escolaridade_certa2 = factor(case_when(
    escolaridade == 1 ~ "Sem instrução",
    escolaridade == 2 ~ "Fundamental incompleto",
    escolaridade %in% c(3,4) ~ "Fundamental completo",
    escolaridade %in% c(5,6,7) ~ "Médio + Superior completo"
  ))) %>% 
  
  srvyr::group_by(inseg_alimentar, escolaridade_certa2) %>% 
  srvyr::summarise(n=survey_total(vartype = "cv"),
                   pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
  
  select(inseg_alimentar, escolaridade_certa2, n, n_cv, pct, pct_cv) %>% 
  pivot_wider(names_from = "escolaridade_certa2", values_from = c("n", "n_cv", "pct", "pct_cv"))


write_clip(tab_7.1, dec=",")

