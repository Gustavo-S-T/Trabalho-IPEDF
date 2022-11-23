getwd()
rm(list = ls())


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


pdad_pes<-fread("pdad2021_moradores.csv")
pdad_dom<-fread("pdad2021_domicilios.csv")


glimpse(pdad_2021)

#Testar quando dar errado fazer a amostra

pdad_pes<- read_delim("pdad2021_moradores.csv",";",
                      escape_double = FALSE, locale = locale(decimal_mark = ","),
                      trim_ws = TRUE)
pdad_dom<- read_delim("pdad2021_domicilios.csv",";",
                      escape_double = FALSE, locale = locale(decimal_mark = ","),
                      trim_ws = TRUE)

# nomes das variáveis
names(pdad_dom)  #Consigo ver em qual coluna está a variável 
table(pdad_dom$arranjos) # Tabular coluna
table(pdad_dom$criterio_brasil)
names(pdad_pes)



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
                                      
                                      
                                 #Home Oficce
                                      
                                      
                                      pdad_2021 <- pdad_2021 %>%
                                        mutate( home_office = ifelse (I08 == 56, 1, 0)  )          
                                      
                                      table(pdad_2021$home_office)
                                      table(pdad_2021$densidade,pdad_2021$criterio_brasil)
                                      
                                      
                                      pdad_2021 <- pdad_2021 %>%
                                        mutate(escolaridade_certa = factor(case_when(
                                                                            escolaridade == 1 ~ "Sem instrução",
                                                                            escolaridade == 2 ~ "Fundamental incompleto",
                                                                            escolaridade %in% c(3,4) ~ "Fundamental completo",
                                                                            escolaridade %in% c(5,6) ~ "Médio completo",
                                                                            escolaridade == 7 ~ "Superior Completo",
                                                                            escolaridade == 8 ~ "Sem classificação")))
                                      summary(pdad_2021$renda_domiciliar_pc_r)
# Criando variáveis Retratos crianças-------------------------------------------------------

#Faixa etária crianças 
                                    
pdad_2021 <- pdad_2021 %>% mutate(
 
  
   faixa_etaria_crianças=factor(case_when(idade>=0&idade<=3 ~"0 a 3 anos",
                                         idade>=4&idade<=5 ~"4 e 5 anos",
                                         idade>=6&idade<=11 ~"6 a 11 anos",
                                         TRUE ~"Outras idades")),
                                      
  faixa_etaria_simples=factor(case_when(idade>=0&idade<=6 ~"0 a 6 anos",
                                        idade>=7&idade<=11 ~"7 a 11 anos",
                                         TRUE ~"Outras idades")),
  
  crianças=factor(case_when(idade>=0&idade<=11 ~ "Crianças",
                          TRUE ~"Outras idades")))
                                   
                               
                                      table(pdad_2021$PESO_PRE)                           
                                   

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
                                      


# 1 PERFIL SOCIODEMOGRÁFICO CRIANÇAS ----------------------------------------
                                   
                                   
#VOLUME              
 
tab_1 <- amostra %>% srvyr::mutate(grupo_idade = case_when(idade>=0&idade<=11 ~"Crianças",
                                                                  TRUE ~"Não Crianças")) %>%                                                                                            
        srvyr::group_by(grupo_idade) %>% 
        srvyr::summarise(n=survey_total(vartype = "cv"),
                                pct = survey_mean(vartype = "cv")) %>% mutate(territorio = "DF") %>% 
        select(territorio, grupo_idade,n, pct, n_cv, pct_cv) %>% pivot_wider(names_from = "grupo_idade",
                                                               values_from = c("n", "n_cv", "pct", "pct_cv"))
                                   
                                
                                   
        write.table(tab_1,file='Tab_1.Volume.csv',
                   row.names = F, sep = ";", dec= ",", fileEncoding = "latin1")
   
        write_clip(tab_1, dec=",")
        
        
        
                                                            
tab_2 <- amostra %>% srvyr::mutate(grupo_idade = case_when(idade>=0&idade<=11 ~ "Crianças",
                                                                        TRUE ~"Não Crianças")) %>% 
                                     srvyr::group_by(RA,grupo_idade) %>% 
                                     srvyr::summarise(n=survey_total(vartype = "cv"),
                                                      pct = survey_mean(vartype = "cv")) %>% rename(territorio = RA) %>% 
                                     select(territorio, grupo_idade,n, pct, n_cv, pct_cv) %>%
                                     pivot_wider(names_from = "grupo_idade", values_from = c("n", "n_cv", "pct", "pct_cv"))

        write_clip(tab_2, dec=",")



        
  tab_3 <- amostra %>% srvyr::mutate(grupo_idade = case_when(idade>=0&idade<=11 ~"Crianças",
                                                                   TRUE ~"Não Crianças")) %>% 
    srvyr::group_by(criterio_brasil,grupo_idade) %>% 
    srvyr::summarise(n=survey_total(vartype = "cv"),
                     pct = survey_mean(vartype = "cv")) %>%
    select(criterio_brasil, grupo_idade,n, pct, n_cv, pct_cv) %>%
    pivot_wider(names_from = "grupo_idade", values_from = c("n", "n_cv", "pct", "pct_cv")) %>% na.omit
        
  write_clip(tab_3, dec=",")        
  
  
  
#DISTRUBUIÇÃO POR FAIXA ETÁRIA
  
  tab_4 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>% 
    srvyr::group_by(faixa_etaria_crianças) %>% 
    srvyr::summarise(n=survey_total(vartype = "cv"),
                     pct = survey_mean(vartype = "cv")) %>% mutate(territorio = "DF") %>% 
    select(territorio, faixa_etaria_crianças,n, pct, n_cv,pct_cv) %>% 
    pivot_wider(names_from = "faixa_etaria_crianças",
                values_from = c("n", "n_cv","pct","pct_cv"))
  

  
  
  tab_4.1 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>% 
    srvyr::group_by(RA,faixa_etaria_crianças) %>% 
    srvyr::summarise(n=survey_total(vartype = "cv"),
                     pct = survey_mean(vartype = "cv")) %>% rename(territorio = RA) %>% 
    select(territorio, faixa_etaria_crianças,n, pct, n_cv,pct_cv) %>% 
    pivot_wider(names_from = "faixa_etaria_crianças",
                values_from = c("n", "n_cv","pct","pct_cv"))
 
  tab_4.2 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>% 
    srvyr::group_by(criterio_brasil,faixa_etaria_crianças) %>% 
    srvyr::summarise(n=survey_total(vartype = "cv"),
                     pct = survey_mean(vartype = "cv")) %>% rename(territorio = criterio_brasil) %>% 
    select(territorio, faixa_etaria_crianças,n, pct, n_cv,pct_cv) %>% 
    pivot_wider(names_from = "faixa_etaria_crianças",
                values_from = c("n", "n_cv","pct","pct_cv")) %>% na.omit
  
  tab_idade <- rbind(tab_4,tab_4.1,tab_4.2)
  
  write_clip(tab_idade, dec=",")  
  
  
  
  
  tab_4.3 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>% 
    srvyr::group_by(faixa_etaria_simples) %>% 
    srvyr::summarise(n=survey_total(vartype = "cv"),
                     pct = survey_mean(vartype = "cv")) %>% mutate(territorio = "DF") %>% 
    select(territorio, faixa_etaria_simples,n, pct, n_cv,pct_cv) %>% 
    pivot_wider(names_from = "faixa_etaria_simples",
                values_from = c("n", "n_cv","pct","pct_cv"))
  
  
  
  
  tab_4.4 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>% 
    srvyr::group_by(RA,faixa_etaria_simples) %>% 
    srvyr::summarise(n=survey_total(vartype = "cv"),
                     pct = survey_mean(vartype = "cv")) %>% rename(territorio = RA) %>% 
    select(territorio, faixa_etaria_simples,n, pct, n_cv,pct_cv) %>% 
    pivot_wider(names_from = "faixa_etaria_simples",
                values_from = c("n", "n_cv","pct","pct_cv"))
  
  tab_4.5 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>% 
    srvyr::group_by(criterio_brasil,faixa_etaria_simples) %>% 
    srvyr::summarise(n=survey_total(vartype = "cv"),
                     pct = survey_mean(vartype = "cv")) %>% rename(territorio = criterio_brasil) %>% 
    select(territorio, faixa_etaria_simples,n, pct, n_cv,pct_cv) %>% 
    pivot_wider(names_from = "faixa_etaria_simples",
                values_from = c("n", "n_cv","pct","pct_cv")) %>% na.omit
  
  tab_idade2 <- rbind(tab_4.3,tab_4.4,tab_4.5)
  
  write_clip(tab_idade2, dec=",")  
  
  
  
  
#DISTRIBUIÇÃO POR SEXO
  
  tab_5 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>% 
    srvyr::group_by(sexo) %>% 
    srvyr::summarise(n=survey_total(vartype = "cv"),
                     pct = survey_mean(vartype = "cv")) %>% mutate(territorio = "DF") %>% 
    select(territorio, sexo,n, pct, n_cv, pct_cv) %>%
    pivot_wider(names_from = "sexo",values_from = c("n", "n_cv","pct", "pct_cv"))
  
  
  
   tab_5 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>% 
    srvyr::group_by(sexo) %>% 
    srvyr::summarise(n=survey_total(vartype = "cv"),
                     pct = survey_mean(vartype = "cv")) %>% mutate(territorio = "DF") %>% 
    select(territorio, sexo,n, pct, n_cv, pct_cv) %>%
    pivot_wider(names_from = "sexo",values_from = c("n", "n_cv","pct", "pct_cv"))
  
  
  tab_5.1 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>% 
    srvyr::group_by(RA,sexo) %>% 
    srvyr::summarise(n=survey_total(vartype = "cv"),
                     pct = survey_mean(vartype = "cv")) %>%  rename(territorio = RA) %>% 
    select(territorio, sexo,n, pct, n_cv, pct_cv) %>%
    pivot_wider(names_from = "sexo",values_from = c("n", "n_cv","pct", "pct_cv"))
  
  tab_5.2 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>% 
    srvyr::group_by(criterio_brasil,sexo) %>% 
    srvyr::summarise(n=survey_total(vartype = "cv"),
                     pct = survey_mean(vartype = "cv")) %>%  rename(territorio = criterio_brasil) %>% 
    select(territorio, sexo,n, pct, n_cv, pct_cv) %>%
    pivot_wider(names_from = "sexo",values_from = c("n", "n_cv","pct", "pct_cv"))
  
  tab_sexo <- rbind(tab_5, tab_5.1, tab_5.2)
  
 write_clip(tab_sexo, dec=",")
  
  
  # Distribuição por raça/cor
  tab_6 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>% 
    srvyr::group_by(raca_cor) %>% 
    srvyr::summarise(n=survey_total(vartype = "cv"),
                     pct = survey_mean(vartype = "cv")) %>% mutate(territorio = "DF") %>% 
    select(territorio, raca_cor,n, pct, n_cv, pct_cv) %>%
    pivot_wider(names_from = "raca_cor",values_from = c("n", "n_cv","pct","pct_cv"))
  
  tab_6.1 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>% 
    srvyr::group_by(RA,raca_cor) %>% 
    srvyr::summarise(n=survey_total(vartype = "cv"),
                     pct = survey_mean(vartype = "cv")) %>%  rename(territorio = RA) %>% 
    select(territorio, raca_cor,n, pct, n_cv, pct_cv) %>%
    pivot_wider(names_from = "raca_cor",values_from = c("n", "n_cv","pct","pct_cv"))
  
  tab_6.2 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>% 
    srvyr::group_by(criterio_brasil,raca_cor) %>% 
    srvyr::summarise(n=survey_total(vartype = "cv"),
                     pct = survey_mean(vartype = "cv")) %>%  rename(territorio = criterio_brasil) %>% 
    select(territorio, raca_cor,n, pct, n_cv, pct_cv) %>%
    pivot_wider(names_from = "raca_cor",values_from = c("n", "n_cv","pct","pct_cv")) %>% na.omit
  
  
  tab_raca <- rbind(tab_6, tab_6.1, tab_6.2)
  
  tabela_perfil1 <- tab_sexo%>%
    left_join(tab_raca) 
  
  write_clip(tabela_perfil1, dec=",")
  
  
  

  # DEFICIÊNCIA
  
  # para o DF como um todo
  tab_12 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>% 
    srvyr::mutate(def_visual = case_when(E09 %in% c(1, 2) ~ 1,
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
    mutate(possui_def = factor(possui_def, labels = c("Não","Sim"))
           
    ) %>% srvyr::group_by(possui_def) %>% 
    srvyr::summarise(n=survey_total(vartype = "cv"),
                     pct=survey_mean(vartype = "cv")) %>% filter(possui_def=="Sim") %>% select(-possui_def) %>% 
    mutate(territorio="DF") %>% select(territorio, everything())
  
  write_clip(tab_12, dec=",")   
  
  
  
  # por RA
  tab_12.1 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>% 
    srvyr::mutate(def_visual = case_when(E09 %in% c(1, 2) ~ 1,
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
    mutate(possui_def = factor(possui_def, labels = c("Não","Sim"))
           
    ) %>% srvyr::group_by(RA,possui_def) %>% 
    srvyr::summarise(n=survey_total(vartype = "cv"),
                     pct=survey_mean(vartype = "cv")) %>% filter(possui_def=="Sim") %>% select(-possui_def) %>% 
    rename(territorio=RA) %>% select(territorio, everything())
  
  # Por classe - Critério Brasil
  tab_12.2 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>% 
    srvyr::mutate(def_visual = case_when(E09 %in% c(1, 2) ~ 1,
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
    mutate(possui_def = factor(possui_def, labels = c("Não","Sim"))
           
    ) %>% srvyr::group_by(criterio_brasil,possui_def) %>% 
    srvyr::summarise(n=survey_total(vartype = "cv"),
                     pct=survey_mean(vartype = "cv")) %>% filter(possui_def=="Sim") %>% select(-possui_def) %>% 
    rename(territorio=criterio_brasil) %>% select(territorio, everything()) %>% na.omit
  
  tab_alguma_deficiencia <- rbind(tab_12, tab_12.1, tab_12.2)
  write_clip(tab_alguma_deficiencia, dec=",")
  
  
  #Por tipo de deficiência 
  
  tab_tipo_deficiencia <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>% 
    srvyr::group_by(tipo_defic) %>% 
    srvyr::summarise(n=survey_total(vartype = "cv"),
                     pct = survey_mean(vartype = "cv")) %>% mutate(territorio = "DF") %>% 
    select(tipo_defic,n, pct, n_cv, pct_cv) 
   
  write_clip(tab_tipo_deficiencia, dec=",")
  

  
#ARRANJOS 
  

   tab_9 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>% 
    srvyr::mutate(arranjo_familiar = factor(case_when(arranjos==1~"Unipessoal",
                                                      arranjos==2~"Monoparental feminino",
                                                      arranjos%in%c(3,4,5)~"Casal com filhos",
                                                      arranjos==6~"Casal sem filhos",
                                                      arranjos==7~"Outro perfil")))%>% 

   srvyr::group_by(arranjo_familiar) %>% 
    srvyr::summarise(n=survey_total(vartype = "cv"),
                     pct=survey_mean(vartype = "cv")) %>% mutate(territorio = "DF") %>% 
     
    select(territorio, arranjo_familiar,n, pct, n_cv, pct_cv) %>% 
    pivot_wider(names_from = "arranjo_familiar",values_from = c("n", "n_cv","pct","pct_cv")) 
  

   write_clip(tab_9, dec=",")
   
   
   # por RA
   
   
   tab_9.1 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>% 
     srvyr::mutate(arranjo_familiar = factor(case_when(arranjos==1~"Unipessoal",
                                                       arranjos==2~"Monoparental feminino",
                                                       arranjos%in%c(3,4,5)~"Casal com filhos",
                                                       arranjos==6~"Casal sem filhos",
                                                       arranjos==7~"Outro perfil")))%>% 
     
     
     srvyr::group_by(RA,arranjo_familiar) %>% 
     srvyr::summarise(n=survey_total(vartype = "cv"),
                      pct=survey_mean(vartype = "cv")) %>%   
     select(RA, arranjo_familiar,n, pct, n_cv, pct_cv) %>%
     pivot_wider(names_from = "arranjo_familiar",values_from = c("n", "n_cv","pct","pct_cv")) 
   
   
   write_clip(tab_9.1, dec=",") 
 
   

   
   
   # por Critério Brasil
   
   
   tab_9.2 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>% 
     srvyr::mutate(arranjo_familiar = factor(case_when(arranjos==1~"Unipessoal",
                                                       arranjos==2~"Monoparental feminino",
                                                       arranjos%in%c(3,4,5)~"Casal com filhos",
                                                       arranjos==6~"Casal sem filhos",
                                                       arranjos==7~"Outro perfil")))%>% 
     
    
     srvyr::group_by(criterio_brasil,arranjo_familiar) %>% 
     srvyr::summarise(n=survey_total(vartype = "cv"),
                      pct=survey_mean(vartype = "cv")) %>% 
     select(criterio_brasil, arranjo_familiar,n, pct, n_cv, pct_cv) %>%
     pivot_wider(names_from = "arranjo_familiar",values_from = c("n", "n_cv","pct","pct_cv"))  
   
   
   write_clip(tab_9.2, dec=",")
   
   
   
   
# 5 EDUCAÇÃO ----------------------------------------
   
   #FREQUÊNCIA 

   # para o DF como um todo
   tab_15 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>% 
     srvyr::mutate(frequenta_escola = case_when(H02 %in% c(1,2)~1,
                                                H02 %in% c(3,4)~2)) %>% 
     mutate(frequenta_escola=factor(frequenta_escola, labels = c("Sim", "Não"))) %>% 
     srvyr::group_by(frequenta_escola) %>% 
     srvyr::summarise(n=survey_total(vartype = "cv"),
                      pct=survey_mean(vartype = "cv")) %>% filter(frequenta_escola=="Sim") %>% 
     select(-frequenta_escola) %>% mutate(territorio="DF") %>% select(territorio, everything()) 
   
   
   write_clip(tab_15, dec=",")
   
   
   # por RA
   tab_15.1 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>% 
     srvyr::mutate(frequenta_escola = case_when(H02 %in% c(1,2)~1,
                                                H02 %in% c(3,4)~2)) %>% 
     mutate(frequenta_escola=factor(frequenta_escola, labels = c("Sim", "Não"))) %>% 
     srvyr::group_by(RA,frequenta_escola) %>% 
     srvyr::summarise(n=survey_total(vartype = "cv"),
                      pct=survey_mean(vartype = "cv")) %>% filter(frequenta_escola=="Sim") %>% 
     select(-frequenta_escola) %>% rename(territorio=RA) %>% select(territorio, everything())
   
   write_clip(tab_15.1, dec=",")
   
   
   
   # por classe critério brasil
   tab_15.2 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>% 
     srvyr::mutate(frequenta_escola = case_when(H02 %in% c(1,2)~1,
                                                H02 %in% c(3,4)~2)) %>% 
     mutate(frequenta_escola=factor(frequenta_escola, labels = c("Sim", "Não"))) %>% 
     srvyr::group_by(criterio_brasil,frequenta_escola) %>% 
     srvyr::summarise(n=survey_total(vartype = "cv"),
                      pct=survey_mean(vartype = "cv")) %>% filter(frequenta_escola=="Sim") %>% 
     select(-frequenta_escola) %>% rename(territorio=criterio_brasil) %>% select(territorio, everything()) 
   
   write_clip(tab_15.2, dec=",")
   
   tab_15.3<- amostra %>% srvyr::filter(idade>=0&idade<=11) %>% 
     srvyr::mutate(frequenta_escola = case_when(H02 %in% c(1,2)~1,
                                                H02 %in% c(3,4)~2)) %>% 
     mutate(frequenta_escola=factor(frequenta_escola, labels = c("Sim", "Não"))) %>% 
     srvyr::group_by(gruposrenda_pdad,frequenta_escola) %>% 
     srvyr::summarise(n=survey_total(vartype = "cv"),
                      pct=survey_mean(vartype = "cv")) %>% filter(frequenta_escola=="Sim") %>% 
     select(-frequenta_escola)  %>% select(gruposrenda_pdad, everything()) %>% na.omit
   
   
   write_clip(tab_15.3, dec=",")
   
   
   # olhar frequência na escola/creche por faixa etária
   
   tab_16 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>% 
     srvyr::mutate(frequenta_escola = case_when(H02 %in% c(1,2)~1,
                                                H02 %in% c(3,4)~2)) %>% 
     mutate(frequenta_escola=factor(frequenta_escola, labels = c("Sim", "Não"))) %>% 
     srvyr::group_by(faixa_etaria_crianças, frequenta_escola) %>% 
     srvyr::summarise(n=survey_total(vartype = "cv"),
                      pct=survey_mean(vartype = "cv")) %>% filter(frequenta_escola=="Sim") %>% mutate(territorio="DF") %>% 
     select(territorio, faixa_etaria_crianças,n, pct, n_cv, pct_cv) %>%
     pivot_wider(names_from = "faixa_etaria_crianças", values_from = c("n", "n_cv", "pct", "pct_cv"))
   
   tab_16.1 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>% 
     srvyr::mutate(frequenta_escola = case_when(H02 %in% c(1,2)~1,
                                                H02 %in% c(3,4)~2)) %>% 
     mutate(frequenta_escola=factor(frequenta_escola, labels = c("Sim", "Não"))) %>% 
     srvyr::group_by(RA, faixa_etaria_crianças, frequenta_escola) %>% 
     srvyr::summarise(n=survey_total(vartype = "cv"),
                      pct=survey_mean(vartype = "cv")) %>% filter(frequenta_escola=="Sim") %>% rename(territorio=RA) %>% 
     select(territorio, faixa_etaria_crianças,n, pct, n_cv, pct_cv) %>%
     pivot_wider(names_from = "faixa_etaria_crianças", values_from = c("n", "n_cv", "pct", "pct_cv")) 
   
   tab_16.2 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>% 
     srvyr::mutate(frequenta_escola = case_when(H02 %in% c(1,2)~1,
                                                H02 %in% c(3,4)~2)) %>% 
     mutate(frequenta_escola=factor(frequenta_escola, labels = c("Sim", "Não"))) %>% 
     srvyr::group_by(criterio_brasil, faixa_etaria_crianças, frequenta_escola) %>% 
     srvyr::summarise(n=survey_total(vartype = "cv"),
                      pct=survey_mean(vartype = "cv")) %>% filter(frequenta_escola=="Sim") %>%
     rename(territorio=criterio_brasil) %>% 
     select(territorio, faixa_etaria_crianças,n, pct, n_cv, pct_cv) %>%
     pivot_wider(names_from = "faixa_etaria_crianças", values_from = c("n", "n_cv", "pct", "pct_cv")) %>% na.omit
   
   tab_freq_escola_idade <- rbind(tab_16, tab_16.1, tab_16.2)
   write_clip(tab_freq_escola_idade, dec=",")
   
   
  
   
   # DEPENDÊNCIA ADMINISTRATIVA 
   
   # Para o DF como um todo
   tab_17 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>% 
     srvyr::filter(H02==1|H02==2) %>% 
     srvyr::mutate(dependencia_escola = case_when(H02==1 ~ "Pública",
                                                  H02==2~"Privada")) %>% 
     srvyr::group_by(dependencia_escola) %>% 
     srvyr::summarise(n=survey_total(vartype = "cv"),
                      pct=survey_mean(vartype = "cv")) %>% mutate(territorio="DF") %>% 
     select(territorio, dependencia_escola, n, n_cv, pct, pct_cv) %>% 
     pivot_wider(names_from = "dependencia_escola", values_from = c("n", "n_cv", "pct", "pct_cv"))
   
   # Por RA
   tab_17.1 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>% 
     srvyr::filter(H02==1|H02==2) %>% 
     srvyr::mutate(dependencia_escola = case_when(H02==1 ~ "Pública",
                                                  H02==2~"Privada")) %>% 
     srvyr::group_by(RA,dependencia_escola) %>% 
     srvyr::summarise(n=survey_total(vartype = "cv"),
                      pct=survey_mean(vartype = "cv")) %>% rename(territorio=RA) %>% 
     select(territorio, dependencia_escola, n, n_cv, pct, pct_cv) %>% 
     pivot_wider(names_from = "dependencia_escola", values_from = c("n", "n_cv", "pct", "pct_cv"))
   
   # Por classe - Critério Brasil
   tab_17.2 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>% 
     srvyr::filter(H02==1|H02==2) %>% 
     srvyr::mutate(dependencia_escola = case_when(H02==1 ~ "Pública",
                                                  H02==2~"Privada")) %>% 
     srvyr::group_by(criterio_brasil,dependencia_escola) %>% 
     srvyr::summarise(n=survey_total(vartype = "cv"),
                      pct=survey_mean(vartype = "cv")) %>% rename(territorio=criterio_brasil) %>% 
     select(territorio, dependencia_escola, n, n_cv, pct, pct_cv) %>% 
     pivot_wider(names_from = "dependencia_escola", values_from = c("n", "n_cv", "pct", "pct_cv")) %>% na.omit
   
  
   
    # faixa etária (0-3, 4-5 e 6-11)
   tab_17.3 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>% 
     srvyr::filter(H02==1|H02==2) %>% 
     srvyr::mutate(dependencia_escola = case_when(H02==1 ~ "Pública",
                                                  H02==2~"Privada")) %>% 
     srvyr::group_by(faixa_etaria_crianças,dependencia_escola) %>% 
     srvyr::summarise(n=survey_total(vartype = "cv"),
                      pct=survey_mean(vartype = "cv")) %>% rename(territorio=faixa_etaria_crianças) %>% 
     select(territorio, dependencia_escola, n, n_cv, pct, pct_cv) %>% 
     pivot_wider(names_from = "dependencia_escola", values_from = c("n", "n_cv", "pct", "pct_cv")) %>% na.omit
   
   
   
   # grupo de renda PDAD
   tab_17.4 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>% 
     srvyr::filter(H02==1|H02==2) %>% 
     srvyr::mutate(dependencia_escola = case_when(H02==1 ~ "Pública",
                                                  H02==2~"Privada")) %>% 
     srvyr::group_by(gruposrenda_pdad,dependencia_escola) %>% 
     srvyr::summarise(n=survey_total(vartype = "cv"),
                      pct=survey_mean(vartype = "cv")) %>% rename(territorio=gruposrenda_pdad) %>% 
     select(territorio, dependencia_escola, n, n_cv, pct, pct_cv) %>%                                  
     pivot_wider(names_from = "dependencia_escola", values_from = c("n", "n_cv", "pct", "pct_cv")) %>% na.omit
   
   
   tab_dep_adm <- rbind(tab_17, tab_17.1,tab_17.2, tab_17.3, tab_17.4) %>% write_clip(dec=",")
   
  
   # olhar por faixa etária e por classe do critério brasil - UMA ANÁLISE INTERESSANTE DE SER FEITA!
   tab_17.5 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>% 
     srvyr::filter(H02==1|H02==2) %>% 
     srvyr::mutate(dependencia_escola = case_when(H02==1 ~ "Pública",
                                                  H02==2~"Privada")) %>% 
     srvyr::group_by(criterio_brasil,faixa_etaria_crianças,dependencia_escola) %>% 
     srvyr::summarise(n=survey_total(vartype = "cv"),
                      pct=survey_mean(vartype = "cv"))
    write_clip(tab_17.5,dec=',')
   
    
    #MODALIDADE DE ENSINO 
    
    
    # Para o DF como um todo
    tab_25 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>% 
      srvyr::filter(H02==1|H02==2) %>% 
      srvyr::mutate(modalidade_ensino = case_when(H02_1==1 ~ "Presencial",
                                                  H02_1==2~"EaD",
                                                  H02_1==3~"Híbrida")) %>% 
      srvyr::group_by(modalidade_ensino) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
      mutate(territorio="DF") %>% 
      select(territorio, modalidade_ensino, n, n_cv, pct, pct_cv) %>% 
      pivot_wider(names_from = "modalidade_ensino", values_from = c("n", "n_cv", "pct", "pct_cv"))
    
    
    # RA
    tab_25.1 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>% 
      srvyr::filter(H02==1|H02==2) %>% 
      srvyr::mutate(modalidade_ensino = case_when(H02_1==1 ~ "Presencial",
                                                  H02_1==2~"EaD",
                                                  H02_1==3~"Híbrida")) %>% 
      srvyr::group_by(RA,modalidade_ensino) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
      rename(territorio=RA) %>% 
      select(territorio, modalidade_ensino, n, n_cv, pct, pct_cv) %>% 
      pivot_wider(names_from = "modalidade_ensino", values_from = c("n", "n_cv", "pct", "pct_cv"))
    
    # Criterio brasil
    tab_25.2 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>% 
      srvyr::filter(H02==1|H02==2) %>% 
      srvyr::mutate(modalidade_ensino = case_when(H02_1==1 ~ "Presencial",
                                                  H02_1==2~"EaD",
                                                  H02_1==3~"Híbrida")) %>% 
      srvyr::group_by(criterio_brasil,modalidade_ensino) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
      rename(territorio=criterio_brasil) %>% 
      select(territorio, modalidade_ensino, n, n_cv, pct, pct_cv) %>% 
      pivot_wider(names_from = "modalidade_ensino", values_from = c("n", "n_cv", "pct", "pct_cv"))
    
    # Faixa de idade
    tab_25.3 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>% 
      srvyr::filter(H02==1|H02==2) %>% 
      srvyr::mutate(modalidade_ensino = case_when(H02_1==1 ~ "Presencial",
                                                  H02_1==2~"EaD",
                                                  H02_1==3~"Híbrida")) %>% 
      srvyr::group_by(faixa_etaria_crianças,modalidade_ensino) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
      rename(territorio=faixa_etaria_crianças) %>% 
      select(territorio, modalidade_ensino, n, n_cv, pct, pct_cv) %>% 
      pivot_wider(names_from = "modalidade_ensino", values_from = c("n", "n_cv", "pct", "pct_cv"))
    
    # Grupo PDAD
    
    tab_25.4 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>% 
      srvyr::filter(H02==1|H02==2) %>% 
      srvyr::mutate(modalidade_ensino = case_when(H02_1==1 ~ "Presencial",
                                                  H02_1==2~"EaD",
                                                  H02_1==3~"Híbrida")) %>% 
      srvyr::group_by(gruposrenda_pdad,modalidade_ensino) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
      rename(territorio=gruposrenda_pdad) %>% 
      select(territorio, modalidade_ensino, n, n_cv, pct, pct_cv) %>% 
      pivot_wider(names_from = "modalidade_ensino", values_from = c("n", "n_cv", "pct", "pct_cv"))
    
    
    tab_mod_ens <- rbind(tab_25, tab_25.1,tab_25.2, tab_25.3, tab_25.4) %>% write_clip(dec=",")
    
    
    #TURNO DE ESTUDO 
    
    
    tab_26 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>% 
      srvyr::filter(H02==1|H02==2) %>% 
      srvyr::mutate(turno_ensino = case_when(H03==1 ~ "Matutino",
                                                  H03==2~"Vespertino",
                                                  H03==3~"Noturno",
                                                  H03==4 ~"Integral")) %>% 
      
      srvyr::group_by(turno_ensino) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
      mutate(territorio="DF") %>% 
      select(territorio, turno_ensino, n, n_cv, pct, pct_cv) %>% 
      pivot_wider(names_from = "turno_ensino", values_from = c("n", "n_cv", "pct", "pct_cv"))
    
    write_clip(tab_26, dec=",")
    
    
    #Critério Brasil
    
    
    
    tab_26.1 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>% 
      srvyr::filter(H02==1|H02==2) %>% 
      srvyr::mutate(turno_ensino = case_when(H03==1 ~ "Matutino",
                                             H03==2~"Vespertino",
                                             H03==3~"Noturno",
                                             H03==4 ~"Integral")) %>% 
      
      srvyr::group_by(criterio_brasil,turno_ensino) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
      rename(territorio=criterio_brasil) %>% 
      select(territorio, turno_ensino, n, n_cv, pct, pct_cv) %>% 
      pivot_wider(names_from = "turno_ensino", values_from = c("n", "n_cv", "pct", "pct_cv"))
    
    write_clip(tab_26.1, dec=",")

    
    
    
    #Faixa etária
    
    
    
    tab_26.2 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>% 
      srvyr::filter(H02==1|H02==2) %>% 
      srvyr::mutate(turno_ensino = case_when(H03==1 ~ "Matutino",
                                             H03==2~"Vespertino",
                                             H03==3~"Noturno",
                                             H03==4 ~"Integral")) %>% 
      
      srvyr::group_by(faixa_etaria_crianças,turno_ensino) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
      rename(territorio=faixa_etaria_crianças) %>% 
      select(territorio, turno_ensino, n, n_cv, pct, pct_cv) %>% 
      pivot_wider(names_from = "turno_ensino", values_from = c("n", "n_cv", "pct", "pct_cv"))
    
    write_clip(tab_26.2, dec=",")
    
    
    

    # Grupo PDAD
    
    tab_26.3 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>% 
      srvyr::filter(H02==1|H02==2) %>% 
      srvyr::mutate(turno_ensino = case_when(H03==1 ~ "Matutino",
                                             H03==2~"Vespertino",
                                             H03==3~"Noturno",
                                             H03==4 ~"Integral")) %>% 
      
      srvyr::group_by(gruposrenda_pdad,turno_ensino) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
      rename(territorio=gruposrenda_pdad) %>% 
      select(territorio, turno_ensino, n, n_cv, pct, pct_cv) %>% 
      pivot_wider(names_from = "turno_ensino", values_from = c("n", "n_cv", "pct", "pct_cv"))
    
    write_clip(tab_26.3, dec=",")
    
    #ESTUDA FORA DA RA 
    
    tab_27 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>%
    
     srvyr::filter(H02==1|H02==2) %>% 
      
     srvyr::mutate(fora_RA = case_when(H04 == A01ra~ "Estuda na RA",
                                       H04!= A01ra~"Estuda fora da RA",
                                            )) %>% 
      srvyr::group_by(fora_RA) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
      mutate(territorio="DF") %>% 
      select(territorio, fora_RA, n, n_cv, pct, pct_cv) %>% 
      pivot_wider(names_from = "fora_RA", values_from = c("n", "n_cv", "pct", "pct_cv"))
    
    write_clip(tab_27, dec=",")
    
    #RA
    
    tab_27.1 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>%
      
      srvyr::filter(H02==1|H02==2) %>% 
      
      srvyr::mutate(fora_RA = case_when(H04 == A01ra~ "Estuda na RA",
                                        H04!= A01ra~"Estuda fora da RA",
                                                               )) %>% 
      srvyr::group_by(RA,fora_RA) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
      rename(territorio=RA) %>% 
      select(territorio, fora_RA, n, n_cv, pct, pct_cv) %>% 
      pivot_wider(names_from = "fora_RA", values_from = c("n", "n_cv", "pct", "pct_cv"))
    
    write_clip(tab_27.1, dec=",")
    
    
    # Criterio Brasil
    
    tab_27.2 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>%
      
      srvyr::filter(H02==1|H02==2) %>% 
      
      srvyr::mutate(fora_RA = case_when(H04 == A01ra~ "Estuda na RA",
                                        H04!= A01ra~"Estuda fora da RA",
      )) %>% 
      srvyr::group_by(criterio_brasil,fora_RA) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
      rename(territorio=criterio_brasil) %>% 
      select(territorio, fora_RA, n, n_cv, pct, pct_cv) %>% 
      pivot_wider(names_from = "fora_RA", values_from = c("n", "n_cv", "pct", "pct_cv"))
    
    write_clip(tab_27.2, dec=",")
    
    # Faixa etária 
    
    
    tab_27.3 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>%
      
      srvyr::filter(H02==1|H02==2) %>% 
      
      srvyr::mutate(fora_RA = case_when(H04 == A01ra~ "Estuda na RA",
                                        H04!= A01ra~"Estuda fora da RA",
      )) %>% 
      srvyr::group_by(faixa_etaria_crianças,fora_RA) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
      rename(territorio=faixa_etaria_crianças) %>% 
      select(territorio, fora_RA, n, n_cv, pct, pct_cv) %>% 
      pivot_wider(names_from = "fora_RA", values_from = c("n", "n_cv", "pct", "pct_cv"))
    
    write_clip(tab_27.3, dec=",")            
    
    
    
    
    # Grupos PDAD 
    
        tab_27.4 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>%
      
      srvyr::filter(H02==1|H02==2) %>% 
      
      srvyr::mutate(fora_RA = case_when(H04 == A01ra~ "Estuda na RA",
                                        H04!= A01ra~"Estuda fora da RA",
      )) %>% 
      srvyr::group_by(gruposrenda_pdad,fora_RA) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
      rename(territorio=gruposrenda_pdad) %>% 
      select(territorio, fora_RA, n, n_cv, pct, pct_cv) %>% 
      pivot_wider(names_from = "fora_RA", values_from = c("n", "n_cv", "pct", "pct_cv"))
    
    write_clip(tab_27.4, dec=",")   
    
    
    
  #TIPO DE TRANSPORTE   
    
  
    tab_28 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>%
      
      srvyr::filter(H02==1|H02==2) %>% 
      
      srvyr::mutate(transporte = case_when(H05_10 == 1~ "Ônibus",
                                        H05_10 == 2~ "Escolar Público",
                                        H05_10 == 3~ "Escolar Privado",
                                        H05_10 == 4~ "Privado (Empresa de aplicativo, Taxi etc)",
                                        H05_10 == 5~ "Automóvel",
                                        H05_10 == 6~ "Metrô",
                                        H05_10 == 7~ "Motocicleta",
                                        H05_10 == 8~ "A pé",
                                        
                                        
      )) %>% 
      srvyr::group_by(transporte) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
      mutate(territorio="DF") %>% 
      select(territorio, transporte, n, n_cv, pct, pct_cv) %>% 
      pivot_wider(names_from = "transporte", values_from = c("n", "n_cv", "pct", "pct_cv"))
    
    write_clip(tab_28, dec=",")
    
    
  # Por RA
    
    tab_28.1 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>%
      
      srvyr::filter(H02==1|H02==2) %>% 
      
      srvyr::mutate(transporte = case_when(H05_10 == 1~ "Ônibus",
                                           H05_10 == 2~ "Escolar Público",
                                           H05_10 == 3~ "Escolar Privado",
                                           H05_10 == 4~ "Privado (Empresa de aplicativo, Taxi etc)",
                                           H05_10 == 5~ "Automóvel",
                                           H05_10 == 6~ "Metrô",
                                           H05_10 == 7~ "Motocicleta",
                                           H05_10 == 8~ "A pé",
                                           
                                           
      )) %>% 
    
    srvyr::group_by(RA,transporte) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
      rename(territorio=RA) %>% 
      select(territorio, transporte, n, n_cv, pct, pct_cv) %>% 
      pivot_wider(names_from = "transporte", values_from = c("n", "n_cv", "pct", "pct_cv")) 
    write_clip(tab_28.1, dec=",")
    
    
    # Critério Brasil 
    
    
    tab_28.2 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>%
      
      srvyr::filter(H02==1|H02==2) %>% 
      
      srvyr::mutate(transporte = case_when(H05_10 == 1~ "Ônibus",
                                           H05_10 == 2~ "Escolar Público",
                                           H05_10 == 3~ "Escolar Privado",
                                           H05_10 == 4~ "Privado (Empresa de aplicativo, Taxi etc)",
                                           H05_10 == 5~ "Automóvel",
                                           H05_10 == 6~ "Metrô",
                                           H05_10 == 7~ "Motocicleta",
                                           H05_10 == 8~ "A pé",
                                           
                                           
      )) %>% 
      
      srvyr::group_by(criterio_brasil,transporte) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
      
      select(criterio_brasil, transporte, n, n_cv, pct, pct_cv) %>% 
      pivot_wider(names_from = "transporte", values_from = c("n", "n_cv", "pct", "pct_cv")) 
    write_clip(tab_28.2, dec=",")
    
    
    # Faixa etária
    
    
    tab_28.3 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>%
      
      srvyr::filter(H02==1|H02==2) %>% 
      
      srvyr::mutate(transporte = case_when(H05_10 == 1~ "Ônibus",
                                           H05_10 == 2~ "Escolar Público",
                                           H05_10 == 3~ "Escolar Privado",
                                           H05_10 == 4~ "Privado (Empresa de aplicativo, Taxi etc)",
                                           H05_10 == 5~ "Automóvel",
                                           H05_10 == 6~ "Metrô",
                                           H05_10 == 7~ "Motocicleta",
                                           H05_10 == 8~ "A pé",
                                           
                                           
      )) %>% 
      
      srvyr::group_by(faixa_etaria_crianças,transporte) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
      
      select(faixa_etaria_crianças, transporte, n, n_cv, pct, pct_cv) %>% 
      pivot_wider(names_from = "transporte", values_from = c("n", "n_cv", "pct", "pct_cv")) 
    write_clip(tab_28.3, dec=",")
    
    
    
    
    # OUTROS CURSOS 
    
 #Língua estangeira 
    
    
    tab_29 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>%
    
        srvyr::filter(H02==1|H02==2) %>% 
      
      
      
      
      srvyr::mutate(lingua_est = case_when(H14_4 == 1~ "Cursa Língua estrangeira",
                                           H14_4 == 2~ " Não Cursa Língua estrangeira",
                                           H14_4 == 88888~ "Não sabe",
                                           
      )) %>% 
      srvyr::group_by(lingua_est) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
      mutate(territorio="DF") %>% 
      select(territorio, lingua_est, n, n_cv, pct, pct_cv) %>% 
      pivot_wider(names_from = "lingua_est", values_from = c("n", "n_cv", "pct", "pct_cv"))
    
    write_clip(tab_29, dec=",")
    
    
   # por classe
    
    tab_29.1 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>%
      
      srvyr::filter(H02==1|H02==2) %>% 
      
      
      srvyr::mutate(lingua_est = case_when(H14_4 == 1~ "Cursa Língua estrangeira",
                                           H14_4 == 2~ " Não Cursa Língua estrangeira",
                                           H14_4 == 88888~ "Não sabe",
                                           
      )) %>% 
      srvyr::group_by(criterio_brasil, lingua_est) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
     
      select(criterio_brasil, lingua_est, n, n_cv, pct, pct_cv) %>% 
      pivot_wider(names_from = "lingua_est", values_from = c("n", "n_cv", "pct", "pct_cv"))
    
    write_clip(tab_29.1, dec=",")
    
    
    # Outros cursos 
    
    tab_30 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>%
      
      srvyr::filter(H02==1|H02==2) %>% 
      
      srvyr::mutate(outros_cursos = case_when(H14_5 == 1~ "Cursa Outros Cursos",
                                           H14_5 == 2~ " Não Cursa Outros Cursos",
                                           H14_5 == 88888~ "Não sabe",
                                           
      )) %>% 
      srvyr::group_by(outros_cursos) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
      mutate(territorio="DF") %>% 
      select(territorio, outros_cursos, n, n_cv, pct, pct_cv) %>% 
      pivot_wider(names_from = "outros_cursos", values_from = c("n", "n_cv", "pct", "pct_cv"))
    
    write_clip(tab_30, dec=",")
    
    
    # por classe
    
    tab_30.1 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>%
    
      srvyr::filter(H02==1|H02==2) %>% 
      
      
      
      srvyr::mutate(outros_cursos = case_when(H14_5 == 1~ "Cursa Outros Cursos",
                                              H14_5 == 2~ " Não Cursa Outros Cursos",
                                              H14_5 == 88888~ "Não sabe",
                                              
                                           
      )) %>% 
      srvyr::group_by(criterio_brasil, outros_cursos) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
      
      select(criterio_brasil, outros_cursos, n, n_cv, pct, pct_cv) %>% 
      pivot_wider(names_from = "outros_cursos", values_from = c("n", "n_cv", "pct", "pct_cv"))
    
    write_clip(tab_30.1, dec=",")
    
    
    #CURSA NO GERAL
    
    
    tab_31 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>%
      
      srvyr::filter(H02==1|H02==2) %>% 
      
      srvyr::mutate(cursa = ifelse(H14_4 ==1 |H14_5 == 1, "Cursa", "Não Cursa"))    %>%
      
     
      srvyr::group_by(cursa) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
      mutate(territorio="DF") %>% 
      select(territorio, cursa, n, n_cv, pct, pct_cv) %>% 
      pivot_wider(names_from = "cursa", values_from = c("n", "n_cv", "pct", "pct_cv"))
    
    write_clip(tab_31, dec=",")
    
    
   #critério brasil 
    
    tab_31.1 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>%
      
      
      srvyr::filter(H02==1|H02==2) %>% 
      
      srvyr::mutate(cursa = ifelse(H14_4 ==1 |H14_5 == 1, "Cursa", "Não Cursa"))    %>%
                                              
                                              
 
      srvyr::group_by(criterio_brasil, cursa) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
      
      select(criterio_brasil, cursa, n, n_cv, pct, pct_cv) %>% 
      pivot_wider(names_from = "cursa", values_from = c("n", "n_cv", "pct", "pct_cv"))
    
    write_clip(tab_31.1, dec=",")
    

    
  #DISTORÇÃO IDADE-SERIE 
    
    tab_31.2 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>%
      
      srvyr::mutate(distorcao = case_when(
                                            H07 == 4 & H08 == 1 & idade>8~1,
                                            H07 == 4 & H08 == 2 & idade>9~1,
                                            H07 == 4 & H08 == 3 & idade>10~1,
                                            H07 == 4 & H08 == 4 & idade>11~1,
                                            H07 == 4 & H08 == 5 & idade>12~1,
                                            H07 == 4 & H08 == 6 & idade>13~1,
                                            H07 == 4 & H08 == 7 & idade>14~1,
                                            H07 == 4 & H08 == 8 & idade>15~1,
                                            H07 == 4 & H08 == 9 & idade>16~1,
                                            H07 %in% c(5,6) & H08 == 1 & idade>17~1,
                                            H07 %in% c(5,6) & H08 == 2 & idade>18~1,
                                            H07 %in% c(5,6) & H08 == 3 & idade>19~1,
                                            H07 %in% c(7,8)~1,
                                            TRUE~0)) %>% 
    
    
      srvyr::mutate( distorcao = factor(distorcao, labels = c("Não apresenta distorção","Apresenta distorção"))) %>% 
      
      

      srvyr::group_by(distorcao) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
      mutate(territorio="DF") %>% 
      select(territorio, distorcao, n, n_cv, pct, pct_cv) %>% 
      pivot_wider(names_from = "distorcao", values_from = c("n", "n_cv", "pct", "pct_cv"))
    
    write_clip(tab_31.2, dec=",")
    
    
    tab_31.3 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>%
      
      srvyr::mutate(distorcao = case_when(
        H07 == 4 & H08 == 1 & idade>8~1,
        H07 == 4 & H08 == 2 & idade>9~1,
        H07 == 4 & H08 == 3 & idade>10~1,
        H07 == 4 & H08 == 4 & idade>11~1,
        H07 == 4 & H08 == 5 & idade>12~1,
        H07 == 4 & H08 == 6 & idade>13~1,
        H07 == 4 & H08 == 7 & idade>14~1,
        H07 == 4 & H08 == 8 & idade>15~1,
        H07 == 4 & H08 == 9 & idade>16~1,
        H07 %in% c(5,6) & H08 == 1 & idade>17~1,
        H07 %in% c(5,6) & H08 == 2 & idade>18~1,
        H07 %in% c(5,6) & H08 == 3 & idade>19~1,
        H07 %in% c(7,8)~1,
        TRUE~0)) %>% 
      
      
      srvyr::mutate( distorcao = factor(distorcao, labels = c("Não apresenta distorção","Apresenta distorção"))) %>% 
      
      
      
      srvyr::group_by(criterio_brasil, distorcao) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
      
      select(criterio_brasil, distorcao, n, n_cv, pct, pct_cv) %>% 
      pivot_wider(names_from = "distorcao", values_from = c("n", "n_cv", "pct", "pct_cv"))
    
    write_clip(tab_31.3, dec=",")
    
    
    
    
    
    
    
    
        
        
        
        
              
# 3 CONECTITIVIDADE --------------------------------------------
    
#ACESSOU A INTERNET 
    
    tab_32 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>%
    
  
      
      srvyr::mutate(internet = ifelse(F04_1 ==1 |F04_1 == 2, "Acessou", "Não acessou"))    %>%
      
      
      srvyr::group_by(internet) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
      mutate(territorio="DF") %>% 
      select(territorio, internet, n, n_cv, pct, pct_cv) %>% 
      pivot_wider(names_from = "internet", values_from = c("n", "n_cv", "pct", "pct_cv"))
    
    write_clip(tab_32, dec=",")
    
    
    tab_32.1 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>%
      
      srvyr::filter(F04==1) %>% 
      
      srvyr::mutate(internet = ifelse(F04_1 ==1 |F04_1 == 2, "Acessou", "Não acessou"))    %>%
      
      
      srvyr::group_by(criterio_brasil,internet) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
     
      select(criterio_brasil, internet, n, n_cv, pct, pct_cv) %>% 
      pivot_wider(names_from = "internet", values_from = c("n", "n_cv", "pct", "pct_cv"))
    
    write_clip(tab_32.1, dec=",")
    
    
    tab_32.2 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>%
      
      srvyr::filter(F04==1) %>% 
      
      srvyr::mutate(internet = ifelse(F04_1 ==1 |F04_1 == 2, "Acessou", "Não acessou"))    %>%
      
      
      srvyr::group_by(faixa_etaria_crianças,internet) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
      
      select(faixa_etaria_crianças, internet, n, n_cv, pct, pct_cv) %>% 
      pivot_wider(names_from = "internet", values_from = c("n", "n_cv", "pct", "pct_cv"))
    
    write_clip(tab_32.2, dec=",")
    
    
    
    tab_32.3 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>%
      
      srvyr::filter(F04==1) %>% 
      
      srvyr::mutate(internet = ifelse(F04_1 ==1 |F04_1 == 2, "Acessou", "Não acessou"))    %>%
      
      
      srvyr::group_by(faixa_etaria_crianças,criterio_brasil, internet) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
      
      select(faixa_etaria_crianças, criterio_brasil, internet, n, n_cv, pct, pct_cv) %>% 
      pivot_wider(names_from = "internet", values_from = c("n", "n_cv", "pct", "pct_cv"))
    
    write_clip(tab_32.3, dec=",")
    
    
    tab_32.4 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>%
      
      srvyr::filter(F04==1) %>% 
      
      srvyr::mutate(internet = ifelse(F04_1 ==1 |F04_1 == 2, "Acessou", "Não acessou"))    %>%
      
      
      srvyr::group_by(criterio_brasil, faixa_etaria_crianças, internet) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
      
      select( criterio_brasil, faixa_etaria_crianças,internet, n, n_cv, pct, pct_cv) %>% 
      pivot_wider(names_from = "internet", values_from = c("n", "n_cv", "pct", "pct_cv"))
    
    write_clip(tab_32.4, dec=",")
    
    
    
   # COMO ACESSOU A INTERNET
    
  
    
    tab_33 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>%
      
      srvyr::filter(F04_1==1|F04_1==2) %>% 
      
      srvyr::mutate(tipo_acesso = case_when(F05_1 == 1~ "Microcomputador",
                                            F05_2 == 1~ " Celular/ Tablet",
                                            F05_3 ==1~ "Outros meios",
                                              
      )) %>% 
      srvyr::group_by(tipo_acesso) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
      mutate(territorio="DF") %>% 
      select(territorio, tipo_acesso, n, n_cv, pct, pct_cv) %>% 
      pivot_wider(names_from = "tipo_acesso", values_from = c("n", "n_cv", "pct", "pct_cv"))
    
    write_clip(tab_33, dec=",")
    
    
    
    
    
    tab_33.1 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>%
      
      srvyr::filter(F04_1==1|F04_1==2) %>%  
      
      srvyr::mutate(tipo_acesso = case_when(F05_1 == 1~ "Microcomputador",
                                            F05_2 == 1~ " Celular/ Tablet",
                                            F05_3 ==1~ "Outros meios",
                                            
      )) %>% 
      srvyr::group_by(criterio_brasil,tipo_acesso) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
      
      select(criterio_brasil, tipo_acesso, n, n_cv, pct, pct_cv) %>% 
      pivot_wider(names_from = "tipo_acesso", values_from = c("n", "n_cv", "pct", "pct_cv"))
    
    write_clip(tab_33.1, dec=",")
    
    
    tab_33.2 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>%
      
      srvyr::filter(F04_1==1|F04_1==2) %>%  
      
      srvyr::mutate(tipo_acesso = case_when(F05_1 == 1~ "Microcomputador",
                                            F05_2 == 1~ " Celular/ Tablet",
                                            F05_3 ==1~ "Outros meios",
                                            
      )) %>% 
      srvyr::group_by(faixa_etaria_crianças,tipo_acesso) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
      
      select(faixa_etaria_crianças, tipo_acesso, n, n_cv, pct, pct_cv) %>% 
      pivot_wider(names_from = "tipo_acesso", values_from = c("n", "n_cv", "pct", "pct_cv"))
    
    write_clip(tab_33.2, dec=",")
    
    
    
    
    
    #PRA QUE ACESSOU A INTERNET
    
    tab_34 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>%
      
      srvyr::filter(F04_1==1|F04_1==2) %>% 
      
      srvyr::mutate(motivo = case_when(F06_1 == 1~ "Trabalho",
                                            F06_2 == 1~ " Educação/Cursos",
                                            F06_3 ==1~ "Informações e Notícias",
                                            F06_4 == 1 ~"Criação e compartilhamento de conteúdos",
                                            F06_5==1 ~ "Lazer e Cultura", 
                                            F06_6 ==1 ~"Comunicação"
      )) %>% 
      srvyr::group_by(motivo) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
      mutate(territorio="DF") %>% 
      select(territorio, motivo, n, n_cv, pct, pct_cv) %>% 
      pivot_wider(names_from = "motivo", values_from = c("n", "n_cv", "pct", "pct_cv"))
    
    write_clip(tab_34, dec=",")
    
    
    tab_34.1 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>%
      
      srvyr::filter(F04_1==1|F04_1==2) %>% 
      
      srvyr::mutate(motivo = case_when(F06_1 == 1~ "Trabalho",
                                       F06_2 == 1~ " Educação/Cursos",
                                       F06_3 ==1~ "Informações e Notícias",
                                       F06_4 == 1 ~"Criação e compartilhamento de conteúdos",
                                       F06_5==1 ~ "Lazer e Cultura", 
                                       F06_6 ==1 ~"Comunicação"
      )) %>% 
      srvyr::group_by(criterio_brasil, motivo) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
     
      select(criterio_brasil, motivo, n, n_cv, pct, pct_cv) %>% 
      pivot_wider(names_from = "motivo", values_from = c("n", "n_cv", "pct", "pct_cv"))
    
    write_clip(tab_34.1, dec=",")
    
    
    
    tab_34.2 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>%
      
      srvyr::filter(F04_1==1|F04_1==2) %>% 
      
      srvyr::mutate(motivo = case_when(F06_1 == 1~ "Trabalho",
                                       F06_2 == 1~ " Educação/Cursos",
                                       F06_3 ==1~ "Informações e Notícias",
                                       F06_4 == 1 ~"Criação e compartilhamento de conteúdos",
                                       F06_5==1 ~ "Lazer e Cultura", 
                                       F06_6 ==1 ~"Comunicação"
      )) %>% 
      srvyr::group_by(faixa_etaria_crianças, motivo) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
      
      select(faixa_etaria_crianças, motivo, n, n_cv, pct, pct_cv) %>% 
      pivot_wider(names_from = "motivo", values_from = c("n", "n_cv", "pct", "pct_cv"))
    
    write_clip(tab_34.2, dec=",")
    
    
    
    
    
# 4 SAÚDE --------------------------------------------
    
#Plano de saude 
    
    
    tab_35 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>%
      
   
      
      srvyr::mutate(plano = case_when(G04 == 1~ "Tem plano de saúde",
                                       G04 == 2~ " Não tem plano de saúde",
                                     G04== 88888 ~ "Não sabe"
                                     )) %>% 
    
      srvyr::group_by(plano) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
      mutate(territorio="DF") %>% 
      select(territorio, plano, n, n_cv, pct, pct_cv) %>% 
      pivot_wider(names_from = "plano", values_from = c("n", "n_cv", "pct", "pct_cv"))
    
    write_clip(tab_35, dec=",")
    
    
    

    tab_35.5 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>% 
      
      srvyr::mutate(plano = case_when(G04 == 1~ "Tem plano de saúde",
                                      G04 == 2~ " Não tem plano de saúde",
                                      G04== 88888 ~ "Não sabe"
      )) %>%
      
      
      srvyr::group_by(RA,plano) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       pct=survey_mean(vartype = "cv")) %>%   
      select(RA, plano,n, pct, n_cv, pct_cv) %>%
      pivot_wider(names_from = "plano",values_from = c("n", "n_cv","pct","pct_cv")) 
    
    
    write_clip(tab_35.5, dec=",") 
    
    

    
    tab_35.1 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>%
      
      
      
      srvyr::mutate(plano = case_when(G04 == 1~ "Tem plano de saúde",
                                      G04 == 2~ " Não tem plano de saúde",
                                      G04== 88888 ~ "Não sabe"
      )) %>% 
      
      srvyr::group_by(criterio_brasil, plano) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
      
      select(criterio_brasil, plano, n, n_cv, pct, pct_cv) %>% 
      pivot_wider(names_from = "plano", values_from = c("n", "n_cv", "pct", "pct_cv"))
    
    write_clip(tab_35.1, dec=",")
    
    
    tab_35.2 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>%
      
      
      
      srvyr::mutate(plano = case_when(G04 == 1~ "Tem plano de saúde",
                                      G04 == 2~ " Não tem plano de saúde",
                                      G04== 88888 ~ "Não sabe"
      )) %>% 
      
      srvyr::group_by(faixa_etaria_simples, plano) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
      
      select(faixa_etaria_simples, plano, n, n_cv, pct, pct_cv) %>% 
      pivot_wider(names_from = "plano", values_from = c("n", "n_cv", "pct", "pct_cv"))
    
    write_clip(tab_35.2, dec=",")
    
    
    tab_35.3 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>%
      
      
      
      srvyr::mutate(plano = case_when(G04 == 1~ "Tem plano de saúde",
                                      G04 == 2~ " Não tem plano de saúde",
                                      G04== 88888 ~ "Não sabe"
      )) %>% 
      
      srvyr::group_by(faixa_etaria_simples, criterio_brasil, plano) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
      
      select(faixa_etaria_simples, criterio_brasil, plano, n, n_cv, pct, pct_cv) %>% 
      pivot_wider(names_from = "plano", values_from = c("n", "n_cv", "pct", "pct_cv"))
    
    write_clip(tab_35.3, dec=",")
    

# Local de atendimento 
    
    tab_36 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>%
      
      
      
      srvyr::mutate(local = case_when(G01 == 0 ~ "Nunca precisou",
                                      G01== 1 ~ "Farmácia", 
                                      G01 == 2 ~"Posto de saúde/Unidade básica de atendimento", 
                                      G01 == 3 ~ "UPA (Unidade de Pronto Atendimento)",
                                      G01== 4 ~"Centro de Especialidades/Policlínica do SUS",
                                      G01 == 5 ~" Pronto-Socorro ou Emergência de hospital público",
                                      G01== 6 ~ "Ambulatório de hospital público", 
                                      G01 == 7 ~"Consultório partícula/clínica privada", 
                                      G01 == 8 ~ "Ambulatório ou consultório de empresa ou sindicato", 
                                      G01 == 9 ~ "Pronto-Atendimento ou Urgência de hospital privado", 
                                      G01 == 10 ~"No domicílio, com profissional da equipe de saúde da família", 
                                      G01==11 ~"No domicílio, com médico particular", 
                                      G01==12 ~"Outro serviço", 
                                      G01 == 88888 ~"Não sabe"
                                      

      )) %>% 
      
      srvyr::group_by(local) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
      mutate(territorio="DF") %>% 
      select(territorio, local, n, n_cv, pct, pct_cv) %>% 
      pivot_wider(names_from = "local", values_from = c("n", "n_cv", "pct", "pct_cv"))
    
    write_clip(tab_36, dec=",")
    
    
    tab_36.1 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>%
      
      
      
      srvyr::mutate(local = case_when(G01 == 0 ~ "Nunca precisou",
                                      G01== 1 ~ "Farmácia", 
                                      G01 == 2 ~"Posto de saúde/Unidade básica de atendimento", 
                                      G01 == 3 ~ "UPA (Unidade de Pronto Atendimento)",
                                      G01== 4 ~"Centro de Especialidades/Policlínica do SUS",
                                      G01 == 5 ~" Pronto-Socorro ou Emergência de hospital público",
                                      G01== 6 ~ "Ambulatório de hospital público", 
                                      G01 == 7 ~"Consultório partícula/clínica privada", 
                                      G01 == 8 ~ "Ambulatório ou consultório de empresa ou sindicato", 
                                      G01 == 9 ~ "Pronto-Atendimento ou Urgência de hospital privado", 
                                      G01 == 10 ~"No domicílio, com profissional da equipe de saúde da família", 
                                      G01==11 ~"No domicílio, com médico particular", 
                                      G01==12 ~"Outro serviço", 
                                      G01 == 88888 ~"Não sabe"
                                      
                                      
      )) %>% 
      
      srvyr::group_by(faixa_etaria_simples, local) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
     
      select(faixa_etaria_simples, local, n, n_cv, pct, pct_cv) %>% 
      pivot_wider(names_from = "local", values_from = c("n", "n_cv", "pct", "pct_cv"))
    
    write_clip(tab_36.1, dec=",")
    
    
    
    tab_36.2 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>%
      
      
      
      srvyr::mutate(local = case_when(G01 == 0 ~ "Nunca precisou",
                                      G01== 1 ~ "Farmácia", 
                                      G01 == 2 ~"Posto de saúde/Unidade básica de atendimento", 
                                      G01 == 3 ~ "UPA (Unidade de Pronto Atendimento)",
                                      G01== 4 ~"Centro de Especialidades/Policlínica do SUS",
                                      G01 == 5 ~" Pronto-Socorro ou Emergência de hospital público",
                                      G01== 6 ~ "Ambulatório de hospital público", 
                                      G01 == 7 ~"Consultório partícula/clínica privada", 
                                      G01 == 8 ~ "Ambulatório ou consultório de empresa ou sindicato", 
                                      G01 == 9 ~ "Pronto-Atendimento ou Urgência de hospital privado", 
                                      G01 == 10 ~"No domicílio, com profissional da equipe de saúde da família", 
                                      G01==11 ~"No domicílio, com médico particular", 
                                      G01==12 ~"Outro serviço", 
                                      G01 == 88888 ~"Não sabe"
                                      
                                      
      )) %>% 
      
      srvyr::group_by(criterio_brasil, local) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
      
      select(criterio_brasil, local, n, n_cv, pct, pct_cv) %>% 
      pivot_wider(names_from = "local", values_from = c("n", "n_cv", "pct", "pct_cv"))
    
    write_clip(tab_36.2, dec=",")
    
    
  # Motivo do atendimento 
    
    
    
    tab_37 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>%
      
      
  
      
            srvyr::mutate(motivo = case_when(G02== 1 ~ "Acidente, lesão ou fratura", 
                                    G02 == 2 ~"Doença (dor, febre, diarreia, etc)", 
                                      G02 == 3 ~ "Continuação de tratamento de doença",
                                      G02== 4 ~"Problema odontológico",
                                      G02 == 5 ~" Reabilitação (Fisioterapia, fonoaudiologia,terapia ocupacional, etc)",
                                      G02== 6 ~ "Pré-natal", 
                                      G02 == 7 ~"Parto", 
                                      G02 == 8 ~ "Exame complementar de diagnóstico", 
                                      G02 == 9 ~ "Vacinação", 
                                      G02 == 10 ~ "Prevenção", 
                                      G02==11 ~"Solicitação de atestado de saúde", 
                                      G02==12 ~"Acompanhamento com psicólogo, nutricionista ou outro profissional de saúde", 
                                      G02 == 13 ~ "Outro",
                                      G02 ==88888 ~"Não sabe"
                                      
                                      
      )) %>% 
      
      srvyr::group_by(motivo) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
      mutate(territorio="DF") %>% 
      select(territorio, motivo, n, n_cv, pct, pct_cv) %>% 
      pivot_wider(names_from = "motivo", values_from = c("n", "n_cv", "pct", "pct_cv"))
    
    write_clip(tab_37, dec=",")
    
    
    tab_37.1 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>%
      
      
      
      
      srvyr::mutate(motivo = case_when(G02== 1 ~ "Acidente, lesão ou fratura", 
                                       G02 == 2 ~"Doença (dor, febre, diarreia, etc)", 
                                       G02 == 3 ~ "Continuação de tratamento de doença",
                                       G02== 4 ~"Problema odontológico",
                                       G02 == 5 ~" Reabilitação (Fisioterapia, fonoaudiologia,terapia ocupacional, etc)",
                                       G02== 6 ~ "Pré-natal", 
                                       G02 == 7 ~"Parto", 
                                       G02 == 8 ~ "Exame complementar de diagnóstico", 
                                       G02 == 9 ~ "Vacinação", 
                                       G02 == 10 ~ "Prevenção", 
                                       G02==11 ~"Solicitação de atestado de saúde", 
                                       G02==12 ~"Acompanhamento com psicólogo, nutricionista ou outro profissional de saúde", 
                                       G02 == 13 ~ "Outro",
                                       G02 ==88888 ~"Não sabe"
                                       
                                       
      )) %>% 
      
      srvyr::group_by(criterio_brasil,motivo) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
     
      select(criterio_brasil, motivo, n, n_cv, pct, pct_cv) %>% 
      pivot_wider(names_from = "motivo", values_from = c("n", "n_cv", "pct", "pct_cv"))
    
    write_clip(tab_37.1, dec=",")
    
    
    tab_37.2 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>%
      
      
      
      
      srvyr::mutate(motivo = case_when(G02== 1 ~ "Acidente, lesão ou fratura", 
                                       G02 == 2 ~"Doença (dor, febre, diarreia, etc)", 
                                       G02 == 3 ~ "Continuação de tratamento de doença",
                                       G02== 4 ~"Problema odontológico",
                                       G02 == 5 ~" Reabilitação (Fisioterapia, fonoaudiologia,terapia ocupacional, etc)",
                                       G02== 6 ~ "Pré-natal", 
                                       G02 == 7 ~"Parto", 
                                       G02 == 8 ~ "Exame complementar de diagnóstico", 
                                       G02 == 9 ~ "Vacinação", 
                                       G02 == 10 ~ "Prevenção", 
                                       G02==11 ~"Solicitação de atestado de saúde", 
                                       G02==12 ~"Acompanhamento com psicólogo, nutricionista ou outro profissional de saúde", 
                                       G02 == 13 ~ "Outro",
                                       G02 ==88888 ~"Não sabe"
                                       
                                       
      )) %>% 
      
      srvyr::group_by(faixa_etaria_simples,motivo) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
      
      select(faixa_etaria_simples, motivo, n, n_cv, pct, pct_cv) %>% 
      pivot_wider(names_from = "motivo", values_from = c("n", "n_cv", "pct", "pct_cv"))
    
    write_clip(tab_37.2, dec=",")
    
    
  # Atendimento em outra RA
    
      tab_38 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>%
      
      
      
      srvyr::mutate(fora_RA = case_when(G03 == A01ra~ "Atendimento na RA",
                                        G03!= A01ra~"Atendimento fora da RA",
      )) %>% 
      
        srvyr::group_by(fora_RA) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
      mutate(territorio="DF") %>% 
      select(territorio, fora_RA, n, n_cv, pct, pct_cv) %>% 
      pivot_wider(names_from = "fora_RA", values_from = c("n", "n_cv", "pct", "pct_cv"))
    
    write_clip(tab_38, dec=",")
    
    
    tab_38.1 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>%
      
      
      
      srvyr::mutate(fora_RA = case_when(G03 == A01ra~ "Atendimento na RA",
                                        G03!= A01ra~"Atendimento fora da RA",
      )) %>% 
      
      srvyr::group_by(criterio_brasil, fora_RA) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
      
      select(criterio_brasil, fora_RA, n, n_cv, pct, pct_cv) %>% 
      pivot_wider(names_from = "fora_RA", values_from = c("n", "n_cv", "pct", "pct_cv"))
    
    write_clip(tab_38.1, dec=",")
    
    
    tab_38.2 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>%
      
      
      
      srvyr::mutate(fora_RA = case_when(G03 == A01ra~ "Atendimento na RA",
                                        G03!= A01ra~"Atendimento fora da RA",
      )) %>% 
      
      srvyr::group_by(RA, fora_RA) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
      
      select(RA, fora_RA, n, n_cv, pct, pct_cv) %>% 
      pivot_wider(names_from = "fora_RA", values_from = c("n", "n_cv", "pct", "pct_cv"))
    
    write_clip(tab_38.2, dec=",")
    
    
    
    
    
    tab_38.2 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>%
      
      
      
      srvyr::mutate(fora_RA = case_when(G03 == A01ra~ "Atendimento na RA",
                                        G03!= A01ra~"Atendimento fora da RA",
      )) %>% 
      
      srvyr::group_by(faixa_etaria_simples, fora_RA) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
      
      select(faixa_etaria_simples, fora_RA, n, n_cv, pct, pct_cv) %>% 
      pivot_wider(names_from = "fora_RA", values_from = c("n", "n_cv", "pct", "pct_cv"))
    
    write_clip(tab_38.2, dec=",")
    
    
    
    
    
# 7 CARACTERISTICAS DOS DOMICÍLIOS --------------------------------------------
    
# Situação do domicílio 
    

    tab_39 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>%
      
     
      
      
      srvyr::mutate(sit_dom = case_when(B03 == 1~ "Próprio,já pago (quitado)",
                                           B03 == 2~ " Próprio, ainda pagando (em aquisição)",
                                           B03 == 3~ "Alugado",
                                           B03 == 4 ~"Cedido pelo empregador",
                                           B03== 5 ~ "Cedido por outro", 
                                           B03== 88888 ~ "Não Sabe"
                                           
      )) %>% 
      srvyr::group_by(sit_dom) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
      mutate(territorio="DF") %>% 
      select(territorio, sit_dom, n, n_cv, pct, pct_cv) %>% 
      pivot_wider(names_from = "sit_dom", values_from = c("n", "n_cv", "pct", "pct_cv"))
    
    write_clip(tab_39, dec=",")
    
    
    
    tab_39.1 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>%
      
      
      
      
      srvyr::mutate(sit_dom = case_when(B03 == 1~ "Próprio,já pago (quitado)",
                                        B03 == 2~ " Próprio, ainda pagando (em aquisição)",
                                        B03 == 3~ "Alugado",
                                        B03 == 4 ~"Cedido pelo empregador",
                                        B03== 5 ~ "Cedido por outro", 
                                        B03== 88888 ~ "Não Sabe"
                                        
      )) %>% 
      srvyr::group_by(faixa_etaria_simples, sit_dom) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
      
      select(faixa_etaria_simples, sit_dom, n, n_cv, pct, pct_cv) %>% 
      pivot_wider(names_from = "sit_dom", values_from = c("n", "n_cv", "pct", "pct_cv"))
    
    write_clip(tab_39.1, dec=",")
    
    
    
    
    tab_39.2 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>%
      
      
      
      
      srvyr::mutate(sit_dom = case_when(B03 == 1~ "Próprio,já pago (quitado)",
                                        B03 == 2~ " Próprio, ainda pagando (em aquisição)",
                                        B03 == 3~ "Alugado",
                                        B03 == 4 ~"Cedido pelo empregador",
                                        B03== 5 ~ "Cedido por outro", 
                                        B03== 88888 ~ "Não Sabe"
                                        
      )) %>% 
      srvyr::group_by(criterio_brasil, sit_dom) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
      
      select(criterio_brasil, sit_dom, n, n_cv, pct, pct_cv) %>% 
      pivot_wider(names_from = "sit_dom", values_from = c("n", "n_cv", "pct", "pct_cv"))
    
    write_clip(tab_39.2, dec=",")
    
 
  # Cômodo para dormir  
    
    
    tab_40 <-  amostra %>%
    srvyr::filter(idade>=0&idade<=11) %>%
     
     srvyr::filter(densidade!=0 & densidade<1000) %>% 
     
       srvyr::group_by(criterio_brasil) %>% 
      srvyr::summarise(media = survey_mean(densidade,na.rm=T, vartype = "cv"))
   
    write_clip(tab_40, dec =",")
    
    tab_40.1 <-  amostra %>%
      srvyr::filter(idade>=0&idade<=11) %>%
      
      srvyr::filter(densidade!=0 & densidade<1000) %>% 
      
      
      srvyr::summarise(media = survey_mean(densidade,na.rm=T, vartype = "cv"))
    
    write_clip(tab_40.1, dec =",")
    
   
     
    # Esgotamento sanitário 
    
    
    tab_41 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>%
      
      
      srvyr::mutate(esgotamento= ifelse(B14_1 ==1 |B14_2 == 1, "Tem esgotamento", "Não tem esgotamento"))    %>%
      
     
      srvyr::group_by(esgotamento) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
      mutate(territorio="DF") %>% 
      select(territorio, esgotamento, n, n_cv, pct, pct_cv) %>% 
      pivot_wider(names_from = "esgotamento", values_from = c("n", "n_cv", "pct", "pct_cv"))
    
    write_clip(tab_41, dec=",")
    
    
    tab_41.1 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>%
      
      
      srvyr::mutate(esgotamento= ifelse(B14_1 ==1 |B14_2 == 1, "Tem esgotamento", "Não tem esgotamento"))    %>%
      
      
      srvyr::group_by(criterio_brasil, esgotamento) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
     
      select(criterio_brasil, esgotamento, n, n_cv, pct, pct_cv) %>% 
      pivot_wider(names_from = "esgotamento", values_from = c("n", "n_cv", "pct", "pct_cv"))
    
    write_clip(tab_41.1, dec=",")
    
    #Coleta de lixo 
    
    
    tab_42 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>%
      
      
      srvyr::mutate(coleta= ifelse(B16_1 ==1 |B16_2 == 1| B16_3==1,  "Tem coleta", "Não tem coleta"))    %>%
      
      
      srvyr::group_by(coleta) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
      mutate(territorio="DF") %>% 
      select(territorio, coleta, n, n_cv, pct, pct_cv) %>% 
      pivot_wider(names_from = "coleta", values_from = c("n", "n_cv", "pct", "pct_cv"))
    
    write_clip(tab_42, dec=",")
    
    
    tab_42.1 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>%
      
      
      srvyr::mutate(coleta= ifelse(B16_1 ==1 |B16_2 == 1| B16_3==1,  "Tem coleta", "Não tem coleta"))    %>%
      
      srvyr::group_by(criterio_brasil, coleta) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
      
      select(criterio_brasil, coleta, n, n_cv, pct, pct_cv) %>% 
      pivot_wider(names_from = "coleta", values_from = c("n", "n_cv", "pct", "pct_cv"))
    
    write_clip(tab_42.1, dec=",")
    
    
    
    # Água própria para consumo 
    
    
    tab_43 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>%
      
      
      srvyr::mutate(agua= ifelse(B13_1 ==1,  "Recebe CAESB", "Não Recebe"))    %>%
      
      
      srvyr::group_by(agua) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
      mutate(territorio="DF") %>% 
      select(territorio, agua, n, n_cv, pct, pct_cv) %>% 
      pivot_wider(names_from = "agua", values_from = c("n", "n_cv", "pct", "pct_cv"))
    
    write_clip(tab_43, dec=",")
    
    
    
    tab_43.1 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>%
      
      
      srvyr::mutate(agua= ifelse(B13_1 ==1,  "Recebe CAESB", "Não Recebe"))    %>%
      
      
      srvyr::group_by(criterio_brasil, agua) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
      
      select(criterio_brasil, agua, n, n_cv, pct, pct_cv) %>% 
      pivot_wider(names_from = "agua", values_from = c("n", "n_cv", "pct", "pct_cv"))
    
    write_clip(tab_43.1, dec=",")
    
    
    #Espaços para lazer perto de casa 
    
    
    
    tab_44 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>%
      
      
      
      
      srvyr::mutate(lazer = case_when(B19_2 == 1~ "Parque",
                                      B19_3 == 1~ " Praça",
                                      B19_4== 1~ "Espaço cultural público",
                                      B19_5== 1 ~"Academia comunitária/PEC",
                                      B19_6== 1 ~ "Quadra espotiva", 
                                      
      )) %>% 
      srvyr::group_by(lazer) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
      
      mutate(territorio="DF") %>% 
            select(territorio, lazer, n, n_cv, pct, pct_cv) %>% 
      pivot_wider(names_from = "lazer", values_from = c("n", "n_cv", "pct", "pct_cv"))
    
    write_clip(tab_44, dec=",")
    
    
    
    
    
    tab_44.1 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>%
      
      
      
      
      srvyr::mutate(lazer = case_when(B19_2 == 1~ "Parque",
                                      B19_3 == 1~ " Praça",
                                      B19_4== 1~ "Espaço cultural público",
                                      B19_5== 1 ~"Academia comunitária/PEC",
                                      B19_6== 1 ~ "Quadra espotiva", 
                                      
      )) %>% 
      srvyr::group_by(criterio_brasil, lazer) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
      
      select(criterio_brasil, lazer, n, n_cv, pct, pct_cv) %>% 
      pivot_wider(names_from = "lazer", values_from = c("n", "n_cv", "pct", "pct_cv"))
    
    write_clip(tab_44.1, dec=",")
    
    
    tab.44.2 <- amostra %>%  srvyr::filter(idade>=0 & idade <=11) %>% 
      
      srvyr::mutate(lazer= ifelse(B19_2 == 1 | B19_3 ==1 | B19_4==1 | B19_5==1 | B19_6==1, "Tem espaço de lazer", "Não tem espaço de Lazer/Não Sabe")) %>% 
    
 
      srvyr::group_by(lazer) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
      
      mutate(territorio="DF") %>% 
      select(territorio, lazer, n, n_cv, pct, pct_cv) %>% 
      pivot_wider(names_from = "lazer", values_from = c("n", "n_cv", "pct", "pct_cv"))
    
    write_clip(tab.44.2, dec=",")
      
      
      
    
    tab_44.3 <- amostra %>%  srvyr::filter(idade>=0 & idade <=11) %>% 
      
      srvyr::mutate(lazer= ifelse(B19_2 == 1 | B19_3 ==1 | B19_4==1 | B19_5==1 | B19_6==1, "Tem espaço de lazer", "Não tem espaço de Lazer/Não Sabe")) %>% 
      
      
      srvyr::group_by(criterio_brasil, lazer) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
      
      
      select(criterio_brasil, lazer, n, n_cv, pct, pct_cv) %>% 
      pivot_wider(names_from = "lazer", values_from = c("n", "n_cv", "pct", "pct_cv"))
    
    write_clip(tab_44.3, dec=",")
    
    
    
    
    
    
# 8 VULNERABILIDADE  --------------------------------------------
    
#Insegurança alimentar 
    
    tab1 <- amostra %>% srvyr::filter(idade>=0 & idade <=11) %>% 
      srvyr::mutate(inseg_alimentar=factor(inseg_alimentar, labels = c("Segurança",
                                                                       "Insegurança leve",
                                                                       "Insegurança moderada",
                                                                       "Insegurança grave",
                                                                       "Sem Classificação"))) %>% 
     
      
       srvyr::group_by(inseg_alimentar) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       prop=survey_mean(vartype = "cv")) %>% write_clip(dec=",")
    
    
    tab2 <- amostra %>% srvyr::filter(idade>=0 & idade <=11) %>% 
      srvyr::mutate(inseg_alimentar=factor(inseg_alimentar, labels = c("Segurança",
                                                                       "Insegurança leve",
                                                                       "Insegurança moderada",
                                                                       "Insegurança grave",
                                                                       "Sem Classificação"))) %>% 
      srvyr::group_by(RA,inseg_alimentar) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       prop=survey_mean(vartype = "cv")) %>% write_clip(dec=",")
    
    tab3 <- amostra %>%srvyr::filter(idade>=0 & idade <=11) %>% 
      srvyr::mutate(inseg_alimentar=factor(inseg_alimentar, labels = c("Segurança",
                                                                       "Insegurança leve",
                                                                       "Insegurança moderada",
                                                                       "Insegurança grave",
                                                                       "Sem Classificação"))) %>% 
      srvyr::group_by(inseg_alimentar,sexo) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       prop=survey_mean(vartype = "cv"))%>% write_clip(dec=",")
    
    tab4 <- amostra %>% srvyr::filter(idade>=0 & idade <=11) %>% 
      srvyr::mutate(inseg_alimentar=factor(inseg_alimentar, labels = c("Segurança",
                                                                       "Insegurança leve",
                                                                       "Insegurança moderada",
                                                                       "Insegurança grave",
                                                                       "Sem Classificação"))) %>% 
      srvyr::group_by(raca_cor,inseg_alimentar) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       prop=survey_mean(vartype = "cv")) %>%  write_clip(dec=",")
    
    tab5 <- amostra %>% srvyr::filter(idade>=0 & idade <=11) %>% 
      srvyr::mutate(inseg_alimentar=factor(inseg_alimentar, labels = c("Segurança",
                                                                       "Insegurança leve",
                                                                       "Insegurança moderada",
                                                                       "Insegurança grave",
                                                                       "Sem Classificação"))) %>% 
     
      srvyr::mutate(arranjo_familiar = factor(case_when(arranjos==1~"Unipessoal",
                                                        arranjos==2~"Monoparental feminino",
                                                        arranjos%in%c(3,4,5)~"Casal com filhos",
                                                        arranjos==6~"Casal sem filhos",
                                                        arranjos==7~"Outro perfil")))%>% 
      
      srvyr::group_by(arranjo_familiar,inseg_alimentar) %>% 
      srvyr::summarise(n=survey_total(vartype = "cv"),
                       prop=survey_mean(vartype = "cv")) %>%  write_clip(dec=",")
    

    
    
 # Pobreza monetária 

    # Critério IBGE 
  
      sm <- 1100
    
    
    summary(pdad_2021$renda_domiciliar_pc_r)
     table(pdad_2021$renda_domiciliar_pc_r)
  
     
     tab_50 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>%
       srvyr::filter(renda_domiciliar_pc_r>=0) %>%
       
      
       
       srvyr::mutate(pobreza_ibge_1= ifelse(renda_domiciliar_pc_r < sm/2,  " Linha da Pobreza", "Renda >= 1/2 SM"))    %>%
       
       
       srvyr::group_by(pobreza_ibge_1) %>% 
       srvyr::summarise(n=survey_total (vartype = "cv"),
                        pct=survey_mean (vartype = "cv")) %>% na.omit %>% 
       mutate(territorio="DF") %>% 
       select(territorio, pobreza_ibge_1, n, n_cv, pct, pct_cv) %>% 
       pivot_wider(names_from = "pobreza_ibge_1", values_from = c("n", "n_cv", "pct", "pct_cv"))
     
   write_clip(tab_50, dec=",")
    
   
   tab_50.1 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>%
     srvyr::filter(renda_domiciliar_pc_r>=0) %>%
     
     
     
     srvyr::mutate(pobreza_ibge_1= ifelse(renda_domiciliar_pc_r < sm/2,  " Linha da Pobreza", "Renda >= 1/2 SM"))    %>%
     
     
     srvyr::group_by(pobreza_ibge_1, criterio_brasil) %>% 
     srvyr::summarise(n=survey_total (vartype = "cv"),
                      pct=survey_mean (vartype = "cv")) %>% na.omit %>% 
   
     select(criterio_brasil, pobreza_ibge_1, n, n_cv, pct, pct_cv) %>% 
     pivot_wider(names_from = "pobreza_ibge_1", values_from = c("n", "n_cv", "pct", "pct_cv"))
   
   write_clip(tab_50.1, dec=",")
   
   
   tab_51 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>%
     srvyr::filter(renda_domiciliar_pc_r>=0) %>%
     
     
     
     srvyr::mutate(pobreza_ibge_1= ifelse(renda_domiciliar_pc_r < sm/4,  " ExtremaPobreza", "Renda >= 1/4 SM"))    %>%
     
     
     srvyr::group_by(pobreza_ibge_1) %>% 
     srvyr::summarise(n=survey_total (vartype = "cv"),
                      pct=survey_mean (vartype = "cv")) %>% na.omit %>% 
     mutate(territorio="DF") %>% 
     select(territorio, pobreza_ibge_1, n, n_cv, pct, pct_cv) %>% 
     pivot_wider(names_from = "pobreza_ibge_1", values_from = c("n", "n_cv", "pct", "pct_cv"))
   
   write_clip(tab_51, dec=",")
   
   
   tab_51.1 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>%
     srvyr::filter(renda_domiciliar_pc_r>=0) %>%
     
     
     
     srvyr::mutate(pobreza_ibge_1= ifelse(renda_domiciliar_pc_r < sm/2,  " Extrema Pobreza", "Renda >= 1/4 SM"))    %>%
     
     
     srvyr::group_by(pobreza_ibge_1, criterio_brasil) %>% 
     srvyr::summarise(n=survey_total (vartype = "cv"),
                      pct=survey_mean (vartype = "cv")) %>% na.omit %>% 
     
     select(criterio_brasil, pobreza_ibge_1, n, n_cv, pct, pct_cv) %>% 
     pivot_wider(names_from = "pobreza_ibge_1", values_from = c("n", "n_cv", "pct", "pct_cv"))
   
   write_clip(tab_51.1, dec=",")
   
   
   
#Ministerio da cidadania 
   
   
   tab_53 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>%
     srvyr::filter(renda_domiciliar_pc_r>=0) %>%
     
     
     
     srvyr::mutate(pobreza_mc= case_when(renda_domiciliar_pc_r > 105 & renda_domiciliar_pc_r  <=210 ~ "Situação de Pobreza",
                                             renda_domiciliar_pc_r <=105 ~" Situação de Extrema Pobreza"
                                             
                                             ))    %>%
     
     
     srvyr::group_by(pobreza_mc) %>% 
     srvyr::summarise(n=survey_total (vartype = "cv"),
                      pct=survey_mean (vartype = "cv")) %>% na.omit %>% 
     mutate(territorio="DF") %>% 
     select(territorio, pobreza_mc, n, n_cv, pct, pct_cv) %>% 
     pivot_wider(names_from = "pobreza_mc", values_from = c("n", "n_cv", "pct", "pct_cv"))
   
   write_clip(tab_53, dec=",")

   tab_53.1 <- amostra %>% srvyr::filter(idade>=0&idade<=11) %>%
     srvyr::filter(renda_domiciliar_pc_r>=0) %>%
     
     
     
     srvyr::mutate(pobreza_mc= case_when(renda_domiciliar_pc_r > 105 & renda_domiciliar_pc_r  <=210 ~ "Situação de Pobreza",
                                         renda_domiciliar_pc_r <=105 ~" Situação de Extrema Pobreza"
                                         
     ))    %>%
     
     
     srvyr::group_by(pobreza_mc, criterio_brasil) %>% 
     srvyr::summarise(n=survey_total (vartype = "cv"),
                      pct=survey_mean (vartype = "cv")) %>% na.omit %>% 
     
     select(criterio_brasil, pobreza_mc, n, n_cv, pct, pct_cv) %>% 
     pivot_wider(names_from = "pobreza_mc", values_from = c("n", "n_cv", "pct", "pct_cv"))
   
   write_clip(tab_53.1, dec=",")
   
   
   
   
   
   
# HOMME - OFIICE -----------------------------
   
   tab_70 <- amostra %>% 
     srvyr::filter(H02== 3| H02== 4) %>% srvyr::filter(idade>=0 & idade <=40) %>% 
  
     srvyr::mutate(pai=ifelse(E05==1 & E04==1, 1, 0)) %>%
     srvyr::mutate(mae=ifelse(E05==1 & E04==2, 1, 0)) %>%
     
     
     
     srvyr::mutate(pai_mae= ifelse( home_office==1 & pai==1 | mae==1, "Pai ou Mãe em Home-Oficce", "Mãe e Pai trabalham fora " )
                                    
                                    
                                   
                                    
     )    %>%
     
     
     
     srvyr::group_by(pai_mae) %>% 
     srvyr::summarise(n=survey_total(vartype = "cv"),
                      pct=survey_mean(vartype = "cv")) %>% na.omit %>% 
     mutate(territorio="DF") %>% 
     select(territorio, pai_mae, n, n_cv, pct, pct_cv) %>% 
     pivot_wider(names_from = "pai_mae", values_from = c("n", "n_cv", "pct", "pct_cv"))
   
   write_clip(tab_70, dec=",")
   
   
   table(pdad_2021$home_office, pdad_2021$arranjos)
  
# COMPOSIÇÃO FAMILIAR DA CRIANÇAS -------------------
   

   
   ### Bibliotecas ###
   
   rm(list = ls())
   
   library(data.table)
   library(RODBC)  # ConexÃ£o com servidor de banco de dados
   library(DBI)    # ConexÃ£o com servidor de banco de dados
   library(survey) # Desenho da amostra complexa da Pdad
   library(srvyr)  # Desenho da amostra complexa da Pdad
   library(tidyverse)  # NecessÃ¡ria para transformar variÃ¡veis
   library(clipr)
   library(rlang)
   
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
                                     
                                     
                                     
                                     faixa_etaria_cca0a11=factor(case_when(idade<12 ~"Crianças (0 a 11 anos)",
                                                                           TRUE ~"Outras idades")),
                                     faixa_etaria_cca0a3=factor(case_when(idade<4 ~"Crianças (0 a 3 anos)",
                                                                          TRUE ~"Outras idades")),
                                     faixa_etaria_cca4faixas=factor(case_when(idade<4 ~"Crianças (0 a 3 anos)",
                                                                              idade>3&idade<6 ~"Crianças (4 a 5 anos)",
                                                                              idade>5&idade<12 ~"Crianças (6 a 11 anos)",
                                                                              TRUE ~"Outras idades")),
                                     # Crianças 0 a 3 anos
                                     cca0a3=ifelse(idade<4,1,0),
                                     # Crianças 0 a 11 anos
                                     cca0a11=ifelse(idade<12,1,0),
                                     # Frequenta escola/ creche
                                     escola=factor(case_when(H02%in% c(1,2)~"Sim",
                                                             H02%in% c(3,4)~"Não")),
                                     
                                     # Indicando a condição no domicílio 
                                     # Pessoa de referência 
                                     e02_pess_ref=ifelse(E05==1,1,0),
                                     # Cônjuge . 
                                     e02_conjuge=ifelse(E05%in% c(2,3),1,0),
                                     # Filho 
                                     e02_filhos=ifelse(E05%in% c(4, 5, 6),1,0),
                                     #  Genro ou nora 
                                     e02_genro=ifelse(E05==7,1,0),
                                     # Pai, mãe, padrasto ou madrasta  
                                     e02_paimae=ifelse(E05==8,1,0),
                                     # Sogro  
                                     e02_sogro=ifelse(E05==9,1,0),
                                     #  Neto, bisneto  
                                     e02_netobis=ifelse(E05%in% c(10,11),1,0),
                                     #  Irmão, irmã  
                                     e02_irmao=ifelse(E05==12,1,0),
                                     # Avô ou avó  
                                     e02_avos=ifelse(E05==13,1,0),
                                     #  Outro parente  
                                     e02_outopar=ifelse(E05==14,1,0),
                                     #  Agregado  
                                     e02_agreg=ifelse(E05==15,1,0),
                                     # Pensionista . 
                                     e02_pensio=ifelse(E05==17,1,0),
                                     # Empregado doméstico . 
                                     e02_empdom=ifelse(E05==18,1,0),
                                     # Parente empregado doméstico . 
                                     e02_par_empdom=ifelse(E05==19,1,0),
                                     
                                     # Contagem de pessoas na base
                                     count_pes=1)
   
   #################################################################################################
   ##### Agrupar a condição no domicílio por grupo de domicílios  #####
   somatorio_conddom <- dplyr::select(pdad_2021,A01nficha,cca0a3,cca0a11,e02_pess_ref,e02_conjuge,
                                      e02_filhos,e02_genro,e02_paimae,e02_sogro,e02_netobis,e02_irmao,e02_avos,
                                      e02_outopar,e02_agreg,e02_pensio,e02_empdom,e02_par_empdom) %>%
     # Agrupar por domicílio
     dplyr::group_by(A01nficha) %>%
     # Somatório de cada condição por domicílio
     dplyr::summarise(                
       cca0a3_sum=sum(cca0a3),
       cca0a11_sum=sum(cca0a11),
       e02_pess_ref_sum=sum(e02_pess_ref),
       e02_conjuge_sum=sum(e02_conjuge),
       e02_filhos_sum=sum(e02_filhos),
       e02_genro_sum=sum(e02_genro),
       e02_paimae_sum=sum(e02_paimae),
       e02_sogro_sum=sum(e02_sogro),
       e02_netobis_sum=sum(e02_netobis),
       e02_irmao_sum=sum(e02_irmao),
       e02_avos_sum=sum(e02_avos),
       e02_outopar_sum=sum(e02_outopar),
       e02_agreg_sum=sum(e02_agreg),
       e02_pensio_sum=sum(e02_pensio),
       e02_empdom_sum=sum(e02_empdom),
       e02_par_empdom_sum=sum(e02_par_empdom))
   
   #################################################################################################
   ##### Construção das variáveis dos cenários por domicílios  #####
   cenarios_parciais <- pdad_2021 %>%
     #cenarios_cea <- pdad_pes_201833 %>%  
     # Agrupar por domicílio
     dplyr::group_by(A01nficha) %>%
     # Marcar por domicílios
     #dplyr::summarise(cenario_mae=sum(pesref==1 $ E03==2)) %>%
     dplyr::mutate(
       cen_pai=ifelse(E05==1&E04==1,1,0),
       cen_mae=ifelse(E05==1&E04==2,1,0),
       cen_conjuge=ifelse(E05==2|E05==3,1,0),
       cen_avos=ifelse(E05%in% c(8,9,10,11,13),1,0),
       cen_outros=ifelse((E05%in% c(4,5,6,7,12,14,15,18) & idade >12),1,0),
       #cen_outros=ifelse((E02%in% c(4,5,6,7,10,11,12,14,15,17) & idade_calculada >12),1,0),
       cen_emprdom=ifelse(E05==18,1,0))
   
   #################################################################################################
   ##### Cenários parciais por domicílios  #####
   somatorio_cenarios_parciais <- cenarios_parciais %>%
     # Agrupar por domicílio
     dplyr::group_by(A01nficha) %>%
     # Somatório de cada condição por domicílio
     dplyr::summarise(                
       cen_pai_sum=sum(cen_pai),
       cen_mae_sum=sum(cen_mae),
       cen_conjuge_sum=sum(cen_conjuge),
       cen_avos_sum=sum(cen_avos),
       cen_outros_sum=sum(cen_outros),
       cen_emprdom_sum=sum(cen_emprdom))%>%
     
     dplyr::mutate(
       cen_pai_flag=ifelse(cen_pai_sum>0,1,0),
       cen_mae_flag=ifelse(cen_mae_sum>0,1,0),
       cen_conjuge_flag=ifelse(cen_conjuge_sum>0,1,0),
       cen_avos_flag=ifelse(cen_avos_sum>0,1,0),
       cen_outros_flag=ifelse(cen_outros_sum>0,1,0),
       cen_emprdom_flag=ifelse(cen_emprdom_sum>0,1,0))
   
   
   # Junção com cenários
   
   pdad_2021 <- pdad_2021 %>% 
     dplyr::left_join(somatorio_cenarios_parciais, by="A01nficha")
   
   
   
   
   ##### Construção de cenários finais por domicílios  #####
   pdad_2021 <- pdad_2021 %>%
     # Construindo cenários por pessoa
     dplyr::mutate(
       sopai=ifelse(cen_pai_flag==1 & cen_conjuge_flag==0,1,0),
       somae=ifelse(cen_mae_flag==1 & cen_conjuge_flag==0,1,0),
       paimae=ifelse((cen_pai_flag==1 | cen_mae_flag==1)& cen_conjuge_flag==1,1,0),
       avos=ifelse(cen_avos_flag==1,1,0),
       outros=ifelse(cen_outros_flag==1|cen_emprdom_flag==1,1,0))%>%
     
     # Construindo cenários FINAIS
     dplyr::mutate(
       c1_maePai=ifelse(paimae==1 & somae==0 & sopai==0 & avos==0 &outros==0,"1. Mãe e pai",NA),
       c2_maePaiOutros=ifelse(paimae==1 & somae==0 & sopai==0 & avos==0 & outros==1,"2. Mãe, pai e outros",NA),
       c3_maePaiAvosOutros=ifelse(paimae==1 & somae==0 & sopai==0 & avos==1 & (c2_maePaiOutros %in% NA),"3. Mãe, pai, avós e outros",NA),
       c4_soMae=ifelse(somae==1 & sopai==0 & paimae==0 & avos==0 &outros==0,"4. Só mãe",NA),
       c5_soMaeOutros=ifelse(somae==1 & sopai==0 & paimae==0 & avos==0 &outros==1,"5. Só mãe e outros",NA),
       c6_soMaeAvosOutros=ifelse(somae==1 & sopai==0 & paimae==0 & avos==1 & (c5_soMaeOutros %in% NA),"6. Só mãe, avós e outros",NA),
       c7_soPai=ifelse(sopai==1 & somae==0 & paimae==0 & avos==0 & outros==0,"7. Só pai",NA),
       c8_soPaiOutros=ifelse(sopai==1 & somae==0 & paimae==0 & avos==0 & outros==1,"8. Só pai e outros",NA),
       c9_soPaiAvosOutros=ifelse(sopai==1 & somae==0 & paimae==0 & avos==1 & (c8_soPaiOutros %in% NA),"9. Só pai, avós e outros",NA),
       cenario_final=cbind(c1_maePai,c2_maePaiOutros,c3_maePaiAvosOutros,c4_soMae,c5_soMaeOutros,c6_soMaeAvosOutros,c7_soPai,c8_soPaiOutros,c9_soPaiAvosOutros))
   
   ###################################################################
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
   
   
   ###################################################################
   ##### Cenários para crianças    #####
   ##### Frequenta escola/ creche  #####
   
   tab_1 <- amostra %>%
     # Filtrar criança de 0 a 11 anos
     srvyr::filter(idade <12) %>% 
     # Agrupamentos
     srvyr::group_by(cenario_final) %>%
     # Calcular os totais para cada situação
     srvyr::summarise(n=survey_total(vartype = "cv"))
   
   
   
   
   tab_2 <- amostra %>%
     # Filtrar criança de 0 a 11 anos
     srvyr::filter(idade <12) %>% 
     # Agrupamentos
     srvyr::group_by( criterio_brasil, cenario_final) %>%
     # Calcular os totais para cada situação
     srvyr::summarise(n=survey_total(vartype = "cv"))
   
   write_clip(tab_2, dec=",")
   
   
   
   
   write.table(tab_1, 
               "C:/Users/37168/Desktop/OneDrive - unb.br/TRABALHO/CRIANÇAS/Crianças.csv",
               row.names = F, sep = ";",
               fileEncoding = "latin1")
   
   
   
   write.table(tab_2, 
               "C:/Users/37168/Desktop/OneDrive - unb.br/TRABALHO/CRIANÇAS/Crianças2.csv",
               row.names = F, sep = ";",
               fileEncoding = "latin1")
   
   

    
