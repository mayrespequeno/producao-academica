#----------------Dados Utilizados e Pacotes requisitados-----------------------
install.packages("readxl")
install.packages("stringr") 

#----------------------HUMANIDADES---------------------------------

Economia_Domestica$Curso[Economia_Domestica$Curso == "Em folha de pagamento"] <- "Economia Domestica"

Arquitetura_e_Urbanismo$Curso[Arquitetura_e_Urbanismo$Curso == "Em folha de pagamento"] <- "Arquitetura e Urbanismo"

Teologia$X5 <- NULL
Teologia$X6 <- NULL
Teologia$X6 <- NULL
Teologia$X7 <- NULL
Teologia$X8 <- NULL
Teologia$X9 <- NULL
Teologia$X10 <- NULL

humanidades = rbind(antropologia,Arqueologia, Arquitetura_e_Urbanismo, Artes, Ciencia_da_Informacao,
                    Ciencia_Politica, Comunicacao, Demografia, Direito, Economia, Educacao,
                    Filosofia, Geografia, Historia, Letras, Linguistica, Museologia, Planejamento_Regional,
                    Psicologia, Servico_Social,Sociologia, Turismo, Teologia, Economia_Domestica)

humanidades <- na.omit(humanidades)

#-------------------------EXATAS---------------------------------------

engenharia_ambiental$X5 <- NULL

engenharia_biomedica$X5 <- NULL
engenharia_biomedica$X6 <- NULL
engenharia_biomedica$X7 <- NULL
engenharia_biomedica$X8 <- NULL
engenharia_biomedica$X9<- NULL
engenharia_biomedica$X10 <- NULL
engenharia_biomedica$X11 <- NULL
engenharia_biomedica$X12 <- NULL
engenharia_biomedica$X13 <- NULL
engenharia_biomedica$X14 <- NULL

engenharia_eletrica$X5 <- NULL
engenharia_nuclear$Situação <- NULL

exatas = rbind(astronomia, ciencia_da_computacao, desenho_industrial, engenharia_aeroespacial,
               engenharia_ambiental, engenharia_biomedica, engenharia_civil, engenharia_eletrica,
               engenharia_materiais_e_metal, engenharia_mecanica, engenharia_minas, engenharia_naval_e_oceanica,
               engenharia_nuclear, engenharia_producao, engenharia_quimica, engenharia_transportes, probabilidade_e_estatistica,
               quimica,fisica, matematica, oceanografia)


exatas <- na.omit(exatas)

#-------------------------VIDA-------------------------------------
colnames(VIDA_biofisica)[4] <- "curso"
colnames(VIDA_nutricao)[4] <- "curso"


ciencias_vida = rbind(VIDA_agronomia,VIDA_aquicultura, VIDA_biofisica, VIDA_biologia_geral,
                      VIDA_bioquimica, VIDA_botanica, VIDA_ciencia_e_tecnologia_de_alimentos,
                      VIDA_ecologia, VIDA_educacao_fisica, VIDA_enfermagem,
                      VIDA_engenharia_agricola, VIDA_farmacia, VIDA_farmacologia,
                      VIDA_fisiologia, VIDA_fisioterapia_e_terapia_ocupacional,
                      VIDA_fonoaudiologia, VIDA_genetica, VIDA_imunologia,
                      VIDA_medicina, VIDA_medicina_veterinaria, VIDA_microbiologia,
                      VIDA_morfologia, VIDA_nutricao, VIDA_odontologia, VIDA_parasitologia,
                      VIDA_recursos_florestais_e_engenharia_florestal, VIDA_recursos_pesqueiros_e_engenharia_da_pesca,
                      VIDA_saude_coletiva, VIDA_zoologia, VIDA_zootecnia)


vida <- na.omit(ciencias_vida)


#-------------------------ALEATORIO--------------------------------

pesquisadoresbrasil <- rbind(exatas,humanidades,vida)
write.csv2(pesquisadoresbrasil, "pesquisadores_brasil.csv", row.names = FALSE)

view(vida)
colnames(vida)[4] <- "Curso"





exatas.amostra<-exatas[sample(nrow(exatas), 200),]
humanidades.amostra<-humanidades[sample(nrow(humanidades),200),]
vida.amostra<-vida[sample(nrow(vida),200),]

PQSamostra <- rbind(exatas.amostra, humanidades.amostra, vida.amostra)
view(PQSamostra)


colnames(vida.amostra)[4] <- "Curso"

PQSamostra$CURSO <- str_to_upper(PQSamostra$Curso)
colnames(PQSamostra)[4] <- "Area"
PQSamostra$Curso <- NULL

PQSamostra$Area <- rm_accent(PQSamostra$Area)

colnames(PQSamostra) <- c("Nome", "Nivel", "Instituicao", "Area")

write.csv2(PQSamostra, "PQSamostra.csv", row.names = FALSE)



#-------------------------EXTRAINDO CV------------------------

agronomia<-c('cv/agronomia/9872041193489007.zip','cv/agronomia/8792988784726241.zip',
             'cv/agronomia/8721647772738636.zip','cv/agronomia/8104143593771412.zip',
             'cv/agronomia/7587208482384059.zip','cv/agronomia/7136020582502092.zip',
             'cv/agronomia/7087372884726559.zip','cv/agronomia/6416589151349057.zip',
             'cv/agronomia/5065299168599484.zip',
             'cv/agronomia/4500797890789377.zip','cv/agronomia/3914738658851006.zip',
             'cv/agronomia/2664659658189146.zip',
             'cv/agronomia/2493049947306479.zip','cv/agronomia/2237448778462839.zip',
             'cv/agronomia/2086301050707005.zip','cv/agronomia/1959399746284558.zip',
             'cv/agronomia/1635430078801792.zip','cv/agronomia/0406123914624749.zip',
             'cv/agronomia/0272708416971499.zip','cv/agronomia/0060989785573065.zip')

Agronomia<-gld_get_lattes_data_from_zip(agronomia, field.qualis ='CIÊNCIAS AGRÁRIAS I')

pubagro<-Agronomia$tpublic.published
pubagro<-pubagro%>%filter(year>2015)

#publicacoes agronomia
tabagro1<-pubagro%>%
  group_by(name)%>%
  summarise(n.artigos = n(),
            max.SJR = max(SJR, na.rm = T),
            media.SJR = mean(SJR, na.rm = T),
            n.A1.qualis = sum(qualis == 'A1', na.rm = T),
            n.A2.qualis = sum(qualis == 'A2', na.rm = T),
            mediana.autoria = median(as.numeric(order.aut), na.rm = T ))

#supervisoes agronomia
supagro <- Agronomia$tsupervisions
supagro <-supagro%>%filter(year.supervision>2015)




a <- data.frame(
  Material = c(rep(1,5), rep(2,3), rep(3,6), 4),
  Fornecedor = c("A","B","A","C","C","B","D","E","A","B","C","F","G","A","A")
)

library(dplyr)
a %>% 
  mutate(Qtde = ifelse(duplicated(.), 0, 1))

#-------------------MANIPULANDO CVS EXTARIDOS----------------

pesquisador <- read.csv("pesquisador.csv")
publicacoes <- read.csv("publicacoes.csv")
publiaceitas <- read.csv("publiaceitas.csv")
livros <- read.csv("livros.csv")
conferencias <- read.csv("conferencias.csv")
supervisao <- read.csv("supervisao.csv")


#variaveis realionadas as informações do pq

pq <- subset(pesquisador, select = c("name","phd.institution","phd.start.year",
                                     "phd.end.year","country.origin","major.field",
                                     "minor.field","id.file", "msc.institution", 
                                     "msc.start.year","msc.end.year"))

pq$anosphd <- round((pq$phd.end.year-pq$phd.start.year),0)           
pq$anosmestrado <- round((pq$msc.end.year-pq$msc.start.year),0)

pq$phd.start.year <- NULL
pq$phd.end.year <- NULL
pq$msc.start.year <- NULL
pq$msc.end.year <- NULL
pq$country.origin <- NULL

colnames(pq) <- c("nome","phd","area","subarea","id.file","mestrado","anosphd","anosmestrado")

pq<- subset(pq, select = c("id.file", "nome","phd","anosphd", "mestrado", "anosmestrado", "area","subarea"))

#variaveis realionadas as publicacoes

publi <- publicacoes%>%
  select(name,qualis,id.file,year)%>%
  group_by(name)%>%
  filter(year>2015)

publia <- publiaceitas%>%
  select(name,qualis,id.file,year)%>%
  group_by(name)%>%
  filter(year>2015)

#banco com tdas as publiacaoes aceitas e publicadas
todaspublicacoes <- rbind(publi, publia)

  
todaspublicacoes$q <- 
  ifelse((todaspublicacoes$qualis == "A1"),1,todaspublicacoes$qualis)

class(todaspublicacoes$q)
todaspublicacoes$u <- NULL

todaspublicacoes[is.na(todaspublicacoes)] <- 0


todaspublicacoes$qualisup <- 
  ifelse((todaspublicacoes$q == "1"),1,todaspublicacoes$q)

todaspublicacoes$qualisup[todaspublicacoes$qualisup > 3] <- NA
todaspublicacoes$qualisup[todaspublicacoes$qualisup < 1] <- NA


todaspublicacoes$q <-
  ifelse((todaspublicacoes$q != "0"),1,todaspublicacoes$q)

todaspublicacoes[is.na(todaspublicacoes)] <- 0

todaspublicacoes$qualisup[todaspublicacoes$qualisup > 0] <- 1


p <- todaspublicacoes%>%
  select(name,id.file,q, qualisup)%>%
  group_by(name)%>%
  summarise(n.artigos = n())


q <- todaspublicacoes%>%
  select(name,id.file,q,qualisup)%>%
  group_by(name)%>%
  summarise(NTotalArtigos = n(),
            QualisCapes = sum(q),
            QUALIS.SUPEIOR = sum(qualisup))

sum(q$NTotalArtigos)

colnames(q)[1] <- c("Nome")
colnames(pq)[2] <- c("Nome")

tabela_merge <- merge(pq, q, by="Nome", all.x=TRUE)

#variaveis realionadas aos livros

liv <- subset(livros, select = c("id.file", "name", "book.type", "book.year"))

liv$capitulo <-
  ifelse((liv$book.type == "Capítulo de livro publicado"),1,0)

liv$publicado <-
  ifelse((liv$book.type == "LIVRO_PUBLICADO"),1,liv$book.type)

liv$publicado <-
  ifelse((liv$book.type == "Capítulo de livro publicado"),1,liv$publicado)

liv$publicado[liv$publicado > 1] <- 1

liv$publicado <-
  ifelse((liv$book.type == "Capítulo de livro publicado"),0,1)

w <- liv%>%
  select(name,book.year,capitulo,publicado)%>%
  filter(book.year >2015)

  w <- w%>%
  select(name, capitulo, publicado)%>%
  summarise(Nlivros = sum(publicado),
            NCapítulosLivros = sum(capitulo))
  
  colnames(w)[1] <- "Nome"

  tabela_merge2 <- merge(tabela_merge, w, by="Nome", all.x=TRUE)
  
  #variaveis realionadas a conferencia
  
  eve <- subset(conferencias, select = c("id.file", "name", "article.year", "article.title"))
  
  e <- eve%>%
    select(name,article.year,article.title)%>%
    filter(article.year > 2015)
  
  e$Eventos <- 1
  e$article.year <- NULL
  e$article.title <- NULL

   e <- e%>%group_by(name)%>%
     summarise(NTotalEventos = n())
   
   colnames(e) <- c("Nome","NTextos")
   
   tabela_merge3 <- merge(tabela_merge2, e, by="Nome", all.x=TRUE)
   
  #variavel que fora esquecida
   
  esqueci <- subset(pesquisador, select = c("name", "bsc.start.year"))
  colnames(esqueci) <- c("Nome", "Ano.Inform.Antiga")
  
  tabela_merge4 <- merge(tabela_merge3, esqueci, by="Nome", all.x = TRUE)
  view(tabela_merge4)
    
   #variaveis realionadas a supervisao
   
  sup <- subset(supervisao, select = c("id.file", "name", "situation", "year.supervision"))
  
  r <- sup%>%
    filter(year.supervision > 2015) %>%
    filter(situation == "CONCLUIDA")
  
  r$year.supervision <- NULL
  
  r$count <- 1
  
  r<- r %>%
    select(name,count)%>%
    group_by(name) %>%
    summarise(OrientacoesConcluidas = n())
  
  colnames(r)[1] <- ("Nome")
  
  tabela_merge5 <- merge(tabela_merge4, r, by="Nome", all.x = TRUE)
  
  
  
  
  
  