#----------------------PRIMEIRO BANCO-------------------------------------

pesquisador <- read.csv("pesquisador.csv")
publicacoes <- read.csv("publicacoes.csv")
publiaceitas <- read.csv("publiaceitas.csv")
livros <- read.csv("livros.csv")
conferencias <- read.csv("conferencias.csv")
supervisao <- read.csv("supervisao.csv")


#-----------------------PESQUISADOR----------------------------------------

pq <- subset(pesquisador, select = c("name","phd.institution","phd.start.year",
                                     "phd.end.year","country.origin","major.field",
                                     "minor.field","id.file", "msc.institution", 
                                     "msc.start.year","msc.end.year", "bsc.start.year"))

pq$anosphd <- round((pq$phd.end.year-pq$phd.start.year),0)           
pq$anosmestrado <- round((pq$msc.end.year-pq$msc.start.year),0)

pq$phd.start.year <- NULL
pq$phd.end.year <- NULL
pq$msc.start.year <- NULL
pq$msc.end.year <- NULL
pq$country.origin <- NULL
pq$phdinstituicao <- NULL
pq$mestrado <- NULL

colnames(pq)

colnames(pq) <- c("BolsistaPQ","GrandeAea","Subarea","id.file","Ano.Info.Antiga","Anos.Doc","Anos.Msc")

#-----------------------PUBLICACOES----------------------------

#variaveis realionadas as publicacoes

publi <- publicacoes%>%
  select(name,id.file,year,qualis)%>%
  group_by(name)%>%
  filter(year>2015)

publia <- publiaceitas%>%
  select(name,id.file,year,qualis)%>%
  group_by(name)%>%
  filter(year>2015)

todaspublicacoes[is.na(todaspublicacoes)] <- 0 #quando usar?

#banco com tdas as publiacaoes aceitas e publicadas
todaspublicacoes <- rbind(publi, publia)


todaspublicacoes$q <- 
  ifelse((todaspublicacoes$qualis == "A1"),1,todaspublicacoes$qualis)

class(todaspublicacoes$q)

todaspublicacoes$qualisup <- 
  ifelse((todaspublicacoes$q == "1"),1,todaspublicacoes$q)

todaspublicacoes$qualisup2 <- 
  ifelse((todaspublicacoes$qualisup == "2"),1,todaspublicacoes$qualisup)

todaspublicacoes$qualisup3 <- 
  ifelse((todaspublicacoes$qualisup2 == "3"),1,todaspublicacoes$qualisup2)


todaspublicacoes$qualisup3[todaspublicacoes$qualisup3 != 1] <- 0

colnames(todaspublicacoes)[8] <- "QUALIS.SUPERIOR"

todaspublicacoes$QualisCapes <- 
  ifelse((todaspublicacoes$qualisup2 != "0"),1,0)

todaspublicacoes$q <- NULL
todaspublicacoes$qualisup <- NULL
todaspublicacoes$qualisup2 <- NULL

todaspublicacoes$QUALIS.SUPERIOR[is.na(todaspublicacoes$QUALIS.SUPERIOR)] <- 0
todaspublicacoes$QualisCapes[is.na(todaspublicacoes$QualisCapes)] <- 0
todaspublicacoes$qualis <- NULL

p <- todaspublicacoes%>%
  select(name, QUALIS.SUPERIOR, QualisCapes)%>%
  group_by(name)%>%
  summarise(QUALIS.SUPERIOR = sum(QUALIS.SUPERIOR),
            QualisCapes = sum(QualisCapes))
  
colnames(p)[1] <- "BolsistaPQ" 

tabelamerge1 <- merge(pq,p, by = "BolsistaPQ", all.x = TRUE)



#-----------------------LIVROS---------------------------

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
  group_by(name) %>%
  summarise(Nlivros = sum(publicado),
            NCapítulosLivros = sum(capitulo))

colnames(w)[1] <- "BolsistaPQ"

tabela_merge2 <- merge(tabelamerge1,w , by="BolsistaPQ", all.x =TRUE)

#-----------------------CONFERENCIAS--------------------------------------------

eve <- subset(conferencias, select = c("id.file", "name", "article.year", "article.title"))

e <- eve%>%
  select(name,article.year,article.title)%>%
  filter(article.year > 2015)

e$Eventos <- 1
e$article.year <- NULL
e$article.title <- NULL

e <- e%>%group_by(name)%>%
  summarise(NTotalEventos = n())

colnames(e) <- c("BolsistaPQ","NTextos")

tabela_merge3 <- merge(tabela_merge2, e, by ="BolsistaPQ", all.x =TRUE)


#-----------------------SUPERVISÃO----------------------------------

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

colnames(r)[1] <- ("BolsistaPQ")

tabela_merge4 <- merge(tabela_merge3, r, by="BolsistaPQ", all.x = TRUE)

#-----------------------PROGREDINDO PARA O FIM-----------------------------------

#NÃO VERIFIQUE ISSO AGORA

#excluindo casos duplicados

duplicados <- duplicated(tabela_merge8,fromLast = TRUE)
which(duplicados)
tabela_merge8 <- tabela_merge8[!duplicados,] 


#ainda coma ausência de três casos, segue:

m <- Reduce(rbind(tabela_merge5, tabela_merge8))


#okays, voltando para o merge

tabela_merge10 <- merge(tabela_merge8, tabela_merge9, by="Nome", all.x = TRUE)

duplicados <- duplicated(tabela_merge10,fromLast = TRUE)
which(duplicados)
tabela_merge10 <- tabela_merge10[!duplicados,] 

tabela_merge11 <- Reduce(rbind, list(tabela_merge5, tabela_merge10))

#-----------------------REFAZENDO------------------------------------

colnames(tabela_merge11)[1] <- "BolsistaPQ"
colnames(tabela_merge11)

tabela_merge5 <- merge(tabela_merge11,tabela_merge4, by= "BolsistaPQ", all.x = TRUE)
colnames(tabela_merge5)

tabela_merge5$OrientacoesConcluidas.y <- NULL
tabela_merge5$NTextos.y <- NULL
tabela_merge5$Nlivros.y <- NULL
tabela_merge5$QualisCapes.y <- NULL
tabela_merge5$QUALIS.SUPERIOR <- NULL
tabela_merge5$NCapítulosLivros <- NULL
colnames(tabela_merge5)[3] <- "QualisCapes"
colnames(tabela_merge5)[7] <- "Ntextos"
colnames(tabela_merge5)[5] <- "Nlivros"
colnames(tabela_merge5)[4] <- "QUALIS.SUPERIOR"
colnames(tabela_merge5)[9] <- "OrientacoesConcluidas"
colnames(tabela_merge5)[10] <- "GrandeArea"

#verificando casos duplicados

duplicados <- duplicated(tabela_merge5,fromLast = TRUE)
which(duplicados)
tabela_merge5 <- tabela_merge5[!duplicados,] 

tabela_merge3$id.file <- NULL

colnames(tabela_merge3)[9] <- "NCapitulosLivros"

tabela_merge3$QualisCapes <- 0
tabela_merge3$QUALIS.SUPERIOR <- 0
tabela_merge3$OrientacoesConcluidas <- 0
tabela_merge5$id.file <- NULL
tabela_merge5$Ano.Info.Antiga <- NULL
colnames(tabela_merge5)[8] <- "Ano.Info.Antiga"

colnames(tabela_merge3)[2] <- "GrandeArea"

colnames(tabela_merge3)
colnames(tabelamerge_6)


tabelamerge_6 <- subset(tabela_merge5, select = c("BolsistaPQ","GrandeArea","Subarea",
                                                  "Ano.Info.Antiga", "Anos.Doc","Anos.Msc",
                                                  "NTotalArtigos","Nlivros","NCapitulosLivros",
                                                 "Ntextos","QualisCapes","QUALIS.SUPERIOR",
                                                 "OrientacoesConcluidas"))

colnames(tabelamerge_6)[7] <- "NtotalArtigos"
colnames(tabelamerge_6)[10] <- "NTextos"

#AGORA VAMOS PARA O RBIND

tabela_merge7 <- Reduce(rbind, list(tabela_merge3, tabelamerge_6 ))


#-----------------------BANCAS------------------------------

bancas <- read.csv("tbancas.csv")

colnames(bancas)

#MESTRADO
bancmestrado <- bancas %>%
  select(name,natureza,tipo,ano)%>%
  filter(natureza == "Mestrado")%>%
  filter(ano > 2015)

bancmestrado$ano <- NULL
bancmestrado$tipo <- NULL

bancmestrado <- bancmestrado%>%
  select(name,natureza)%>%
  group_by(name)%>%
  summarise(BancasMestrado = n())

#DOUTORADO
bancdoutorado <- bancas %>%
  select(name,natureza,tipo,ano)%>%
  filter(natureza == "Doutorado")%>%
  filter(ano > 2015)

bancdoutorado$ano <- NULL
bancdoutorado$tipo <- NULL

bancdoutorado <- bancdoutorado%>%
  select(name,natureza)%>%
  group_by(name)%>%
  summarise(BancasDoutorado = n())

#GRADUAÇÃO
bancgraduacao <- bancas %>%
  select(name,natureza,tipo,ano)%>%
  filter(natureza == "Graduação")%>%
  filter(ano > 2015)

bancdoutorado$ano <- NULL
bancdoutorado$tipo <- NULL

bancgraduacao <- bancgraduacao%>%
  select(name,natureza)%>%
  group_by(name)%>%
  summarise(BancasGraduação = n())

#ESPECIALIZAÇÃO
bancespecializacao <- bancas %>%
  select(name,natureza,tipo,ano)%>%
  filter(natureza == "Curso de aperfeiçoamento/especialização")%>%
  filter(ano > 2015)

bancdoutorado$ano <- NULL
bancdoutorado$tipo <- NULL

bancespecializacao <- bancespecializacao%>%
  select(name,natureza)%>%
  group_by(name)%>%
  summarise(BancasEspecialização = n())

b1 <- merge(bancdoutorado, bancmestrado, by = "name", all = TRUE)
b2 <- merge(b1, bancgraduacao, by = "name", all = TRUE)
b3 <- merge(b2, bancespecializacao, by = "name", all = TRUE)


b3[is.na(b3)] <- 0

#-----------------------EVENTOS------------------

eventos <- read.csv("teventos.csv")
colnames(eventos)

even <- eventos %>%
  select(name,natureza,ano,pais)%>%
  filter(ano > 2015)

even$pais <- as.character(even$pais)
class(even$pais)

even$inter <- ifelse(even$pais != "Brasil", even$pais, NA)

even$natureza <- NULL
even$ano <- NULL
even$pais <- NULL

internaciona <- even

duplicados <- duplicated(internaciona,fromLast = TRUE)
which(duplicados)
internaciona <- internaciona[!duplicados,]

internaciona <- na.omit(internaciona)
#VOCE DEVE PROCURAR COMO REDUZ ESSE DATA.FRAME, voltemos para:

even <- eventos %>%
  select(name,natureza,ano,pais)%>%
  filter(ano > 2015)

even[2:4] <- NULL

even <- even%>%
  group_by(name)%>%
  summarise(Eventos = n())


tabela_merge9 <- merge(b3, even, by = "name", all = TRUE)
tabela_merge9[is.na(tabela_merge9)] <- 0


#-----------------------ENTREVISTAS----------------------
midiaticas <- read.csv("trevist.csv")
colnames(midiaticas)

midia <- midiaticas%>%
  select(name, ano)%>%
  filter(ano > 2015)%>%
  group_by(name)%>%
  summarise(PublicacoesMidiaticas = n())

tabela_merge12 <- merge(tabela_merge9, midia, by = "name", all = TRUE)
tabela_merge12[is.na(tabela_merge12)] <- 0

colnames(tabela_merge12)[1] <- "BolsistaPQ"

tabela_merge13 <- merge(tabela_merge7,tabela_merge12, by="BolsistaPQ", all = TRUE)

#-----------------------SELECT PARA CASOS COM NA---------------

tabela1 <- tabela_merge13[order(tabela_merge13$GrandeArea, decreasing=TRUE),]
write.csv(tabela1, "TABELA1.csv")

#-----------------------tabela2------------------------------------
tabela2 <- tabela_merge13[c(5,
                            31,
                            32,
                            43,
                            45,
                            46,
                            48,
                            52,
                            53,
                            58,
                            94,
                            98,
                            99,
                            104,
                            115,
                            119,
                            149,
                            156,
                            188,
                            189,
                            190,
                            191,
                            196,
                            214,
                            241,
                            257,
                            258,
                            278,
                            286,
                            291,
                            294,
                            297,
                            302,
                            337,
                            339,
                            348,
                            349,
                            357,
                            364,
                            367,
                            370,
                            372,
                            381,
                            392,
                            400,
                            413,
                            417,
                            423,
                            437,
                            444,
                            463,
                            471,
                            485,
                            487,
                            490,
                            502,
                            508,
                            513,
                            526,
                            532,
                            540,
                            544,
                            561,
                            570),]

tabela_merge14 <- tabela_merge13[-linhas,]

#MERGE DA TABELA 2 COM AS NOVAS VARIAVEIS

tabela6 <- merge(tabela2, pq, by="BolsistaPQ", all.x = TRUE)



#RBIND COM TABELA_MERGE14



#-----------------------tabela3-----------------------------
tabela3 <- tabela_merge13[c(63,
                            152,
                            161,
                            247,
                            249,
                            323,
                            325,
                            436,
                            101,
                            129,
                            142,
                            212,
                            330,
                            428,
                            461,
                            555,
                            589,
                            594,
                            47,
                            66,
                            88,
                            121,
                            140,
                            148,
                            165,
                            179,
                            265,
                            328,
                            353,
                            356,
                            385,
                            404,
                            430,
                            431,
                            439,
                            486,
                            501,
                            504,
                            505,
                            542,
                            573,
                            596,
                            38,
                            51,
                            68,
                            93,
                            110,
                            116,
                            137,
                            208,
                            215,
                            263,
                            290,
                            354,
                            368,
                            374,
                            399,
                            510,
                            525,
                            533,
                            580,
                            585,
                            586,
                            25,
                            276,
                            344,
                            379,
                            441,
                            489,
                            537,
                            587,
                            592,
                            22,
                            39,
                            118,
                            124,
                            141,
                            231,
                            329,
                            371,
                            375,
                            376,
                            397,
                            416,
                            445,
                            449,
                            477,
                            500,
                            147,
                            447,
                            1,
                            2,
                            3,
                            46,
                            94,
                            98,
                            119,
                            189,
                            191,
                            278,
                            297,
                            337,
                            348,
                            349,
                            357,
                            364,
                            367,
                            372,
                            392,
                            400,
                            413,
                            437,
                            444,
                            485,
                            490,
                            508,
                            513,
                            532,
                            540),]

write.csv()

#-----------------------continua----------------------


amostra <- read.csv2("PQSamostraUTF.csv", encoding = "UTF-8")

duplicados <- duplicated(amostra,fromLast = TRUE)
which(duplicados)
amostra <- amostra[!duplicados,]

colnames(amostra)[1] <- "BolsistaPQ"

linhas <- data.frame(BolsistaPQ = c("Ademar Dutra","Adriana Roseli Wunsch Takahashi", "Alberto Luiz Albertin"), 
                     Nivel = c("PQ-2", "PQ-2","PQ-2"),
                     Instituicao = c("UFSC", "UFPR", "FDV-SP"),
                     Area = c("ADMINISTRACAO", "ADMINISTRACAO", "ADMINISTRACAO"))

amostra <- rbind(amostra, linhas)


#MERGE PARA ATUAIS VARIAVEIS QUE TENHO

tabela_merge15 <- merge(tabela_merge13, amostra, by="BolsistaPQ", all = TRUE)

colnames(tabela_merge15)

tabela_merge15 <- subset(tabela_merge15, select = c("BolsistaPQ","Nivel","Area","GrandeArea",
                                                    "Subarea","Ano.Info.Antiga", "Instituicao",
                                                    "Anos.Doc","Anos.Msc","NtotalArtigos","Nlivros",
                                                    "NCapitulosLivros","NTextos","QualisCapes",
                                                    "QUALIS.SUPERIOR","OrientacoesConcluidas",
                                                    "BancasDoutorado", "BancasMestrado",
                                                    "BancasGraduação", "BancasEspecialização",
                                                    "Eventos", "PublicacoesMidiaticas"))

#-----------------------APRESENTACOES-----------------------------------

colnames(eventos)

even <- eventos %>%
  select(name,natureza,titulo,ano,pais)%>%
  filter(ano > 2015)

even$titulo <- ifelse((even$titulo == ""),NA,even$titulo)

even <- na.omit(even)

even[2:5] <- NULL

even <- even%>%
  group_by(name)%>%
  summarise(Napresentacoes = n())


colnames(even)[1] <- "BolsistaPQ"

tabela_merge16 <- merge(tabela_merge15, even, by = "BolsistaPQ", all.x = TRUE)

#-----------------------INTERNACIONALIZACAO---------------------------------------

duplicados <- duplicated(internaciona,fromLast = TRUE)
which(duplicados)
internaciona <- internaciona[!duplicados,]

internaciona <- na.omit(internaciona)
colnames(internaciona)[1] <- "BolsistaPQ"

write.csv(internaciona,"internacionalizacao.csv", row.names = FALSE)

df <- import(file = "internacionalizacao.xlsx")

colnames(df)[2] <- "Internacionalizacao"


#------------------------MERGE17---------------------------------------------------

tabela_merge17 <- merge(tabela_merge16, df, by = "BolsistaPQ", all.x = TRUE)

tabela_merge17$Subarea <- NULL

colnames(tabela_merge17)[3] <- "Subarea"

write.csv(tabela_merge17,"PQS601.csv", row.names = FALSE)

#-----------------------REGIAO----------------------------------
ies <- import("DADOS_IES_REGS.csv")

ies$DescricaoInstituicao <- str_to_upper(ies$NO_IES)
ies$Instituicao <- str_to_upper(ies$SG_IES)

tabela_merge17$Instituicao <- str_to_upper(tabela_merge17$Instituicao)

tabela_merge17$Instituicao[tabela_merge17$Instituicao == "PUC/SP"] <- "PUCSP"
tabela_merge17$Instituicao[tabela_merge17$Instituicao == "PUC/PR"] <- "PUCPR"

ies$Regiao <- factor(ies$CO_REGIAO, levels = c("1", "2" , "3", "4", "5"),
                     labels = c("Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste"))

ies$CategoriaAdministrativa <- factor(ies$TP_CATEGORIA_ADMINISTRATIVA, levels = c("1", "2" , "3", "4", "5","6", "7", "8", "9"),
                                      labels = c("Pública Federal", "Pública Estadual", "Pública Municipal", "Privada com fins lucrativos",
                                                 "Privada sem fins lucrativos", "Privada - Particular em sentido estrito",
                                                 "Especial", "Privada comunitária", "Privada confessional"))
ies[1:3] <- NULL
colnames(ies)

#ordenando novo subset

ies_rec <- subset(ies, select = c("Instituicao","DescricaoInstituicao","CO_REGIAO","Regiao",
                                  "TP_CATEGORIA_ADMINISTRATIVA", "CategoriaAdministrativa","CO_UF"))
colnames(ies_rec)[3] <- "CodigoRegiao"
colnames(ies_rec)[5] <- "CodigoCategoriaAsministrativa"
colnames(ies_rec)[7] <- "CodigoUF"

tabela_merge18 <- merge(tabela_merge17, ies_rec, by = "Instituicao", all.x = TRUE)



#modificando alguma sinstituicoes que não fizeram merge

#-----------------------TIPO_EMPREGO------------------------------------------

tabela_merge18$emprego1 <- 
  ifelse((tabela_merge18$CategoriaAdministrativa == "Pública Federal"),
         1,NA)

tabela_merge18$emprego2 <- 
  ifelse((tabela_merge18$CategoriaAdministrativa == "Pública Estadual"),
         1,tabela_merge18$emprego1)

tabela_merge18$emprego3 <- 
  ifelse((tabela_merge18$CategoriaAdministrativa == "Pública Municipal"),
         1,tabela_merge18$emprego2)


tabela_merge18$emprego4 <- 
  ifelse((tabela_merge18$CategoriaAdministrativa == "Privada com fins lucrativos"),
         2,tabela_merge18$emprego3)

tabela_merge18$emprego5 <- 
  ifelse((tabela_merge18$CategoriaAdministrativa == "Privada sem fins lucrativos"),
         2,tabela_merge18$emprego4)

tabela_merge18$emprego6 <- 
  ifelse((tabela_merge18$CategoriaAdministrativa == "Privada - Particular em sentido estrito"),
         2,tabela_merge18$emprego5)

tabela_merge18$emprego7 <- 
  ifelse((tabela_merge18$CategoriaAdministrativa == "Privada confessional"),
         2,tabela_merge18$emprego6)

colnames(tabela_merge18)
tabela_merge18[30:35] <- NULL
colnames(tabela_merge18)[30] <- "CodigoTipoEmprego"

tabela_merge18$TipoEmprego <- factor(tabela_merge18$CodigoEmprego, levels = c("1", "2"),
                                     labels = c("Professor de Universidade Pública", 
                                                "Professor de Universidade Privada ou TS"))
#excluindo categorias administrativas
tabela_merge18[27:28] <- NULL


#------------------------GRAPHICS-------------------------------------
colnames(Base_de_Dados_PQs_em_Sociologia_Brasil_18_12_2019)

socio <- Base_de_Dados_PQs_em_Sociologia_Brasil_18_12_2019

socio$Tipo.Bolsa1 <- as.character(socio$Tipo.Bolsa)
socio$Tipo.Bolsa1 <- as.numeric(socio$Tipo.Bolsa)

socio$Tipo.Bolsa <- factor(socio$Tipo.Bolsa1, levels = c("1", "2" , "3", "4", "5", "6"),
                                  labels = c("PQ-1D",
                                             "PQ-1C",
                                             "PQ-1B",
                                             "PQ-1A",
                                             "PQ-SR",
                                             "PQ-2"))

socio$Sexo <- as.character(socio$Sexo)
socio$Sexo <- as.numeric(socio$Sexo)
socio$Rec.Sexo <- factor(socio$Sexo, levels = c("1", "2"),
                               labels = c("Feminino",
                                          "Masculino"))



  

ggplot(data=socio, aes(x= Tipo.Bolsa1, fill = Tipo.Bolsa)) + 
  geom_bar()+
  facet_grid(.~Rec.Sexo)+
  labs(x = "Categorias", y = "Contagem")+
  scale_y_continuous(expand = c(0,0), breaks = c(0, 10,20,30,40,50,60))+
  scale_fill_brewer()+
  ggtitle("Gráfico de barras")+
  theme(plot.title = element_text(hjust = -0.07, size = 13))+
  theme(axis.text.y = black.bold.12.text)

  
  
  


#------------------------AREA DE QUALIS----------------------------------------

tabela_merge18$avcapes1 <- 
    ifelse((tabela_merge18$Subarea == "AGRONOMIA"),"CIÊNCIAS AGRÁRIAS I",NA)

tabela_merge18$avcapes2 <- 
  ifelse((tabela_merge18$Subarea == "ARQUITETURA E URBANISMO"),'ARQUITETURA, URBANISMO E DESIGN',tabela_merge18$avcapes1)

tabela_merge18$avcapes3 <- 
  ifelse((tabela_merge18$Subarea == "ANTROPOLOGIA"),'ANTROPOLOGIA / ARQUEOLOGIA',tabela_merge18$avcapes2)

tabela_merge18$avcapes4 <- 
  ifelse((tabela_merge18$Subarea == "ARQUEOLOGIA"),'ANTROPOLOGIA / ARQUEOLOGIA',tabela_merge18$avcapes3)

tabela_merge18$avcapes5 <- 
  ifelse((tabela_merge18$Subarea == "ARTES"),'ARTES / MÚSICA',tabela_merge18$avcapes4)

tabela_merge18$avcapes6 <- 
  ifelse((tabela_merge18$Subarea == "ASTRONOMIA"),'ASTRONOMIA / FÍSICA',tabela_merge18$avcapes5)

tabela_merge18$avcapes7 <- 
  ifelse((tabela_merge18$Subarea == "BIOFISICA"),'ASTRONOMIA / FÍSICA',tabela_merge18$avcapes6)

tabela_merge18$avcapes8 <- 
  ifelse((tabela_merge18$Subarea == "BIOQUIMICA"),'CIÊNCIAS BIOLÓGICAS II',tabela_merge18$avcapes7)

tabela_merge18$avcapes9 <- 
  ifelse((tabela_merge18$Subarea == "BOTANICA"),'BIODIVERSIDADE',tabela_merge18$avcapes8)

tabela_merge18$avcapes10 <- 
  ifelse((tabela_merge18$Subarea == "CIENCIA DA COMPUTACAO"),'CIÊNCIA DA COMPUTAÇÃO',tabela_merge18$avcapes9)

tabela_merge18$avcapes11 <- 
  ifelse((tabela_merge18$Subarea == "CIENCIA DA INFORMACAO"),"COMUNICACÃO E INFORMAÇÃO",tabela_merge18$avcapes10)

tabela_merge18$avcapes12 <- 
  ifelse((tabela_merge18$Subarea == "CIENCIA E TECNOLOGIA DE ALIMENTOS"),'CIÊNCIA DE ALIMENTOS',tabela_merge18$avcapes11)

tabela_merge18$avcapes13 <- 
  ifelse((tabela_merge18$Subarea == "CIENCIA POLITICA"),'CIÊNCIA POLÍTICA E RELAÇÕES INTERNACIONAIS',tabela_merge18$avcapes12)

tabela_merge18$avcapes14 <- 
  ifelse((tabela_merge18$Subarea == "COMUNICACAO"),'COMUNICÃO E INFORMAÇÃO',tabela_merge18$avcapes13)

tabela_merge18$avcapes15 <- 
  ifelse((tabela_merge18$Subarea == "DEMOGRAFIA"),
         'PLANEJAMENTO URBANO E REGIONAL / DEMOGRAFIA',tabela_merge18$avcapes14)

tabela_merge18$avcapes16 <- 
  ifelse((tabela_merge18$Subarea == "DIREITO"),
         'DIREITO',tabela_merge18$avcapes15)

tabela_merge18$avcapes17 <- 
  ifelse((tabela_merge18$Subarea == "ECOLOGIA"),
         'BIODIVERSIDADE',tabela_merge18$avcapes16)

tabela_merge18$avcapes18 <- 
  ifelse((tabela_merge18$Subarea == "ECONOMIA"),
         "ECONOMIA",tabela_merge18$avcapes17)

tabela_merge18$avcapes19 <- 
  ifelse((tabela_merge18$Subarea == "EDUCACAO"),
         "EDUCAÇÃO",tabela_merge18$avcapes18)

tabela_merge18$avcapes20 <- 
  ifelse((tabela_merge18$Subarea == "EDUCACAO FISICA"),
         "EDUCAÇÃO FÍSICA",tabela_merge18$avcapes19)

colnames(tabela_merge18)
tabela_merge18[30:48] <- NULL

tabela_merge18$avcapes21 <- 
  ifelse((tabela_merge18$Subarea == "ENFERMAGEM"),
         "ENFERMAGEM",tabela_merge18$avcapes20)

tabela_merge18$avcapes22 <- 
  ifelse((tabela_merge18$Subarea == "ENGENHARIA AEROESPACIAL"),
         'ENGENHARIAS III',tabela_merge18$avcapes21)

tabela_merge18$avcapes23 <- 
  ifelse((tabela_merge18$Subarea == "ENGENHARIA AGRICOLA"),
         'CIENCIAS AGRARIAS I',tabela_merge18$avcapes22)

tabela_merge18$avcapes24 <- 
  ifelse((tabela_merge18$Subarea == "ENGENHARIA AMBIENTAL"),
         'ENGENHARIAS I',tabela_merge18$avcapes23)

tabela_merge18$avcapes25 <- 
  ifelse((tabela_merge18$Subarea == "ENGENHARIA CIVIL"),
         'ENGENHARIAS I',tabela_merge18$avcapes24)

tabela_merge18$avcapes26 <- 
  ifelse((tabela_merge18$Subarea == "ENGENHARIA DE MATERIAIS E METALURGICA"),
         'ENGENHARIAS II',tabela_merge18$avcapes25)

tabela_merge18$avcapes27 <- 
  ifelse((tabela_merge18$Subarea == "ENGENHARIA DE PRODUCAO"),
         'ENGENHARIAS III',tabela_merge18$avcapes26)

tabela_merge18$avcapes28 <- 
ifelse((tabela_merge18$Subarea == "ENGENHARIA DE TRANSPORTES"),
       'ENGENHARIAS I',tabela_merge18$avcapes27)

tabela_merge18$avcapes29 <- 
  ifelse((tabela_merge18$Subarea == "ENGENHARIA ELETRICA E MICROELETRONICA"),
         "ENGENHARIAS IV",tabela_merge18$avcapes28)

tabela_merge18$avcapes30 <- 
  ifelse((tabela_merge18$Subarea == "ENGENHARIA MECANICA"),
         'ENGENHARIAS III',tabela_merge18$avcapes29)

colnames(tabela_merge18)
tabela_merge18[30:39] <- NULL


tabela_merge18$avcapes31 <- 
  ifelse((tabela_merge18$Subarea == "ENGENHARIA NUCLEAR"),
         'ENGENHARIAS II',tabela_merge18$avcapes30)

tabela_merge18$avcapes32 <- 
  ifelse((tabela_merge18$Subarea == "ENGENHARIA QUIMICA"),
         'ENGENHARIAS II',tabela_merge18$avcapes31)

tabela_merge18$avcapes33 <- 
  ifelse((tabela_merge18$Subarea == "FARMACIA"),
         'FARMÁCIA',tabela_merge18$avcapes32)

tabela_merge18$avcapes34 <- 
  ifelse((tabela_merge18$Subarea == "FARMACOLOGIA"),
         "CIÊNCIAS BIOLÓGICAS II",tabela_merge18$avcapes33)

tabela_merge18$avcapes35 <- 
  ifelse((tabela_merge18$Subarea == "FILOSOFIA"),
         "FILOSOFIA",tabela_merge18$avcapes34)

tabela_merge18$avcapes36 <- 
  ifelse((tabela_merge18$Subarea == "FISICA"),
         "ASTRONOMIA / FÍSICA",tabela_merge18$avcapes35)

tabela_merge18$avcapes37 <- 
  ifelse((tabela_merge18$Subarea == "FISIOLOGIA"),
         "CIÊNCIAS BIOLÓGICAS II",tabela_merge18$avcapes36)

tabela_merge18$avcapes38 <- 
  ifelse((tabela_merge18$Subarea == "FONOAUDIOLOGIA"),
         "EDUCAÇÃO FÍSICA",tabela_merge18$avcapes37)

tabela_merge18$avcapes39 <- 
  ifelse((tabela_merge18$Subarea == "GENETICA"),
         "CIÊNCIAS BIOLÓGICAS I",tabela_merge18$avcapes38)

tabela_merge18$avcapes40 <- 
  ifelse((tabela_merge18$Subarea == "GEOGRAFIA"),
         "GEOGRAFIA'",tabela_merge18$avcapes39)
#atencao pra essa aspa que ficou aqui

colnames(tabela_merge18)
tabela_merge18[30:39] <- NULL

tabela_merge18$avcapes41 <- 
  ifelse((tabela_merge18$Subarea == "HISTORIA"),
         "HISTORIA",tabela_merge18$avcapes40)

tabela_merge18$avcapes42 <- 
  ifelse((tabela_merge18$Subarea == "IMUNOLOGIA"),
         "CIÊNCIAS BIOLÓGICAS III",tabela_merge18$avcapes41)

tabela_merge18$avcapes43 <- 
  ifelse((tabela_merge18$Subarea == "LETRAS"),
         "LETRAS / LINGUÍSTICA",tabela_merge18$avcapes42)

tabela_merge18$avcapes44 <- 
  ifelse((tabela_merge18$Subarea == "LINGUISTICA"),
         "LETRAS / LINGUÍSTICA",tabela_merge18$avcapes43)

tabela_merge18$avcapes45 <- 
  ifelse((tabela_merge18$Subarea == "MATEMATICA"),
         "MATEMÁTICA / PROBABILIDADE E ESTATÍSTICA",tabela_merge18$avcapes44)

tabela_merge18$avcapes46 <- 
  ifelse((tabela_merge18$Subarea == "MATEMATICA"),
         "MATEMÁTICA / PROBABILIDADE E ESTATÍSTICA",tabela_merge18$avcapes45)

tabela_merge18$avcapes46 <- 
  ifelse((tabela_merge18$Subarea == "MEDICINA"),
         'MEDICINA I',tabela_merge18$avcapes45)

tabela_merge18$avcapes47 <- 
  ifelse((tabela_merge18$Subarea == "MEDICINA VETERINARIA"),
         'MEDICINA VETERINARIA',tabela_merge18$avcapes46)

tabela_merge18$avcapes47 <- 
  ifelse((tabela_merge18$Subarea == "MICROBIOLOGIA"),
         'CIÊNCIAS BIOLÓGICAS III',tabela_merge18$avcapes46)

tabela_merge18$avcapes48 <- 
  ifelse((tabela_merge18$Subarea == "MORFOLOGIA"),
         'CIÊNCIAS BIOLÓGICAS II',tabela_merge18$avcapes47)

tabela_merge18$avcapes49 <- 
  ifelse((tabela_merge18$Subarea == "MUSEOLOGIA"),
         'COMUNICAÇÃO E INFORMAÇÃO',tabela_merge18$avcapes48)

tabela_merge18$avcapes50 <- 
  ifelse((tabela_merge18$Subarea == "MUSEOLOGIA"),
         'COMUNICAÇÃO E INFORMAÇÃO',tabela_merge18$avcapes49)

colnames(tabela_merge18)
tabela_merge18[30:39] <- NULL

tabela_merge18$avcapes51 <- 
  ifelse((tabela_merge18$Subarea == "NUTRICAO"),
         'NUTRIÇÃO',tabela_merge18$avcapes50)

tabela_merge18$avcapes52 <- 
  ifelse((tabela_merge18$Subarea == "OCEANOGRAFIA"),
         'BIODIVERSIDADE',tabela_merge18$avcapes51)

tabela_merge18$avcapes53 <- 
  ifelse((tabela_merge18$Subarea == "ODONTOLOGIA"),
         'ODONTOLOGIA',tabela_merge18$avcapes52)

tabela_merge18$avcapes54 <- 
  ifelse((tabela_merge18$Subarea == "PARASITOLOGIA"),
         'CIÊNCIAS BIOLÓGICAS III',tabela_merge18$avcapes53)

tabela_merge18$avcapes55 <- 
  ifelse((tabela_merge18$Subarea == "PLANEJAMENTO REGIONAL"),
         'PLANEJAMENTO URBANO E REGIONAL / DEMOGRAFIA',tabela_merge18$avcapes54)

tabela_merge18$avcapes56 <- 
  ifelse((tabela_merge18$Subarea == "PROBABILIDADE E ESTATISTICA"),
         'MATEMÁTICA / PROBABILIDADE E ESTATÍSTICA',tabela_merge18$avcapes55)

tabela_merge18$avcapes57 <- 
  ifelse((tabela_merge18$Subarea == "PSICOLOGIA"),
         'PSICOLOGIA',tabela_merge18$avcapes56)

tabela_merge18$avcapes58 <- 
  ifelse((tabela_merge18$Subarea == "QUIMICA"),
         'QUIMICA',tabela_merge18$avcapes57)

tabela_merge18$avcapes59 <- 
  ifelse((tabela_merge18$Subarea == "RECURSOS FLORESTAIS E ENGENHARIA FLORESTAL"),
         'CIÊNCIAS AGRÁRIAS I',tabela_merge18$avcapes58)

tabela_merge18$avcapes60 <- 
  ifelse((tabela_merge18$Subarea == "SAUDE COLETIVA"),
         'SAÚDE COLETIVA',tabela_merge18$avcapes59)

tabela_merge18$avcapes61 <- 
  ifelse((tabela_merge18$Subarea == "SERVICO SOCIAL"),
         'SERVIÇO SOCIAL',tabela_merge18$avcapes60)

tabela_merge18$avcapes62 <- 
  ifelse((tabela_merge18$Subarea == "SOCIOLOGIA"),
         'SOCIOLOGIA',tabela_merge18$avcapes61)

tabela_merge18$avcapes63 <- 
  ifelse((tabela_merge18$Subarea == "ZOOLOGIA"),
         'BIODIVERSIDADE',tabela_merge18$avcapes62)

tabela_merge18$avcapes64 <- 
  ifelse((tabela_merge18$Subarea == "ZOOTECNIA"),
         'ZOOTECNIA / RECURSOS PESQUEIROS',tabela_merge18$avcapes63)

colnames(tabela_merge18)
tabela_merge18[30:43] <- NULL

colnames(tabela_merge18)[30] <- "Av.Capes"

tabela_merge18$Av.Capes2 <- 
  ifelse((tabela_merge18$Subarea == "MEDICINA VETERINARIA"),
         'MEDICINA VETERINÁRIA',tabela_merge18$Av.Capes)

tabela_merge18$Av.Capes3 <- 
  ifelse((tabela_merge18$Subarea == "RECURSOS PESQUEIROS E ENGENHARIA DA PESCA"),
         'ZOOTECNIA / RECURSOS PESQUEIROS',tabela_merge18$Av.Capes2)

tabela_merge18$Av.Capes4 <- 
  ifelse((tabela_merge18$Subarea == "ADMINISTRACAO"),
         'ADMINISTRAÇÃO PÚBLICA E DE EMPRESAS, CIÊNCIAS CONTÁBEIS E TURISMO',tabela_merge18$Av.Capes3)

tabela_merge18[30:32] <- NULL

colnames(tabela_merge18)[28] <- "CodigoEmprego"

tabela_merge18$TipoEmprego <- NULL

#------------------------NOTA CAPES-----------------------------------
write.csv(tabela_merge18, "tabela_merge18.csv", row.names = FALSE)

tabela_merge19 <- read.csv("BASE_PQS_18_04_2020.csv", encoding = "UTF-8")


colnames(adm_xlsx_Programa)[10] <- "Av.Capes"


nota_capes <- rbind(adm_xlsx_Programa,
                    antropo_xlsx_Programa,
                    arq_xlsx_Programa,
                    artes_xlsx_Programa,
                    astro_xlsx_Programa,
                    biodiver_xlsx_Programa,
                    cagrarias1_xlsx_Programa,
                    calimento_xlsx_Programa,
                    cbio1_xlsx_Programa_1_,
                    cbio2_xlsx_Programa_1_,
                    cbio3_xlsx_Programa,
                    ccomp_xlsx_Programa,
                    comunicinfo_xlsx_Programa,
                    cp_xlsx_Programa,
                    direito_xlsx_Programa,
                    econo_xlsx_Programa,
                    educ_fisc_xlsx_Programa,
                    educ_xlsx_Programa,
                    enferm_xlsx_Programa,
                    engen1_xlsx_Programa,
                    engen2_xlsx_Programa,
                    engen3_xlsx_Programa,
                    engen4_xlsx_Programa,
                    farma_xlsx_Programa,
                    filo_xlsx_Programa,
                    geografia_xlsx_Programa,
                    hist_xlsx_Programa,
                    linguisticaelit_xlsx_Programa,
                    matprob_xlsx_Programa,
                    med1_xlsx_Programa,
                    medvet_xlsx_Programa,
                    nutri_xlsx_Programa,
                    odonto_xlsx_Programa,
                    planejamedemografia_xlsx_Programa,
                    psico_xlsx_Programa,
                    quimic_Programa,
                    saudecoletiva_xlsx_Programa,
                    socio_xlsx_Programa,
                    sso_xlsx_Programa,
                    zootecpesqueiros_xlsx_Programa)

colnames(nota_capes)[3] <- "DescricaoInstituicao"

nota_capes$DP <- NULL
nota_capes$MP <- NULL

duplicados <- duplicated(nota_capes,fromLast = TRUE)
which(duplicados)
nota_capes <- nota_capes[!duplicados,]
nota_capes$ME <- NULL

str(nota_capes$DO)

nota_capes$DO[nota_capes$DO == "-"] <- NA

nota_capes <- na.omit(nota_capes)


tabela_merge20 <- merge(tabela_merge19, nota_capes, 
                        by=c("DescricaoInstituicao", "Av.Capes"), all.x = TRUE)

#----------------------TABELA_FINAL---------------------------------------------------------

tabela_merge21 <- read.csv("PQS_BRASIL_22_04_2020.csv", encoding = "UTF-8")

tabela_merge21$Rec1 <- 
  ifelse((tabela_merge21$Tipo.Bolsa == "PQ-1D"),
         1,NA)

tabela_merge21$Rec2 <- 
  ifelse((tabela_merge21$Tipo.Bolsa == "PQ-1C"),
         2,tabela_merge21$Rec1)

tabela_merge21$Rec3 <- 
  ifelse((tabela_merge21$Tipo.Bolsa == "PQ-1B"),
         3,tabela_merge21$Rec2)

tabela_merge21$Rec4 <- 
  ifelse((tabela_merge21$Tipo.Bolsa == "PQ-1A"),
         4,tabela_merge21$Rec3)

tabela_merge21$Rec5 <- 
  ifelse((tabela_merge21$Tipo.Bolsa == "PQ-SR"),
         5,tabela_merge21$Rec4)

tabela_merge21$Rec6 <- 
  ifelse((tabela_merge21$Tipo.Bolsa == "PQ-2"),
         6,tabela_merge21$Rec5)


tabela_merge21[33:37] <- NULL

colnames(tabela_merge21)[33] <- "Rec.Tipo.Bolsa"

write.csv2(tabela_merge21, "PQS_BRASIL_22_04_18H.csv", 
          row.names = FALSE)



#LENDO OUTRO ARQUIVO 
install.packages("memisc")
library(memisc)
dataset <- data.frame(as.data.set(spss.system.file("Sem outliers.sav")))
