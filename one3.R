#----------------------PRIMEIRO BANCO-------------------------------------

pesquisador <- read.csv("pesq-ultm.csv")
publicacoes <- read.csv("pub_ultm.csv")
publiaceitas <- read.csv("pub_a_ultm.csv")
livros <- read.csv("book_ultm.csv")
conferencias <- read.csv("conf_ultm.csv")
supervisao <- read.csv("sup_ultm.csv")


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
pq$phd.institution <- NULL
pq$msc.institution <- NULL

colnames(pq)

colnames(pq) <- c("BolsistaPQ","GrandeAea","Subarea","id.file","Ano.Info.Antiga",
                  "Anos.Doc","Anos.Msc")

#-----------------------PUBLICACOES----------------------------

#variaveis realionadas as publicacoes

publi <- publicacoes%>%
  select(name,id.file,year)%>%
  group_by(name)%>%
  filter(year>2015)


p <- publicacoes%>%
  select(name,id.file, year)%>%
  group_by(name)%>%
  filter(year>2015)%>%
  summarise(NtotalArtigos = n())


#AQUI CANCELA, MUDE PARA:

publia <- publiaceitas%>%
  select(name,id.file,year)%>%
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
