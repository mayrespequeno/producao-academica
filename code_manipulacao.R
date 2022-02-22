
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
