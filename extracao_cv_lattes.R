#Utilizando o GetLattesData
devtools::install_github('msperlin/GetLattesData')
#crie um objeto com os ID's dos currículos lattes que você quer analisar
#é importante que o diretório esteja presente no objeto
#como coloquei os .zip no diretório do projeto não coloquei
cp<-c('5100245942386773.zip','5670542372454107.zip', '9421549755358841.zip',
      '3980009343505300.zip', '5838080096724721.zip', '9709510879683091.zip',
      '7338438190459190.zip')

cidadania<-c('5100245942386773.zip','5670542372454107.zip', '9421549755358841.zip')
#Faça uso da função gld_get_lattes_data_from_zip, para ler os .zip
# a função é estruturada em: o objeto com os IDS e o campo que quero analisar
gcpp<-gld_get_lattes_data_from_zip(cp, field.qualis ='CIÊNCIA POLÍTICA E RELAÇÕES INTERNACIONAIS' )

#Perceba que foi criado uma lista, com 6 dataframes, você tem os valores agregados ali.
gcpp
#Criei agora um objeto só com as pub aceitas
pub<-gcpp$tpublic.published


#filtrei por ano, usando ano apartir de 2015
pub<-pub%>%filter(year>2015)

#pedi um gráfico, ele ficou seboso pois estava com preguiça de reordenar os factor
ggplot(pub, aes(x = qualis)) +
  geom_bar(position = 'identity') + facet_wrap(~name)


#criando uma tabela com valores descritivos
tab<-pub%>%
group_by(name)%>%
  summarise(n.artigos = n(),
            max.SJR = max(SJR, na.rm = T),
            media.SJR = mean(SJR, na.rm = T),
            n.A1.qualis = sum(qualis == 'A1', na.rm = T),
            n.A2.qualis = sum(qualis == 'A2', na.rm = T),
            mediana.autoria = median(as.numeric(order.aut), na.rm = T ))
knitr::kable(tab)

agronomia<-c('cv/agronomia/9872041193489007.zip','cv/agronomia/8792988784726241.zip',
             'cv/agronomia/8721647772738636.zip','cv/agronomia/8104143593771412.zip',
             'cv/agronomia/7587208482384059.zip','cv/agronomia/7136020582502092.zip',
             'cv/agronomia/7087372884726559.zip','cv/agronomia/6416589151349057.zip',
             'cv/agronomia/5065299168599484.zip','cv/agronomia/5778107814199711.zip',
             'cv/agronomia/4500797890789377.zip','cv/agronomia/3914738658851006.zip',
             'cv/agronomia/2664659658189146.zip','cv/agronomia/3555567955507382.zip', 
             'cv/agronomia/2493049947306479.zip','cv/agronomia/2237448778462839.zip',
             'cv/agronomia/2086301050707005.zip','cv/agronomia/1959399746284558.zip',
             'cv/agronomia/1635430078801792.zip','cv/agronomia/0406123914624749.zip',
             'cv/agronomia/0272708416971499.zip','cv/agronomia/0060989785573065.zip',
             'cv/agronomia/0646280144981793.zip', 'cv/agronomia/1739441747281321.zip')
# não funcionou, Raúl Alberto Laumann não adicionou
#o msc, a função não atribui missing ao msc, removi do código
Agronomia<-gld_get_lattes_data_from_zip(agronomia, field.qualis ='CIÊNCIAS AGRÁRIAS I' )

antropo.arq<-c('cv/antro.arque/0627069259285805.zip','cv/antro.arque/1272957108821414.zip',
               'cv/antro.arque/2111200163433960.zip','cv/antro.arque/1938486524292595.zip',
               'cv/antro.arque/3066846674993272.zip','cv/antro.arque/3331234730616226.zip',
               'cv/antro.arque/4444190320161227.zip','cv/antro.arque/8685032805425734.zip',
               'cv/antro.arque/6491817482510411.zip','cv/antro.arque/4620683047125316.zip',
               'cv/antro.arque/8701649902541495.zip','cv/antro.arque/9227294437122390.zip',
               'cv/antro.arque/9918173934473073.zip','cv/antro.arque/4293140275808300.zip')
#' , não preencheu
#' a aba graduação, ou msc, o pacote não assume
#'como omisso, também corrigir
Antropo.Arq<-gld_get_lattes_data_from_zip(antropo.arq, field.qualis ='ANTROPOLOGIA / ARQUEOLOGIA' )

arquit<-c('cv/arquitetura e urbanismo/0090037095817957.zip','cv/arquitetura e urbanismo/0190250269877536.zip',
          'cv/arquitetura e urbanismo/0657974772296515.zip','cv/arquitetura e urbanismo/0959256350186452.zip',
          'cv/arquitetura e urbanismo/2481182425564161.zip','cv/arquitetura e urbanismo/3061713076071714.zip',
          'cv/arquitetura e urbanismo/5922400672416402.zip','cv/arquitetura e urbanismo/5406427546690123.zip',
          'cv/arquitetura e urbanismo/8646138560231908.zip','cv/arquitetura e urbanismo/9782385817332156.zip',
          'cv/arquitetura e urbanismo/9867808000721457.zip')
# NÃO CONTÉM A ABA MSC, mesmo erro relatado
Arquit<-gld_get_lattes_data_from_zip(arquit, field.qualis = 'ARQUITETURA, URBANISMO E DESIGN')
artes<-c('cv/artes/0292814253435774.zip','cv/artes/1650414096296319.zip',
         'cv/artes/2054847883460812.zip','cv/artes/4866833198348847.zip',
         'cv/artes/5707581316608650.zip', 'cv/artes/1253291573608039.zip')
Artes<-gld_get_lattes_data_from_zip(artes, field.qualis = 'ARTES / MÚSICA')
#, ERRO DE MSC
biof<- c('cv/biofisica/4168957754548160.zip','cv/biofisica/3040267502248395.zip')
Biof<- gld_get_lattes_data_from_zip(biof, field.qualis = 'ASTRONOMIA / FÍSICA')

bioquimica<-c('cv/bioquimica/0527652166877006.zip','cv/bioquimica/7716423349207719.zip',
              'cv/bioquimica/0957665422453699.zip','cv/bioquimica/3402195711564745.zip',
              'cv/bioquimica/9264244341601758.zip','cv/bioquimica/5080772639084492.zip',
              'cv/bioquimica/5080772639084492.zip','cv/bioquimica/0957665422453699.zip')
Bioquimica<-gld_get_lattes_data_from_zip(bioquimica, field.qualis = 'CIÊNCIAS BIOLÓGICAS II')

#'cv/bioquimica/8471949500798737.zip','cv/bioquimica/8598274096498065.zip', problema com o msc
botanica<- c('cv/botanica/0613852447482455.zip','cv/botanica/1649741077906139.zip',
             'cv/botanica/2528061531430488.zip','cv/botanica/2653496390637757.zip',
             'cv/botanica/3084343775765440.zip','cv/botanica/3386021312697854.zip',
             'cv/botanica/5838426864167473.zip','cv/botanica/9449484790926524.zip')
Botanica<-gld_get_lattes_data_from_zip(botanica, field.qualis = 'BIODIVERSIDADE')

c.alimentos<-c('cv/ciencia d alimentos/1341499646322200.zip','cv/ciencia d alimentos/2414704858566970.zip',
               'cv/ciencia d alimentos/3452623210043423.zip','cv/ciencia d alimentos/3760619041563045.zip',
               'cv/ciencia d alimentos/9804923403746874.zip', 'cv/ciencia d alimentos/7651891582073608.zip')
C.alimentos<-gld_get_lattes_data_from_zip(c.alimentos, field.qualis = 'CIÊNCIA DE ALIMENTOS')
#,
#' problema do MSC
c.comput<- c('cv/ciencia da computaçãp/1531713258988427.zip','cv/ciencia da computaçãp/1278004515460973.zip',
             'cv/ciencia da computaçãp/2741458816539568.zip','cv/ciencia da computaçãp/2815946827655352.zip',
             'cv/ciencia da computaçãp/2985003469291138.zip','cv/ciencia da computaçãp/3957046121364560.zip',
             'cv/ciencia da computaçãp/5118629875846648.zip','cv/ciencia da computaçãp/5660469902738038.zip',
             'cv/ciencia da computaçãp/5885575084085406.zip','cv/ciencia da computaçãp/8436386349098166.zip',
             'cv/ciencia da computaçãp/8626964624628522.zip','cv/ciencia da computaçãp/9553770402705512.zip',
             'cv/ciencia da computaçãp/9612704468357790.zip','cv/ciencia da computaçãp/9838400375894439.zip')
C.comput<- gld_get_lattes_data_from_zip(c.comput, field.qualis = 'CIÊNCIA DA COMPUTAÇÃO')
C.inf<- gld_get_lattes_data_from_zip('cv/ciencia da informacao/9981280495277166.zip',
                                     field.qualis = "COMUNICAÇÃO E INFORMAÇÃO" )

c.p<-c('cv/ciencia politica/0924434202055031.zip','cv/ciencia politica/0988858984939965.zip',
       'cv/ciencia politica/1896848346931079.zip','cv/ciencia politica/4366732261389204.zip',
       'cv/ciencia politica/5543774684916326.zip','cv/ciencia politica/6734760027498076.zip',
       'cv/ciencia politica/8157447462904324.zip','cv/ciencia politica/9941579627170607.zip')
C.p<- gld_get_lattes_data_from_zip(c.p, field.qualis = 'CIÊNCIA POLÍTICA E RELAÇÕES INTERNACIONAIS')
demo <-c('cv/demografia/3691103797905606.zip', 'cv/demografia/7768163469697986.zip')
Demo<-gld_get_lattes_data_from_zip(demo, field.qualis = 'PLANEJAMENTO URBANO E REGIONAL / DEMOGRAFIA')
direito<- c('cv/direito/2694901201572235.zip','cv/direito/4676278711005293.zip',
            'cv/direito/6098382779643532.zip','cv/direito/8987677644041247.zip')
Direto<-gld_get_lattes_data_from_zip(direito, field.qualis = 'DIREITO')
comu<- c('cv/comunicacao/1254684857020143.zip','cv/comunicacao/1736334142033813.zip',
         'cv/comunicacao/3147241105057327.zip','cv/comunicacao/9704125905278773.zip')
Comu<- gld_get_lattes_data_from_zip(comu, field.qualis = 'COMUNICAÇÃO E INFORMAÇÃO')

educacao<-c("cv/educacao/0078129824340671.zip","cv/educacao/0273416390435225.zip",
"cv/educacao/0416966816426212.zip","cv/educacao/1047127256639285.zip",
"cv/educacao/1063498116202429.zip","cv/educacao/1100512325355728.zip",
"cv/educacao/2297203444364327.zip","cv/educacao/1302777252317171.zip",
"cv/educacao/2379217359202523.zip","cv/educacao/2592903523310055.zip",
"cv/educacao/2662372508607004.zip","cv/educacao/3162292964889276.zip",
"cv/educacao/3751287338655685.zip","cv/educacao/5387293048319734.zip",
"cv/educacao/5766441805995117.zip","cv/educacao/5810253486833165.zip",
"cv/educacao/6873414697016839.zip","cv/educacao/7405584392319464.zip",
"cv/educacao/7640634913198342.zip","cv/educacao/7661737809414762.zip",
"cv/educacao/8469011549621156.zip","cv/educacao/9226900461088151.zip",
"cv/educacao/9511377122315447.zip","cv/educacao/9990513381571092.zip")
Educacao<-gld_get_lattes_data_from_zip(educacao, field.qualis = 'EDUCAÇÃO')

econo<- c('cv/economia/0931860042124079.zip','cv/economia/1791535374833777.zip',
          'cv/economia/3740950346967524.zip','cv/economia/3708985718656687.zip',
          'cv/economia/4984859173592703.zip','cv/economia/5113493410800601.zip',
          'cv/economia/9703745652815543.zip','cv/economia/9723758439733361.zip',
          'cv/economia/7741121296126010.zip','cv/economia/8039060787792626.zip',
          'cv/economia/5113493410800601.zip')
Econ<-gld_get_lattes_data_from_zip(econo, field.qualis = 'ECONOMIA')

eco<-c('cv/ecologia/0483754754945290.zip','cv/ecologia/0579026269226734.zip',
       'cv/ecologia/9040659895653917.zip','cv/ecologia/0625926234026025.zip',
       'cv/ecologia/6555771530199324.zip','cv/ecologia/4889413058276292.zip',
       'cv/ecologia/5644626331586827.zip','cv/ecologia/8559146085477218.zip',
       'cv/ecologia/9625562877284120.zip','cv/ecologia/5834346164614238.zip')
Eco<- gld_get_lattes_data_from_zip(eco, field.qualis = 'BIODIVERSIDADE')
e.f<-c('cv/educacao fisica/1652339643129712.zip','cv/educacao fisica/5261830927402052.zip',
       'cv/educacao fisica/6471278386232940.zip')
E.F<- gld_get_lattes_data_from_zip(e.f, field.qualis = 'EDUCAÇÃO FÍSICA')

enfermagem<-c('cv/enfermagem/2407163915684451.zip','cv/enfermagem/2569298609941547.zip',
              'cv/enfermagem/4975298732179917.zip','cv/enfermagem/6589878915501349.zip',
              'cv/enfermagem/7042087959416545.zip','cv/enfermagem/7211210423500459.zip')

Enfermagem<- gld_get_lattes_data_from_zip(enfermagem, field.qualis ='ENFERMAGEM' )

Eng.aero<- gld_get_lattes_data_from_zip('cv/engenharia aeroespacial/7740917144757410.zip', field.qualis = 'ENGENHARIAS III')

eng.agri<-c('cv/engenharia agricola/5644626331586827.zip','cv/engenharia agricola/5834346164614238.zip',
            'cv/engenharia agricola/8559146085477218.zip', 'cv/engenharia agricola/5631386564754689.zip',
            'cv/engenharia agricola/9829135243038746.zip')
Eng.agri<- gld_get_lattes_data_from_zip(eng.agri, field.qualis = 'CIENCIAS AGRARIAS I')

eng.amb<- c('cv/engenharia ambiental/0287280830465959.zip','cv/engenharia ambiental/2069883404215099.zip',
            'cv/engenharia ambiental/2505780539057327.zip','cv/engenharia ambiental/3087677954298076.zip',
            'cv/engenharia ambiental/6037303709386038.zip','cv/engenharia ambiental/6910124128030609.zip',
            'cv/engenharia ambiental/7422951276755637.zip','cv/engenharia ambiental/7972936754516344.zip')
Eng.amb<-gld_get_lattes_data_from_zip(eng.amb, field.qualis = 'ENGENHARIAS I')
eng.civ<- c('cv/engenharia civil/1237671923222436.zip','cv/engenharia civil/1539316781096410.zip',
            'cv/engenharia civil/2127623490383448.zip','cv/engenharia civil/2490783568114909.zip',
            'cv/engenharia civil/3299561421006772.zip','cv/engenharia civil/4778269687775430.zip',
            'cv/engenharia civil/5613026506538994.zip','cv/engenharia civil/5937902167536869.zip',
            'cv/engenharia civil/6483888857335060.zip','cv/engenharia civil/6645567596194852.zip',
            'cv/engenharia civil/8929629898766359.zip','cv/engenharia civil/9045444814367137.zip',
            'cv/engenharia civil/9376514233715653.zip', 'cv/engenharia civil/3179808724747993.zip')
Eng.civ<-gld_get_lattes_data_from_zip(eng.civ, field.qualis = 'ENGENHARIAS I')

eng.mat<- c('cv/engenharia de materiais e metalurgica/2949983867418338.zip','cv/engenharia de materiais e metalurgica/3470395641242374.zip',
            'cv/engenharia de materiais e metalurgica/4487123407292953.zip','cv/engenharia de materiais e metalurgica/5764983862728793.zip',
            'cv/engenharia de materiais e metalurgica/5914976951761736.zip','cv/engenharia de materiais e metalurgica/6189339614488552.zip',
            'cv/engenharia de materiais e metalurgica/6275585300349034.zip','cv/engenharia de materiais e metalurgica/7604179450699702.zip',
            'cv/engenharia de materiais e metalurgica/9180718046391058.zip','cv/engenharia de materiais e metalurgica/9325051019864649.zip',
            'cv/engenharia de materiais e metalurgica/9558299312183852.zip','cv/engenharia de materiais e metalurgica/9659007776030357.zip')
Eng.mat<-gld_get_lattes_data_from_zip(eng.mat, field.qualis = 'ENGENHARIAS II')

eng.prod<-c('cv/engenharia de producao/2004501146244643.zip','cv/engenharia de producao/4017306470784388.zip',
            'cv/engenharia de producao/6078945717558464.zip','cv/engenharia de producao/6312739422908628.zip',
            'cv/engenharia de producao/6639164567036709.zip','cv/engenharia de producao/9598512164868745.zip')
Eng.prod<-gld_get_lattes_data_from_zip(eng.prod, field.qualis = 'ENGENHARIAS III')

eng.trans<-c('cv/engenharia de transportes/0853783751848766.zip',
             'cv/engenharia de transportes/9910879761251201.zip')
Eng.Trans<- gld_get_lattes_data_from_zip(eng.trans, field.qualis = 'ENGENHARIAS I')
eng.elet <-c('cv/engenharia eletrica e microeletronica/0250052255460941.zip','cv/engenharia eletrica e microeletronica/6284386218725402.zip',
             'cv/engenharia eletrica e microeletronica/1260224604290354.zip','cv/engenharia eletrica e microeletronica/6308173225779684.zip',
             'cv/engenharia eletrica e microeletronica/1987295209521433.zip','cv/engenharia eletrica e microeletronica/7226228571158486.zip',
             'cv/engenharia eletrica e microeletronica/3566465649283245.zip','cv/engenharia eletrica e microeletronica/7413544381333504.zip',
             'cv/engenharia eletrica e microeletronica/3992941500638443.zip','cv/engenharia eletrica e microeletronica/7452085194074257.zip',
             'cv/engenharia eletrica e microeletronica/4070109317911706.zip','cv/engenharia eletrica e microeletronica/9220229463548248.zip')
Eng.elet<-gld_get_lattes_data_from_zip(eng.elet, field.qualis = 'ENGENHARIAS IV')
eng.mec<-c('cv/engenharia mecanica/0011565883560592.zip','cv/engenharia mecanica/0203667980247686.zip',
           'cv/engenharia mecanica/0301261650624992.zip','cv/engenharia mecanica/0934428004504955.zip',
           'cv/engenharia mecanica/0934428004504955.zip','cv/engenharia mecanica/1399058889534718.zip',
           'cv/engenharia mecanica/1743826010794846.zip','cv/engenharia mecanica/2207239537360072.zip',
           'cv/engenharia mecanica/2511836494580088.zip','cv/engenharia mecanica/3356131637634546.zip',
           'cv/engenharia mecanica/3996404436225621.zip','cv/engenharia mecanica/4595794486045839.zip',
           'cv/engenharia mecanica/6603422516062090.zip','cv/engenharia mecanica/6692605216100628.zip',
           'cv/engenharia mecanica/7018522179059194.zip','cv/engenharia mecanica/7735107032972317.zip',
           'cv/engenharia mecanica/8157858526322556.zip','cv/engenharia mecanica/9607255646013062.zip')

Eng.mec<- gld_get_lattes_data_from_zip(eng.mec, field.qualis = 'ENGENHARIAS III')

eng.nuc<- c('cv/engenharia nucelar/0983858173807551.zip','cv/engenharia nucelar/3897283209013869.zip',
            'cv/engenharia nucelar/7388174990363675.zip','cv/engenharia nucelar/9870663748100803.zip')

Eng.nuc<-gld_get_lattes_data_from_zip(eng.nuc, field.qualis = 'ENGENHARIAS II')

eng.qui<-c('cv/engenharia quimica/1876482890634213.zip','cv/engenharia quimica/2617251488980560.zip',
           'cv/engenharia quimica/2730416923915211.zip','cv/engenharia quimica/3720919394286410.zip',
           'cv/engenharia quimica/3804707640110777.zip','cv/engenharia quimica/5412544199323879.zip',
           'cv/engenharia quimica/6298775450874291.zip','cv/engenharia quimica/7586955305848168.zip',
           'cv/engenharia quimica/8017080285788684.zip','cv/engenharia quimica/8303589765054331.zip',
           'cv/engenharia quimica/9790871977193250.zip')
Eng.qui<-gld_get_lattes_data_from_zip(eng.qui, field.qualis = 'ENGENHARIAS II')

farmacia<-c('cv/farmacia/2062982268663101.zip','cv/farmacia/4436300782833384.zip',
            'cv/farmacia/5980170934785365.zip','cv/farmacia/6036320391106370.zip',
            'cv/farmacia/6907806915889763.zip','cv/farmacia/9794813977231934.zip')
Farmacia<-gld_get_lattes_data_from_zip(farmacia, field.qualis = 'FARMÁCIA')

farmaco<-c('cv/farmacologia/0057645238802910.zip','cv/farmacologia/2288636936450109.zip',
           'cv/farmacologia/3081112950297594.zip','cv/farmacologia/5885528105039995.zip',
           'cv/farmacologia/5962306335524096.zip','cv/farmacologia/9631573633970523.zip',
           'cv/farmacologia/9800155373924394.zip')
Farmaco<-gld_get_lattes_data_from_zip(farmaco, field.qualis = 'CIÊNCIAS BIOLÓGICAS II')

filo<-c('cv/filosofia/0078129824340671.zip','cv/filosofia/0416614425134935.zip',
        'cv/filosofia/0831017619527813.zip','cv/filosofia/0966229092773143.zip',
        'cv/filosofia/1009093134243267.zip','cv/filosofia/1253046260139699.zip',
        'cv/filosofia/1717411733761791.zip','cv/filosofia/2452772438199184.zip',
        'cv/filosofia/3976705366604804.zip','cv/filosofia/4839214907972946.zip',
        'cv/filosofia/5250139595129075.zip','cv/filosofia/5265150425993880.zip',
        'cv/filosofia/5391034042353187.zip','cv/filosofia/6334847644105045.zip',
        'cv/filosofia/6592343865729395.zip','cv/filosofia/7163791603588084.zip',
        'cv/filosofia/7195033535563005.zip','cv/filosofia/8594637607405428.zip',
        'cv/filosofia/9318710710545447.zip','cv/filosofia/9661213429808142.zip',
        'cv/filosofia/9672130191987782.zip','cv/filosofia/9726601553271394.zip')
Filo<-gld_get_lattes_data_from_zip(filo, field.qualis = 'FILOSOFIA')

fisica<-c('cv/fisica/0128415670124607.zip','cv/fisica/0233665338105542.zip',
          'cv/fisica/0785743182319556.zip','cv/fisica/0957444591428758.zip',
          'cv/fisica/1108613073699132.zip','cv/fisica/1109849519017980.zip',
          'cv/fisica/1229547927722481.zip','cv/fisica/1417657874331629.zip',
          'cv/fisica/1608472474770322.zip','cv/fisica/1641624738512086.zip',
          'cv/fisica/1967754065410160.zip','cv/fisica/2081306887051760.zip',
          'cv/fisica/2425549033248106.zip','cv/fisica/3404171170651705.zip',
          'cv/fisica/3406280529286517.zip','cv/fisica/3647704384239600.zip',
          'cv/fisica/3915676505538726.zip','cv/fisica/4058118793813648.zip',
          'cv/fisica/4134159224948682.zip','cv/fisica/4164041157405626.zip',
          'cv/fisica/4417117445512655.zip','cv/fisica/4418162621330447.zip',
          'cv/fisica/4496249071363237.zip','cv/fisica/4909761891473754.zip',
          'cv/fisica/5206053788556679.zip','cv/fisica/5358286408119219.zip',
          'cv/fisica/5548512717869486.zip','cv/fisica/5661661960969099.zip',
          'cv/fisica/5662245728609756.zip','cv/fisica/5695762274195703.zip',
          'cv/fisica/5720500395612472.zip','cv/fisica/6231675319360093.zip',
          'cv/fisica/6264982300012380.zip','cv/fisica/6328420212181284.zip',
          'cv/fisica/6443348814893849.zip','cv/fisica/6604074224038905.zip',
          'cv/fisica/6745509539934322.zip','cv/fisica/6888892134532265.zip',
          'cv/fisica/6903653527541008.zip','cv/fisica/6949879982808858.zip',
          'cv/fisica/7109489698613515.zip','cv/fisica/7502758036614082.zip',
          'cv/fisica/8713770387750524.zip','cv/fisica/8859998369703134.zip',
          'cv/fisica/8862660046981466.zip','cv/fisica/8885496748170584.zip',
          'cv/fisica/9200128043346112.zip','cv/fisica/9287776078149551.zip',
          'cv/fisica/9477024092198309.zip','cv/fisica/9572410339084315.zip',
          'cv/fisica/9797420148146484.zip','cv/fisica/9890322878300982.zip',
          'cv/fisica/4296821710899356.zip','cv/fisica/3890234334285222.zip')
Fisica<-gld_get_lattes_data_from_zip(fisica, field.qualis = 'ASTRONOMIA / FÍSICA')

fisio<-c('cv/fisiologia/4299344810509965.zip','cv/fisiologia/6923553808016055.zip',
         'cv/fisiologia/7729955482153416.zip')
Fisio<-gld_get_lattes_data_from_zip(fisio, field.qualis ='CIÊNCIAS BIOLÓGICAS II' )

fono<-c('cv/fonoaudiologia/1167294060627845.zip','cv/fonoaudiologia/2804492308015336.zip')
Fono<-gld_get_lattes_data_from_zip(fono, field.qualis = 'EDUCAÇÃO FÍSICA')

genet<- c('cv/genetica/1881149609989843.zip','cv/genetica/2291552542715323.zip',
          'cv/genetica/4362051219348099.zip','cv/genetica/4979293133856180.zip',
          'cv/genetica/6744869566831263.zip','cv/genetica/7269625907774399.zip',
          'cv/genetica/8023869177216297.zip','cv/genetica/9135538820816061.zip') 
Genet<-gld_get_lattes_data_from_zip(genet, field.qualis = 'CIÊNCIAS BIOLÓGICAS I')
geo<-c('cv/geografia/0638383316638349.zip','cv/geografia/1361567073713480.zip',
       'cv/geografia/1525603220583356.zip','cv/geografia/3711345228200246.zip',
       'cv/geografia/4553923987301470.zip','cv/geografia/6904357543865276.zip',
       'cv/geografia/9341762649252420.zip')
Geo<-gld_get_lattes_data_from_zip(geo, field.qualis = 'GEOGRAFIA')

hist<-c('cv/historia/0234065711037139.zip','cv/historia/3145422372446614.zip',
        'cv/historia/3230173359064082.zip','cv/historia/3321652451642202.zip',
        'cv/historia/3677327042198821.zip','cv/historia/5558125167892774.zip',
        'cv/historia/5727089712546469.zip','cv/historia/6904951754558127.zip',
        'cv/historia/7314978347718720.zip','cv/historia/7643639413441117.zip',
        'cv/historia/8807863760286159.zip','cv/historia/8828847259602722.zip',
        'cv/historia/9100012030618652.zip','cv/historia/5695745490223689.zip',
        'cv/historia/5695745490223689.zip')
Hist<-gld_get_lattes_data_from_zip(hist, field.qualis = 'HISTÓRIA')
imuno<-c('cv/imonologia/1736334142033813.zip','cv/imonologia/2996863322076939.zip',
         'cv/imonologia/5080772639084492.zip','cv/imonologia/9264244341601758.zip',
         'cv/imonologia/9704125905278773.zip','cv/imonologia/9981280495277166.zip',
         'cv/imonologia/5237110292041340.zip','cv/imonologia/3087665610359216.zip','cv/imonologia/3066846674993272.zip',
         'cv/imonologia/7326620682313478.zip','cv/imonologia/8217524313663232.zip','cv/imonologia/8471949500798737.zip',
         'cv/imonologia/8598274096498065.zip')
Imuno<-gld_get_lattes_data_from_zip(imuno, field.qualis = 'CIÊNCIAS BIOLÓGICAS III')
letras<-c('cv/letras/0042692173243900.zip','cv/letras/1184566292052342.zip',
          'cv/letras/2167995922283328.zip','cv/letras/2420574923794536.zip',
          'cv/letras/2424576397841507.zip','cv/letras/4168957754548160.zip',
          'cv/letras/4258903462581090.zip','cv/letras/4829873824662045.zip',
          'cv/letras/6252177082780748.zip','cv/letras/6438010553186114.zip',
          'cv/letras/8723885160615840.zip','cv/letras/8769804694444711.zip',
          'cv/letras/9997225424532634.zip','cv/letras/36773270421988214.zip',
          'cv/letras/3677327042198821.zip')

Letras<-gld_get_lattes_data_from_zip(letras, field.qualis = 'LETRAS / LINGUÍSTICA')
ling<-c('cv/linguistica/0180606646463169.zip','cv/linguistica/1789493896240761.zip',
        'cv/linguistica/2850948628761143.zip','cv/linguistica/3268283471587710.zip',
        'cv/linguistica/3359712703810469.zip','cv/linguistica/3426116122713706.zip',
        'cv/linguistica/3846654142638437.zip','cv/linguistica/4241121897850677.zip',
        'cv/linguistica/4506394252320885.zip','cv/linguistica/5470485171881359.zip',
        'cv/linguistica/5506513991215666.zip','cv/linguistica/5621060170321062.zip',
        'cv/linguistica/6702374101936937.zip','cv/linguistica/7530569358334953.zip',
        'cv/linguistica/7746358781255459.zip','cv/linguistica/8959064517534406.zip',
        'cv/linguistica/9443575304118422.zip','cv/linguistica/9868929094681867.zip')

Ling<-gld_get_lattes_data_from_zip(ling, field.qualis = 'LETRAS / LINGUÍSTICA')
mat<-c('cv/matematica/0389175519424617.zip','cv/matematica/1072971611618332.zip',
       'cv/matematica/1369564161669232.zip','cv/matematica/2237596477064578.zip',
       'cv/matematica/3474965783942591.zip','cv/matematica/3970539639640901.zip',
       'cv/matematica/3979149526460720.zip','cv/matematica/6832516110313266.zip')
Mat<-gld_get_lattes_data_from_zip(mat, field.qualis = 'MATEMÁTICA / PROBABILIDADE E ESTATÍSTICA')
med<-c('cv/medicina/0591884301805680.zip','cv/medicina/3944020856408245.zip',
       'cv/medicina/4045374522226547.zip','cv/medicina/4274059403069528.zip',
       'cv/medicina/4855524850504285.zip','cv/medicina/4973019092516868.zip',
       'cv/medicina/5136945141059977.zip','cv/medicina/5415585177439707.zip',
       'cv/medicina/6395956022175653.zip','cv/medicina/6443038320654989.zip',
       'cv/medicina/6870859879978339.zip','cv/medicina/7441423216614403.zip',
       'cv/medicina/7787515012796496.zip','cv/medicina/7913451338414184.zip',
       'cv/medicina/7970471613479900.zip','cv/medicina/8253870907570489.zip',
       'cv/medicina/8681520193756072.zip','cv/medicina/8716990559512126.zip',
       'cv/medicina/8736721273068415.zip','cv/medicina/9544422831539412.zip',
       'cv/medicina/9548262587954222.zip','cv/medicina/9560591817035298.zip') 

Med<-gld_get_lattes_data_from_zip(med, field.qualis = 'MEDICINA I')
med.vet<- c('cv/MEDICINA VETERINARIA/1004000475494796.zip','cv/MEDICINA VETERINARIA/1383126157031968.zip',
            'cv/MEDICINA VETERINARIA/1672008572442721.zip','cv/MEDICINA VETERINARIA/2210784968488771.zip',
            'cv/MEDICINA VETERINARIA/2284090069006192.zip','cv/MEDICINA VETERINARIA/2716804135366844.zip',
            'cv/MEDICINA VETERINARIA/3610262531647532.zip','cv/MEDICINA VETERINARIA/5326072118518067.zip',
            'cv/MEDICINA VETERINARIA/6020984937849801.zip','cv/MEDICINA VETERINARIA/6779567235545423.zip',
            'cv/MEDICINA VETERINARIA/7995036285650904.zip','cv/MEDICINA VETERINARIA/8310937495075551.zip',
            'cv/MEDICINA VETERINARIA/9933875438761784.zip')
Med.vet<-gld_get_lattes_data_from_zip(med.vet, field.qualis = 'MEDICINA VETERINÁRIA')
microbio<-c('cv/MICROBIOLOGIA/0232738544349053.zip','cv/MICROBIOLOGIA/0977453259767928.zip',
            'cv/MICROBIOLOGIA/1463249528959656.zip','cv/MICROBIOLOGIA/2621111326126440.zip',
            'cv/MICROBIOLOGIA/6119482284286728.zip','cv/MICROBIOLOGIA/8237569228020224.zip',
            'cv/MICROBIOLOGIA/8348645460666798.zip','cv/MICROBIOLOGIA/9715273430967528.zip')

Microbio<-gld_get_lattes_data_from_zip(microbio, field.qualis ='CIÊNCIAS BIOLÓGICAS III' )
morfo<- c('cv/MORFOLOGIA/2970551390843240.zip','cv/MORFOLOGIA/3057188718614807.zip',
          'cv/MORFOLOGIA/5194562537283859.zip','cv/MORFOLOGIA/6171323341671821.zip',
          'cv/MORFOLOGIA/8991062042568398.zip')

Morfo<-gld_get_lattes_data_from_zip(morfo, field.qualis ='CIÊNCIAS BIOLÓGICAS II' )
Museo<- gld_get_lattes_data_from_zip('cv/MUSEOLOGIA/8746315302380257.zip', field.qualis = 'COMUNICAÇÃO E INFORMAÇÃO')

nut<-c('cv/NUTRIÇÃO/2868331941630408.zip','cv/NUTRIÇÃO/7419672108203411.zip',
       'cv/NUTRIÇÃO/9039273079632174.zip','cv/NUTRIÇÃO/9620060759784508.zip')

Nut<-gld_get_lattes_data_from_zip(nut, field.qualis = 'NUTRIÇÃO')
oceano<-c('cv/OCEANOGRAFIA/0268414770907216.zip','cv/OCEANOGRAFIA/7670196776586335.zip',
          'cv/OCEANOGRAFIA/8802957897143630.zip')
Oceano<- gld_get_lattes_data_from_zip(oceano, field.qualis = 'BIODIVERSIDADE')
odonto<-c('cv/ODONTOLOGIA/0188606139001986.zip','cv/ODONTOLOGIA/2224561056978177.zip',
          'cv/ODONTOLOGIA/1317718068735423.zip','cv/ODONTOLOGIA/0535309012233484.zip',
          'cv/ODONTOLOGIA/2405878864266963.zip','cv/ODONTOLOGIA/6100859465871929.zip',
          'cv/ODONTOLOGIA/8017993512298330.zip','cv/ODONTOLOGIA/8289325716140743.zip',
          'cv/ODONTOLOGIA/8571576530024407.zip', 'cv/ODONTOLOGIA/8672890018200588.zip',
          'cv/ODONTOLOGIA/5071350422821688.zip')
Odonto<-gld_get_lattes_data_from_zip(odonto, field.qualis = 'ODONTOLOGIA')
parasito<- c('cv/PARASITOLOGIA/0624659188288065.zip','cv/PARASITOLOGIA/9211345386050612.zip',
             'cv/PARASITOLOGIA/4648411135225077.zip','cv/PARASITOLOGIA/0814854098256062.zip',
             'cv/PARASITOLOGIA/5520555713752325.zip','cv/PARASITOLOGIA/4746511137641620.zip',
             'cv/PARASITOLOGIA/5618495903090773.zip','cv/PARASITOLOGIA/5870589702797914.zip',
             'cv/PARASITOLOGIA/9986209582169493.zip')

Parasito<-gld_get_lattes_data_from_zip(parasito, field.qualis = 'CIÊNCIAS BIOLÓGICAS III')

planej<-c('cv/PLANEJAMENTO REGIONAL/1304424697105730.zip','cv/PLANEJAMENTO REGIONAL/1453116800487905.zip',
          'cv/PLANEJAMENTO REGIONAL/2679184234958431.zip','cv/PLANEJAMENTO REGIONAL/3660344606587105.zip',
          'cv/PLANEJAMENTO REGIONAL/5904602438774021.zip','cv/PLANEJAMENTO REGIONAL/7704277249098732.zip',
          'cv/PLANEJAMENTO REGIONAL/8205038071681049.zip','cv/PLANEJAMENTO REGIONAL/3417734567782570.zip',
          'cv/PLANEJAMENTO REGIONAL/3331234730616226.zip')

Planej<-gld_get_lattes_data_from_zip(planej, field.qualis = 'PLANEJAMENTO URBANO E REGIONAL / DEMOGRAFIA')
estatistica<-c('cv/PROBABILIDADE E ESTATISTICA/1069731541677061.zip','cv/PROBABILIDADE E ESTATISTICA/5787038322381726.zip',
               'cv/PROBABILIDADE E ESTATISTICA/7307804984087687.zip','cv/PROBABILIDADE E ESTATISTICA/9904863693302949.zip')
Estat<-gld_get_lattes_data_from_zip(estatistica, field.qualis = 'MATEMÁTICA / PROBABILIDADE E ESTATÍSTICA')
psico<-c('cv/PSICOLOGIA/1410667846560350.zip','cv/PSICOLOGIA/2180690229666836.zip',
         'cv/PSICOLOGIA/3826964831651958.zip','cv/PSICOLOGIA/4615061799860194.zip',
         'cv/PSICOLOGIA/5312038855744142.zip','cv/PSICOLOGIA/5503576309484010.zip',
         'cv/PSICOLOGIA/7231331062174024.zip','cv/PSICOLOGIA/8991172780503312.zip',
         'cv/PSICOLOGIA/9098072774674013.zip','cv/PSICOLOGIA/9102480955435391.zip',
         'cv/PSICOLOGIA/9239252166751422.zip')
Psico<-gld_get_lattes_data_from_zip(psico, field.qualis = 'PSICOLOGIA')
quim<-c('cv/QUIMICA/0065966792532426.zip','cv/QUIMICA/0084554486502562.zip','cv/QUIMICA/0845421586641237.zip',
        'cv/QUIMICA/1374256752569626.zip','cv/QUIMICA/1929867739425527.zip','cv/QUIMICA/1876245161372968.zip',
        'cv/QUIMICA/4164247517680621.zip','cv/QUIMICA/1869692918489026.zip','cv/QUIMICA/1962782589609789.zip',
        'cv/QUIMICA/2302116915931540.zip','cv/QUIMICA/2497397243924771.zip','cv/QUIMICA/3192125098361121.zip',
        'cv/QUIMICA/4228028695889214.zip','cv/QUIMICA/5607366782144472.zip','cv/QUIMICA/6445841478978597.zip',
        'cv/QUIMICA/6446047463034654.zip','cv/QUIMICA/4416308592967165.zip','cv/QUIMICA/6607268864945311.zip',
        'cv/QUIMICA/6920140377260878.zip','cv/QUIMICA/7045415884414249.zip','cv/QUIMICA/7201928600704530.zip',
        'cv/QUIMICA/7781282422851911.zip','cv/QUIMICA/8475640212486290.zip','cv/QUIMICA/8892459126928726.zip',
        'cv/QUIMICA/9563158536061549.zip','cv/QUIMICA/9573924260693571.zip','cv/QUIMICA/9971202585286967.zip',
        'cv/QUIMICA/5481920586591029.zip')
Quim<-gld_get_lattes_data_from_zip(quim, field.qualis = 'QUÍMICA')
recur.flor<- c('cv/RECURSOS FLORESTAIS E ENGENHARIA FLORESTAL/0406606438474637.zip','cv/RECURSOS FLORESTAIS E ENGENHARIA FLORESTAL/7099569427531441.zip',
               'cv/RECURSOS FLORESTAIS E ENGENHARIA FLORESTAL/7194205256185786.zip','cv/RECURSOS FLORESTAIS E ENGENHARIA FLORESTAL/7358904722082814.zip')
Recur.flor<-gld_get_lattes_data_from_zip(recur.flor, field.qualis = 'CIÊNCIAS AGRÁRIAS I')
recur.pes<-c('cv/RECURSOS PESQUEIROS E ENGENHARIA DA PESCA/4500254461990247.zip','cv/RECURSOS PESQUEIROS E ENGENHARIA DA PESCA/9203413733944127.zip')
Recur.pes<- gld_get_lattes_data_from_zip(recur.pes, field.qualis = 'ZOOTECNIA / RECURSOS PESQUEIROS')
saude.col<-c('cv/SAUDE COLETIVA/0492208637289837.zip','cv/SAUDE COLETIVA/3757649582071576.zip',
             'cv/SAUDE COLETIVA/6777670296858134.zip','cv/SAUDE COLETIVA/5594705199631576.zip',
             'cv/SAUDE COLETIVA/8093148033189159.zip')
Saude.col<-gld_get_lattes_data_from_zip(saude.col, field.qualis = 'SAÚDE COLETIVA')
sso<-c('cv/SERVICO SOCIAL/0342378193333117.zip','cv/SERVICO SOCIAL/2337937419444833.zip',
       'cv/SERVICO SOCIAL/2303230429484169.zip','cv/SERVICO SOCIAL/1702450858580199.zip',
       'cv/SERVICO SOCIAL/3630123802753991.zip','cv/SERVICO SOCIAL/4053383742253379.zip',
       'cv/SERVICO SOCIAL/5089887428145825.zip','cv/SERVICO SOCIAL/5321036427335727.zip',
       'cv/SERVICO SOCIAL/5681987657897099.zip','cv/SERVICO SOCIAL/6004407980129154.zip',
       'cv/SERVICO SOCIAL/6821974709618583.zip')
SSo<- gld_get_lattes_data_from_zip(sso, field.qualis = 'SERVIÇO SOCIAL')
socio<-c('cv/SOCIOLOGIA/0466443465021289.zip', 'cv/SOCIOLOGIA/0837207761621512.zip',
         'cv/SOCIOLOGIA/1404480775743293.zip','cv/SOCIOLOGIA/2382723098584720.zip',
         'cv/SOCIOLOGIA/3823939489789066.zip','cv/SOCIOLOGIA/5515536378374204.zip')
Socio<-gld_get_lattes_data_from_zip(socio, field.qualis = 'SOCIOLOGIA')
zoolo<-c('cv/ZOOLOGIA/1095372100037046.zip','cv/ZOOLOGIA/1309165918810655.zip',
         'cv/ZOOLOGIA/3964807651730603.zip','cv/ZOOLOGIA/4386272833508083.zip',
         'cv/ZOOLOGIA/6463344507508582.zip','cv/ZOOLOGIA/6600267892415480.zip',
         'cv/ZOOLOGIA/8603988130191881.zip','cv/ZOOLOGIA/9059654634446193.zip',
         'cv/ZOOLOGIA/9263833464663272.zip')
Zoolo<-gld_get_lattes_data_from_zip(zoolo, field.qualis = 'BIODIVERSIDADE')
zootec<-c('cv/ZOOTECNIA/0253635901049492.zip','cv/ZOOTECNIA/2461030262283546.zip',
          'cv/ZOOTECNIA/3850491807035834.zip','cv/ZOOTECNIA/7445254960858159.zip',
          'cv/ZOOTECNIA/7703594344797645.zip','cv/ZOOTECNIA/7900381645698082.zip')
Zootec<-gld_get_lattes_data_from_zip(zootec, field.qualis = 'ZOOTECNIA / RECURSOS PESQUEIROS')  


#'cv/enfermagem/.zip',





pesquisador<-rbind(Agronomia$tpesq,
                   Antropo.Arq$tpesq,
                   Arquit$tpesq,
                   Artes$tpesq,
                   Bioquimica$tpesq,
                   Botanica$tpesq,
                   C.alimentos$tpesq,
                   C.comput$tpesq,
                   C.p$tpesq,
                   Educacao$tpesq,
                   E.F$tpesq, Enfermagem$tpesq, Eng.aero$tpesq,
                   Eng.amb$tpesq, Eng.civ$tpesq,
                   Eng.elet$tpesq,Eng.mat$tpesq,
                   Eng.mec$tpesq,Eng.nuc$tpesq,
                   Eng.prod$tpesq,Eng.qui$tpesq,
                   Eng.Trans$tpesq,Estat$tpesq,
                   Farmacia$tpesq,Farmaco$tpesq,
                   Filo$tpesq,Fisica$tpesq,
                   Fisio$tpesq,Fono$tpesq,
                   Genet$tpesq,Geo$tpesq,
                   Hist$tpesq,Letras$tpesq,
                   Ling$tpesq,Mat$tpesq,
                   Med$tpesq,Med.vet$tpesq,
                   Microbio$tpesq,Morfo$tpesq,
                   Museo$tpesq,Nut$tpesq,
                   Oceano$tpesq,Odonto$tpesq,
                   Parasito$tpesq,Planej$tpesq,
                   Psico$tpesq,Quim$tpesq,
                   Recur.flor$tpesq,Recur.pes$tpesq,
                   Saude.col$tpesq,Socio$tpesq,
                   SSo$tpesq,Zoolo$tpesq,
                   Zootec$tpesq,
                   Biof$tpesq,
                   C.inf$tpesq,
                   Comu$tpesq,
                   Demo$tpesq,
                   Direto$tpesq,
                   Eco$tpesq,
                   Econ$tpesq,
                   Imuno$tpesq)

publi<-rbind(Agronomia$tpublic.published,
             Antropo.Arq$tpublic.published,
             Arquit$tpublic.published,
             Artes$tpublic.published,
             Bioquimica$tpublic.published,
             Botanica$tpublic.published,
             C.alimentos$tpublic.published,
             C.comput$tpublic.published,
             C.p$tpublic.published,
             Educacao$tpublic.published,
             E.F$tpublic.published, Enfermagem$tpublic.published, Eng.aero$tpublic.published,
             Eng.amb$tpublic.published, Eng.civ$tpublic.published,
             Eng.elet$tpublic.published,Eng.mat$tpublic.published,
             Eng.mec$tpublic.published,Eng.nuc$tpublic.published,
             Eng.prod$tpublic.published,Eng.qui$tpublic.published,
             Eng.Trans$tpublic.published,Estat$tpublic.published,
             Farmacia$tpublic.published,Farmaco$tpublic.published,
             Filo$tpublic.published,Fisica$tpublic.published,
             Fisio$tpublic.published,Fono$tpublic.published,
             Genet$tpublic.published,Geo$tpublic.published,
             Hist$tpublic.published,Letras$tpublic.published,
             Ling$tpublic.published,Mat$tpublic.published,
             Med$tpublic.published,Med.vet$tpublic.published,
             Microbio$tpublic.published,Morfo$tpublic.published,
             Museo$tpublic.published,Nut$tpublic.published,
             Oceano$tpublic.published,Odonto$tpublic.published,
             Parasito$tpublic.published,Planej$tpublic.published,
             Psico$tpublic.published,Quim$tpublic.published,
             Recur.flor$tpublic.published,Recur.pes$tpublic.published,
             Saude.col$tpublic.published,Socio$tpublic.published,
             SSo$tpublic.published,Zoolo$tpublic.published,
             Zootec$tpublic.published,
             Biof$tpublic.published,
             C.inf$tpublic.published,
             Comu$tpublic.published,
             Demo$tpublic.published,
             Direto$tpublic.published,
             Eco$tpublic.published,
             Econ$tpublic.published,
             Imuno$tpublic.published)
publi.aceita<-rbind(Agronomia$tpublic.accepted,Antropo.Arq$tpublic.accepted,
                    Arquit$tpublic.accepted,Artes$tpublic.accepted,
                    Bioquimica$tpublic.accepted, Botanica$tpublic.accepted,
                    C.alimentos$tpublic.accepted, C.comput$tpublic.accepted,
                    C.p$tpublic.accepted,
                    Educacao$tpublic.accepted,
                    E.F$tpublic.accepted, Enfermagem$tpublic.accepted, Eng.aero$tpublic.accepted,
                    Eng.amb$tpublic.accepted, Eng.civ$tpublic.accepted,
                    Eng.elet$tpublic.accepted,Eng.mat$tpublic.accepted,
                    Eng.mec$tpublic.accepted,Eng.nuc$tpublic.accepted,
                    Eng.prod$tpublic.accepted,Eng.qui$tpublic.accepted,
                    Eng.Trans$tpublic.accepted,Estat$tpublic.accepted,
                    Farmacia$tpublic.accepted,Farmaco$tpublic.accepted,
                    Filo$tpublic.accepted,Fisica$tpublic.accepted,
                    Fisio$tpublic.accepted,Fono$tpublic.accepted,
                    Genet$tpublic.accepted,Geo$tpublic.accepted,
                    Hist$tpublic.accepted,Letras$tpublic.accepted,
                    Ling$tpublic.accepted,Mat$tpublic.accepted,
                    Med$tpublic.accepted,Med.vet$tpublic.accepted,
                    Microbio$tpublic.accepted,Morfo$tpublic.accepted,
                    Museo$tpublic.accepted,Nut$tpublic.accepted,
                    Oceano$tpublic.accepted,Odonto$tpublic.accepted,
                    Parasito$tpublic.accepted,Planej$tpublic.accepted,
                    Psico$tpublic.accepted,Quim$tpublic.accepted,
                    Recur.flor$tpublic.accepted,Recur.pes$tpublic.accepted,
                    Saude.col$tpublic.accepted,Socio$tpublic.accepted,
                    SSo$tpublic.accepted,Zoolo$tpublic.accepted,
                    Zootec$tpublic.accepted,Biof$tpublic.accepted,
                    C.inf$tpublic.accepted,
                    Comu$tpublic.accepted,
                    Demo$tpublic.accepted,
                    Direto$tpublic.accepted,
                    Eco$tpublic.accepted,
                    Econ$tpublic.accepted,
                    Imuno$tpublic.accepted)
supervisao<-rbind(Agronomia$tsupervisions,Antropo.Arq$tsupervisions,
                  Arquit$tsupervisions,Artes$tsupervisions,
                  Bioquimica$tsupervisions, Botanica$tsupervisions,
                  C.alimentos$tsupervisions, C.comput$tsupervisions, C.p$tsupervisions,
                  Educacao$tsupervisions,
                  E.F$tsupervisions, Enfermagem$tsupervisions, Eng.aero$tsupervisions,
                  Eng.amb$tsupervisions, Eng.civ$tsupervisions,
                  Eng.elet$tsupervisions,Eng.mat$tsupervisions,
                  Eng.mec$tsupervisions,Eng.nuc$tsupervisions,
                  Eng.prod$tsupervisions,Eng.qui$tsupervisions,
                  Eng.Trans$tsupervisions,Estat$tsupervisions,
                  Farmacia$tsupervisions,Farmaco$tsupervisions,
                  Filo$tsupervisions,Fisica$tsupervisions,
                  Fisio$tsupervisions,Fono$tsupervisions,
                  Genet$tsupervisions,Geo$tsupervisions,
                  Hist$tsupervisions,Letras$tsupervisions,
                  Ling$tsupervisions,Mat$tsupervisions,
                  Med$tsupervisions,Med.vet$tsupervisions,
                  Microbio$tsupervisions,Morfo$tsupervisions,
                  Museo$tsupervisions,Nut$tsupervisions,
                  Oceano$tsupervisions,Odonto$tsupervisions,
                  Parasito$tsupervisions,Planej$tsupervisions,
                  Psico$tsupervisions,Quim$tsupervisions,
                  Recur.flor$tsupervisions,Recur.pes$tsupervisions,
                  Saude.col$tsupervisions,Socio$tsupervisions,
                  SSo$tsupervisions,Zoolo$tsupervisions,
                  Zootec$tsupervisions,
                  Biof$tsupervisions,
                  C.inf$tsupervisions,
                  Comu$tsupervisions,
                  Demo$tsupervisions,
                  Direto$tsupervisions,
                  Eco$tsupervisions,
                  Econ$tsupervisions,
                  Imuno$tsupervisions)
livros<-rbind(Agronomia$tbooks,Antropo.Arq$tbooks,
              Arquit$tbooks,Artes$tbooks,
              Bioquimica$tbooks, Botanica$tbooks,
              C.alimentos$tbooks, C.comput$tbooks, C.p$tbooks,
              Educacao$tbooks,
              E.F$tbooks, Enfermagem$tbooks, Eng.aero$tbooks,
              Eng.amb$tbooks, Eng.civ$tbooks,
              Eng.elet$tbooks,Eng.mat$tbooks,
              Eng.mec$tbooks,Eng.nuc$tbooks,
              Eng.prod$tbooks,Eng.qui$tbooks,
              Eng.Trans$tbooks,Estat$tbooks,
              Farmacia$tbooks,Farmaco$tbooks,
              Filo$tbooks,Fisica$tbooks,
              Fisio$tbooks,Fono$tbooks,
              Genet$tbooks,Geo$tbooks,
              Hist$tbooks,Letras$tbooks,
              Ling$tbooks,Mat$tbooks,
              Med$tbooks,Med.vet$tbooks,
              Microbio$tbooks,Morfo$tbooks,
              Museo$tbooks,Nut$tbooks,
              Oceano$tbooks,Odonto$tbooks,
              Parasito$tbooks,Planej$tbooks,
              Psico$tbooks,Quim$tbooks,
              Recur.flor$tbooks,Recur.pes$tbooks,
              Saude.col$tbooks,Socio$tbooks,
              SSo$tbooks,Zoolo$tbooks,
              Zootec$tbooks,Biof$tbooks,
              C.inf$tbooks,
              Comu$tbooks,
              Demo$tbooks,
              Direto$tbooks,
              Eco$tbooks,
              Econ$tbooks,
              Imuno$tbooks)

conferencias<-rbind(Agronomia$tconferences,Antropo.Arq$tconferences,
                    Arquit$tconferences,Artes$tconferences,
                    Bioquimica$tconferences, Botanica$tconferences,
                    C.alimentos$tconferences, C.comput$tconferences, C.p$tconferences,
                    Educacao$tconferences,
                    E.F$tconferences, Enfermagem$tconferences, Eng.aero$tconferences,
                    Eng.amb$tconferences, Eng.civ$tconferences,
                    Eng.elet$tconferences,Eng.mat$tconferences,
                    Eng.mec$tconferences,Eng.nuc$tconferences,
                    Eng.prod$tconferences,Eng.qui$tconferences,
                    Eng.Trans$tconferences,Estat$tconferences,
                    Farmacia$tconferences,Farmaco$tconferences,
                    Filo$tconferences,Fisica$tconferences,
                    Fisio$tconferences,Fono$tconferences,
                    Genet$tconferences,Geo$tconferences,
                    Hist$tconferences,Letras$tconferences,
                    Ling$tconferences,Mat$tconferences,
                    Med$tconferences,Med.vet$tconferences,
                    Microbio$tconferences,Morfo$tconferences,
                    Museo$tconferences,Nut$tconferences,
                    Oceano$tconferences,Odonto$tconferences,
                    Parasito$tconferences,Planej$tconferences,
                    Psico$tconferences,Quim$tconferences,
                    Recur.flor$tconferences,Recur.pes$tconferences,
                    Saude.col$tconferences,Socio$tconferences,
                    SSo$tconferences,Zoolo$tconferences,
                    Zootec$tconferences,Biof$tconferences,
                    C.inf$tconferences,
                    Comu$tconferences,
                    Demo$tconferences,
                    Direto$tconferences,
                    Eco$tconferences,
                    Econ$tconferences,
                    Imuno$tconferences)
write.csv(pesquisador, "pesquisador.csv")
write.csv(publi, "publicacoes.csv")
write.csv(publi.aceita, "publiaceitas.csv")
write.csv(supervisao, "supervisao.csv")
write.csv(livros, "livros.csv")
write.csv(conferencias, "conferencias.csv")
