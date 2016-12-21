library(data.table)
library(stringi)

pmqc.file <- "Dados.PMQC.16.12"
pmqc.path <- "/Volumes/NO NAME/ExplAnalysis_PMQC/"
mypath <- paste0(pmqc.path,pmqc.file,".csv")

# cclass <- rep("character",23)
pmqc.dt <- fread(mypath,header=T,na.strings="",select=c(1,2,6,7,12:14,18,20,21),
                 check.names=T,colClasses="character",encoding="Latin-1")
# pmqc.dt <- fread(mypath,header=T,na.strings="",select=c(1:7,12:23),
#                  check.names=T,colClasses=cclass,encoding="UTF-8")
# pmqc.dt <- fread(mypath,header=T,na.strings="",select=c(1:7,12:23),
#                  check.names=T,colClasses=cclass)
names(pmqc.dt) <- make.names(names(pmqc.dt))
# Eliminating special characters and diacritical marks (accents)
pmqc.dt[,Nao.conforme:=stri_trans_general(pmqc.dt[,Nao.conforme],"Latin-ASCII")]
# pmqc.dt[,Regiao.politica:=stri_trans_general(pmqc.dt[,Regiao.politica],"Latin-ASCII")]
# pmqc.dt[,Bandeira:=stri_trans_general(pmqc.dt[,Bandeira],"Latin-ASCII")]
pmqc.dt[,Componente:=stri_trans_general(pmqc.dt[,Componente],"Latin-ASCII")]
pmqc.dt[,Contratada:=stri_trans_general(pmqc.dt[,Contratada],"Latin-ASCII")]
pmqc.dt[,Ensaio:=stri_trans_general(pmqc.dt[,Ensaio],"Latin-ASCII")]
pmqc.dt[,Grupo.produto:=stri_trans_general(pmqc.dt[,Grupo.produto],"Latin-ASCII")]
pmqc.dt[,Id.text:=stri_trans_general(pmqc.dt[,Id.text],"Latin-ASCII")]
# pmqc.dt[,Localidade:=stri_trans_general(pmqc.dt[,Localidade],"Latin-ASCII")]
# pmqc.dt[,Posto:=stri_trans_general(pmqc.dt[,Posto],"Latin-ASCII")]
pmqc.dt[,Produto:=stri_trans_general(pmqc.dt[,Produto],"Latin-ASCII")]
# pmqc.dt[,Produto:=stri_trans_general(pmqc.dt[,Produto],"Any-Hex/Unicode")]
# pmqc.dt[,Regiao:=stri_trans_general(pmqc.dt[,Regiao],"Latin-ASCII")]

# ensaios.dt <- data.table(Componente=pmqc.dt[,unique(Componente)],
#                          Ensaio.=c("Aspecto","Cor","ME20C","TEth",rep("Dest",5),
#                                    "CElet","ME20C","TAlc","pH","THC","TMet",
#                                    "Enxofre",rep("Dest",4),"ME20C","Fulgor",
#                                    "CorASTM","Dest","Marcador","B100","Enxofre",
#                                    "MatPart","AguaLivre","Dest","Fulgor",
#                                    "Enxofre","Enxofre","Fulgor"))

x33 <- c("Aspecto","Condutividade Eletrica","Cor","Massa Especifica a 20ºC",
         "Mat. Part.","Potencial Hidrogenionico (pH)","Teor Alcoolico",
         "Teor de Hidrocarbonetos","Teor de Metanol","Agua Livre",
         "Destilacao - 10% Evaporado","Destilacao - 50% Evaporado",
         "Destilacao - 90% Evaporado","Destilacao - PFE","Destilacao - Perda",
         "Destilacao - Residuo","Massa Especifica a 20°C","Teor de Etanol",
         "Teor de Marcador","Destilacao - 10% Recuperados",
         "Destilacao - 50% Recuperados","Destilacao - 95% Recuperados",
         "ME a 20ºC","Ponto de Fulgor (D93)","Teor de Biodiesel",
         "Destilacao - 85% Recuperados","Destilacao - 90% Recuperados",
         "Teor de Enxofre (D4294)","Ponto de Fulgor (D56)","Cor ASTM",
         "Teor de Enxofre (D5453)","Teor de Enxofre","Aromatico","Benzeno",
         "IAD","MON","Olefina","RON","Saturado","Numero de Cetano Derivado",
         "Teor de Agua e Sedimentos","Teor de Agua","Material nao Volatil")

x42 <- c("Aspecto","CElet","Cor","ME20C","MatPart","pH","TAlc","THC","TMet",
         "AguaLivre","Dest","Dest","Dest","Dest","Dest","Dest","ME20C","TEth",
         "Marcador","Dest","Dest","Dest","ME20C","Fulgor","B100","Dest","Dest",
         "Enxofre","Fulgor","CorASTM","Enxofre","Enxofre","Aromatico","Benzeno",
         "IAD","MON","Olefina","RON","Saturado","Cetano","TAgSed","TAg",
         "MatNaoVol")

ensaios.dt <- data.table(Componente=x33,Ensaio.=x42)

# write.csv(ensaios.dt,file="/Volumes/NO NAME/ExplAnalysis_PMQC/ensaios.dt.csv",
#           row.names=FALSE)
# read.csv("/Volumes/NO NAME/ExplAnalysis_PMQC/ensaios.dt.csv")

# ensaios.dt <- fread(
#         "/Volumes/NO NAME/ExplAnalysis_PMQC/ensaios.dt.csv",
#         header=T,
#         check.names=T,colClasses=rep("character",2),encoding="unknown"
#         )

# Encoding(ensaios.dt[,Componente]) <- "latin1"
# ensaios.dt[,Componente:=stri_trans_general(ensaios.dt[,Componente],"Latin-ASCII")]

setkey(ensaios.dt,Componente)
setkey(pmqc.dt,Id.numeric,Test.number,Componente)

# Creating column Ensaio. due to the presence of Sumario* in Ensaio
for(i in ensaios.dt[,Componente]) {
        pmqc.dt[Componente==i,Ensaio.:=ensaios.dt[Componente==i,Ensaio.]]
}

pmqc.un.dt <- pmqc.dt[!duplicated(pmqc.dt[,.(Id.numeric,Componente)]) & 
                              !is.na(pmqc.dt[,Resultado])]

# pmqc.dt[is.na(Ensaio.)]
# pmqc.un.dt[is.na(Ensaio.)]
# dput(pmqc.dt,"/Volumes/NO NAME/ExplAnalysis_PMQC/Dados.PMQC.16.07.pmqc.dt.R")
# dput(pmqc.un.dt,"/Volumes/NO NAME/ExplAnalysis_PMQC/Dados.PMQC.16.07.pmqc.un.dt.R")



write.csv(pmqc.dt,paste0(pmqc.path,pmqc.file,".pmqc.dt.csv"),
          row.names=FALSE)
write.csv(pmqc.un.dt,paste0(pmqc.path,pmqc.file,".pmqc.un.dt.csv"),
          row.names=FALSE)
