#  READING CREDSCO_BIN
 load("d:/karina/docencia/DataMiningEI/Practiques/2CredscoProfiling/credscok_bin")
dd <- read.table("D:/karina/docencia/DataMiningGEI/practiques/3Visualization/credscoclean.csv",header=T, sep=";");
                  

objects()

#
# VISUALISATION OF DATA
#
# PRINCIPAL COMPONENT ANALYSIS OF CONTINcUOUS VARIABLES, WITH Dictamen PROJECTED AS ILLUSTRATIVE
#

# CREATION OF THE DATA FRAME OF CONTINUOUS VARIABLES

attach(dd)

Rati.Fin = 100*Importe.solicitado/Precio.del.bien.financiado

hist(Rati.Fin)

# CAPACITY TO SAVE

Estalvi <- (Ingresos-Gastos-(Cargas.patrimoniales/100))/(Importe.solicitado/Plazo)

hist(Estalvi)

dd$Estalvi=Estalvi
dd$Rati.Fin=Rati.Fin


dcon <- data.frame (Antiguedad.Trabajo,Plazo,Edad,Gastos,Ingresos,Patrimonio,Cargas.patrimoniales,Importe.solicitado,Precio.del.bien.financiado,Estalvi, Rati.Fin)



# PRINCIPAL COMPONENT ANALYSIS OF dcon

pc1 = prcomp(dcon, scale=T)

print(pc1)




# WHICH PERCENTAGE OF THE TOTAL INERTIA IS REPRESENTED IN SUBSPACES?

inerProj<- pc1$sdev^2 
totalIner<- sum(inerProj)
pinerEix<- 100*inerProj/totalIner

#Cummulated Inertia in subspaces, from first principal component to the 11th dimension subspace
barplot(100*cumsum(pc1$sdev[1:dim(dcon)[2]]^2)/dim(dcon)[2])


# SELECTION OF THE SIGIFICATIVE DIMENSIONS (LAST ELBOW TECHNIQUE) (DIFFICULT THIS TIME EH!)

nd = 7

# STORAGE OF THE EIGENVALUES, EIGENVECTORS AND PROJECTIONS IN THE nd DIMENSIONS

lbd = pc1$sdev[1:nd]^2
U = pc1$rotation[,1:nd]
Psi = pc1$x[,1:nd]

# STORAGE OF LABELS FOR INDIVIDUALS AND VARIABLES

iden = row.names(dcon)
etiq = names(dcon)
ze = rep(0,length(etiq)) # WE WILL NEED THIS VECTOR AFTERWARDS FOR THE GRAPHICS

# PLOT OF INDIVIDUALS

plot(Psi[,1],Psi[,2],type="n")
text(Psi[,1],Psi[,2],labels=iden)
axis(side=1, pos= 0, labels = F, col="cyan")
axis(side=3, pos= 0, labels = F, col="cyan")
axis(side=2, pos= 0, labels = F, col="cyan")
axis(side=4, pos= 0, labels = F, col="cyan")

# PLOT OF VARIABLES

Phi = cor(dcon,Psi)
plot(Phi[,1],Phi[,2],type="none",xlim=c(min(Phi[,1],0),max(Phi[,1],0)))
axis(side=1, pos= 0, labels = F)
axis(side=3, pos= 0, labels = F)
axis(side=2, pos= 0, labels = F)
axis(side=4, pos= 0, labels = F)
arrows(ze, ze, Phi[,1], Phi[,2], length = 0.07,col="blue")
text(Phi[,1],Phi[,2],labels=etiq,col="darkblue")


# BIPLOT DE RP

biplot(Psi,U)

#fm = round(max(abs(Psi[,1]))) # ES UN FACTOR D'ESCALA PER DIBUIXAR LES FLETXES MES VISIBLES EN EL GRAFIC

#plot(Psi[,1],Psi[,2],col="gray")
#arrows(ze, ze, fm*U[,1], fm*U[,2], length = 0.07,col="red")
#text(fm*U[,1],fm*U[,2],labels=etiq,col="red")


# 
# INTERPRET THE TWO FIRST AXES?
#
cor(dcon,Psi)

# CALCULATE THE PROJECTIONS OF THE VARIABLES PHI

U %*% diag(sqrt(lbd))

# WRITE THE FIRST PRINCIPAL COMPONENT FUNCTION OF THE OBSERVED VARIABLES
#

# WRITE THE Ingressos AS FUNCTION OF THE SELECTED FACTORS
# EVERY PRINCIPAL COMPONENT IS A COMBINATION OF THE ORIGINAL VARIABLESS (STANDARDIZED SICE SCALE=T)
# 

Z    = scale(dcon)
reg1 = lm(Psi[,1]~ Z)
print(reg1)
U[,1]

# LIKEWISE EVERY VARIABLE CAN BE EXPLAINED BY THE PRINCIPAL COMPONENTS (LATENT FACTORS) 

#reg2 = lm(Z[,1]~ pc1$x[,1:nd])
#print(reg2)
#U[1,]

# HOWEVER TO IDENTIFY THE LATENT FACTORS IS BETTER TO PERFORM A ROTATION OF THE DIRECTIONS TO FACILITATE THIR INTERPRETABILITY
# RUN VARIMAX ROTATION AND LOOK FOR LOADING (=CORRELATION) > 0.7

#pcrot = varimax(Phi)
#print(pcrot)

# PROJECTION OF dICATAMEN AS ILLUSTRATIVE

# PROJECCIÓ OF INDIVIDUALS DIFFERENTIATING THE Dictamen
# (we need a numeric Dictamen to color)

idict=dd[,1]
plot(Psi[,1],Psi[,2],col=idict)
axis(side=1, pos= 0, labels = F, col="darkgray")
axis(side=3, pos= 0, labels = F, col="darkgray")
axis(side=2, pos= 0, labels = F, col="darkgray")
axis(side=4, pos= 0, labels = F, col="darkgray")
legend("topright",c("pos","neg"),pch=1,col=c(1,2))


#Tipus de contracte
tcon=dd[,8]
plot(Psi[,1],Psi[,2],col=tcon)
axis(side=1, pos= 0, labels = F, col="darkgray")
axis(side=3, pos= 0, labels = F, col="darkgray")
axis(side=2, pos= 0, labels = F, col="darkgray")
axis(side=4, pos= 0, labels = F, col="darkgray")
legend("topright",c("fix","tmp", "aut", "otro"),pch=1,col=c(1,2,3,4,5))


# NOW WE PROJECT THE CDG OF BOTH LEVELS OF Dictament

fdic1 = tapply(Psi[,1],Dictamen,mean)
fdic2 = tapply(Psi[,2],Dictamen,mean) 

points(fdic1,fdic2,pch=16,col="yellow", labels=levels(Dictamen))
text(fdic1,fdic2,labels=levels(Dictamen),col="yellow")

levels(Dictamen) <- c(NA, "positiu","negatiu")
levels(Vivienda) <- c("VivUnkown", "lloguer","escriptura","contr_privat","ignora_cont","pares","altres viv")
levels(Estado.civil) <- c("ECUnknown", "solter","casat","vidu","separat","divorciat")
levels(Registros) <- c("reg_no","reg_si")
levels(Tipo.trabajo) <- c("WorkingTypeUnknown","fixe","temporal","autonom","altres sit")

fdic1 = tapply(Psi[,1],Dictamen,mean)
fdic2 = tapply(Psi[,2],Dictamen,mean) 

points(fdic1,fdic2,pch=16,col="yellow")
text(fdic1,fdic2,labels=levels(Dictamen),col="yellow")

#tipus de contrato
fdic1 = tapply(Psi[,1],Tipo.trabajo,mean)
fdic2 = tapply(Psi[,2],Tipo.trabajo,mean) 

#points(fdic1,fdic2,pch=16,col="red")
text(fdic1,fdic2,labels=levels(Tipo.trabajo),col="red")


#Les variables numèriques admeten una discretització auxiliar per representar-ne els trams units i enriquir la interpretació
#mostra la no linealitat


#RECODING THE CONTINUOUS VARIABLES TO CATEGORICAL (TO ASSESS NON LINEARITIES)

antigR <- cut(Antiguedad.Trabajo, breaks=c(-1,1,3,8,14,99))
plaçR <- cut(Plazo, breaks=c(0,12,24,36,48,99))
edatR <- cut(Edad, breaks=c(0,25,30,40,50,99))
despesesR <- cut(Gastos, breaks=c(0,40,50,60,80,9999))
ingressosR <- cut(Ingresos, breaks=c(0,80,110,140,190,9999))
patrimoniR <- cut(Patrimonio, breaks=c(-1,0,3000,5000,8000,999999))
carrecsR <- cut(Cargas.patrimoniales, breaks=c(-1,0,500,1500,2500,999999))
importR <- cut(Importe.solicitado, breaks=c(0,600,900,1100,1400,99999))
preuR <- cut(Precio.del.bien.financiado, breaks=c(0,1000,1300,1500,1800,99999))
ratfinR <- cut(Rati.Fin, breaks=c(0,50,70,80,90,100))
estalviR <- cut(Estalvi, breaks=c(-99,0,2,4,6,99))

levels(antigR) <- paste("Antig",levels(antigR))
levels(plaçR) <- paste("Plaç",levels(plaçR))
levels(edatR) <- paste("Edat",levels(edatR))
levels(despesesR) <- paste("Desp",levels(despesesR))
levels(ingressosR) <- paste("Ingr",levels(ingressosR))
levels(patrimoniR) <- paste("Patr",levels(patrimoniR))
levels(carrecsR) <- paste("Carr",levels(carrecsR))
levels(importR) <- paste("Import",levels(importR))
levels(preuR) <- paste("Preu",levels(preuR))
levels(ratfinR) <- paste("Ratfin",levels(ratfinR))
levels(estalviR) <- paste("Estalv",levels(estalviR))



#Projectem els trams d'edat sobre el primer pla factorial



fdic1 = tapply(Psi[,1],edatR,mean)
fdic2 = tapply(Psi[,2],edatR,mean) 

points(fdic1,fdic2,pch=16,col="cyan")
text(fdic1,fdic2,labels=levels(edatR),col="cyan")

#connect modalities of same variable together
lines(fdic1,fdic2,pch=16,col="cyan")


# 
# LETS TAKE ALL THE INFORMATION AVAILABLE: ALL CATEGORICAL VARIABLES 
# WE WILL REPEAT THE SAME PCA ANALYSIS BUT WITH MULTIPLE CORRESPONDENCE ANALYSIS

# CREATION OF THE DATA FRAME OF CATEGORICAL VARIABLES

Dictamen    <- as.factor(Dictamen)
Vivienda     <- as.factor(Vivienda)
Estado.civil <- as.factor(Estado.civil)
Registros   <- as.factor(Registros)
Tipo.trabajo <- as.factor(Tipo.trabajo)

dcat <- data.frame(edatR,antigR,plaçR,despesesR,ingressosR,patrimoniR,carrecsR,importR,preuR,ratfinR,estalviR,Vivienda,Estado.civil,Registros,Tipo.trabajo)

# CREATION OF THE DATA FRAME WITH THE ILLUSTRATIVE VARIABLE

dict <- data.frame(Dictamen)


# LOADING ACM FUNTION

source("D:/karina/docencia/DataMiningEI/practiques/3Visualization/acm.r") # LA TROBAREU A LA PAGINA WEB DE L'ASSIGNATURA



ac1 <- acm(dcat,dict)

attributes(ac1)

# HISTOGRAM OF EIGENVALUES OF THE MCA (WHY ARE SO DIFFERENT?)

barplot(ac1$vaps)

# WHAT IS THE SIGNIFICANT DIMENSION IN MCA?

i <- 1

while (ac1$vaps[i] > 1/ncol(dcat)) i = i+1

nd = i-1

# PLOT OF INDIVIDUALS (USELESS)

plot(ac1$rs[,1],ac1$rs[,2],type="n")
text(ac1$rs[,1],ac1$rs[,2],labels=row.names(ac1$rs))
axis(side=1, pos= 0, labels = F, col="cyan")
axis(side=3, pos= 0, labels = F, col="cyan")
axis(side=2, pos= 0, labels = F, col="cyan")
axis(side=4, pos= 0, labels = F, col="cyan")

# PLOT OF MODALITIES (LEVELS OF CATEGORICAL VARIABLES) 

ivar = gl(11,5)
ivar = c(ivar,rep(12,6),rep(13,5),rep(14,2),rep(15,4)) # ivar IDENTIFIES TO WHICH CATEGORICAL VARIABLE BELONGS EACH MODALITY, TO BE USED FOR COLOR PURPOSES)

plot(ac1$cs[,1],ac1$cs[,2],type="none")
axis(side=1, pos= 0, labels = F, col="gray")
axis(side=3, pos= 0, labels = F, col="gray")
axis(side=2, pos= 0, labels = F, col="gray")
axis(side=4, pos= 0, labels = F, col="gray")
text(ac1$cs[,1],ac1$cs[,2],labels=row.names(ac1$cs),col=ivar,cex=0.6)


# PLOT OF THE LEVELS OF Dictamen

text(ac1$csup[,1],ac1$csup[,2],labels=row.names(ac1$csup))

# PLOT of INDIVIDUALS ACCODING THE LEVEL OF dICTAMEN

plot(ac1$rs[,1],ac1$rs[,2],col=idict)


