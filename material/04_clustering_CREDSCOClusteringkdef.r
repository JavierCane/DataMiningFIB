#Retrieve the data saved AFTER the profiling practice...... this means data already cleaned
#load("d:/karina/docencia/DataMiningEI/Practiques/4Clustering/credscok_bin")

dd <- read.table("D:/karina/docencia/DataMiningGEI/practiques/4Clustering/credscoClean.csv",header=T, sep=";");



attach(dd)

Rati.Fin = 100*Importe.solicitado/Precio.del.bien.financiado

hist(Rati.Fin)

# CAPACITY TO SAVE

Estalvi <- (Ingresos-Gastos-(Cargas.patrimoniales/100))/(Importe.solicitado/Plazo)

hist(Estalvi)

dd$Estalvi=Estalvi
dd$Rati.Fin=Rati.Fin

#set a list of numerical variables


dcon <- data.frame (Antiguedad.Trabajo,Plazo,Edad,Gastos,Ingresos,Patrimonio,Cargas.patrimoniales,Importe.solicitado,Precio.del.bien.financiado,Estalvi, Rati.Fin)

#use the numerical variables to find principal components


#principal correspondence analysis of numerical variables

#
# CLUSTERING
#

#
# LETS TAKE THE INDIVIDUALS DEFINED BY FACTORIAL COORDINATES ISSUED FROM THE PCA (alternatively MCA or PCA+MCA)
# 
pc1 = prcomp(dcon, scale=T)

print(pc1)

#let's take the 7 first principal components

nd=7

lbd = pc1$sdev[1:nd]^2
U = pc1$rotation[,1:nd]
Psi = pc1$x[,1:nd]


Dictamen    <- as.factor(Dictamen)
Vivienda     <- as.factor(Vivienda)
Estado.civil <- as.factor(Estado.civil)
Registros   <- as.factor(Registros)
Tipo.trabajo <- as.factor(Tipo.trabajo)
levels(Dictamen) <- c(NA, "positiu","negatiu")
levels(Vivienda) <- c("VivUnkown", "lloguer","escriptura","contr_privat","ignora_cont","pares","altres viv")
levels(Estado.civil) <- c("ECUnknown", "solter","casat","vidu","separat","divorciat")
levels(Registros) <- c("reg_no","reg_si")
levels(Tipo.trabajo) <- c("WorkingTypeUnknown","fixe","temporal","autonom","altres sit")

dcat <- data.frame(Vivienda,Estado.civil,Registros,Tipo.trabajo)
dict <- data.frame(Dictamen)

source("D:/karina/docencia/DataMiningEI/practiques/4Clustering/acm.r") # LA TROBAREU A LA PAGINA WEB DE L'ASSIGNATURA


ac1 <- acm(dcat,dict)

i <- 1

while (ac1$vaps[i] > 1/ncol(dcat)) i = i+1

nd = i-1

FI= ac1$rs[,1:nd]

factors=data.frame(Psi, FI)

objects(factors)

# KMEANS RUN, BUT HOW MANY CLASSES?

k1 <- kmeans(factors,5)

attributes(k1)

k1$size

k1$withinss

k1$centers

# LETS COMPUTE THE DECOMPOSITION OF INERTIA

Bss <- sum(rowSums(k1$centers^2)*k1$size)
Bss
Wss <- sum(k1$withinss)
Wss
Tss <- sum(rowSums(Psi^2))
Tss

Bss+Wss

Ib1 <- 100*Bss/(Bss+Wss)
Ib1

# LETS REPEAT THE KMEANS RUN WITH K=5

k2 <- kmeans(factors,5)
k2$size

Bss <- sum(rowSums(k2$centers^2)*k2$size)
Bss
Wss <- sum(k2$withinss)
Wss

Ib2 <- 100*Bss/(Bss+Wss)
Ib2

# WHY WE HAVE OBTAINED DIFFERENT RESULTS?, AND WHICH RUN IS BETTER?

# NOW TRY K=8

k3 <- kmeans(factors,8)
k3$size

Bss <- sum(rowSums(k3$centers^2)*k3$size)
Wss <- sum(k3$withinss)

Ib3 <- 100*Bss/(Bss+Wss)
Ib3


# HIERARCHICAL CLUSTERING

d  <- dist(factors)
h1 <- hclust(d,method="ward")  # NOTICE THE COST
plot(h1)

# BUT WE ONLY NEED WHERE THERE ARE THE LEAPS OF THE HEIGHT

# WHERE ARE THER THE LEAPS? WHERE WILL YOU CUT THE DENDREOGRAM?, HOW MANY CLASSES WILL YOU OBTAIN?

nc = 3

c1 <- cutree(h1,nc)

c1[1:20]

# LETS SEE THE PARTITION VISUALLY

plot(Psi[,1],Psi[,2],col=c1,main="Clustering of credit data in 3 classes")
legend("topleft",c("c1","c2","c3"),pch=1,col=c(1:3))

#plot(FI[,1],FI[,2],col=c1,main="Clustering of credit data in 3 classes")
#legend("topleft",c("c1","c2","c3"),pch=1,col=c(1:3))

# LETS SEE THE QUALITY OF THE HIERARCHICAL PARTITION

cdg <- aggregate(as.data.frame(factors),list(c1),mean)[,2:(nd+1)]
cdg


Bss <- sum(rowSums(cdg^2)*as.numeric(table(c1)))

Ib4 <- 100*Bss/Tss
Ib4

# LETS CONSOLIDATE THE PARTITION

k5 <- kmeans(Psi,centers=cdg)
k5$size

Bss <- sum(rowSums(k5$centers^2)*k5$size)
Wss <- sum(k5$withinss)

Ib5 <- 100*Bss/(Bss+Wss)
Ib5

#
# CLUSTERING OF LARGE DATA SETS
#

# FIRST 2 KMEANS WITH K=14

n1 = 14

k1 <- kmeans(factors,n1)
k2 <- kmeans(factors,n1)

table(k2$cluster,k1$cluster)

clas <- (k2$cluster-1)*n1+k1$cluster

freq <- table(clas)
freq[1:10]

# WHAT DO WE HAVE IN VECTOR freq?

cdclas <- aggregate(as.data.frame(Psi),list(clas),mean)[,2:(nd+1)]

# SECOND HIERARCHICAL CLUSTERING UPON THE CENTROIDS OF CROSSING THE 2 KMEANS PARTITIONS

d2 <- dist(cdclas)
h2 <- hclust(d2,method="ward",members=freq)  # COMPARE THE COST

plot(h2)
barplot(h2$height[(dim(cdclas)[1]-40):(dim(cdclas)[1]-1)])

c2 <- cutree(h2,nc)

# WARNING c2 NOT ALLOW TO CLASSIFY DIRECTLY THE 4446 INDIVIDUALS BUT THE ELEMENTS OF freq
# WARNING cdclas CONTAINS THE COORDINATES OF THE CROSSINGS BUT THEY NEED TO BE WEIGHED BY  freq

#cdg <- aggregate(cdclas,list(c2),mean)[,2:(nd+1)]  ALL ELEMENTS COUNT EQUAL

cdg <- aggregate((diag(freq/sum(freq)) %*% as.matrix(cdclas)),list(c2),sum)[,2:(nd+1)]
cdg


# CONSOLIDATION

k6 <- kmeans(Psi,centers=cdg)
k6$size

Bss <- sum(rowSums(k6$centers^2)*k6$size)
Wss <- sum(k6$withinss)

Ib6 <- 100*Bss/(Bss+Wss)
Ib6


# PROBALISTIC CLUSTERING 

library(mclust)

emc <- Mclust(Psi,G=7:9) # AGAIN THE COST

print(emc)

attributes(emc)

# NOW FOR EACH INDIVIDUAL WE HAVE A PROBABILITY TO BELONG TO EACH CLASS

emc$z[1:10,]

# LETS SEE TO WHICH CLASS IS ASSGINED EVERY INDIVIDUAL (BY MAX PROB)

emc$classification[1:10]

# LETS COMPUTE THE QUALITY OF THE PARTITION

cdg <- aggregate(as.data.frame(Psi),list(emc$classification),mean)[,2:(nd+1)]
cdg

Bss <- sum(rowSums(cdg^2)*as.numeric(table(c1)))

Ib7 <- 100*Bss/Tss
Ib7

# LETS SEE THE PARTITION VISUALLY

plot(Psi[,1],Psi[,2],col=emc$classification,main="Clustering of credit data in 8 classes")
legend("topleft",c("c1","c2","c3","c4","c5","c6","c7","c8"),pch=1,col=c(1:8))


