#Insertar el path de donde se encuentre el archivo
# setwd("/Users/javierferrer/Google Drive/Studies/02 MD/MD grupo/datos") # No necesario si abrimos fichero .R

#Modificar el nombre del archivo a conveniencia
data <- read.table("/Users/javierferrer/Documents/Uni/MD/data/state_varios-oldest_ge_45-youngest_le_27-5910_reg.csv",sep=";",header=TRUE)


###################################################################################################################################################
################################################################ Lab 1: Data load #################################################################
###################################################################################################################################################

#Treating variables as qualitative/quantitative
data$record_type <- as.factor(data$record_type) # Factor -> Qualitative
data$group_size <- as.factor(data$group_size)
data$day <- as.factor(data$day)
data$time <- as.numeric(data$time) #Numeric -> Quantitative
data$location <- as.factor(data$location)
data$homeowner <- as.factor(data$homeowner)
data$risk_factor <- as.factor(data$risk_factor)
data$married_couple <- as.factor(data$married_couple)
data$C_previous <- as.factor(data$C_previous)
data$A <- as.factor(data$A)
data$B <- as.factor(data$B)
data$C <- as.factor(data$C)
data$D <- as.factor(data$D)
data$E <- as.factor(data$E)
data$F <- as.factor(data$F)
data$G <- as.factor(data$G)

summary(data)


###################################################################################################################################################
############################################################ Lab 2: Preprocessing ############################################################
###################################################################################################################################################

hist(data$customer_ID)
hist(data$shopping_pt) #1
plot(data$record_type)
plot(data$day) #
hist(data$time) #2
plot(data$state)
hist(data$location) #3
plot(data$group_size) #factor
plot(data$homeowner)
hist(data$car_age) #4
plot(data$car_value)
plot(data$risk_factor) #factor
hist(data$age_oldest) #5
hist(data$age_youngest) #6
plot(data$married_couple)
plot(data$C_previous) #factor
hist(data$duration_previous) #7
plot(data$A) #factor
plot(data$B) #factor
plot(data$C) #factor
plot(data$D) #factor
plot(data$E) #factor
plot(data$F) #factor
plot(data$G) #factor
hist(data$cost) #8

dura_previ <-data$duration_previous

C_previ <- data$C_previous
levels(C_previ) <- c(1,2,3,4,5)
for (i in 1:length(C_previ)){
	if(is.na(C_previ[i])) C_previ[i] <- 5
}
summary(C_previ)

factor_risc <- data$risk_factor
levels(factor_risc) <- c(1,2,3,4,5)
for (i in 1:length(factor_risc)){
	if(is.na(factor_risc[i])) factor_risc[i] <- 5
}
summary(factor_risc)

data$C_previous <- C_previ
data$risk_factor <- factor_risc

summary(data)

library(class)

table(is.na(data$duration_previous))

mean_dura_previ = mean(dura_previ[!is.na(dura_previ)])

for (i in 1:length(dura_previ)){
	if(is.na(dura_previ[i])) dura_previ[i] <- mean_dura_previ
}
summary(dura_previ)
data$duration_previous <- dura_previ
#Fixing duration_previous:
#aaux = data[,-17]
#dim(aaux)
#aaux1 = aaux[!is.na(dura_previ),]
#dim(aaux1)
#aaux2 = aaux[is.na(dura_previ),]
#dim(aaux2)
#length(aaux2[,1])
#length(dura_previ[!is.na(dura_previ)])
#knn2.ing = knn(aaux1,aaux2,dura_previ[!is.na(dura_previ)])   # NEITHER AUX1, AUX2 CAN CONTAIN NAs)
#dura_previ[is.na(dura_previ)] = knn.ing

summary(data)

###################################################################################################################################################
####################################################### Lab 3: Principal Component Analysis #######################################################
###################################################################################################################################################

dcon <- data.frame ("Shopping_pt" = data$shopping_pt, "Time" = data$time, "Location" = data$location, "Car_age" = data$car_age, "Age_oldest" = data$age_oldest, "Age_youngest" = data$age_youngest, "Duration_previous" = data$duration_previous, "Cost" = data$cost)
pc1 = prcomp(dcon, scale=T)

# WHICH PERCENTAGE OF THE TOTAL INERTIA IS REPRESENTED IN SUBSPACES?

inerProj<- pc1$sdev^2 
totalIner<- sum(inerProj)
pinerEix<- 100*inerProj/totalIner

#Cummulated Inertia in subspaces, from first principal component to the 11th dimension subspace
barplot(100*cumsum(pc1$sdev[1:dim(dcon)[2]]^2)/dim(dcon)[2])

# SELECTION OF THE SIGIFICATIVE DIMENSIONS

nd = 6

# STORAGE OF THE EIGENVALUES, EIGENVECTORS AND PROJECTIONS IN THE nd DIMENSIONS

lbd = pc1$sdev[1:nd]^2
U = pc1$rotation[,1:nd]
Psi = pc1$x[,1:nd]

# STORAGE OF LABELS FOR INDIVIDUALS AND VARIABLES

etiq = names(dcon)
ze = rep(0,length(etiq)) # WE WILL NEED THIS VECTOR AFTERWARDS FOR THE GRAPHICS


# PLOT OF INDIVIDUALS

plot(Psi[,1],Psi[,2])
axis(side=1, pos= 0, labels = F, col="cyan")
axis(side=3, pos= 0, labels = F, col="cyan")
axis(side=2, pos= 0, labels = F, col="cyan")
axis(side=4, pos= 0, labels = F, col="cyan")

Phi = cor(dcon,Psi)
plot(Phi[,1],Phi[,2],type="none",xlim=c(min(Phi[,1]-0.2,0),max(Phi[,1]+0.2,0)))
axis(side=1, pos= 0, labels = F)
axis(side=3, pos= 0, labels = F)
axis(side=2, pos= 0, labels = F)
axis(side=4, pos= 0, labels = F)
arrows(ze, ze, Phi[,1], Phi[,2], length = 0.07,col="blue")
text(Phi[,1],Phi[,2],labels=etiq,col="darkblue")
fm = round(max(abs(Psi[,1])))


plot(Psi[,1],Psi[,2],col="gray", xlim=range(-6:6), ylim=range(-4:4), pch=20)
arrows(ze, ze, fm*U[,1], fm*U[,2], length = 0.07,col="red")
text(fm*U[,1],fm*U[,2],labels=etiq,col="red")


# 
# INTERPRET THE TWO FIRST AXES?
#
cor(dcon,Psi)

# CALCULATE THE PROJECTIONS OF THE VARIABLES PHI

U %*% diag(sqrt(lbd))

# WRITE THE FIRST PRINCIPAL COMPONENT FUNCTION OF THE OBSERVED VARIABLES
# THE FIRST COMPONENT IS


Z    = scale(dcon)
reg1 = lm(Psi[,1]~ Z)
print(reg1)
U[,1]

# LIKEWISE EVERY VARIABLE CAN BE EXPLAINED BY THE PRINCIPAL COMPONENTS (LATENT FACTORS) 

reg2 = lm(Z[,1]~ pc1$x[,1:nd])
print(reg2)
U[1,]

# HOWEVER TO IDENTIFY THE LATENT FACTORS IS BETTER TO PERFORM A ROTATION OF THE DIRECTIONS TO FACILITATE THIR INTERPRETABILITY
# RUN VARIMAX ROTATION AND LOOK FOR LOADING (=CORRELATION) > 0.7

pcrot = varimax(Phi)
print(pcrot)

# PROJECTION OF CATEGORICAL VALUES AS ILLUSTRATIVE
# (we need a numeric Dictamen to color)

idict=data$risk_factor
plot(Psi[,1],Psi[,2],col=idict)
axis(side=1, pos= 0, labels = F, col="darkgray")
axis(side=3, pos= 0, labels = F, col="darkgray")
axis(side=2, pos= 0, labels = F, col="darkgray")
axis(side=4, pos= 0, labels = F, col="darkgray")
legend("topright",c("1","2","3","4","Not Available"),pch=1,col=c(1,2,3,4,5,6))

#Tipus de contracte
tcon=data$married
plot(Psi[,1],Psi[,2],col=tcon)
axis(side=1, pos= 0, labels = F, col="darkgray")
axis(side=3, pos= 0, labels = F, col="darkgray")
axis(side=2, pos= 0, labels = F, col="darkgray")
axis(side=4, pos= 0, labels = F, col="darkgray")
legend("topright",c("Not married","Married"),pch=1,col=c(1,2,3,4,5,6))

tcon=data$homeowner
plot(Psi[,1],Psi[,2],col=tcon)
axis(side=1, pos= 0, labels = F, col="darkgray")
axis(side=3, pos= 0, labels = F, col="darkgray")
axis(side=2, pos= 0, labels = F, col="darkgray")
axis(side=4, pos= 0, labels = F, col="darkgray")
legend("topright",c("Does not owns a house","Owns a house"),pch=1,col=c(1,2,3,4,5,6))

tcon=data$day
plot(Psi[,1],Psi[,2],col=tcon)
axis(side=1, pos= 0, labels = F, col="darkgray")
axis(side=3, pos= 0, labels = F, col="darkgray")
axis(side=2, pos= 0, labels = F, col="darkgray")
axis(side=4, pos= 0, labels = F, col="darkgray")
legend("topright",c("Dll","Dm", "Dc", "Dj", "Dv","Ds"),pch=1,col=c(1,2,3,4,5,6))

###################################################################################################################################################
######################################################## Lab 4: Feature selection & K-means #######################################################
###################################################################################################################################################

dcat <- data.frame(data$record_type, data$group_size, data$day, data$homeowner, data$risk_factor, data$married_couple, data$C_previous, data$A, data$B, data$C, data$D, data$E, data$F, data$G)
dict <- data.frame(data$shopping_pt, data$time, data$time, data$time, data$time, data$time)

source("/Users/javierferrer/Documents/Uni/MD/assets/acm.r")

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
