#Insertar el path de donde se encuentre el archivo
# setwd("/Users/javierferrer/Google Drive/Studies/02 MD/MD grupo/datos") # No necesario si abrimos fichero .R

#Modificar el nombre del archivo a conveniencia
data <- read.table("/Users/javierferrer/Documents/Uni/MD/data/state_varios-oldest_ge_45-youngest_le_27-5910_reg.csv",sep=";",header=TRUE)


#Lab 1: Data load
#Treating variables as qualitative/quantitative
data$record_type <- as.factor(data$record_type) # Factor -> Qualitative
data$group_size <- as.factor(data$group_size)
data$day <- as.factor(data$day)
data$time <- as.numeric(data$time) #Numeric -> Quantitative
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

#Lab 2: Preprocessing
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


#Lab 3: Principal Component Analysis
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

#Lab 4: Feature selection & K-means

pvalcon <- NULL

varc <- list(data$shopping_pt, data$record_type, data$day, data$time, data$state, data$location, data$group_size, data$homeowner, data$car_age, data$car_value, data$risk_factor, data$age_oldest, data$age_youngest, data$married_couple, data$C_previous, data$duration_previous, data$A, data$B, data$C, data$D, data$E, data$F, data$G, data$cost)

qqnorm(data$shopping_pt)
qqline(data$shopping_pt)
ks.test(x=data$shopping_pt, y='pnorm')

for (i in 1:11) { pvalcon[i] <- (oneway.test(varc[[i]]~data$risk_factor))$p.value }

pvalcon = matrix(pvalcon)
#row.names(pvalcon) = c("Antig_feina","Pla�","Edat","Despeses","Ingressos","Patrimoni","Carrecs_pat","Import_sol","Preu_finan","Rati_fin","Estalvi")
row.names(pvalcon) = c("Antiquity","Timing","Age","Expenditures","Incomes","Patrimonium","Loads","QuantityRequired","TargetPrice","RatioFin","SavingCap")

# ORDERED LIST OF CONTINUOUS VARIABLES ACCORDING THEIR DEPENDENCE OF Dictamen

sort(pvalcon[,1])

#
# FEATURE SELECTION: FOR CATEGORICAL VARIABLES  CHI-SQUARE
# RESPONSE VARIABLE: DICTAMEN
#

pvalcat <- NULL
#vark <- list(edatR,antigR,pla�R,despesesR,ingressosR,patrimoniR,carrecsR,importR,preuR,ratfinR,estalviR,Vivienda,Estado.civil,Registros,Tipo.trabajo)

vark <- list(Vivienda,Estado.civil,Registros,Tipo.trabajo)

for (i in 1:4) { pvalcat[i] <- (chisq.test(vark[[i]],Dictamen))$p.value }

pvalcat = matrix(pvalcat)
row.names(pvalcat) = c("Housing","CivilStatus","Registers","WorkingType")



# ORDERED LIST OF CATEGORICAL VARIABLES ACCORDING THEIR DEPENDENCE OF Dictamen

sort(pvalcat[,1])

#let's have a global look with the whole set of variables
pval<-NULL
pval=append(pvalcat, pvalcon)
pval = matrix(pval)
row.names(pval)= c("Antiquity","Timing","Age","Expenditures","Incomes","Patrimonium","Loads","QuantityRequired","TargetPrice","RatioFin","SavingCap","Housing","CivilStatus","Registers","WorkingType")


sort(pval[,1])
