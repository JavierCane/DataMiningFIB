#  READING CREDSCO.CSV. NOTE: Change the path of the file for the proper one in your computer
dd <- read.table("D:/karina/docencia/DataMiningGEI/practiques/2Preprocessing/credsco.csv",header=T, sep=";");
                  

dim(dd)

names(dd)

# FIRST CONTROL: MAXIMS, MINIMUMS, ... (FINDING ERRORS AND OUTLIERS)
# WHICH RESPONSE?
# WHICH ARE CATEGORICAL AND WHICH ARE CONTINUOUS
# ARE THERE MISSINGS?

summary(dd)
hist(dd[,10])

# Metainfo: Missing data represented by 0 in qualitative variables
# DEALING WITH MISSINGS: ELIMINATE FROM DICTAMENT MANCANT, VIVENDA MANCANT, ESTAT CIVIL MANCANT I TIPUS DE FEINA MANCANT 
table(dd[,1]==0)
table(dd[,3]==0)
table(dd[,6]==0)
table(dd[,8]==0)
#(there are few missing in qualitative variables, see how many complete rows are available)

dd2 <- dd[dd[,1] != 0 & dd[,3] != 0 & dd[,6] != 0 & dd[,8] != 0,]
dim(dd2)
#just keep as a new modality


# Metainfo: (99999999 MISSING CODE for numerical variables)
# LOOK FOR MISSING VALUES AMONG THE CONTINUOUS VARIABLES 
# LETS WORK WITH DIRECTLY WITH THE VARIABLES OF THE DATA FRAME

objects()

attach(dd)

objects()

table(Ingresos == 99999999)

table(Patrimonio == 99999999)

table(Cargas.patrimoniales == 99999999)

table(Antiguedad.Trabajo == 0)

table(Ingresos == 0)

# The numerical vars have too much missing data. Corresponding rows cannot be deleted
# Missing data treatment required
# Recode missing data to NA, including '0' in Incomes

Ingresos[Ingresos == 99999999 | Ingresos == 0] <- NA
Patrimonio[Patrimonio == 99999999] <- NA
Cargas.patrimoniales[Cargas.patrimoniales == 99999999] <- NA

# WARNING: NOW dd[,10] Ingressos DOESNT HAVE THE SAME CONTENT

summary(dd[,10])
summary(Ingresos)


# IMPUTATION By THE 1NN

library(class)

# FOR EVERY INDIVIDUAL WITH INGRESSOS MISSING WE LOOK FOR THE MOST SIMILAR INDIVIDUAL ACCORDING THE REMAINING VARIABLES AND WE COPY THE VALUE OF INGRESSOS ON THE FIRST 

aux = dd[,-10]
dim(aux)
aux1 = aux[!is.na(Ingresos),]
dim(aux1)
aux2 = aux[is.na(Ingresos),]
dim(aux2)
knn.ing = knn(aux1,aux2,Ingresos[!is.na(Ingresos)])   
# NEITHER AUX1, AUX2 CAN CONTAIN NAs)
Ingresos[is.na(Ingresos)] = knn.ing

# IMPUTATION OF PATRIMONI
aux = dd[,-11]
aux1 = aux[!is.na(Patrimonio),]
aux2 = aux[is.na(Patrimonio),]
knn.pat = knn(aux1,aux2,Patrimonio[!is.na(Patrimonio)])
Patrimonio[is.na(Patrimonio)] = knn.pat

# IMPUTATION OF CARRECS PATRIMONIALS
aux = dd[,-12]
aux1 = aux[!is.na(Cargas.patrimoniales),]
aux2 = aux[is.na(Cargas.patrimoniales),]
knn.car = knn(aux1,aux2, Cargas.patrimoniales[!is.na(Cargas.patrimoniales)])
Cargas.patrimoniales[is.na(Cargas.patrimoniales)] = knn.car
Cargas.patrimoniales[Patrimonio==0] <- 0

#move imputed data to the data frame
dd[,10] <- Ingresos
dd[,11] <- Patrimonio
dd[,12] <- Cargas.patrimoniales

# VERIFY AGAIN MAX AND MIN

dim(dd)
summary(dd)
hist(Ingresos)

# DECLARE CATEGORICAL 

Dictamen    <- as.factor(Dictamen)
Vivienda     <- as.factor(Vivienda)
Estado.civil <- as.factor(Estado.civil)
Registros   <- as.factor(Registros)
Tipo.trabajo <- as.factor(Tipo.trabajo)

Summary(Dictamen)

#look at modalities
levels(Dictamen)
levels(Vivienda)
levels(Estado.civil)
levels(Registros)
levels(Tipo.trabajo)

#labelling modalities, when required. Check dictionary. WARNING: Sequential assignment with levels
levels(Dictamen) <- c(NA, "positiu","negatiu")
levels(Vivienda) <- c("VivUnkown", "lloguer","escriptura","contr_privat","ignora_cont","pares","altres viv")
levels(Estado.civil) <- c("ECUnknown", "solter","casat","vidu","separat","divorciat")
levels(Registros) <- c("reg_no","reg_si")
levels(Tipo.trabajo) <- c("WorkingTypeUnknown","fixe","temporal","autonom","altres sit")

sumamry(Dictamen)

# WARNING

is.factor(Dictamen)
is.factor(dd[,1])

# Creation of new derived VARIABLES: “FEATURE EXTRACTION”

# RATIO OF FINANCEMENT 

Rati_fin = 100*Importe.solicitado/Precio.del.bien.financiado

hist(Rati_fin)

# CAPACITY TO SAVE

Estalvi <- (Ingresos-Gastos-(Cargas.patrimoniales/100))/(Importe.solicitado/Plazo)

hist(Estalvi)

logIncome<-log(Ingresos)
hist(logIncome)

#
# FEATURE SELECTION: FOR CONTINUOUS VARIABLES  FISFER's F
# RESPONSE VARIABLE: DICTAMEN
#

pvalcon <- NULL

varc <- list(Antiguedad.Trabajo,Plazo,Edad,Gastos,Ingresos,Patrimonio,Cargas.patrimoniales,Importe.solicitado,Precio.del.bien.financiado,Rati_fin,Estalvi)

length(varc)

for (i in 1:11) { pvalcon[i] <- (oneway.test(varc[[i]]~Dictamen))$p.value }

pvalcon = matrix(pvalcon)
#row.names(pvalcon) = c("Antig_feina","Plaç","Edat","Despeses","Ingressos","Patrimoni","Carrecs_pat","Import_sol","Preu_finan","Rati_fin","Estalvi")
row.names(pvalcon) = c("Antiquity","Timing","Age","Expenditures","Incomes","Patrimonium","Loads","QuantityRequired","TargetPrice","RatioFin","SavingCap")

# ORDERED LIST OF CONTINUOUS VARIABLES ACCORDING THEIR DEPENDENCE OF Dictamen

sort(pvalcon[,1])

#
# FEATURE SELECTION: FOR CATEGORICAL VARIABLES  CHI-SQUARE
# RESPONSE VARIABLE: DICTAMEN
#

pvalcat <- NULL
#vark <- list(edatR,antigR,plaçR,despesesR,ingressosR,patrimoniR,carrecsR,importR,preuR,ratfinR,estalviR,Vivienda,Estado.civil,Registros,Tipo.trabajo)

vark <- list(Vivienda,Estado.civil,Registros,Tipo.trabajo)

for (i in 1:4) { pvalcat[i] <- (chisq.test(vark[[i]],Dictamen))$p.value }

pvalcat = matrix(pvalcat)
row.names(pvalcat) = c("Housing","CivilStatus","Registers","WorkingType")



# ORDERED LIST OF CATEGORICAL VARIABLES ACCORDING THEIR DEPENDENCE OF Dictamen

sort(pvalcat[,1])

#let's have a global look with the whole set of variables
pval<-NULL
pval=append(pvalcon, pvalcat)
pval = matrix(pval)
#WARNING ORDER OF VARIABLES
row.names(pval)= append(row.names(pvalcon),row.names(pvalcat))
sort(pval[,1])


#
# WHICH VARIABLE CAN BE DISCARDED?
#

#Are the linearity hypothesis for oneway reasonable?

par(ask=TRUE)
for (i in 1:11) { boxplot(varc[[i]]~Dictamen, main=paste(row.names(pvalcon)[i])) }

#No sense with two modalities



# NEVERTHELESS ALL EXPLANOTORY VARIABLES HAVE BEEN CHOSEN BY AN EXPERT (FROM HUNDREDS IN THE DB) !


#
# PROFILE OF Dictamen
#

#
# GRAPHICAL REPRESENTATION Dictamen * CONTINUOUS VARIABLES
#

par(ask=TRUE)

ncon <- nrow(pvalcon)

for (i in 1:ncon) {
	barplot(tapply(varc[[i]], Dictamen, mean),main=paste("Means by",row.names(pvalcon)[i]))
	abline(h=mean(varc[[i]]))
	legend(0,mean(varc[[i]]),"global mean",bty="n") }



# Test the influence of the variable wrt the type of Dictamen 
# PVALUE OF THE HYPOTHESIS TEST COMPARING THE MEAN OF THE GROUP WITH THE GLOBAL MEAN
# WE DETECT POSITIVE DEVIATIONS ONLY
#
p.xk <- function(vec,fac){nk <- as.vector(table(fac)); n <- sum(nk); xk <- tapply(vec,fac,mean);
           txk <- (xk-mean(vec))/(sd(vec)*sqrt((n-nk)/(n*nk))); pxk <- pt(txk,n-1,lower.tail=F)}
#

# FUNCTION 
# PVALUE OF THE HYPOTHESIS TEST COMPARING THE PROPORTION OF THE GROUP WITHIN ONE MODALITY WITH THE GLOBAL PROPORTION OF THE GROUP
#
p.zkj <- function(resp,expl){taula <- table(resp,expl);n <- sum(taula); pk <- apply(taula,1,sum)/n;
      pj <- apply(taula,2,sum)/n;pf <- taula/(n*pk);
      pjm <- matrix(data=pj,nrow=dim(pf)[1],ncol=dim(pf)[2], byrow=T);      
      dpf <- pf - pjm; dvt <- sqrt(((1-pk)/(n*pk))%*%t(pj*(1-pj))); zkj <- dpf/dvt; 
      pzkj <- pnorm(zkj,lower.tail=F);
      list(rowpf=pf,vtest=zkj,pval=pzkj)}

nresp <- length(levels(Dictamen))

pvalk.con <- matrix(NA,nresp,ncon)
rownames(pvalk.con) <- levels(Dictamen)
colnames(pvalk.con) <- row.names(pvalcon)


for (i in 1:ncon) {
	pvalk.con[,i] = p.xk(varc[[i]],Dictamen) }

for (k in 1:nresp) { print(paste("P.values of Dictamen:",levels(Dictamen)[k])); print(sort(pvalk.con[k,])) }


#
# GRAPHICAL REPRESENTATION Dictamen * CATEGORICAL VARIABLES
#

#par(mfrow=c(1,3))

par(ask=TRUE)

n <- nrow(dd)
ncat <- nrow(pvalcat)



for (i in 1:ncat) {

	rowprof <- p.zkj(Dictamen,vark[[i]])$rowpf

	marg <- table(vark[[i]])/n
        print(append("Categories=",levels(vark[[i]])))
	plot(marg,type="l",ylim=c(0,1),main=paste("Prop. of pos & neg by",row.names(pvalcat)[i]))
	lines(rowprof[1,],col="blue")
	lines(rowprof[2,],col="red")
	legend("topright",c("pos","neg"),col=c("blue","red"),lty=1) 
        }


#par(mfrow=c(1,1))


pvalk.cat = NULL

for (i in 1:ncat) {	
 	auxpvalk <- p.zkj(Dictamen,vark[[i]])$pval 
	pvalk.cat = cbind(pvalk.cat,auxpvalk) }

for (k in 1:nresp) { print(paste("P.values of Dictamen:",levels(Dictamen)[k])); print(sort(pvalk.cat[k,])) }


# SAVING THE TRANSFORMATIONS IN A INTERNAL R FILE

save.image("D:/karina/docencia/DataMiningEI/practiques/2CredscoProfiling/credsco_bin")

names(dd)
dd[,15]<-Estalvi
dd[,16]<-Rati_fin
colnames(dd)[15]<-Estalvi
colnames(dd)[16]<-Rati.Fin


#saving the dataframe in an external file
write.table(dd, file = "D:/karina/docencia/DataMiningGEI/practiques/2Preprocessing/credscoClean.csv", sep = ";", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)

