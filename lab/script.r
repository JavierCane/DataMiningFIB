#Insertar el path de donde se encuentre el archivo
setwd("/Users/javierferrer/Documents/Uni/MD/") # No necesario si abrimos fichero .R

#Modificar el nombre del archivo a conveniencia
data <- read.table("data/state_varios-oldest_ge_45-youngest_le_27-5910_reg.csv",sep=";",header=TRUE)

source('assets/clean_acm.r')

# Function to save plots easily.
save_plot <- function(file="plot.png", type=png, width=800, height=600, units="px", res=72) {
  err <- dev.copy( type, paste( "results/", file, sep="" ), width=width, height=height, units=units, res=res )
  err <- dev.off()
}

###################################################################################################################################################
################################################################ Lab 1: Data load #################################################################
###################################################################################################################################################

# Treating variables as qualitative
data$customer_ID    <- as.factor(data$customer_ID)
data$shopping_pt    <- as.factor(data$shopping_pt)
data$record_type    <- as.factor(data$record_type)
data$day            <- as.factor(data$day)
data$location       <- as.factor(data$location)
data$group_size     <- as.factor(data$group_size)
data$homeowner      <- as.factor(data$homeowner)
data$risk_factor    <- as.factor(data$risk_factor)
data$married_couple <- as.factor(data$married_couple)
data$C_previous     <- as.factor(data$C_previous)
data$A              <- as.factor(data$A)
data$B              <- as.factor(data$B)
data$C              <- as.factor(data$C)
data$D              <- as.factor(data$D)
data$E              <- as.factor(data$E)
data$F              <- as.factor(data$F)
data$G              <- as.factor(data$G)

# Correcting data$time. The new format is "minutes since 00:00".
aux_time <- NULL
for (i in 1:nrow(data)) {
  hhmm <- unlist(strsplit(as.character(data$time[i]), ":"))
  aux_time[i] <- as.numeric(hhmm[1])*60 + as.numeric(hhmm[2])
}
data$time <- aux_time
rm(aux_time, hhmm, i)


###################################################################################################################################################
############################################################ Lab 2: Preprocessing ############################################################
###################################################################################################################################################

# NA's become a new class in C_previous (5th) and in risk_factor (5th)
levels(data$C_previous) <- c(1,2,3,4,5)
data$C_previous[is.na(data$C_previous)] <- 5

levels(data$risk_factor) <- c(1,2,3,4,5)
data$risk_factor[is.na(data$risk_factor)] <- 5

# Fixing duration_previous:
# With the mean
data$duration_previous[is.na(data$duration_previous)] = mean(data$duration_previous[!is.na(data$duration_previous)]) 

# With the median
#data$duration_previous[is.na(data$duration_previous)] = median(data$duration_previous[!is.na(data$duration_previous)])

# With the K-Nearest Neighbour (NOT WORKING)
#library(class)
#data_without_dp = data[, !names(data) %in% c("duration_previous")]
#train = data_without_dp[!is.na(data$duration_previous),]
#test  = data_without_dp[is.na(data$duration_previous),]
#true_classifications_dp = data$duration_previous[!is.na(data$duration_previous)];
#data$duration_previous[is.na(data$duration_previous)] = knn(train, test, true_classifications_dp)
#rm(data_without_dp, train, test, true_classifications_dp)


###################################################################################################################################################
####################################################### Lab 3: Principal Component Analysis #######################################################
###################################################################################################################################################

cardinals <- c("1st","2nd","3rd","4th","5th","6th","7th","8th","9th","10th","11th","12th","13th","14th","15th","16th","17th","18th","19th","20th")

# Spliting of qualitative and quantitative data:
is_factor <- sapply(data, is.factor)
data_qualitative  <- data[,is_factor]
data_quantitative <- data[,!is_factor]

# Performing the Principal Components Analysis over the quantitative data columns:
principal_components = prcomp(data_quantitative, scale=TRUE)

# What percentatge of the total inertia is represented in each subspace?
inertia_projection  <- principal_components$sdev^2 
total_inertia <- sum(inertia_projection)
percent_inertia_axis <- 100*inertia_projection/total_inertia

# Cummulated inertia in each subspaces, from the first principal component to the Nth dimension subspace
cummulated_inertia = cumsum(principal_components$sdev^2) / ncol(data_quantitative)
barplot( 100*cummulated_inertia,
         main = "Cummulated inertia in each principal component",
         xlab = "principal components",
         ylab = "Cummulated inertia (%)",
         names.arg = cardinals[1:ncol(data_quantitative)])
save_plot("101_cummulated_inertia.png")

# Selection of the significative dimensions
#num_signif_dim <- 4 #4 o 5?! (4 is just below 80%)
num_signif_dim <- min( which(cummulated_inertia>=0.8) )

# Storage of the eigenvalues, eigenvectors and projections in the num_signif_dim dimensions
inertia_projection_signif <- principal_components$sdev[1:num_signif_dim]^2
rotation_PC_signif        <- principal_components$rotation[,1:num_signif_dim]
points_projected_PCs      <- principal_components$x[,1:num_signif_dim]


# Plots
Psi <- points_projected_PCs
Phi <- cor(data_quantitative, points_projected_PCs)

# 1st plot (points, axes, arrows):
for (axis_h in 1:(num_signif_dim-1)) {
  for (axis_v in (axis_h+1):num_signif_dim)  {
    range_h <- c(min(Psi[,axis_h]-0.2,0), max(Psi[,axis_h]+0.2,0)) # copied from Jaume. I guess we don't really need the ",0"... And we should be consistents and do the +-0.2 in the next plots!
    range_v <- c(min(Psi[,axis_v]-0.2,0), max(Psi[,axis_v]+0.2,0))
    
    plot( Psi[,axis_h],
          Psi[,axis_v],
          xlim = range_h,
          ylim = range_v,
          pch = 20,
          col="grey",
          main = paste("Original axis ploted on the ",cardinals[axis_h]," and ",cardinals[axis_v]," principal components", sep=""),
          xlab = paste(cardinals[axis_h]," principal component", sep=""),
          ylab = paste(cardinals[axis_v]," principal component", sep="") )
    
    axis(side=1, pos= 0, labels = F, col="black")
    axis(side=3, pos= 0, labels = F, col="black")
    axis(side=2, pos= 0, labels = F, col="black")
    axis(side=4, pos= 0, labels = F, col="black")
    
    scale_arrows <- min( range_v[1]/min(Phi[,axis_v]),
                         range_h[1]/min(Phi[,axis_h]),
                         range_v[2]/max(Phi[,axis_v]),
                         range_h[2]/max(Phi[,axis_h]))
    
    aux_zeros = rep(0, num_signif_dim)
    
    arrows( aux_zeros,
            aux_zeros,
            scale_arrows*Phi[,axis_h],
            scale_arrows*Phi[,axis_v],
            length=0.1,
            col="blue")
    
    text( scale_arrows*Phi[,axis_h],
          scale_arrows*Phi[,axis_v],
          labels=names(data_quantitative),
          col="blue")
    
    save_plot(paste("157_original_axis_on_pc",axis_h,"_and_pc",axis_v,".png", sep=""))
  }
}
rm(axis_h, axis_v, range_h, range_v, scale_arrows, aux_zeros)


# 2nd plot (axis, arrows, centroids)
ploted_classes <- c(3:5,7:8,11) # these should be choosen carefully!!!
#colors <- rainbow(ncol(data_qualitative)) # these colors should be tunned ;)
#Another option (i think it's more visible)
colors <- c("orange", "brown","green", "darkgrey", "purple", "red", "orange", "darkgreen","green", "orange", "brown", "darkblue", "purple", "red", "darkgrey", "darkgreen")
for (axis_h in 1:(num_signif_dim-1)) {
  for (axis_v in (axis_h+1):num_signif_dim)  {
    range_h <- c(-1,1)
    range_v <- c(-1,1)
    
    plot( Phi[,axis_h],
          Phi[,axis_v],
          xlim = range_h,
          ylim = range_v,
          col="white",
          main = paste("Centroids ploted on the ",cardinals[axis_h]," and ",cardinals[axis_v]," principal components", sep=""),
          xlab = paste(cardinals[axis_h]," principal component", sep=""),
          ylab = paste(cardinals[axis_v]," principal component", sep="") )
    
    axis(side=1, pos= 0, labels = F, col="black")
    axis(side=3, pos= 0, labels = F, col="black")
    axis(side=2, pos= 0, labels = F, col="black")
    axis(side=4, pos= 0, labels = F, col="black")
    
    aux_zeros = rep(0, num_signif_dim)
    
    arrows( aux_zeros,
            aux_zeros,
            Phi[,axis_h],
            Phi[,axis_v],
            length=0.1,
            col="blue")
    
    text( Phi[,axis_h],
          Phi[,axis_v],
          labels=names(data_quantitative),
          col="blue")
    
    for (i in ploted_classes) { #maybe we could make 2 loops, one for "lines" and one for "points".
      column <- data_qualitative[,i]
      centroids_h <- tapply(Psi[,axis_h], column, mean)
      centroids_v <- tapply(Psi[,axis_v], column, mean)
      text  (centroids_h, centroids_v, col=colors[i], labels=levels(column))
    }
    
    legend("topleft",names(data_qualitative[ploted_classes]),pch=20,col=colors[ploted_classes])
    
    save_plot(paste("210_centroids_on_pc",axis_h,"_and_pc",axis_v,".png", sep=""))
  }
}
rm(ploted_classes, colors, axis_h, axis_v, range_h, range_v, aux_zeros, i, column, centroids_h, centroids_v)


# 3rd plot (points, types):
colors <- c("red","black","green","blue","yellow","purple","cyan","orange","brown","magenta","grey","dark red","dark blue","dark green",1:1000) # these colors should be tunned too! :)

for (i in 11:11) { #for (i in 1:ncol(data_qualitative)) { #be careful, it generates 200+ images and takes a while
  for (axis_h in 1:(num_signif_dim-1)) {
    for (axis_v in (axis_h+1):num_signif_dim)  {
      column <- data_qualitative[i]
      name_column <- names(column)
      
      range_h <- c(min(Psi[,axis_h]-0.2, 0), max(Psi[,axis_h]+0.2, 0))
      range_v <- c(min(Psi[,axis_v]-0.2, 0), max(Psi[,axis_v]+0.2, 0))
      
      plot( Psi[,axis_h],
            Psi[,axis_v],
            xlim = range_h,
            ylim = range_v,
            pch = 20,
            col = colors[1:nlevels(column[,1])],
            main = paste("Types of ",name_column," ploted on the ",cardinals[axis_h]," and ",cardinals[axis_v]," principal components", sep=""),
            xlab = paste(cardinals[axis_h]," principal component", sep=""),
            ylab = paste(cardinals[axis_v]," principal component", sep="") )
      
      
      legend("topright",legend=levels(column[,1]),pch=20,col=colors[1:nlevels(column[,1])])
      
      save_plot(paste("241_types_",name_column,"_on_pc",axis_h,"_and_pc",axis_v,".png", sep=""))
    }
  }
}
rm(colors, i, axis_h, axis_v, column, name_column, range_h, range_v, cardinals)

# Aqui falten coses ...

###################################################################################################################################################
######################################################## Lab 4: Feature selection & K-means #######################################################
###################################################################################################################################################

# Spliting of qualitative and quantitative data:
is_factor <- sapply(data, is.factor)
data_qualitative  <- data[,is_factor]
data_qualitative <- data_qualitative[,-1]
data_qualitative <- data_qualitative[,-1]
data_qualitative <- data_qualitative[,-1]
data_qualitative <- data_qualitative[,-3]
data_qualitative <- data_qualitative[,-5]
data_quantitative <- data[,!is_factor]

# Renaming, for convenience
Psi <- points_projected_PCs

# Determining the Target Variable (Risk Factor)
target_variable <- data.frame(data$risk_factor) # This is a problem since we have a 5th class corresponding to NA's!

# Performing the ACM analisis (what does "ACM" stands for?)
# Anyone knows what is she doing here?
ac1 <- acm(data_qualitative, target_variable) # Please, lets use better names than "ac1". And this takes a lot of time.

nd <- max( which(ac1$vaps > 1/ncol(data_qualitative)) )
FI <- ac1$rs[,1:nd]
factors <- data.frame(Psi, FI)

# K-means (Each time the results differ as it starts randomly!)
cardinals <- c("1st","2nd","3rd","4th","5th","6th","7th","8th","9th","10th","11th","12th","13th","14th","15th","16th","17th","18th","19th","20th")
colors <- 1:ncol(data_qualitative) # these colors should be tunned too ^_^

for (k in 4:4) {
  result_kmeans <- kmeans(factors, k)
  Bss <- sum( rowSums(result_kmeans$centers^2) * result_kmeans$size )
  Wss <- sum( result_kmeans$withinss )
  #Tss <- sum( rowSums(Psi^2) )
  Ib1 <- 100*Bss/(Bss+Wss)
  
  for (axis_h in 1:(num_signif_dim-1)) {
    for (axis_v in (axis_h+1):num_signif_dim)  {
      plot(
            Psi[,axis_h],
            Psi[,axis_v],
            pch = 20,
            col = colors[result_kmeans$cluster],
            main = paste("Clustering of data in ",k," classes using K-means", sep=""),
            xlab = paste(cardinals[axis_h]," principal component", sep=""),
            ylab = paste(cardinals[axis_v]," principal component", sep="") )
      
      mtext( paste("Decomposition of intertia = ",Ib1, sep="") )
      
      legend("topleft", c(paste("cluster",1:k)), pch=20, col=colors[1:k] )
      
      save_plot(paste("299_kmeans_",k,"_on_pc",axis_h,"_and_pc",axis_v,".png", sep=""))
    }
  }
}

#Alternative for the first two axis trying different numbers
numclases <- c(3,4,5,6,8)
par(ask=TRUE)
axis_h <- 1
axis_v <- 2
for (i in 1:length(numclases)){
	#Whereas k is the number of clases 
	k = numclases[i]
	k1 <- kmeans(factors,k)
	attributes(k1)
	k1$size
	k1$withinss
	k1$centers
	colors <- c("green", "orange", "brown", "darkblue", "purple", "red", "darkgrey", "darkgreen")
	clases <- rep(NA, length(data[,1]))
	for(j in 1:length(clases)){
		l <- k1$cluster[j]
		clases[j] <- colors[l]
	}
	title <- paste("Clustering of insurance policy data in ",as.character(k)," clases")
	xaxislab <- paste(cardinals[axis_h]," principal component", sep="")
	yaxislab <- paste(cardinals[axis_v]," principal component", sep="") 
	plot(Psi[,axis_h],Psi[,axis_v],col=clases,main=title, xlab=xaxislab, ylab=yaxislab)
     	legend("topleft", c(paste("cluster",1:k)), pch=20, col=colors)
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
	cat (k, "classes: ",Bss," ", Wss, " ", Tss, " " , Bss+Wss, " ", Ib1, "\n")
	mtext( paste("Decomposition of intertia = ",Ib1, sep="") )
      save_plot(paste("299_kmeans_",k,"_on_pc",axis_h,"_and_pc",axis_v,".png", sep=""))
}


# Hierarchical clustering
result_hierarchical_clustering <- hclust(dist(factors), method="ward") # This is incredibly slow.

# Plot
plot(result_hierarchical_clustering)
save_plot("310_hierarchical_clustering.png")

# Determining the best cuts
optimal_num_groups <- NULL
height_joins       <- array(result_hierarchical_clustering$height)
num_joins          <- nrow(height_joins)
diff_height_cuts   <- height_joins[2:num_joins] - height_joins[1:(num_joins-1)]
for (i in 1:3) {
  aux_max_cut <- which.max(diff_height_cuts)
  diff_height_cuts[aux_max_cut] <- 0
  optimal_num_groups[i] <- num_joins - aux_max_cut + 1
}
optimal_num_groups <- array(optimal_num_groups)
rm(height_joins, num_joins, diff_height_cuts, i, aux_max_cut)

# Ploting the points considering the best cuts:
cardinals <- c("1st","2nd","3rd","4th","5th","6th","7th","8th","9th","10th","11th","12th","13th","14th","15th","16th","17th","18th","19th","20th")
colors <- 1:max(optimal_num_groups) # these colors should be tunned too *_*

for (i in 1:nrow(optimal_num_groups)) {
  num_groups <- optimal_num_groups[i]
  groups <- cutree(result_hierarchical_clustering, num_groups)
  
  cdg <- aggregate(as.data.frame(factors), list(groups), mean)[,2:(nd+1)]
  Tss <- sum(rowSums(Psi^2))
  Bss <- sum(rowSums(cdg^2)*as.numeric(table(groups)))
  Ib4 <- 100*Bss/Tss
  
  legend("topleft",c("c1","c2","c3","c4"),pch=1,col=c(1:4))
  for (axis_h in 1:(num_signif_dim-1)) {
    for (axis_v in (axis_h+1):num_signif_dim)  {
      plot(
        Psi[,axis_h],
        Psi[,axis_v],
        pch = 20,
        col = colors[groups],
        main = paste("Hierarchical clustering in ",num_groups," classes (",cardinals[i]," best cut)", sep=""),
        xlab = paste(cardinals[axis_h]," principal component", sep=""),
        ylab = paste(cardinals[axis_v]," principal component", sep="") )
      
      mtext( paste("Decomposition of intertia = ",Ib4, sep="") )
      
      legend("topleft", c(paste("cluster",1:num_groups)), pch=20, col=colors[1:num_groups] )
      
      save_plot(paste("352_hierarchical_",num_groups,"groups_",i,"best_on_pc",axis_h,"_and_pc",axis_v,".png", sep=""))
    }
  }
}







###################################################################################################################################################
######################################################## P-Value #######################################################
###################################################################################################################################################


#
# FEATURE SELECTION: FOR CONTINUOUS VARIABLES  FISFER's F
# RESPONSE VARIABLE: RISK FACTOR
#

dcat <- data.frame(data$record_type, data$group_size, data$day, data$homeowner, data$risk_factor, data$married_couple, data$C_previous, data$A, data$B, data$C, data$D, data$E, data$F, data$G)
dcon <- data.frame ("Car_age" = data$car_age, "Age_oldest" = data$age_oldest, "Age_youngest" = data$age_youngest, "Duration_previous" = data$duration_previous, "Cost" = data$cost)
pc1 = prcomp(dcon, scale=T)

varc <- dcon
pvalcon <- NULL
for (i in 1:length(dcon)) { pvalcon[i] <- (oneway.test(varc[[i]]~data$risk_factor))$p.value }

pvalcon = matrix(pvalcon)
rownames(pvalcon) = colnames(varc)

# ORDERED LIST OF CONTINUOUS VARIABLES ACCORDING THEIR DEPENDENCE OF RISK FACTOR
sort(pvalcon[,1])

#
# FEATURE SELECTION: FOR CATEGORICAL VARIABLES  CHI-SQUARE
# RESPONSE VARIABLE: RISK FACTOR
#

pvalcat <- NULL
vark <- dcat

for (i in 1:length(dcat)) { pvalcat[i] <- (chisq.test(vark[[i]],data$risk_factor))$p.value }

pvalcat = matrix(pvalcat)
rownames(pvalcat) = colnames(dcat)



# ORDERED LIST OF CATEGORICAL VARIABLES ACCORDING THEIR DEPENDENCE OF RISK FACTOR

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
for (i in 1:length(varc)) { boxplot(varc[[i]]~data$risk_factor, main=paste(row.names(pvalcon)[i])) }

#No sense with two modalities



# NEVERTHELESS ALL EXPLANOTORY VARIABLES HAVE BEEN CHOSEN BY AN EXPERT (FROM HUNDREDS IN THE DB) !


#
# PROFILE OF RISK FACTOR
#

#
# GRAPHICAL REPRESENTATION RISK FACTOR * CONTINUOUS VARIABLES
#

par(ask=TRUE)

ncon <- nrow(pvalcon)

for (i in 1:ncon) {
	barplot(tapply(varc[[i]], data$risk_factor, mean),main=paste("Means by",row.names(pvalcon)[i]))
	abline(h=mean(varc[[i]]))
	legend(0,mean(varc[[i]]),"global mean",bty="n") }



# Test the influence of the variable wrt the type of RISK FACTOR
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

nresp <- length(levels(data$risk_factor))

pvalk.con <- matrix(NA,nresp,ncon)
rownames(pvalk.con) <- levels(data$risk_factor)
colnames(pvalk.con) <- row.names(pvalcon)


for (i in 1:ncon) {
	pvalk.con[,i] = p.xk(varc[[i]],data$risk_factor) }

for (k in 1:nresp) { print(paste("P.values of Risk Factor:",levels(data$risk_factor)[k])); print(sort(pvalk.con[k,])) }


#
# GRAPHICAL REPRESENTATION RISK FACTOR * CATEGORICAL VARIABLES
#

#par(mfrow=c(1,3))

par(ask=TRUE)

n <- nrow(data)
ncat <- nrow(pvalcat)



for (i in 1:ncat) {

	rowprof <- p.zkj(data$risk_factor,vark[[i]])$rowpf

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
 	auxpvalk <- p.zkj(data$risk_factor,vark[[i]])$pval 
	pvalk.cat = cbind(pvalk.cat,auxpvalk) }

for (k in 1:nresp) { print(paste("P.values of Risk Factor:",levels(data$risk_factor)[k])); print(sort(pvalk.cat[k,])) }
