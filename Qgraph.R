# Demography 
# Attaching data
dem<-read.csv2("Demography_Raters.csv", header=T)
head(dem) 

### New functions
skewness1<-function(x){(sum((x-mean(x))^3)/length(x))/(sum((x-mean(x))^2)/(length(x)-1))^(3/2)}
kurtotis1<-function(x){length(x) * sum((x - mean(x))^4)/(sum((x - mean(x))^2)^2)}

# Making sense of data using qgraph: 

setwd("D:/Disk 16 gb/Dominance_FinAn/Altogether_21_4_21/TDom_TSext")

##### Cameroonian men - 2013

library(qgraph)
library(bootnet)
# CMR_Males_2013
CMR.13.M <- read.csv2("CMR_13_Males.csv", T)

# ICCs
library(psych)
CMR13m.masc<-CMR.13.M[,67:143]
ICC(CMR13m.masc, alpha = 0.05)

# Used variables:
CMR.13.M<-CMR.13.M[,c(6,9,11,14,16,32,33,34,66,145)]

summary(CMR.13.M)
colnames(CMR.13.M)[which(colnames(CMR.13.M)=='SShD_div')] <- 'SShD'
colnames(CMR.13.M)[which(colnames(CMR.13.M)=='AVRG_div')] <- 'AVRG'
colnames(CMR.13.M)[which(colnames(CMR.13.M)=='Mean_masc')] <- 'Masc'
colnames(CMR.13.M)[which(colnames(CMR.13.M)=='Mean_Dom')] <- 'Dom'
colnames(CMR.13.M)[which(colnames(CMR.13.M)=='L')] <- 'L*'
colnames(CMR.13.M)[which(colnames(CMR.13.M)=='a')] <- 'a*'
colnames(CMR.13.M)[which(colnames(CMR.13.M)=='b')] <- 'b*'
colnames(CMR.13.M)[which(colnames(CMR.13.M)=='age')] <- 'Age'
summary(CMR.13.M)

CMR13.M.cor<-cor(CMR.13.M)
Names<-names(CMR.13.M)

# 1 Basic layout (below I attempted some silly labels, doesn't matter now):

CMR13MG <- qgraph(CMR13.M.cor, layout = "spring", graph="cor", label.cex=1.8, labels=colnames(CMR.13.M), label.scale=F, edge.labels=T,
                       sampleSize=49, edge.label.cex=1, repulsion=0.8, maximum = 0.5,minimum=0.01, esize=12, vsize=8, details=F)

CMR13MG$Edgelist$weight # Prints correlation coefficients
CMR13MG$graphAttributes$Nodes$names
CMR13MG$graphAttributes$Edges$labels 
CMR13MG$layout 

CMR13MG_Pcors<-CMR13MG$Edgelist$weight

# Saving network as an object (?list, probably) which can be utilised to draw a graph - BUT USING PACKAGE 'BOOTNET'
CMR13M_Network <- estimateNetwork(CMR.13.M, default = "glasso")
CMR13M_Network$graph # Which shows the coefficients 
# to draw a graph:
plot(CMR13M_Network, edge.labels=T) # Which creates appropriate edges

# Plots based on the two methods differ only slightly
centralityPlot(CMR13M_Network)
centralityPlot(CMR13MG)

# Booting CI of the estimate (using "estimateNetwork" from the bootnet package "bootnet")

CMR13Mboot<-bootnet(CMR13M_Network, nBoots = 2500, nCores = 2, default = "glasso")

print(CMR13Mboot)

#Use plot(CMR13Mboot$sample) to plot estimated network of original sample 
#Use summary(CMR13Mboot) to inspect summarized statistics (see ?summary.bootnet for details) 
#Use plot(CMR13Mboot) to plot summarized statistics (see ?plot.bootnet for details)

plot(CMR13Mboot$sample, edge.labels=T)
samery<-summary(CMR13Mboot)
plot(CMR13Mboot)
# And in more useful order (see that it clearly reflects coeffficients of CMR13M_network, however, not of the CMR13MG)
plot(CMR13Mboot, labels = TRUE, order="sample")

# ALE - když qgraphu øeknu, že chci "glasso", tak to udìlá stejnì jako bootnet.  # Write it in English please
CMR13MG2 <- qgraph(CMR13.M.cor, graph="glasso", tuning= 0.5, sampleSize=49, layout = "spring", label.cex=1.8, labels=colnames(CMR.13.M), label.scale=F, edge.labels=T,
                  sampleSize=49, edge.label.cex=1, repulsion=0.8, maximum = 0.5, minimum=0.0, esize=12, vsize=8, details=F, cut=0.1)

# a když mu øeknu, že EBIC parametr má být 0.0, udìlá ještì tøetí typ grafu (tzn. EBIC = 0 není argument k dosažení prosté korelaèní sítì):
CMR13MG3 <- qgraph(CMR13.M.cor, graph="glasso", tuning= 0.0,layout = "spring", label.cex=1.8, labels=colnames(CMR.13.M), label.scale=F, edge.labels=T,
                   sampleSize=49, edge.label.cex=1, repulsion=0.8, maximum = 0.5,minimum=0.01, esize=12, vsize=8, details=F)

CMR13MG3 <- qgraph(CMR13.M.cor, graph="pcor",layout = "spring", label.cex=1.8, labels=colnames(CMR.13.M), label.scale=F, edge.labels=T,
                   sampleSize=49, edge.label.cex=1, repulsion=0.8, maximum = 0.5,minimum=0.01, cut=0.3, esize=12, vsize=8, details=F)

# 2 Bootnet s default="cor"
CMR13M_Network <- estimateNetwork(CMR.13.M, default = "cor")
CMR13M_Network$graph # Which shows the coefficients - a jsou stejné jako ve výchozí korelaèní tabulce
# to draw a graph:
plot(CMR13M_Network, edge.labels=T) # Which creates appropriate edges - sice jinak otoèený, ale zase "správnì"

# Ploty dle obou metod (bootnet a qgraph) jsou ÚPLNÌ STEJNÉ!
centralityPlot(CMR13M_Network)
centralityPlot(CMR13MG)

# Bootujeme CI kolem odhadu (Ale podle "estimateNetwork" z package "bootnet")

CMR13Mboot<-bootnet(CMR13M_Network, nBoots = 2500, nCores = 2, default = "cor")

print(CMR13Mboot)

#Use plot(CMR13Mboot$sample) to plot estimated network of original sample 
#Use summary(CMR13Mboot) to inspect summarized statistics (see ?summary.bootnet for details) 
#Use plot(CMR13Mboot) to plot summarized statistics (see ?plot.bootnet for details)

plot(CMR13Mboot$sample, edge.labels=T)
plot(CMR13Mboot)
plot(CMR13Mboot, labels = TRUE, order="sample")

# Bootujeme CI kolem odhadu (Ale podle "estimateNetwork" z package "bootnet")

CMR13Mboot<-bootnet(CMR13M_Network, nBoots = 2500, nCores = 2, default = "pcor")

print(CMR13Mboot)

# Addding significant coefficients in black colour:

CMR13MG <- qgraph(CMR13.M.cor, layout = "spring", graph="cor", label.cex=1.8, labels=colnames(CMR.13.M), label.scale=F, edge.labels=T,
                  sampleSize=49, alpha=0.05, edge.label.cex=1, repulsion=0.8, maximum = 0.5,minimum=0.01, esize=12, vsize=8, details=F)


corCAMM13<-Hmisc::rcorr(as.matrix((CMR.13.M), type = "pearson"))

# loop to add text
for(i in 1:(nrow(CMR13MG$layout)-1)){
  for(j in (i+1):nrow(CMR13MG$layout)){
    
    # is correlation statistically significant
    if(corCAMM13$P[i,j] < 0.05){
      # using stored layout values, col1 is x, col2 is y
      loc_center_x <- (CMR13MG$layout[i,1]+CMR13MG$layout[j,1])/2
      loc_center_y <- (CMR13MG$layout[i,2]+CMR13MG$layout[j,2])/2
      
      # finding angle of vector
      rotation <- atan((CMR13MG$layout[i,2]-CMR13MG$layout[j,2])/(CMR13MG$layout[i,1]-CMR13MG$layout[j,1]))*180/pi
      
      # radial separation
      radius <- 0.1
      
      # putting text at location
      text(labels=round(cor(CMR.13.M,method="pearson")[i,j],digits=2) # text of correlation with rounded digits
           ,x=loc_center_x + abs(radius*sin(rotation*pi/180))
           ,y=loc_center_y + abs(radius*cos(rotation*pi/180))
           ,srt=rotation
           ,cex=1)
      
    }
  }
}

#### Bootstrap and centrality indices: 

# the plot method can be used to show the bootstrapped CIS for estimated egde parameters: 
plot(boot1, labels=FALSE, order="sample")

library("psychTools")
data(bfi)
bfiSub <- bfi[,1:25]

# Estimate network:
Network <- estimateNetwork(bfiSub, default = "EBICglasso")

##### Cameroonian women - 2013

library(qgraph)
library(bootnet)
# CMR_Males_2013
CMR.13.F <- read.csv2("CMR_13_Females.csv", T)

# Used variables:
head(CMR.13.F)
CMR.13.F<-CMR.13.F[,c(6,9,11,14,16,33,34,35,73,152)]

summary(CMR.13.F)
colnames(CMR.13.F)[which(colnames(CMR.13.F)=='SShD_div')] <- 'SShD'
colnames(CMR.13.F)[which(colnames(CMR.13.F)=='AVRG_div')] <- 'AVRG'
colnames(CMR.13.F)[which(colnames(CMR.13.F)=='Mean_fem')] <- 'Fem'
# colnames(CMR.13.F)[which(colnames(CMR.13.F)=='Mean_atr')] <- 'Attr'
colnames(CMR.13.F)[which(colnames(CMR.13.F)=='Mean_dom')] <- 'Dom'
colnames(CMR.13.F)[which(colnames(CMR.13.F)=='L')] <- 'L*'
colnames(CMR.13.F)[which(colnames(CMR.13.F)=='a')] <- 'a*'
colnames(CMR.13.F)[which(colnames(CMR.13.F)=='b')] <- 'b*'
colnames(CMR.13.F)[which(colnames(CMR.13.F)=='age')] <- 'Age'
summary(CMR.13.F)

CMR13.F.cor<-cor(CMR.13.F)
Names<-names(CMR.13.F)

# Basic layout:

CMR13FG <- qgraph(CMR13.F.cor, layout = "spring", graph="cor", label.cex=1.8, labels=colnames(CMR.13.F), label.scale=F, edge.labels=T,
                  sampleSize=50, edge.label.cex=1, repulsion=0.8, maximum = 0.5,minimum=0.01, esize=12, vsize=8, details=F)

# Addding significant coefficients in black colour: 

corCAMF13<-Hmisc::rcorr(as.matrix((CMR.13.F), type = "pearson"))

# loop to add text
for(i in 1:(nrow(CMR13FG$layout)-1)){
  for(j in (i+1):nrow(CMR13FG$layout)){
    
    # is correlation statistically significant
    if(corCAMF13$P[i,j] < 0.05){
      # using stored layout values, col1 is x, col2 is y
      loc_center_x <- (CMR13FG$layout[i,1]+CMR13FG$layout[j,1])/2
      loc_center_y <- (CMR13FG$layout[i,2]+CMR13FG$layout[j,2])/2
      
      # finding angle of vector
      rotation <- atan((CMR13FG$layout[i,2]-CMR13FG$layout[j,2])/(CMR13FG$layout[i,1]-CMR13FG$layout[j,1]))*180/pi
      
      # radial separation
      radius <- 0.1
      
      # putting text at location
      text(labels=round(cor(CMR.13.F,method="pearson")[i,j],digits=2) # text of correlation with rounded digits
           ,x=loc_center_x + abs(radius*sin(rotation*pi/180))
           ,y=loc_center_y + abs(radius*cos(rotation*pi/180))
           ,srt=rotation
           ,cex=1)
      
    }
  }
}

A<-cor_auto(CMR.13.F)
A
EBICglasso(A, n=49)

##### Czech men - 2016

library(qgraph)
library(bootnet)

CZ.16.M <- read.csv2("CZ_16_Males.csv", T)

# Used variables:
head(CZ.16.M)
CZ.16.M<-CZ.16.M[,c(6,9,11,14,16,43,44,45,142,375)]

summary(CZ.16.M)
colnames(CZ.16.M)[which(colnames(CZ.16.M)=='SShD_div')] <- 'SShD'
colnames(CZ.16.M)[which(colnames(CZ.16.M)=='AVRG_div')] <- 'AVRG'
colnames(CZ.16.M)[which(colnames(CZ.16.M)=='Masc_mean')] <- 'Masc'
#colnames(CZ.16.M)[which(colnames(CZ.16.M)=='Mean_Atr')] <- 'Attr'
colnames(CZ.16.M)[which(colnames(CZ.16.M)=='Mean_Dom')] <- 'Dom'
colnames(CZ.16.M)[which(colnames(CZ.16.M)=='L..Facial.mean.')] <- 'L*'
colnames(CZ.16.M)[which(colnames(CZ.16.M)=='a..facial.mean.')] <- 'a*'
colnames(CZ.16.M)[which(colnames(CZ.16.M)=='b..facial.mean.')] <- 'b*'
colnames(CZ.16.M)[which(colnames(CZ.16.M)=='age')] <- 'Age'
summary(CZ.16.M)

CZ16.M.cor<-cor(CZ.16.M)
Names<-names(CZ.16.M)

# Basic layout:

CZ16MG <- qgraph(CZ16.M.cor, layout = "spring", graph="cor", label.cex=1.8, labels=colnames(CZ.16.M), label.scale=F, edge.labels=T,
                  sampleSize=50, edge.label.cex=1, repulsion=0.8, maximum = 0.5,minimum=0.01, esize=12, vsize=8, details=F)

# Addding significant coefficients in black colour: 

corCZM16<-Hmisc::rcorr(as.matrix((CZ.16.M), type = "pearson"))

# loop to add text
for(i in 1:(nrow(CZ16MG$layout)-1)){
  for(j in (i+1):nrow(CZ16MG$layout)){
    
    # is correlation statistically significant
    if(corCZM16$P[i,j] < 0.05){
      # using stored layout values, col1 is x, col2 is y
      loc_center_x <- (CZ16MG$layout[i,1]+CZ16MG$layout[j,1])/2
      loc_center_y <- (CZ16MG$layout[i,2]+CZ16MG$layout[j,2])/2
      
      # finding angle of vector
      rotation <- atan((CZ16MG$layout[i,2]-CZ16MG$layout[j,2])/(CZ16MG$layout[i,1]-CZ16MG$layout[j,1]))*180/pi
      
      # radial separation
      radius <- 0.1
      
      # putting text at location
      text(labels=round(cor(CZ.16.M,method="pearson")[i,j],digits=2) # text of correlation with rounded digits
           ,x=loc_center_x + abs(radius*sin(rotation*pi/180))
           ,y=loc_center_y + abs(radius*cos(rotation*pi/180))
           ,srt=rotation
           ,cex=1)
      
    }
  }
}

A<-cor_auto(CZ.16.M)
A
EBICglasso(A, n=50)


##### Czech women - 2016

library(qgraph)
library(bootnet)

CZ.16.F <- read.csv2("CZ_16_Females.csv", T)

# Used variables:
head(CZ.16.F)
CZ.16.F<-CZ.16.F[,c(6,9,11,14,16,44,45,46,87,133)]

summary(CZ.16.F)
colnames(CZ.16.F)[which(colnames(CZ.16.F)=='SShD_Div')] <- 'SShD'
colnames(CZ.16.F)[which(colnames(CZ.16.F)=='AVRG_div')] <- 'AVRG'
colnames(CZ.16.F)[which(colnames(CZ.16.F)=='Mean_Fem')] <- 'Fem'
#colnames(CZ.16.F)[which(colnames(CZ.16.F)=='Mean_Atr')] <- 'Attr'
colnames(CZ.16.F)[which(colnames(CZ.16.F)=='Mean_Dom')] <- 'Dom'
colnames(CZ.16.F)[which(colnames(CZ.16.F)=='L_FAC_M')] <- 'L*'
colnames(CZ.16.F)[which(colnames(CZ.16.F)=='A_FAC_M')] <- 'a*'
colnames(CZ.16.F)[which(colnames(CZ.16.F)=='B_FAC_M')] <- 'b*'
colnames(CZ.16.F)[which(colnames(CZ.16.F)=='age')] <- 'Age'
summary(CZ.16.F)

CZ16.F.cor<-cor(CZ.16.F)
Names<-names(CZ.16.F)

# Basic layout:

CZ16FG <- qgraph(CZ16.F.cor, layout = "spring", graph="cor", label.cex=1.8, labels=colnames(CZ.16.F), label.scale=F, edge.labels=T,
                 sampleSize=50, edge.label.cex=1, repulsion=0.8, maximum = 0.5,minimum=0.01, esize=12, vsize=8, details=F)

# Addding significant coefficients in black colour: 

corCZF16<-Hmisc::rcorr(as.matrix((CZ.16.F), type = "pearson"))

# loop to add text
for(i in 1:(nrow(CZ16FG$layout)-1)){
  for(j in (i+1):nrow(CZ16FG$layout)){
    
    # is correlation statistically significant
    if(corCZF16$P[i,j] < 0.05){
      # using stored layout values, col1 is x, col2 is y
      loc_center_x <- (CZ16FG$layout[i,1]+CZ16FG$layout[j,1])/2
      loc_center_y <- (CZ16FG$layout[i,2]+CZ16FG$layout[j,2])/2
      
      # finding angle of vector
      rotation <- atan((CZ16FG$layout[i,2]-CZ16FG$layout[j,2])/(CZ16FG$layout[i,1]-CZ16FG$layout[j,1]))*180/pi
      
      # radial separation
      radius <- 0.1
      
      # putting text at location
      text(labels=round(cor(CZ.16.F,method="pearson")[i,j],digits=2) # text of correlation with rounded digits
           ,x=loc_center_x + abs(radius*sin(rotation*pi/180))
           ,y=loc_center_y + abs(radius*cos(rotation*pi/180))
           ,srt=rotation
           ,cex=1)
      
    }
  }
}

A<-cor_auto(CZ.16.F)
A
EBICglasso(A, n=50)

##### Czech men - 2019

library(qgraph)
library(bootnet)

CZ.19.M <- read.csv2("CZ_19_Males.csv", T)

# Used variables:
head(CZ.19.M)
CZ.19.M<-CZ.19.M[,c(5,8,12,14,42,43,44,119,184)]

summary(CZ.19.M)
colnames(CZ.19.M)[which(colnames(CZ.19.M)=='SShD_Div')] <- 'SShD'
colnames(CZ.19.M)[which(colnames(CZ.19.M)=='AVRG_Div')] <- 'AVRG'
colnames(CZ.19.M)[which(colnames(CZ.19.M)=='Mean_Masc')] <- 'Masc'
#colnames(CZ.19.M)[which(colnames(CZ.19.M)=='Mean_Atr')] <- 'Attr'
colnames(CZ.19.M)[which(colnames(CZ.19.M)=='Mean_Dom')] <- 'Dom'
colnames(CZ.19.M)[which(colnames(CZ.19.M)=='L_FAC_M')] <- 'L*'
colnames(CZ.19.M)[which(colnames(CZ.19.M)=='A_FAC_M')] <- 'a*'
colnames(CZ.19.M)[which(colnames(CZ.19.M)=='B_FAC_M')] <- 'b*'
colnames(CZ.19.M)[which(colnames(CZ.19.M)=='age')] <- 'Age'
summary(CZ.19.M)

CZ19.M.cor<-cor(CZ.19.M)
Names<-names(CZ.19.M)

# Basic layout:

CZ19MG <- qgraph(CZ19.M.cor, layout = "spring", graph="cor", label.cex=1.8, labels=colnames(CZ.19.M), label.scale=F, edge.labels=T,
                 sampleSize=50, edge.label.cex=1, repulsion=0.8, maximum = 0.5,minimum=0.01, esize=10, vsize=8, details=F)

# Addding significant coefficients in black colour: 

corCZM19<-Hmisc::rcorr(as.matrix((CZ.19.M), type = "pearson"))

# loop to add text
for(i in 1:(nrow(CZ19MG$layout)-1)){
  for(j in (i+1):nrow(CZ19MG$layout)){
    
    # is correlation statistically significant
    if(corCZM19$P[i,j] < 0.05){
      # using stored layout values, col1 is x, col2 is y
      loc_center_x <- (CZ19MG$layout[i,1]+CZ19MG$layout[j,1])/2
      loc_center_y <- (CZ19MG$layout[i,2]+CZ19MG$layout[j,2])/2
      
      # finding angle of vector
      rotation <- atan((CZ19MG$layout[i,2]-CZ19MG$layout[j,2])/(CZ19MG$layout[i,1]-CZ19MG$layout[j,1]))*180/pi
      
      # radial separation
      radius <- 0.1
      
      # putting text at location
      text(labels=round(cor(CZ.19.M,method="pearson")[i,j],digits=2) # text of correlation with rounded digits
           ,x=loc_center_x + abs(radius*sin(rotation*pi/180))
           ,y=loc_center_y + abs(radius*cos(rotation*pi/180))
           ,srt=rotation
           ,cex=1)
      
    }
  }
}

A<-cor_auto(CZ.19.M)
A
EBICglasso(A, n=50)

##### Czech women - 2019

library(qgraph)
library(bootnet)

CZ.19.F <- read.csv2("CZ_19_Females.csv", T)

# Used variables:
head(CZ.19.F)
CZ.19.F<-CZ.19.F[,c(5,8,9,12,14,42,43,44,78,140)]

summary(CZ.19.F)
colnames(CZ.19.F)[which(colnames(CZ.19.F)=='SShD_Div')] <- 'SShD'
colnames(CZ.19.F)[which(colnames(CZ.19.F)=='AVRG_div')] <- 'AVRG'
colnames(CZ.19.F)[which(colnames(CZ.19.F)=='Mean_Fem')] <- 'Fem'
#colnames(CZ.19.F)[which(colnames(CZ.19.F)=='Mean_Atr')] <- 'Attr'
colnames(CZ.19.F)[which(colnames(CZ.19.F)=='Mean_Dom')] <- 'Dom'
colnames(CZ.19.F)[which(colnames(CZ.19.F)=='L_FAC_M')] <- 'L*'
colnames(CZ.19.F)[which(colnames(CZ.19.F)=='A_FAC_M')] <- 'a*'
colnames(CZ.19.F)[which(colnames(CZ.19.F)=='B_FAC_M')] <- 'b*'
colnames(CZ.19.F)[which(colnames(CZ.19.F)=='age')] <- 'Age'
summary(CZ.19.F)

CZ19.F.cor<-cor(CZ.19.F)
Names<-names(CZ.19.F)

# Basic layout:

CZ19FG <- qgraph(CZ19.F.cor, layout = "spring", graph="cor", label.cex=1.8, labels=colnames(CZ.19.F), label.scale=F, edge.labels=T,
                 sampleSize=50, edge.label.cex=1, repulsion=0.8, maximum = 0.5,minimum=0.01, esize=10, vsize=8, details=F)

# Addding significant coefficients in black colour: 

corCZF19<-Hmisc::rcorr(as.matrix((CZ.19.F), type = "pearson"))

# loop to add text
for(i in 1:(nrow(CZ19FG$layout)-1)){
  for(j in (i+1):nrow(CZ19FG$layout)){
    
    # is correlation statistically significant
    if(corCZF19$P[i,j] < 0.05){
      # using stored layout values, col1 is x, col2 is y
      loc_center_x <- (CZ19FG$layout[i,1]+CZ19FG$layout[j,1])/2
      loc_center_y <- (CZ19FG$layout[i,2]+CZ19FG$layout[j,2])/2
      
      # finding angle of vector
      rotation <- atan((CZ19FG$layout[i,2]-CZ19FG$layout[j,2])/(CZ19FG$layout[i,1]-CZ19FG$layout[j,1]))*180/pi
      
      # radial separation
      radius <- 0.1
      
      # putting text at location
      text(labels=round(cor(CZ.19.F,method="pearson")[i,j],digits=2) # text of correlation with rounded digits
           ,x=loc_center_x + abs(radius*sin(rotation*pi/180))
           ,y=loc_center_y + abs(radius*cos(rotation*pi/180))
           ,srt=rotation
           ,cex=1)
      
    }
  }
}

A<-cor_auto(CZ.19.F)
A
EBICglasso(A, n=50)
