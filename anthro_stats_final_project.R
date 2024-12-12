library(geiger)
library(caper)
library(tidyverse)
library(dplyr)

#Load in WD
setwd("~/Desktop/All_fish_raw")


#loading in the tree
MyTree <- read.tree(file = "Rabosky_et_al_timetree.tre")

#looking at the tree
par(mfrow=c(1,1))
plot(ladderize(MyTree), cex=0.5)


MyTaxa <- c("Tautoga_onitis", "Amia_calva", "Amia_calva", "Amia_calva", "Amia_calva", "Amia_calva", 
            "Amia_calva", "Anarhichas_lupus", "Anguilla_rostrata", "Antennarius_striatus", "Arapaima_gigas",
            "Ariopsis_felis", "Bagre_marinus", "Opsanus_pardus", "Tylosurus_crocodilus_crocodilus",
            "Alectis_ciliaris", "Alectis_ciliaris", "Carangoides_bartholomaei", "Caranx_hippos",
            "Caranx_crysos", "Caranx_hippos", "Naucrates_ductor", "Selar_crumenophthalmus",
            "Selene_setapinnis", "Selene_vomer", "Seriola_dumerili", "Trachinotus_carolinus",
            "Trachinotus_goodei", "Trachinotus_falcatus", "Catostomus_commersonii",
            "Ictiobus_bubalus", "Moxostoma_erythrurum", "Micropterus_salmoides",
            "Serrasalmus_elongatus", "Astronotus_ocellatus", "Alosa_sapidissima",
            "Brevoortia_patronus", "Coryphaena_hippurus", "Myloplus_planquettei",
            "Hypophthalmichthys_nobilis", "Cyprinus_carpio_carpio", "Echeneis_naucrates",
            "Remora_remora", "Electrophorus_electricus", "Elops_saurus",
            "Chaetodipterus_faber", "Chaetodipterus_faber", "Esox_lucius", "Ctenogobius_saepepallens",
            "Orthopristis_chrysoptera", "Hiodon_alosoides", "Ameiurus_nebulosus", "Pylodictis_olivaris",
            "Ictalurus_punctatus", "Kyphosus_analogus", "Kyphosus_analogus", "Tautogolabrus_adspersus",
            "Lepisosteus_osseus", "Lepisosteus_oculatus", "Lobotes_pacificus",
            "Lophius_americanus", "Hypostomus_ancistroides", "Lutjanus_griseus",
            "Lutjanus_vivanus", "Lutjanus_griseus", "Lutjanus_synagris", "Lutjanus_campechanus",
            "Ocyurus_chrysurus", "Rhomboplites_aurorubens", "Megalops_atlanticus",
            "Morone_americana", "Morone_saxatilis", "Gymnothorax_moringa", "Ophichthus_gomesii",
            "Echiophis_punctifer", "Ophichthus_rex", "Brotula_barbata","Osteoglossum_bicirrhosum", 
            "Scleropages_formosus", "Perca_flavescens", "Sander_vitreus", "Polyodon_spathula", "Polyodon_spathula",
            "Pomatomus_saltatrix", "Priacanthus_arenatus", "Rachycentron_canadum", "Rachycentron_canadum",
            "Oncorhynchus_kisutch", "Oncorhynchus_mykiss","Salmo_salar", "Cynoscion_nebulosus", "Cynoscion_nebulosus", "Cynoscion_arenarius",
            "Leiostomus_xanthurus", "Menticirrhus_americanus", "Micropogonias_undulatus", "Pogonias_cromis", "Euthynnus_alletteratus", "Sarda_sarda",
            "Scomber_scombrus", "Scomberomorus_cavalla", "Thunnus_atlanticus", "Thunnus_obesus", "Acanthocybium_solandri",
            "Thunnus_albacares", "Sebastes_norvegicus", "Epinephelus_adscensionis", "Hyporthodus_niveatus", "Hyporthodus_nigritus",
            "Mycteroperca_bonaci", "Rypticus_saponaceus", "Rypticus_nigripinnis", "Archosargus_probatocephalus", "Calamus_calamus", 
            "Diplodus_holbrookii", "Lagodon_rhomboides", "Lagodon_rhomboides", "Pagrus_pagrus", "Stenotomus_chrysops", "Sphyraena_guachancho",
            "Sphyraena_barracuda", "Peprilus_simillimus", "Trichiurus_lepturus", "Trichiurus_brevis",
            "Prionotus_scitulus", "Prionotus_scitulus", "Kathetostoma_canaster", "Zoarces_americanus")

#making the pruned tree
MyNewTree <- keep.tip(phy = MyTree, tip = MyTaxa)

#visulizing the tree
plot.phylo(ladderize(MyNewTree), label.offset = 1, cex= 0.3)
axisPhylo() #adds scale bar to plot [example: time moy]

#saves tree
write.tree(phy = MyNewTree, file = "Macroevofinal.tre")


##pruneing the tree based on our data
MyNewTree

str(MyNewTree)

MyData <- read.csv("RawCompStats.csv")
MyData
str(MyData)


meandata <-aggregate(cbind(CB1, CB5, CH, Man, as.factor(Clad)) ~as.factor(Phyloname), data = MyData, FUN = mean, na.rm = TRUE, na.action = NULL)
row.names(meandata) <- meandata$`as.factor(Phyloname)`   #need the names to be set as row names... adding row names into the data frame          

Prune <- treedata(phy = MyTree, data= meandata)

str(Prune)

PrunePhy<- Prune$phy
PruneData<- Prune$data


PruneData<-as.data.frame(PruneData)
str(PruneData)


#V5 =CLAD 
PruneData$V5 <-as.factor(PruneData$V5)
str(PruneData)



PruneData$Man <-as.numeric(PruneData$Man)
str(PruneData)


PruneData$CH <-as.numeric(PruneData$CH)
str(PruneData)

PruneData$CB1 <-as.numeric(PruneData$CB1)
str(PruneData)

PruneData$CB5 <-as.numeric(PruneData$CB5)
str(PruneData)

PrunePhy$tip.label #what are the names of the taxa in the tree?
rownames(PruneData)

match(PrunePhy$tip.label, rownames(PruneData)) #shifts data so it matches the tree

PruneData1 <- PruneData[match(PrunePhy$tip.label, rownames(PruneData)),]
PruneData1
str(PruneData1)
#run PruneData1 <- as.data.frame(PruneData1) if it comes up as a matrix
class(PruneData1) #check that this is not a matrix, we need a data fram for everything to work

---
  
##Comparitive Methods time
library(caper)

MyTree <- read.tree(file = "Rabosky_et_al_timetree.tre")
MyData <- read.csv("RawCompStats.csv")
str(MyData)


meandata <-aggregate(cbind(CB1, CB5, CH, Man, as.factor(Clad)) ~as.factor(Phyloname), data = MyData, FUN = mean, na.rm = TRUE, na.action = NULL)
colnames(meandata)[1] <- "Phyloname"


MyCompData <- comparative.data(phy= MyTree, data = meandata, names.col = "Phyloname")
MyCompData 
MyCompData$phy
MyCompData$data

MyCompData$data$V5 <-as.factor(MyCompData$data$V5)
str(MyCompData$data$V5)

MyCompData

---
library(ggplot2)
#lambda model 
  
##CH
#among all fish 
lamM1 <- pgls(log10(CH) ~ log10(Man), data = MyCompData, lambda = 'ML') #lambda a tree trasformation paramitter... turns everything to zero and then correct everything based on that... removing the effects of phylogenetics signal 
vcv(MyCompData$phy) #the length of the tree

summary(lamM1)
anova(lamM1)
coef(lamM1)

# Determine the F critical value 
qf(p=2.2e-16, df1=1, df2=111, lower.tail=FALSE)

par(mfrow=c(2,2))
plot(lamM1)



ColVec <- c("violetred2", "darkorange3", "seagreen", "cornflowerblue")

class(MyCompData) # "comparative.data" 
class(PruneData1) # "data.frame"

par(mfrow=c(1,1))
plot( x = log10(MyCompData$data$Man), y = log10(MyCompData$data$CH), pch=19, col=ColVec[unclass(MyCompData$data$V5)], xlab = "Log Mandible", ylab= "Log Ceratohyal", ylim=c(0.0,1.4),xlim=c(0.0,1.4))
abline(lamM1)# from how comparative analysis 


#with in clade 

ancova_model01 <-  pgls(log10(CH) ~ log10(Man)*V5, data = MyCompData, lambda = 'ML')# change the modle to match this one lamM1 <- pgls( log10(CH) ~ log10(Man)*Clad, data = MyCompData, lambda = 'ML') 
anova(ancova_model01)
summary(ancova_model01)

## Determine the t critical value
qt(p=0.53267, df=105, lower.tail=TRUE)

# Determine the F critical value (man)
qf(p=2e-16, df1=1, df2=105, lower.tail=FALSE)

# Determine the F critical value (man:clade)
qf(p=0.1580, df1=3, df2=105, lower.tail=FALSE)



ancova_model01_noninter <-  pgls(log10(CH) ~ log10(Man)+V5, data = MyCompData, lambda = 'ML')
anova(ancova_model01_noninter)
summary(ancova_model01_noninter)

# Determine the F critical value (man)
qf(p=2e-16, df1=1, df2=108, lower.tail=FALSE)

par(mfrow=c(2,2))
plot(ancova_model01)




#ggplot clade
plot1 = ggplot(MyCompData$data, aes(x=log10(MyCompData$data$Man), y=log10(MyCompData$data$CH), color= MyCompData$data$V5)) +  
             geom_point()

# View the plot
plot1

Plot1_new <- plot1+geom_smooth(method=lm)+ labs(x = "Log Mandible", y = "Log Ceratohyal" ) + #lables the axis
  theme_bw() + 
  scale_color_manual(values=c("violetred2", "darkorange3", "seagreen", "cornflowerblue"), labels = c("Primitive Fish/ Out Group","Otocephala","Basal Euteleosts","Percomorpha" ))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

Plot1_new + labs(color= "Clade") 


##CB1
#among all fish 
lamM2 <- pgls(log10(CB1) ~ log10(Man), data = MyCompData, lambda = 'ML') #lambda a tree trasformation paramitter... turns everything to zero and then correct everything based on that... removing the effects of phylogenetics signal 
vcv(MyCompData$phy) #the length of the tree

summary(lamM2)
anova(lamM2)
coef(lamM2)

# Determine the F critical value 
qf(p=2.2e-16, df1=1, df2=111, lower.tail=FALSE)

par(mfrow=c(2,2))
plot(lamM2)


ColVec <- c("violetred2", "darkorange3", "seagreen", "cornflowerblue")

class(MyCompData) # "comparative.data" 
class(PruneData1) # "data.frame"

par(mfrow=c(1,1))
plot( x = log10(MyCompData$data$Man), y = log10(MyCompData$data$CB1), pch=19, col=ColVec[unclass(MyCompData$data$V5)], xlab = "Log Mandible", ylab= "Log Ceratobranchial 1", ylim=c(0.0,1.4),xlim=c(0.0,1.4)) 
abline(lamM2)# from how comparative analysis 



#with in clade 
ancova_model02 <-  pgls(log10(CB1) ~ log10(Man)*V5, data = MyCompData, lambda = 'ML') # change the modle to match this one lamM1 <- pgls( log10(CH) ~ log10(Man)*Clad, data = MyCompData, lambda = 'ML') 
summary(ancova_model02)
anova(ancova_model02)

## Determine the t critical value
qt(p=0.480859, df=105, lower.tail=TRUE)

# Determine the F critical value (man)
qf(p=2e-16, df1=1, df2=105, lower.tail=FALSE)

# Determine the F critical value (clade)
qf(p=0.04872, df1=3, df2=105, lower.tail=FALSE)

# Determine the F critical value (man:clade)
qf(p=0.03031, df1=3, df2=105, lower.tail=FALSE)


ancova_model02_noninter <-  pgls(log10(CB1) ~ log10(Man)+V5, data = MyCompData, lambda = 'ML')
anova(ancova_model02_noninter)
#summary(ancova_model02_noninter)

Model_list <- list(ancova_model02_noninter, ancova_model02)
library(AICcmodavg)
AIC(Model_list)

par(mfrow=c(2,2))
plot(ancova_model02)

#model selection for cb1
#mod1= pgls(log10(CB1) ~ log10(Man)+ V5 + log10(Man):V5 , data = MyCompData, lambda = 'ML')
#mod2 = pgls(log10(CB1) ~ log10(Man)+ V5 , data = MyCompData, lambda = 'ML')
#mod3= pgls(log10(CB1) ~ 1, data = MyCompData, lambda = 'ML') 
  
#Model_list <- list(mod1, mod2, mod3)

#library(AICcmodavg)
#AIC(Model_list)






#clade ggplot
plot2 = ggplot(MyCompData$data, aes(x=log10(MyCompData$data$Man), y=log10(MyCompData$data$CB1), color= MyCompData$data$V5)) +  
  geom_point()

# View the plot
plot2

Plot2_new <- plot2+geom_smooth(method=lm)+ labs(x = "Log Mandible", y = "Log Ceratobranchial 1" ) + #lables the axis
  theme_bw() + 
  scale_color_manual(values=c("violetred2", "darkorange3", "seagreen", "cornflowerblue"), labels = c("Primitive Fish/ Out Group","Otocephala","Basal Euteleosts","Percomorpha" ))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

Plot2_new + labs(color= "Clade") 


##CB5
#among all fish 
lamM3 <- pgls(log10(CB5) ~ log10(Man), data = MyCompData, lambda = 'ML') #lambda a tree trasformation paramitter... turns everything to zero and then correct everything based on that... removing the effects of phylogenetics signal 
vcv(MyCompData$phy) #the length of the tree

summary(lamM3)
anova(lamM3)
coef(lamM3)

# Determine the F critical value 
qf(p=2.2e-16, df1=1, df2=111, lower.tail=FALSE)

par(mfrow=c(2,2))
plot(lamM3)


ColVec <- c("violetred2", "darkorange3", "seagreen", "cornflowerblue")

class(MyCompData) # "comparative.data" 
class(PruneData1) # "data.frame"

par(mfrow=c(1,1))
plot( x = log10(MyCompData$data$Man), y = log10(MyCompData$data$CB5), pch=19, col=ColVec[unclass(MyCompData$data$V5)], xlab = "Log Mandible", ylab= "Log Ceratobranchial 5", ylim=c(0.0,1.4),xlim=c(0.0,1.4))
abline(lamM3)# from how comparative analysis 



#with in clade 
ancova_model03 <-  pgls(log10(CB5) ~ log10(Man)*V5, data = MyCompData, lambda = 'ML') # change the modle to match this one lamM1 <- pgls( log10(CH) ~ log10(Man)*Clad, data = MyCompData, lambda = 'ML') 
summary(ancova_model03)
anova(ancova_model03)


## Determine the t critical value
qt(p=0.004329, df=105, lower.tail=TRUE)

# Determine the F critical value (man)
qf(p=2e-16, df1=1, df2=105, lower.tail=FALSE)

# Determine the F critical value (clade)
qf(p=0.01257, df1=3, df2=105, lower.tail=FALSE)


par(mfrow=c(2,2))
plot(ancova_model03)

ancova_model03_noninter <-  pgls(log10(CB5) ~ log10(Man)+V5, data = MyCompData, lambda = 'ML')
anova(ancova_model03_noninter)
summary(ancova_model03_noninter)



#ggplot clade
plot3 = ggplot(MyCompData$data, aes(x=log10(MyCompData$data$Man), y=log10(MyCompData$data$CB5), color= MyCompData$data$V5)) +  
  geom_point()

# View the plot
plot3

Plot3_new <- plot3+geom_smooth(method=lm)+ labs(x = "Log Mandible", y = "Log Ceratobranchial 5") + #lables the axis
  theme_bw()+
  scale_color_manual(values=c("violetred2", "darkorange3", "seagreen", "cornflowerblue"), labels = c("Primitive Fish/ Out Group","Otocephala","Basal Euteleosts","Percomorpha" ))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

Plot3_new + labs(color= "Clade") 

