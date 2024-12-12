library(geiger)

#Load in WD
setwd("/Users/leticjaflamuraj/Desktop/All_fish_raw")


#loading in the tree
MyTree <- read.tree(file = "Rabosky_et_al_timetree.tre")

#looking at the tree
plot(ladderize(MyTree), cex=0.5)

MyTaxa <- c("Tautoga_onitis",  "Anarhichas_lupus", 
            "Alectis_ciliaris", "Alectis_ciliaris", "Carangoides_bartholomaei", "Caranx_hippos",
            "Caranx_crysos", "Caranx_hippos", "Naucrates_ductor", 
            "Selene_setapinnis", "Selene_vomer", "Seriola_dumerili", "Trachinotus_carolinus",
            "Trachinotus_goodei", "Trachinotus_falcatus",  "Micropterus_salmoides",
            "Astronotus_ocellatus",  "Coryphaena_hippurus",  "Echeneis_naucrates",
            "Remora_remora", "Electrophorus_electricus", "Elops_saurus",
            "Chaetodipterus_faber", "Chaetodipterus_faber", "Ctenogobius_saepepallens",
            "Orthopristis_chrysoptera", "Kyphosus_analogus", "Kyphosus_analogus", "Tautogolabrus_adspersus", "Lobotes_pacificus", "Lutjanus_griseus",
            "Lutjanus_vivanus", "Lutjanus_griseus", "Lutjanus_synagris", "Lutjanus_campechanus",
            "Ocyurus_chrysurus", "Rhomboplites_aurorubens", 
            "Morone_americana", "Morone_saxatilis",  "Perca_flavescens", "Sander_vitreus",
            "Pomatomus_saltatrix", "Priacanthus_arenatus", "Rachycentron_canadum", "Rachycentron_canadum", "Cynoscion_nebulosus", "Cynoscion_nebulosus", "Cynoscion_arenarius",
            "Leiostomus_xanthurus", "Menticirrhus_americanus", "Micropogonias_undulatus", "Pogonias_cromis", "Euthynnus_alletteratus", "Sarda_sarda",
            "Scomber_scombrus", "Scomberomorus_cavalla", "Thunnus_atlanticus", "Thunnus_obesus", "Acanthocybium_solandri",
            "Thunnus_albacares", "Epinephelus_adscensionis", "Hyporthodus_niveatus", "Hyporthodus_nigritus",
            "Mycteroperca_bonaci", "Rypticus_saponaceus", "Rypticus_nigripinnis", "Archosargus_probatocephalus", "Calamus_calamus", 
            "Diplodus_holbrookii", "Lagodon_rhomboides", "Lagodon_rhomboides", "Pagrus_pagrus", "Stenotomus_chrysops", "Sphyraena_guachancho",
            "Sphyraena_barracuda", "Peprilus_simillimus", "Trichiurus_lepturus", "Trichiurus_brevis",
            "Kathetostoma_canaster", "Zoarces_americanus")

#making the pruned tree
MyNewTree <- keep.tip(phy = MyTree, tip = MyTaxa)

#visulizing the tree
plot(ladderize(MyNewTree), label.offset = 0.5, cex = 0.5)
axisPhylo() #adds scale bar to plot [example: time moy]


#saves tree
write.tree(phy = MyNewTree, file = "Macroevofinal.tre")


##pruneing the tree based on our data
MyNewTree

str(MyNewTree)

MyData <- read.csv("perc.csv")
MyData
str(MyData)


meandata <-aggregate(cbind(CB1, Man, as.factor(Clad)) ~as.factor(Phyloname), data = MyData, FUN = mean, na.rm = TRUE, na.action = NULL)
row.names(meandata) <- meandata$`as.factor(Phyloname)`   #need the names to be set as row names... adding row names into the data frame          

Prune <- treedata(phy = MyTree, data= meandata)

str(Prune)

PrunePhy<- Prune$phy
PruneData<- Prune$data


PruneData<-as.data.frame(PruneData)
str(PruneData)


#V5 =CLAD 
PruneData$V3 <-as.factor(PruneData$V3)
str(PruneData)


PruneData$Man <-as.numeric(PruneData$Man)
str(PruneData)


PruneData$CB1 <-as.numeric(PruneData$CB1)
str(PruneData)


PrunePhy$tip.label #what are the names of the taxa in the tree?
rownames(PruneData)

match(PrunePhy$tip.label, rownames(PruneData)) #shifts data so it matches the tree

PruneData1 <- PruneData[match(PrunePhy$tip.label, rownames(PruneData)),]
PruneData1
str(PruneData1)
#run PruneData1 <- as.data.frame(PruneData1) if it comes up as a matrix
class(PruneData1) #check that this is not a matrix, we need a data fram for everything to work

library(caper)

MyTree <- read.tree(file = "Rabosky_et_al_timetree.tre")
MyData2 <- read.csv("perc.csv")
str(MyData2)


meandata2 <-aggregate(cbind(CB1, Man, as.factor(Clad)) ~as.factor(Phyloname), data = MyData2, FUN = mean, na.rm = TRUE, na.action = NULL)
colnames(meandata2)[1] <- "Phyloname"


MyCompData <- comparative.data(phy= MyTree, data = meandata2, names.col = "Phyloname")
MyCompData 
MyCompData$phy
MyCompData$data

MyCompData$data$V3 <-as.factor(MyCompData$data$V3)
str(MyCompData$data$V3)

MyCompData

model01 <-  pgls(log10(CB1) ~ log10(Man), data = MyCompData, lambda = 'ML')  
summary(model01)
anova(model01)

par(mfrow=c(1,1))
ColVec <- c("cornflowerblue")
plot( x = log10(MyCompData$data$Man), y = log10(MyCompData$data$CB1), pch=19, col=ColVec[unclass(MyCompData$data$V3)], xlab = "Log Mandible", ylab= "Log Ceratobranchial 1", ylim=c(0.0,1.4),xlim=c(0.0,1.4)) 
abline(model01)