library(geiger)

#Load in WD
setwd("/Users/leticjaflamuraj/Desktop/All_fish_raw")


#loading in the tree
MyTree <- read.tree(file = "Rabosky_et_al_timetree.tre")

#looking at the tree
plot(ladderize(MyTree), cex=0.5)

MyTaxa <- c("Amia_calva", "Amia_calva", "Amia_calva", "Amia_calva", "Amia_calva", "Amia_calva", "Anguilla_rostrata", "Arapaima_gigas", "Elops_saurus", "Hiodon_alosoides", "Lepisosteus_osseus", 
            "Lepisosteus_oculatus", "Megalops_atlanticus", "Gymnothorax_moringa", "Ophichthus_gomesii", "Echiophis_punctifer", "Ophichthus_rex", "Osteoglossum_bicirrhosum", "Scleropages_formosus", 
            "Polyodon_spathula", "Polyodon_spathula")

#making the pruned tree
MyNewTree <- keep.tip(phy = MyTree, tip = MyTaxa)

#visulizing the tree
plot(ladderize(MyNewTree), label.offset = 0.5)
axisPhylo() #adds scale bar to plot [example: time moy]


#saves tree
write.tree(phy = MyNewTree, file = "Macroevofinal.tre")


##pruneing the tree based on our data
MyNewTree

str(MyNewTree)

MyData <- read.csv("Primative.csv")
MyData
str(MyData)


meandata <-aggregate(cbind(CB5, Man, as.factor(Clad)) ~as.factor(Phyloname), data = MyData, FUN = mean, na.rm = TRUE, na.action = NULL)
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

library(caper)

MyTree <- read.tree(file = "Rabosky_et_al_timetree.tre")
MyData2 <- read.csv("Primative.csv")
str(MyData2)


meandata2 <-aggregate(cbind(CB5, Man, as.factor(Clad)) ~as.factor(Phyloname), data = MyData2, FUN = mean, na.rm = TRUE, na.action = NULL)
colnames(meandata2)[1] <- "Phyloname"


MyCompData <- comparative.data(phy= MyTree, data = meandata2, names.col = "Phyloname")
MyCompData 
MyCompData$phy
MyCompData$data

MyCompData$data$V3 <-as.factor(MyCompData$data$V3)
str(MyCompData$data$V3)

MyCompData

model01 <-  pgls(log10(CB5) ~ log10(Man), data = MyCompData, lambda = 'ML')  
summary(model01)
anova(model01)

par(mfrow=c(1,1))

ColVec <- c("violetred2")
plot( x = log10(MyCompData$data$Man), y = log10(MyCompData$data$CB5), pch=19,  col=ColVec[unclass(MyCompData$data$V3)], xlab = "Log Mandible", ylab= "Log Ceratobranchial 5", ylim=c(0.0,1.4),xlim=c(0.0,1.4)) 
abline(model01)

