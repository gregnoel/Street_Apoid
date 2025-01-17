##Setting working directory
setwd("C://Users/Abeille/OneDrive/STREETBEES_KAUWBEES 2019-2020/STREETBEES/Data")

##Loading some packages
library(ggplot2)
library(plyr)
library(viridis)
library(dplyr)
library(tidyr)
library(forcats)
library(hrbrthemes)
DataJoint<-read.csv2("JoinSize_Article.csv")
str(DataJoint)
DataJoint$Espece_Conc<-as.factor(DataJoint$Espece_Conc)

#Graphic plotting


level_order <- c("Andrena_barbilabris","Andrena_vaga","Dasypoda_hirtipes","Lasioglossum_fulvicorne"
                 ,"Lasioglossum_laticeps","Lasioglossum_sexstrigatum","Cerceris_arenaria",
                 "Cerceris_quadricincta","Cerceris_rybyensis","Philanthus_triangulum") #this vector might be useful for other plots/analyses

#c("Andrena barbilabris","Andrena vaga","Dasypoda hirtipes","Lasioglossum fulvicorne","Lasioglossum laticeps","Lasioglossum sexstrigatum","Cerceris arenaria", "Cerceris quadricincta","Cerceris rybyensis","Philanthus triangulum")

p<-ggplot(DataJoint, aes(x=factor(Espece_Conc, level = level_order), y=Size, color=Family)) +
  geom_boxplot() + ylab("Joint widht [cm]") +xlab("Species")+  coord_flip() + scale_color_viridis(discrete=TRUE)+ scale_fill_viridis(discrete=TRUE) + scale_x_discrete(labels = c("Andrena barbilabris","Andrena vaga","Dasypoda hirtipes","Lasioglossum fulvicorne","Lasioglossum laticeps","Lasioglossum sexstrigatum","Cerceris arenaria", "Cerceris quadricincta","Cerceris rybyensis","Philanthus triangulum"))
                                                                                
p<-p+theme_bw(base_size = 11) +theme(legend.position="right") + theme(axis.text.y = element_text(face = "italic"))
p
ggsave("FigJoint_Article.png", plot = p,width = 17,units = "cm", dpi=750)


library(Rmisc)#access to function summarySE
library(car)
library(dunn.test)
library(gridExtra)
library(ggsignif)

#Descriptive Statistics by Species and Family
#summarySE function provides the standard deviation, standard error of the mean, and a (default 95%) confidence interval
DataJoint<-read.csv2("JoinSize_Article.csv")
str(DataJoint)
DataJoint$Espece_Conc<-as.factor(DataJoint$Espece_Conc)
tgc <- summarySE(DataJoint, measurevar="Size", groupvars=c("Espece_Conc","Family"))
tgc
write.csv2(tgc,"Table_Joint_Article.csv")
mean(DataJoint$Size) #==> 1.08
sd(DataJoint$Size) #==> 0.57
max(DataJoint$Size)  #==> 3.00
min(DataJoint$Size)  #==> 0.2



###ANOVA/KW test Size Joint
res.aov<-aov(DataJoint$Size~DataJoint$Espece_Conc)
summary(aov(DataJoint$Size~DataJoint$Espece_Conc))
shapiro.test(res.aov$residuals)

TukeyHSD(res.aov, "DataJoint$Espece_Conc" , conf.level = 0.95)
#kruskal.test(DataJoint$Size~DataJoint$Espece_Conc)
#dunn.test(DataJoint$Size,DataJoint$Espece_Conc, method = "bonferroni") # 

##Linear regression
DataJointSpeciesSize<-read.csv2("JoinSize_ITD_SPE.csv")
DataJointSpeciesSize2 <- aggregate(DataJointSpeciesSize[,c(2,8,9)], by=list(ECH=DataJointSpeciesSize$ECH, Species = DataJointSpeciesSize$Espece_Conc, Family = DataJointSpeciesSize$Famille), FUN=mean)
str(DataJointSpeciesSize2)

LM <- lm(DataJointSpeciesSize2$Largeur~DataJointSpeciesSize2$ITD)
summary(LM)


shapiro.test(LM$residuals)

#Function creation

ggplotRegression <- function (fit,Species, xname, yname) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point(aes(color = Species), size = 3) +
    stat_smooth(method = "lm", col = "black") + xlab(xname) +ylab(yname)
}



library(ggtext)


p<-ggplot(DataJointSpeciesSize2, aes(x = ITD, y = Largeur)) + 
  geom_point(aes(color = Species), size = 4) +
  stat_smooth(method = "lm", col = "black") + xlab("ITD [mm]") +ylab("Joint size [cm]")
p<- p + theme_bw(base_size=9)+  theme(legend.position="right")
#p<-p + scale_color_viridis(discrete=T, labels = c("*Andrena barbilabris*","*Andrena vaga*","*Dasypoda hirtipes*","*Lasioglossum fulvicorne*","*Lasioglossum laticeps*","*Lasioglossum sexstrigatum*","*Cerceris arenaria*", "*Cerceris quadricincta*","*Cerceris rybyensis*","*Philanthus triangulum*"))
p<-p+ scale_color_manual(labels = c("*Andrena barbilabris*","*Andrena vaga*","*Cerceris arenaria*", "*Cerceris quadricincta*","*Cerceris rybyensis*","*Dasypoda hirtipes*","*Lasioglossum fulvicorne*","*Lasioglossum laticeps*","*Lasioglossum sexstrigatum*","*Philanthus triangulum*"),values = c("#CBD588", "#5F7FC7", "orange","#DA5724", "#508578", "#CD9BCD","#AD6F3B", "#673770","#D14285", "#652926"))

p <- p + theme(legend.text = element_markdown()) 
p

ggsave("FigJoint_ITD_Article.png", plot = p,width = 17,units = "cm", dpi=750)







