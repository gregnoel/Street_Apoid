###Particle_size Analysis
# https://cran.r-project.org/web/packages/soiltexture/vignettes/soiltexture_vignette.pdf


library(soiltexture)


###Test avec autre chose
# Load the required libraries
library(ggtern)
library(plyr)
library(grid)

# Load the Data. (Available in ggtern 1.0.3.0 next version)
data(USDA)

# Put tile labels at the midpoint of each tile.
USDA.LAB = ddply(USDA, 'Label', function(df) {
  apply(df[, 1:3], 2, mean)
})

# Tweak
USDA.LAB$Angle = 0
USDA.LAB$Angle[which(USDA.LAB$Label == 'Loamy Sand')] = -35

# Construct the plot.
# NOTE aes(color=Label,fill=Label) in 3rd line below
base = ggplot(data = USDA, aes(y=Clay, x=Sand, z=Silt)) +
  coord_tern(L="x",T="y",R="z") +
  geom_polygon(alpha = 0.75, size = 0.5, color = 'black',aes(color=Label,fill=Label)) +
  geom_text(data = USDA.LAB,
            aes(label = Label, angle = Angle),
            color = 'black',
            size = 3.5) +
  theme_rgbw() +
  theme_showsecondary() +
  theme_showarrows() +
  custom_percent("Percent") +
  theme(legend.justification = c(0, 1),
        legend.position      = c(0, 1) ) +
  labs(title = 'USDA Textural Classification Chart',
       fill  = 'Textural Class',
       color = 'Textural Class')
base
df = structure(list(Clay = c(1.430976431,1.080050826,1.593959732,2.453303596,1.361731844,1.532951289,0.773889637,1.532951289,0,1.41955836,2.061132922,1.475204018,1.143607049,1.943005181,0.307647231,0.669736291,0.669736291,1.487179487,2.393932211,1.396529835,0.871694417,0.903102422,0.724637681,1.167485955,1.979858091,1.772126552,1.272727273,0.048309179,1.12244898,1.093786363,0.659606094,0.916936354,0.596703921,0.602189781,0.164346481,0.810810811,1.284660392,1.915103653,2.795395819,1.235584843,2.121936641,3.379805661,1.490585774,1.088270859,0.494951495,0.585525053,0.532342306,1.565008026,0.254424779,1.657317695,4.98078155,2.069218092,0.312029266), 
                    Silt = c(1.430976431,1.080050826,1.593959732,2.453303596,1.361731844,1.532951289,0.773889637,1.532951289,0,1.41955836,2.061132922,1.475204018,1.143607049,1.943005181,0.307647231,0.669736291,0.669736291,1.487179487,2.393932211,1.396529835,0.871694417,0.903102422,0.724637681,1.167485955,1.979858091,1.772126552,1.272727273,0.048309179,1.12244898,1.093786363,0.659606094,0.916936354,0.596703921,0.602189781,0.164346481,0.810810811,1.284660392,1.915103653,2.795395819,1.235584843,2.121936641,3.379805661,1.490585774,1.088270859,0.494951495,0.585525053,0.532342306,1.565008026,0.254424779,1.657317695,4.98078155,2.069218092,0.312029266), 
                    Sand = c(97.14,97.84,96.81,95.09,97.28,96.93,98.45,96.93,100.00,97.16,95.88,97.05,97.71,96.11,99.38,98.66,98.66,97.03,95.21,97.21,98.26,98.19,98.55,97.67,96.04,96.46,97.45,99.90,97.76,97.81,98.68,98.17,98.81,98.80,99.67,98.38,97.43,96.17,94.41,97.53,95.76,93.24,97.02,97.82,99.01,98.83,98.94,96.87,99.49,96.69,90.04,95.86,99.38)), 
               .Names = c("Clay", "Silt","Sand"), row.names = c(NA, -53L), class = "data.frame")
base<-base + geom_point(data=df,size=3)
base ##Take it in zoom
setwd("C://Users/Abeille/OneDrive/STREETBEES_KAUWBEES 2019-2020/STREETBEES/Data")
ggsave("Texture.png", plot = base, width=20, height=10, units="in", dpi=500)

####PCA Granulo
DataGranulo<-read.csv2("Granulo_PCA.csv")


library(factoextra)
library(FactoMineR)

#define the factors 
DataGranulo$Espece_Conc<-factor(DataGranulo$Espece_Conc)
#pollen$month <- factor(pollen$month, levels=c("March", "April", "May","June","July", "August", "September"))
DataGranulo$Famille <-factor(DataGranulo$Famille)
str(DataGranulo)
#jaccard index for presence/absence matrix
Granu.matrix<-as.matrix(DataGranulo[,c(22:27)])


pca_granulo<-PCA(Granu.matrix,scale.unit = T, graph = T)
eig.val<-get_eigenvalue(pca_granulo)
eig.val

fviz_eig(pca_granulo,addlabels = T, ylim = c(0,50))

fviz_pca_ind(pca_granulo,
             geom.ind = "point",
             col.ind = DataGranulo$Espece_Conc,
             addEllipses = T)


