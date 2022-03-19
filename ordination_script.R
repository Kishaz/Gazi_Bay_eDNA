
#Load these packages
library(ggplot2)
library(vegan)
library(dplyr)
library (cowplot)

#Non-Metric Multidimensional Scaling (NMDS)
##calculate the means and maximums of each species
apply(data[,c(-1,-2)],2,mean, na.rm=TRUE)
apply(data[,c(-1,-2)],2,max)
apply(data[,c(-1,-2)],2,var, na.rm=TRUE)

##Performed standardization
data.std <- wisconsin(data[,c(-1,-2)])
apply(data.std[,c(-1,-2)],2,max)
apply(data.std[,c(-1,-2)],2,var, na.rm=TRUE)

##Generate a distance matrix
data.dist <- vegdist(data.std,"bray")

##Performed a non-metric multidimensional scaling with; -two dimensions (k=2) -using principle coordinates analysis to create the initial configuration -used a maximum of 20 random starts -use (modified) Kruskal's stress to determine the match between ordination distances and ecological distances -use Procrustes rotation to determine whether the configurations have converged -center the axes scores and rotated the points such that the greatest plane of variation is orientated with the primary axis.
data.nmds <- metaMDS(data)

##Check the final stress value. Should be < 0.2
data.nmds$stress

##Check the NMDS configurations
data.nmds

##Examined the Shepard (stress) plot to see how well the configuration in two dimensions matches the original data patterns.
stressplot(data.nmds)

## score the nmds.
site.scrs <- as.data.frame(scores(data.nmds, display = "sites")) #save NMDS results into dataframe
site.scrs <- cbind(site.scrs, Habitat = st_data$Habitat) #add grouping variable "Habitat" to dataframe
site.scrs <- cbind(site.scrs, Site = st_data$Site) #add grouping variable of cluster grouping to dataframe
site.scrs <- cbind(site.scrs, Temperature = st_data$`Temperature(0C)`)
site.scrs <- cbind(site.scrs, Depth = st_data$`Depth (m)`)
site.scrs <- cbind(site.scrs, Salinity = st_data$`Salinity (ppt)`)
####site.scrs <- cbind(site.scrs, Site = rownames(site.scrs)) #add site names as variable if you want to display on plot
site.scrs


##Species plot

ordiplot(data.nmds, type="p") # displays species points
ordiellipse(data.nmds, groups = site.scrs$Habitat, draw = "polygon", lty = 1:10, kind="ehull", col=1:8, alpha = 80, label= TRUE)
### AND
plot (data.nmds, type="text") # displays species names
ordiellipse(data.nmds, groups = site.scrs$Habitat, draw = "polygon", lty = 1:10, kind="ehull", col=1:8, alpha = 80, label= TRUE)

##Fit species and environmental vectors
data.envfit <- envfit(data.nmds, st_data, permutations = 999) # this fits environmental vectors
data.spp.fit <- envfit(data.nmds, data, permutations = 999) # this fits species vectors

##Transform species data to vectors and list p values
spp.scrs <- as.data.frame(scores(data.spp.fit, display = "vectors")) #save species intrinsic values into dataframe
spp.scrs <- cbind(spp.scrs, Species = rownames(spp.scrs)) #add species names to dataframe
spp.scrs <- cbind(spp.scrs, pval = data.spp.fit$vectors$pvals) #add pvalues to dataframe so you can select species which are significant
spp.scrs<- cbind(spp.scrs, abrev = abbreviate(spp.scrs$Species, minlength = 6)) #abbreviate species names
sig.spp.scrs <- subset(spp.scrs, pval<=0.05) #subset data to show species significant at 0.05

spp.scrs

##Transform environmental data to vectors and list p value
env.scores.data <- as.data.frame(scores(data.envfit, display = "vectors")) #extracts relevant scores from envifit
env.scores.data <- cbind(env.scores.data, env.variables = rownames(env.scores.data)) #and then gives them their names

env.scores.data <- cbind(env.scores.data, pval = data.envfit$vectors$pvals) # add pvalues to dataframe
sig.env.scrs <- subset(env.scores.data, pval<=0.05) #subset data to show variables significant at 0.05

env.scores.data

## Plot simple ordinate
nmds.plot.data <- ggplot(site.scrs, aes(x=NMDS1, y=NMDS2))+ #sets up the plot
  geom_point(aes(NMDS1, NMDS2, colour = factor(Habitat), shape = factor(Site)), size = 2)+ #adds site points to plot, shape determined by Site, colour determined by Habitat
  coord_fixed()+
  theme_classic()+ 
  theme(panel.background = element_rect(fill = NA, colour = "black", size = 1, linetype = "solid"))+
  labs(colour = "Habitat", shape = "Site")+ # add legend labels for Habitat and Site
  theme(legend.position = "right", legend.text = element_text(size = 12), legend.title = element_text(size = 12), axis.text = element_text(size = 10)) # add legend at right of plot

nmds.plot.data + labs(title = "Ordination plot") #displays plot

### calculate distance for NMDS
bol <- metaMDS(data)
bol

###Add column group to NMDS data frame.
NMDS = data.frame(NMDS1 = bol$points[,1], NMDS2 = bol$points[,2], Habitat=st_data$Habitat)

##Plot significant species and habitat patterns- Significant species plot
nmds.plot.data+
  geom_segment(data = sig.spp.scrs, aes(x = 0, xend=NMDS1, y=0, yend=NMDS2), arrow = arrow(length = unit(0.25, "cm")), colour = "grey10", lwd=0.3) + #add vector arrows of significant species
  ggrepel::geom_text_repel(data = sig.spp.scrs, aes(x=NMDS1, y=NMDS2, label = Species), cex = 3, direction = "both", segment.size = 0.25)+ #add labels for species, use ggrepel::geom_text_repel so that labels do not overlap
  geom_point(data=spp.scrs, aes(x=NMDS1, y=NMDS2))+ 
   # + labs(title = "Ordination with species vectors")
  geom_polygon(data= NMDS, aes(NMDS1, NMDS2, linetype = Habitat, colour = Habitat, fill = Habitat, alpha = 150 )) 

##Significant environment variable plot
nmds.plot.data+
  geom_segment(data = sig.env.scrs, aes(x = 0, xend=NMDS1, y=0, yend=NMDS2), arrow = arrow(length = unit(0.25, "cm")), colour = "grey10", lwd=0.3) + #add vector arrows of significant env variables
  ggrepel::geom_text_repel(data = sig.env.scrs, aes(x=NMDS1, y=NMDS2, label = env.variables), cex = 4, direction = "both", segment.size = 0.25)+ #add labels for env variables
  geom_point(data=spp.scrs, aes(x=NMDS1, y=NMDS2))+ 
  #+labs(title="Ordination with environmental vectors")
  geom_polygon(data= NMDS, aes(NMDS1, NMDS2, linetype = Habitat, colour = Habitat, fill = Habitat, alpha = 150 )) 

##Inference test

### Wisconsin double standardization then followed by Bray-Curtis dissimilarity matrix.
data.dist<- vegdist (wisconsin(data), "bray")

### Permutational multivariate analysis of variance-adonis. To test the significance of habitat difference in species distribution patterns.
adonis(data.dist~st_data$Habitat)
adonis(data.dist~st_data$`Temperature(0C)`)
adonis(data.dist~st_data$`Depth (m)`)
adonis(data.dist~st_data$`Salinity (ppt)`)
adonis(data.dist~st_data$pH)

