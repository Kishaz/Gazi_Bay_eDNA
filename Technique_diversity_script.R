##Prerequisite
library (vegan)
library (ggplot2)
library (cowplot)

## subsetting data columns
data <- data[, 2:131]

####CALCULATING ALPHA DIVERSITY

# Shannon's H'
H <- diversity(data)

# Observed Richness
richness <- specnumber(data)  

# Pielou's Evenness
evenness <- H/log(richness)

# Create alpha diversity dataframe including environmental data
alpha <- cbind(shannon = H, richness = richness, pielou = evenness, technique_data)
list(alpha)

####PLOT ALPHA DIVERSITY
plot.shan <- ggplot(alpha, aes(x =Technique , y = shannon, colour = Technique)) +
  geom_point(size = 3) +
  scale_colour_viridis_d(option = "magma", begin = 0.2, end = 0.8) +
  ylab("Shannon's H'") + 
  xlab("") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4))

plot.rich <-ggplot(alpha, aes(x = Technique, y = richness, colour = Technique)) +
  geom_point(size = 3) +
  scale_colour_viridis_d(option = "magma", begin = 0.2, end = 0.8) +
  ylab("Species Richness") +
  xlab("") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4))

plot.even <- ggplot(alpha, aes(x = Technique, y = evenness, colour = Technique)) +
  geom_point(size = 3) +
  scale_colour_viridis_d(option = "magma", begin = 0.2, end = 0.8) +
  ylab("Pielou's Evenness") +
  xlab("") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4))

legend <- get_legend(plot.even)

plot_grid(plot.shan + theme(legend.position = "none"), plot.rich + theme(legend.position = "none"), plot.even + theme(legend.position = "none"),ncol = 3)


#####BETA DIVERSITY
##Calculate pairwise dissimilarity
## Bray-Curtis Dissimilarity
data.mdf <- as.matrix.data.frame(data)
rownames(data.mdf) <- technique_data$Technique

data.bray <- vegdist(data.mdf, method = "bray")
data.bray

##Jaccard Dissimilarity
data.jac <- vegdist(data.mdf, method = "jaccard", binary = T)
data.jac
##SIMPER
simper(data, technique_data$Technique, permutations = 999)

####ORDINATION
##Principal Coordinates Analysis (PCoA)
# calculate principal coordinates analysis (Bray-Curtis)
pcoa.data.bray <- cmdscale(data.bray, k = 2, eig = T)

# extract axis positions for each site from cmdscale object and create a dataframe for plotting
pcoa.data.bray.plotting <- as.data.frame(pcoa.data.bray$points)
colnames(pcoa.data.bray.plotting) <- c("axis_1", "axis_2")
pcoa.data.bray.plotting$Technique <- rownames(pcoa.data.bray.plotting)

# calculate the proportion of variance in the data which is explained by the first two PCoA axes
pcoa.data.bray$eig[1]/(sum(pcoa.data.bray$eig))

pcoa.data.bray$eig[2]/(sum(pcoa.data.bray$eig))

# create a PCoA plot
pcoa.data.bray.plot <- ggplot(pcoa.data.bray.plotting, aes(x = axis_1, y = axis_2, colour = Technique)) +
  geom_point(size = 3) +
  scale_colour_viridis_d(option = "magma", begin = 0.2, end = 0.8) +
  theme_bw() + 
  xlab("PCoA 1 (30.8%)") +
  ylab("PCoA 2 (22.3%)") +
  annotate(geom = 'text', label = 'Bray-Curtis', x = Inf, y = -Inf, hjust = 1.15, vjust = -1)

# repeat process with Jaccard dissimilarity matrix
pcoa.data.jac <- cmdscale(data.jac, k = 2, eig = T)

pcoa.data.jac.plotting <- as.data.frame(pcoa.data.jac$points)
colnames(pcoa.data.jac.plotting) <- c("axis_1", "axis_2")
pcoa.data.jac.plotting$Technique <- rownames(pcoa.data.jac.plotting)

pcoa.data.jac$eig[1]/(sum(pcoa.data.jac$eig))

pcoa.data.jac$eig[2]/(sum(pcoa.data.jac$eig))

pcoa.data.jac.plot <- ggplot(pcoa.data.jac.plotting, aes(x = axis_1, y = axis_2, colour = Technique)) +
  geom_point(size = 3) +
  scale_colour_viridis_d(option = "magma", begin = 0.2, end = 0.8) +
  theme_bw() + 
  xlab("PCoA 1 (31.6%)") +
  ylab("PCoA 2 (24.0%)") +
  annotate(geom = 'text', label = 'Jaccard', x = Inf, y = -Inf, hjust = 1.215, vjust = -1)


# extract plot legend
legend <- get_legend(pcoa.data.jac.plot)

# plot Bray-Curtis PCoA and Jaccard PCoA side by side
plot_grid(pcoa.data.bray.plot + theme(legend.position = 'none'), pcoa.data.jac.plot + theme(legend.position = 'none'), legend, ncol = 3, rel_widths = c(1,1,0.5))