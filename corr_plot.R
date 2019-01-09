installed.packages("ggcorrplot", dependencies = TRUE)

top_100_marvel = read.csv(file = './Documents/Shaunak work/DV Project Superheroes/data/top_100_marvel_v2.csv')
head(top_100_marvel)

numeric_cols <- c("Intelligence", "Strength", "Speed", "Durability", "Energy.Projection", "Fighting.Skills", "APPEARANCES")

top_100_marvel_cols <- colnames(top_100_marvel)

top_100_marvel_cols_num <- top_100_marvel_cols[c(4,5,6,7,8,9,17)]

top_100_marvel_num <- top_100_marvel[top_100_marvel_cols_num]

View(top_100_marvel_num)


corr_matrix <- round(cor(top_100_marvel_num),2)

library(reshape2)

melted_corr_matrix <- melt(corr_matrix)
head(melted_corr_matrix)
library(ggplot2)


ggplot(data = melted_corr_matrix, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()


# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(cormat = corr_matrix)
upper_tri
melted_cormat <- melt(upper_tri, na.rm = TRUE)
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()


reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}


# Reorder the correlation matrix
cormat <- reorder_cormat(corr_matrix)
upper_tri <- get_upper_tri(corr_matrix)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
# Print the heatmap
print(ggheatmap)


ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))
