library(dplyr)
library(tidyr)


top_100_marvel = read.csv(file = './Documents/Shaunak work/DV Project Superheroes/data/top_100_marvel_v2.csv')
head(top_100_marvel)


cols_all <- colnames(top_100_marvel)
cols_to_keep <- cols_all[c(2,4:9,11,14,16,17)]


top_100_marvel_heatmap <- top_100_marvel[cols_to_keep]
View(top_100_marvel_heatmap)

top_100_marvel_heatmap_final <- top_100_marvel_heatmap %>% gather(Attribute, Attribute_Score, Intelligence:Fighting.Skills)

View(top_100_marvel_heatmap_final)

write.csv(top_100_marvel_heatmap_final, './Documents/Shaunak work/DV Project Superheroes/data/heatmap_data.csv')
  