library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(ggthemes)

setwd('C:/Users/cat/Documents/buph/Figures')

df <- read_csv('j10c12.csv')
palette.colors(palette = "Okabe-Ito")

# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
               "#0072B2", "#D55E00", "#CC79A7")

pal <- c("#000000","#004949","#009292","#ff6db6","#ffb6db",
         "#490092","#006ddb","#b66dff","#6db6ff","#b6dbff",
         "#920000","#924900","#db6d00","#24ff24","#ffff6d")


df$Lineage <- factor(df$Lineage,
                       levels = c('Viridiplantae:n=425',
                                  'Liliopsida:n=3236',
                                  'Eudicots:n=2326',
                                  'Fungi:n=758',
                                  'Ascomycota:n=1706',
                                  'Basidiomycota:n=1764',
                                  'Metazoa:n=954',
                                  'Arthropoda:n=1013',
                                  'Vertebrata:n=3354'),ordered = TRUE)

df$Type <- factor(df$Type,
                 levels = c('Single', 'Duplicated'), ordered = 1)

#for horizonal
df$Lineage <- fct_rev(df$Lineage)
df$Type <- fct_rev(df$Type)
df$Percent <- df$Percent*100


#special chars
#v1 had #009292 instead of #D55E00. #0072B2 borders.


ggplot(data = df, aes(x=Lineage, y=Percent, fill=Type, color = Type)) +
  geom_boxplot(outlier.size=0.1) +
  #scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  #geom_jitter(color="black", size=0.01, alpha=0.1) +
  scale_fill_manual(name= "Type", values = c("#D55E00", "#6db6ff"))+
  scale_color_manual(name = "Type", values = c("#924900", "#0072B2")) +
    ylab("Percentage") + 
    scale_x_discrete(labels = c("Vertebrata", "Arthropoda", "Metazoa", 
                                "Basidiomycota","Ascomycota", "Fungi", 
                                "Eudicots", "Liliopsida", "Viridiplantae")) +
    coord_flip() +
  theme_light() +
  theme(legend.text=element_text(size=13),
    #legend.position="none",
    plot.title = element_text(size=14, hjust = 0.5),
    axis.text.x = element_text(size=14, angle = 0),
    axis.title.x=element_text(size = 15, hjust = 0.5),
    axis.title.y=element_blank(),
    axis.text.y = element_text(size=15,angle = 0)
  ) #+ ggtitle("miniBUSCO results for major lineages") 

  

ggsave('F1Bv3.pdf', device = 'pdf', width = 13, height = 6)



# function for number of observations 
give.n <- function(x){
  return(c(y = median(x)*1.15, label = length(x))) 
  # experiment with the multiplier to find the perfect position
}

subset(df, Lineage == 'Viridiplantae:n=425') %>% 
  subset(phylum == 'Streptophyta') %>%
  ggplot( aes(x=class, y=Percent, fill = Type)) +
  geom_boxplot(outlier.size=0.1) +
  #coord_flip() +
  #scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="blue", position = position_jitterdodge(), size=0.5, alpha=0.5) +
  scale_fill_manual(name= "Lineage", values = rev(pal[2:15]))+
  scale_color_manual(name = "Type", values = rev(c("#999999", "#009F73"))) +
  theme_light() +
  theme(
    #legend.position="none",
    plot.title = element_text(size=14, hjust = 0.5),
    axis.text.x = element_text(size=10, angle = 90, vjust = 0.5, hjust = 1),
    axis.title.x=element_text(size = 11, hjust = 0.5),
    axis.title.y=element_blank(),
    axis.text.y = element_text(size=12,angle = 0)
  ) +
  ggtitle("miniBUSCO results for major classes of streptophytes") #+ 
  #stat_summary(fun.data = give.n, geom = "text", fun.y = median)

ggsave('F1E1.pdf', device = 'pdf', width = 9, height = 6)




subset(df, Lineage == 'Viridiplantae:n=425') %>% 
  subset(class == 'Magnoliopsida') %>%
  ggplot( aes(x=order, y=Percent, fill = Type)) +
  geom_boxplot(outlier.size=0.1) +
  #coord_flip() +
  #scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="blue", position = position_jitterdodge(), size=0.5, alpha=0.5) +
  scale_fill_manual(name= "Lineage", values = rev(pal[2:15]))+
  scale_color_manual(name = "Type", values = rev(c("#999999", "#009F73"))) +
  theme_light() +
  theme(
    #legend.position="none",
    plot.title = element_text(size=14, hjust = 0.5),
    axis.text.x = element_text(size=10, angle = 90, vjust = 0.5, hjust = 1),
    axis.title.x=element_text(size = 11, hjust = 0.5),
    axis.title.y=element_blank(),
    axis.text.y = element_text(size=12,angle = 0)
  ) +
  ggtitle("miniBUSCO results for orders of magnoliophytes") 

ggsave('F1E2.pdf', device = 'pdf', width = 15, height = 8)




subset(df, Lineage == 'Fungi:n=758') %>% 
  ggplot( aes(x=phylum, y=Percent, fill = Type)) +
  geom_boxplot(outlier.size=0.1) +
  #coord_flip() +
  #scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="blue", position = position_jitterdodge(), size=0.5, alpha=0.5) +
  scale_fill_manual(name= "Lineage", values = rev(pal[2:15]))+
  scale_color_manual(name = "Type", values = rev(c("#999999", "#009F73"))) +
  theme_light() +
  theme(
    #legend.position="none",
    plot.title = element_text(size=14, hjust = 0.5),
    axis.text.x = element_text(size=10, angle = 90, vjust = 0.5, hjust = 1),
    axis.title.x=element_text(size = 11, hjust = 0.5),
    axis.title.y=element_blank(),
    axis.text.y = element_text(size=12,angle = 0)
  ) +
  ggtitle("miniBUSCO results for 10 fungal phyla")


ggsave('F1E3.pdf', device = 'pdf', width = 8, height = 8)


subset(df, Lineage == 'Fungi:n=758') %>% 
  subset(class == 'Saccharomycetes') %>% 
  ggplot( aes(x=family, y=Percent, fill = Type)) +
  geom_boxplot(outlier.size=0.1) +
  #coord_flip() +
  #scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="blue", position = position_jitterdodge(), size=0.5, alpha=0.5) +
  scale_fill_manual(name= "Lineage", values = rev(pal[2:15]))+
  scale_color_manual(name = "Type", values = rev(c("#999999", "#009F73"))) +
  theme_light() +
  theme(
    #legend.position="none",
    plot.title = element_text(size=14, hjust = 0.5),
    axis.text.x = element_text(size=10, angle = 90, vjust = 0.5, hjust = 1),
    axis.title.x=element_text(size = 11, hjust = 0.5),
    axis.title.y=element_blank(),
    axis.text.y = element_text(size=12,angle = 0)
  ) +
  ggtitle("miniBUSCO results for 10 fungal phyla")




subset(df, Lineage == 'Basidiomycota:n=1764') %>% 
  ggplot(aes(x=class, y=Percent, fill = Type)) +
  geom_boxplot(outlier.size=0.1) +
  #coord_flip() +
  #scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="blue", position = position_jitterdodge(), size=0.5, alpha=0.5) +
  scale_fill_manual(name= "Lineage", values = rev(pal[2:15]))+
  scale_color_manual(name = "Type", values = rev(c("#999999", "#009F73"))) +
  theme_light() +
  theme(
    #legend.position="none",
    plot.title = element_text(size=14, hjust = 0.5),
    axis.text.x = element_text(size=10, angle = 90, vjust = 0.5, hjust = 1),
    axis.title.x=element_text(size = 11, hjust = 0.5),
    axis.title.y=element_blank(),
    axis.text.y = element_text(size=12,angle = 0)
  ) +
  ggtitle("miniBUSCO results for major classes of Basidiomycetes")

ggsave('F1E4.pdf', device = 'pdf', width = 12, height = 8)



subset(df, Lineage == 'Basidiomycota:n=1764') %>% 
  subset(class == 'Pucciniomycetes') %>% 
  ggplot(aes(x=family, y=Percent, fill = Type)) +
  geom_boxplot(outlier.size=0.1) +
  #coord_flip() +
  #scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="blue", position = position_jitterdodge(), size=0.5, alpha=0.5) +
  scale_fill_manual(name= "Lineage", values = rev(pal[2:15]))+
  scale_color_manual(name = "Type", values = rev(c("#999999", "#009F73"))) +
  theme_light() +
  theme(
    #legend.position="none",
    plot.title = element_text(size=14, hjust = 0.5),
    axis.text.x = element_text(size=10, angle = 90, vjust = 0.5, hjust = 1),
    axis.title.x=element_text(size = 11, hjust = 0.5),
    axis.title.y=element_blank(),
    axis.text.y = element_text(size=12,angle = 0)
  ) +
  ggtitle("miniBUSCO results for families of class Pucciniomycetes")

ggsave('F1E5.pdf', device = 'pdf', width = 8, height = 8)


subset(df, Lineage == 'Basidiomycota:n=1764') %>% 
  subset(family == 'Pucciniaceae') %>% 
  ggplot(aes(x=genus, y=Percent, fill = Type)) +
  geom_boxplot(outlier.size=0.1) +
  #coord_flip() +
  #scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="blue", position = position_jitterdodge(), size=0.5, alpha=0.5) +
  scale_fill_manual(name= "Lineage", values = rev(pal[2:15]))+
  scale_color_manual(name = "Type", values = rev(c("#999999", "#009F73"))) +
  theme_light() +
  theme(
    #legend.position="none",
    plot.title = element_text(size=14, hjust = 0.5),
    axis.text.x = element_text(size=10, angle = 90, vjust = 0.5, hjust = 1),
    axis.title.x=element_text(size = 11, hjust = 0.5),
    axis.title.y=element_blank(),
    axis.text.y = element_text(size=12,angle = 0)
  ) +
  ggtitle("miniBUSCO results for genuses of family Pucciniomyceae")

ggsave('F1E6.pdf', device = 'pdf', width = 8, height = 6)



subset(df, Lineage == 'Metazoa:n=954') %>% 
  ggplot(aes(x=class, y=Percent, fill = Type)) +
  geom_boxplot(outlier.size=0.1) +
  #coord_flip() +
  #scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="blue", position = position_jitterdodge(), size=0.5, alpha=0.5) +
  scale_fill_manual(name= "Lineage", values = rev(pal[2:15]))+
  scale_color_manual(name = "Type", values = rev(c("#999999", "#009F73"))) +
  theme_light() +
  theme(
    #legend.position="none",
    plot.title = element_text(size=14, hjust = 0.5),
    axis.text.x = element_text(size=10, angle = 90, vjust = 0.5, hjust = 1),
    axis.title.x=element_text(size = 11, hjust = 0.5),
    axis.title.y=element_blank(),
    axis.text.y = element_text(size=12,angle = 0)
  ) +
  ggtitle("miniBUSCO results for major classes of metazoans")

ggsave('F1E7.pdf', device = 'pdf', width = 16, height = 8)


subset(df, Lineage == 'Metazoa:n=954') %>% 
  ggplot(aes(x=phylum, y=Percent, fill = Type)) +
  geom_boxplot(outlier.size=0.1) +
  #coord_flip() +
  #scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="blue", position = position_jitterdodge(), size=0.5, alpha=0.5) +
  scale_fill_manual(name= "Lineage", values = rev(pal[2:15]))+
  scale_color_manual(name = "Type", values = rev(c("#999999", "#009F73"))) +
  theme_light() +
  theme(
    #legend.position="none",
    plot.title = element_text(size=14, hjust = 0.5),
    axis.text.x = element_text(size=10, angle = 90, vjust = 0.5, hjust = 1),
    axis.title.x=element_text(size = 11, hjust = 0.5),
    axis.title.y=element_blank(),
    axis.text.y = element_text(size=12,angle = 0)
  ) +
  ggtitle("miniBUSCO results for major phyla of metazoans")

ggsave('F1E8.pdf', device = 'pdf', width = 12, height = 8)


subset(df, Lineage == 'Vertebrata:n=3354') %>% 
  ggplot(aes(x=class, y=Percent, fill = Type)) +
  geom_boxplot(outlier.size=0.1) +
  #coord_flip() +
  #scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="blue", position = position_jitterdodge(), size=0.5, alpha=0.5) +
  scale_fill_manual(name= "Lineage", values = rev(pal[2:15]))+
  scale_color_manual(name = "Type", values = rev(c("#999999", "#009F73"))) +
  theme_light() +
  theme(
    #legend.position="none",
    plot.title = element_text(size=14, hjust = 0.5),
    axis.text.x = element_text(size=10, angle = 90, vjust = 0.5, hjust = 1),
    axis.title.x=element_text(size = 11, hjust = 0.5),
    axis.title.y=element_blank(),
    axis.text.y = element_text(size=12,angle = 0)
  ) +
  ggtitle("miniBUSCO results for major classes of vertebrates")

ggsave('F1E9.pdf', device = 'pdf', width = 10, height = 8)


subset(df, Lineage == 'Vertebrata:n=3354') %>% 
  subset(class == 'Amphibia') %>% 
  ggplot(aes(x=family, y=Percent, fill = Type)) +
  geom_boxplot(outlier.size=0.1) +
  #coord_flip() +
  #scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="blue", position = position_jitterdodge(), size=1, alpha=0.8) +
  scale_fill_manual(name= "Lineage", values = rev(pal[2:15]))+
  scale_color_manual(name = "Type", values = rev(c("#999999", "#009F73"))) +
  theme_light() +
  theme(
    #legend.position="none",
    plot.title = element_text(size=14, hjust = 0.5),
    axis.text.x = element_text(size=10, angle = 90, vjust = 0.5, hjust = 1),
    axis.title.x=element_text(size = 11, hjust = 0.5),
    axis.title.y=element_blank(),
    axis.text.y = element_text(size=12,angle = 0)
  ) +
  ggtitle("miniBUSCO results for families of amphibians")

ggsave('F1E10.pdf', device = 'pdf', width = 10, height = 8)



subset(df, Lineage == 'Arthropoda:n=1013') %>% 
  subset(phylum == 'Arthropoda') %>% 
  ggplot(aes(x=class, y=Percent, fill = Type)) +
  geom_boxplot(outlier.size=0.1) +
  #coord_flip() +
  #scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="blue", position = position_jitterdodge(), size=0.5, alpha=0.5) +
  scale_fill_manual(name= "Lineage", values = rev(pal[2:15]))+
  scale_color_manual(name = "Type", values = rev(c("#999999", "#009F73"))) +
  theme_light() +
  theme(
    #legend.position="none",
    plot.title = element_text(size=14, hjust = 0.5),
    axis.text.x = element_text(size=10, angle = 90, vjust = 0.5, hjust = 1),
    axis.title.x=element_text(size = 11, hjust = 0.5),
    axis.title.y=element_blank(),
    axis.text.y = element_text(size=12,angle = 0)
  ) +
  ggtitle("miniBUSCO results for classes of arthropods")

ggsave('F1E11.pdf', device = 'pdf', width = 10, height = 8)



subset(df, Lineage == 'Arthropoda:n=1013') %>% 
  subset(class == 'Insecta') %>% 
  ggplot(aes(x=order, y=Percent, fill = Type)) +
  geom_boxplot(outlier.size=0.1) +
  #coord_flip() +
  #scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="blue", position = position_jitterdodge(), size=0.5, alpha=0.5) +
  scale_fill_manual(name= "Lineage", values = rev(pal[2:15]))+
  scale_color_manual(name = "Type", values = rev(c("#999999", "#009F73"))) +
  theme_light() +
  theme(
    #legend.position="none",
    plot.title = element_text(size=14, hjust = 0.5),
    axis.text.x = element_text(size=10, angle = 90, vjust = 0.5, hjust = 1),
    axis.title.x=element_text(size = 11, hjust = 0.5),
    axis.title.y=element_blank(),
    axis.text.y = element_text(size=12,angle = 0)
  ) +
  ggtitle("miniBUSCO results for orders of insects")

ggsave('F1E12.pdf', device = 'pdf', width = 10, height = 8)


subset(df, Lineage == 'Arthropoda:n=1013') %>% 
  subset(order == 'Lepidoptera') %>% 
  ggplot(aes(x=family, y=Percent, fill = Type)) +
  geom_boxplot(outlier.size=0.1) +
  #coord_flip() +
  #scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="blue", position = position_jitterdodge(), size=0.5, alpha=0.5) +
  scale_fill_manual(name= "Lineage", values = rev(pal[2:15]))+
  scale_color_manual(name = "Type", values = rev(c("#999999", "#009F73"))) +
  theme_light() +
  theme(
    #legend.position="none",
    plot.title = element_text(size=14, hjust = 0.5),
    axis.text.x = element_text(size=10, angle = 90, vjust = 0.5, hjust = 1),
    axis.title.x=element_text(size = 11, hjust = 0.5),
    axis.title.y=element_blank(),
    axis.text.y = element_text(size=12,angle = 0)
  ) +
  ggtitle("miniBUSCO results for families of lepidopterans")

ggsave('F1E13.pdf', device = 'pdf', width = 14, height = 8)



subset(df, Lineage == 'Arthropoda:n=1013') %>% 
  subset(order == 'Hymenoptera') %>% 
  ggplot(aes(x=family, y=Percent, fill = Type)) +
  geom_boxplot(outlier.size=0.1) +
  #coord_flip() +
  #scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="blue", position = position_jitterdodge(), size=0.5, alpha=0.5) +
  scale_fill_manual(name= "Lineage", values = rev(pal[2:15]))+
  scale_color_manual(name = "Type", values = rev(c("#999999", "#009F73"))) +
  theme_light() +
  theme(
    #legend.position="none",
    plot.title = element_text(size=14, hjust = 0.5),
    axis.text.x = element_text(size=10, angle = 90, vjust = 0.5, hjust = 1),
    axis.title.x=element_text(size = 11, hjust = 0.5),
    axis.title.y=element_blank(),
    axis.text.y = element_text(size=12,angle = 0)
  ) +
  ggtitle("miniBUSCO results for families of Hemipterans")

ggsave('F1E14.pdf', device = 'pdf', width = 14, height = 8)





