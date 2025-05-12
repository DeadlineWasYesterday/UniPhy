library(ggplot2)

setwd('C:/Users/cat/Documents/buph/Figures')

df <- read.csv('../meta/mcomp.tsv', sep = '\t')
df <- na.omit(df[df$Ploidy < 10,])
df$Ploidy <- as.factor(df$Ploidy)

ggplot(df, aes(x = Assembly_level, y = Inflation)) + 
  #geom_point(color = "#0099f9", size = 2) +
  geom_point(aes(shape = Ploidy), size = 2.5, fill = '#0099f9') +
  scale_shape_manual(values = c(21:24))+
  scale_x_continuous(breaks = seq(2,8,1), labels = seq(2,8))+
  scale_y_continuous(breaks = seq(2,7,1))+
  labs(title = "Mean BUSCO Copies vs Number of Assembly Haplotypes",) + 
  ylab("Mean BUSCO copies") + 
  xlab("Assembly haplotypes") +
  theme_light() +
  theme(panel.grid.minor = element_blank(),    plot.title = element_text(color = "#000000", size = 14, hjust = 0.5))

ggsave('BC.pdf', device = 'pdf', width = 7, height = 7)

cor.test(as.numeric(df$Assembly_level), df$Inflation)


summary(lm(unlist(df['Assembly_level']) ~ unlist(df['Inflation'])))
