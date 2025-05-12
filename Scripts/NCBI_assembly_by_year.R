library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(ggthemes)

setwd('C:/Users/cat/Documents/buph/Figures')


pal <- c("#000000","#004949","#009292","#ff6db6","#ffb6db",
         "#490092","#006ddb","#b66dff","#6db6ff","#b6dbff",
         "#920000","#924900","#db6d00","#24ff24","#ffff6d")


df <- read_csv('BUSCO_db.csv')

t <- c()
for (i in c('2015-01-01', '2016-01-01', '2017-01-01', '2018-01-01', '2019-01-01',
            '2020-01-01', '2021-01-01', '2022-01-01', '2023-01-01', '2024-01-01')) {
  
  tdf <- subset(df, `Release Date` < i)
    t<- c(t, c(
         length(unique(subset(tdf, Lineage == 'Viridiplantae:n=425')$Genus)),
         length(unique(subset(tdf, Lineage == 'Fungi:n=758')$Genus)),
         length(unique(subset(tdf, Lineage == 'Metazoa:n=954')$Genus)))) }

ten_y <- data.frame(Year = rep(1:10, each=3), Count = t, Lineage = rep(c('Plant', 'Fungi', 'Animal'), 10))

ten_y %>% ggplot(mapping = aes(x=Year, y=Count, color = Lineage)) +
  geom_line(size = 1) + 
  geom_point(shape=21, color="black", fill="#009292", size=2) +
  scale_color_manual(name = "Lineage", values = c('deeppink3', '#006ddb', 'forestgreen')) +
  xlab("Year") + ylab("Unique genera") +
  theme_light() + #ggtitle("NCBI genome counts over the years") +
  scale_x_discrete(limits = factor(1:10), labels = c('2015','2016','2017','2018','2019',
                     '2020','2021','2022','2023','2024'))+
  theme(legend.text=element_text(size=12),
    #legend.position="none",
    plot.title = element_text(size=12, hjust = 0.5),
    axis.text.x = element_text(size=12, angle = 0),
    axis.title.x = element_text(size = 13, hjust = 0.5),
    axis.title.y = element_text(size = 13),
    axis.text.y = element_text(size=12,angle = 0)
  )
  
ggsave('F1Av2.pdf', device = 'pdf', width = 7, height = 5.5)


ten_y

t <- c()
for (i in c('2015-01-01', '2016-01-01', '2017-01-01', '2018-01-01', '2019-01-01',
            '2020-01-01', '2021-01-01', '2022-01-01', '2023-01-01', '2024-01-01')) {
  
  tdf <- subset(df, `Release Date` < i)
  t<- c(t, c(
    length(unique(subset(tdf, Lineage == 'Viridiplantae:n=425')$Family)),
    length(unique(subset(tdf, Lineage == 'Fungi:n=758')$Family)),
    length(unique(subset(tdf, Lineage == 'Metazoa:n=954')$Family)))) }


ten_y <- data.frame(Year = rep(1:10, each=3), Count = t, Lineage = rep(c('Plant', 'Fungi', 'Animal'), 10))

ten_y %>% ggplot(mapping = aes(x=Year, y=Count, color = Lineage)) +
  geom_line(size = 1) + 
  geom_point(shape=21, color="black", fill="#009292", size=2) +
  scale_color_manual(name = "Lineage", values = c('deeppink3', '#006ddb', 'forestgreen')) +
  xlab("Year") + ylab("Unique families") +
  theme_light() + ggtitle("NCBI genome counts over the years") +
  scale_x_discrete(limits = factor(1:10), labels = c('2015','2016','2017','2018','2019',
                                                     '2020','2021','2022','2023','2024'))+
  theme(
    #legend.position="none",
    plot.title = element_text(size=14, hjust = 0.5),
    axis.text.x = element_text(size=10, angle = 0),
    axis.title.x = element_text(size = 11, hjust = 0.5),
    axis.title.y = element_text(size = 11),
    axis.text.y = element_text(size=11,angle = 0)
  )

ggsave('F1AE1.pdf', device = 'pdf', width = 7, height = 5.5)



ggplot(df, aes(Assembly, fill = cut)) +
  geom_histogram(binwidth = 500)


