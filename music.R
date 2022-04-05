music <- read.csv("music.csv",header = TRUE)
library(dplyr)
music <- music %>% 
mutate(popularity = as.numeric(popularity))
names(music)[1]<-paste("genere")
names(music)[2]<-paste("artista")
names(music)[3]<-paste("nome_traccia")
names(music)[5]<-paste("popolarità")
names(music)[6]<-paste("acustica")
names(music)[7]<-paste("ballabilità")
names(music)[9]<-paste("energia")
names(music)[12]<-paste("vividezza")
names(music)[13]<-paste("rumorosità")
names(music)[18]<-paste("valenza")
music2 <- music %>% 
select(genere,artista,nome_traccia,popolarità,acustica,ballabilità,energia,vividezza,rumorosità,valenza)
str(music2)
summary(music2)
standard <- as.data.frame(scale(music2[,c(4:10)]))
summary(standard)
WSS <- function(data, maxCluster = 8) {
SSW <- (nrow(data) - 1) * sum(apply(data, 2, var))
SSW <- vector()
for (i in 2:maxCluster) {SSW[i] <- sum(kmeans(data, centers = i)$withinss)}
plot(1:maxCluster, SSW, type = "o", xlab = "Numero di Cluster", ylab = "Devianza entro gruppi", pch=19)}
WSS(standard)
set.seed(200)
kmeans1 <- kmeans(standard,4,nstart = 20)
kmeans_cluster <- kmeans1$cluster
kmeans_centroidi <- data.frame(kmeans1$centers,cluster = rownames(kmeans1$centers))
pca <- prcomp(standard[,1:7],center = T)
print(pca)
summary(pca) 
screeplot(pca,type=c("lines"))
standard <- data.frame(standard,music2[,-c(4:10)])
standard$cluster <- as.factor(kmeans_cluster)
pr1 <- data.frame(pca$x, cluster = factor(kmeans_cluster))
library(ggplot2)
library(ggrepel)
dataframe_pc <- data.frame(varnames = rownames(pca$rotation), pca$rotation)
x <- "PC1"
y <- "PC2"
data <- data.frame(obsnames=seq(nrow(pca$x)), pca$x)
mult <- min((max(data[,y]) - min(data[,y])/(max(dataframe_pc[,y])-min(dataframe_pc[,y]))),(max(data[,x]) - min(data[,x])/(max(dataframe_pc[,x])-min(dataframe_pc[,x]))))
dataframe_pc <- transform(dataframe_pc,v1 = .9 * mult * (get(x)),v2 = .9 * mult * (get(y)))
ggplot(pr1, aes(x=PC1, y=PC2)) + geom_hline(aes(yintercept=0), size=.2) + 
geom_vline(aes(xintercept=0), size=.2) + coord_equal() +
geom_point(aes(color = cluster),size = 0.2) + geom_segment(data = dataframe_pc, aes(x=0, y=0, xend=v1, yend=v2), arrow = arrow(length=unit(0.2, "cm"))) +
geom_text_repel(data = dataframe_pc, aes((varnames)),point.padding = -10,segment.size = 0.5) + scale_color_brewer(palette = "Dark2")+
guides(colour = guide_legend(override.aes = list(size=3)))+
labs(title = "Analisi cluster con la PCA e i Factor Loading",color = "Cluster")
normalize <- function(x){return ((x - min(x))/(max(x) - min(x)))}
standard2 <- normalize(standard[,c(1:7)])
standard2 <- cbind(standard2,standard[,-c(1:7)])
library(tidyr)
standard2 %>% 
  group_by(cluster) %>% 
  summarise(Acustica = mean(acustica),Rumorosità = mean(rumorosità),
            Ballabilità = mean(ballabilità),Energia = mean(energia),
            Vividezza = mean(vividezza),Valenza = mean(valenza)) %>% 
  select(Rumorosità,Acustica,Ballabilità,Energia,Vividezza,Valenza,cluster) %>% 
  gather("Name","Value",-cluster) %>% 
  ggplot(aes(y=Value,x = cluster,col=Name,group = Name)) +
  geom_point()+ geom_line()+ facet_wrap(~Name,scales = "free_y")+
  scale_color_brewer(palette = "Dark2")+
  labs(x="Cluster",col = "Attributi",title = "Attributi di ogni cluster")
standard2 %>% 
    group_by(cluster) %>% 
    summarise(Popolarità = mean(popolarità)) %>% 
    select(Popolarità,cluster) %>% 
    gather("name","value",-cluster) %>% 
    ggplot(aes(y=value,x = cluster,group = name))+
    geom_point()+ geom_line()+
    labs(y = "Popolarità",x="Cluster",title = "Popolarità media per ogni cluster")
comb = list()
for (i in 1:4){
x <- data.frame(standard2 %>% filter(cluster == i) %>% arrange(desc(popolarità)) %>% head(1))
comb[[i]] <- x} 
combine <- do.call(rbind, comb)     
combine %>% select(artista,nome_traccia,cluster,popolarità)
combine %>% 
gather("name","value",2:7) %>% 
mutate(label = as.character(paste(cluster,artista,"-", (nome_traccia))), text = round(value,2)) %>%
arrange(artista) %>% 
ggplot(aes(x=name,y=value,fill = (name)))+
geom_col(position = position_stack(),aes(fill = (name)))+
geom_text(aes(label=text),position = position_stack(vjust = .5),size=2)+
facet_wrap(~label)+
scale_fill_brewer(palette = "Dark2")+
theme(axis.text.x = element_text(size = 0),strip.text = element_text(size = 5))+labs(x=NULL,y =NULL,fill = "Attributi",title = "Canzoni Più Popolari Per Ogni Cluster")
