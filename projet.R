setwd("C:/Users/rugby/Documents/cours/donnees")

library(factoextra)
library(cluster)


teams<-read.csv("nba_2017_att_val_elo_with_cluster.csv")
salary_twitt_wiki<-read.csv("nba_2017_players_with_salary_wiki_twitter.csv")
salary_twitt_wiki <- na.omit(salary_twitt_wiki) 

endorsement<-read.csv("nba_2017_endorsements.csv")
salary_twitt<-read.csv("nba_2016_2017_100.csv")

city<-read.csv("sub-est2019_all.csv")


X=teams[ , c("ELO", "VALUE_MILLIONS")]

X=teams[,c(8,7) ]

x1=teams$ELO
y1=teams$VALUE_MILLIONS

plot(x1,y1)


set.seed(1)

#perform k-means clustering with k = 4 clusters
km <- kmeans(X, centers = 3, nstart=29)


fviz_cluster(km, data = X)


final_data <- cbind(teams, cluster = km$cluster1)


###### CONclusions : trois types d'equipe avec KMeans


######################PARTIE JOUEUR ###########################




X=salary_twitt_wiki[ , c("POINTS", "WINS_RPM")]


gap_stat <- clusGap(X,
                    FUN = kmeans,
                    nstart = 42,
                    K.max = 10,
                    B = 50)

#plot number of clusters vs. gap statistic
fviz_gap_stat(gap_stat)


  4
set.seed(1)

#perform k-means clustering with k = 4 clusters
km <- kmeans(X, centers = 4, nstart=42)
fviz_cluster(km, data = X)


salary_twitt_wiki <- cbind(salary_twitt_wiki, cluster = km$cluster)


################ 4 type de joueurs #############





cluster1 <- subset(salary_twitt_wiki, cluster == 1)
cluster2 <- subset(salary_twitt_wiki, cluster == 3)
cluster3 <- subset(salary_twitt_wiki, cluster == 4)
cluster4 <- subset(salary_twitt_wiki, cluster == 2)


label1=cluster1[order(-cluster1$SALARY_MILLIONS),][1:7,]
label2=cluster2[order(-cluster2$SALARY_MILLIONS),][1:7,]
label3=cluster3[order(-cluster3$SALARY_MILLIONS),][1:7,]
label4=cluster4[order(-cluster4$SALARY_MILLIONS),][1:7,]

label1=label1$PLAYER
label2=label2$PLAYER
label3=label3$PLAYER
label4=label4$PLAYER


library(ggpubr)


ggscatter(cluster1, x = "POINTS", y = "WINS_RPM", color = "SALARY_MILLIONS",
          size = "PAGEVIEWS",label = "PLAYER",label.select =label1)

ggscatter(cluster2, x = "POINTS", y = "WINS_RPM", color = "SALARY_MILLIONS",
          size = "PAGEVIEWS",label = "PLAYER" ,label.select =label2)

ggscatter(cluster3, x = "POINTS", y = "WINS_RPM", color = "SALARY_MILLIONS",
          size = "PAGEVIEWS",label = "PLAYER",label.select =label3)

ggscatter(cluster4, x = "POINTS", y = "WINS_RPM", color = "SALARY_MILLIONS",
          size = "PAGEVIEWS",label = "PLAYER" ,label.select =label4)



############## On peut vooir que les joueurs du premier groupe sont beaucoup plus regarder sur les sites et gagne plus d'argent que les autres ####
############# ON peut ###
salary_twitt$TWITTER_FOLLOWER_COUNT_MILLIONS

label=


label=salary_twitt[1:15,]
label=label$PLAYER_NAME


ggscatter(salary_twitt, x = "SALARY_MILLIONS", y = "PTS", 
          add = "loess", conf.int = TRUE, size="TWITTER_FOLLOWER_COUNT_MILLIONS",
          color="W_PCT",label="PLAYER_NAME",label.select =label)

ggscatter(salary_twitt, x = "SALARY_MILLIONS", y = "AST_PCT", 
          add = "loess", conf.int = TRUE, size="TWITTER_FOLLOWER_COUNT_MILLIONS",
          color="W_PCT",label="PLAYER_NAME",label.select =label)

ggscatter(salary_twitt, x = "SALARY_MILLIONS", y = "REB_PCT", 
          add = "loess", conf.int = TRUE, size="TWITTER_FOLLOWER_COUNT_MILLIONS",
          color="W_PCT",label="PLAYER_NAME",label.select =label)



####################### ON peut voir que c'est les points qui font le plus joué le salaire #####

df=salary_twitt_wiki[ , c("WINS_RPM","POINTS","W","TWITTER_FAVORITE_COUNT","PAGEVIEWS")]
test=salary_twitt_wiki[5:43 ]
test$TEAM <- NULL
mcor <- cor(test)
mcor
library(corrplot)
corrplot(mcor, type="upper", order="hclust", tl.col="black", tl.srt=45)



ggscatter(salary_twitt_wiki, x = "WINS_RPM", y = "PAGEVIEWS", 
          add = "loess", conf.int = TRUE, color="SALARY_MILLIONS",size="TWITTER_FAVORITE_COUNT",
          label="PLAYER",label.select =label)

ggscatter(salary_twitt_wiki, x = "POINTS", y = "PAGEVIEWS", 
          add = "loess", conf.int = TRUE, color="SALARY_MILLIONS",size="TWITTER_FAVORITE_COUNT",
          label="PLAYER",label.select =label)



ggscatter(salary_twitt_wiki, x = "POINTS", y = "AGE", 
          add = "loess", conf.int = TRUE, color="SALARY_MILLIONS",size="TWITTER_FAVORITE_COUNT",
          label="PLAYER",label.select =label)



df=salary_twitt_wiki[, c("POINTS","AGE","MP")]
mcor <- cor(df)
mcor
##################### AUCUNE CORRELATION ENTRE L'AGE ET LE NOMBRE DE POINTS, et le nombre matche joué ####



city <- city[c(54074,
               3093,
               8174,
               3225,
               59894,
               46218,
               71677),]


names<-city$NAME

city<- city[13:22]


library(data.table)

t_city<-transpose(city)

colnames(t_city) <- names

date<-data.frame(as.Date(c("2010-01-01", "2011-01-01", "2012-01-01", "2013-01-01",
                           "2014-01-01","2015-01-01","2016-01-01","2017-01-01","2018-01-01","2019-01-01")))
names(date)<-c("date")


t_city<-cbind(t_city,date)



ggscatter(t_city, x = "date", y = "Cleveland city")
ggscatter(t_city, x = "date", y = "Los Angeles city")
ggscatter(t_city, x = "date", y = "Chicago city")
ggscatter(t_city, x = "date", y = "San Francisco city")
ggscatter(t_city, x = "date", y = "Oklahoma City city")
ggscatter(t_city, x = "date", y = "New York")
ggscatter(t_city, x = "date", y = "Houston city")




##### AUCUNE CORRELATION ENTRE LA POPULATION ET L'ARRIVE D'UN GRAND JOUEUR ####

library(tmap)

all_season<-read.csv("all_seasons.csv")


other_countries= all_season[ all_season$country!='USA',]

pts=aggregate(x = other_countries$pts,              
          by = list(other_countries$country),             
          FUN = mean)  



colnames(pts) <- c("region","value")


life.exp.map <- left_join(pts, world_map, by = "region")

ggplot(world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill="lightgray", colour = "white")+
  geom_polygon(data= life.exp.map,aes(fill = value ), color = "white")+
  scale_fill_viridis_c(option = "C")




other_countries= other_countries[ other_countries$draft_year!='Undrafted',]


nbr_draft=other_countries %>%                         
  group_by(country) %>% 
  summarise(n = n())


colnames(nbr_draft) <- c("region","value")
life.exp.map <- left_join(nbr_draft, world_map, by = "region")

ggplot(world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill="lightgray", colour = "white")+
  geom_polygon(data= life.exp.map,aes(fill = value ), color = "white")+
  scale_fill_viridis_c(option = "C")



#### On peut voir que la france est le pays drafté hors US mais fait partie du milieu de ####
#### tableau concernant la moyenne de point ####

draft= all_season[ all_season$draft_round!='Undrafted',]
draft= draft[ draft$college!='None',]


best_college=aggregate(x = draft$pts,              
              by = list(draft$college),             
              FUN = mean)  


nbr_draft_college=draft %>%                         
  group_by(college) %>% 
  summarise(n = n())



#### DAVIDSON 11 ####

draft$draft_number<-as.numeric(as.character(draft$draft_number)) 



draft<-draft[order(draft$draft_number),]



ggscatter(draft, x = "draft_number", y = "pts")

###### on peut voir qu'il ya une corrélation entre la place de draft et le nbr de pointmarqués####




stats<- all_season[,13:15]
PCA_data <- princomp(stats ,cor="False")
biplot (PCA_data)

pca1=PCA_data$loadings[,1]
pca1




