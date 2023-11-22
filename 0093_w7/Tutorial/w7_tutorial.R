library(igraph)
library(dplyr)
library(tidyr)
library(DescTools) #contains the world country cooridinates data
library(ggplot2)
library(maps)
library(rstudioapi)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
raw_data_2018 <- read.csv("https://www.dropbox.com/s/0wz4thqvuty2kvg/TradeData_2018.csv?raw=1")
raw_data_2018_extracted <- raw_data_2018[, c(7:10,12:14,32,44)]
head(raw_data_2018_extracted)

country_1<-raw_data_2018_extracted[,1:3]
country_2<-raw_data_2018_extracted[,5:7]
names(country_1)<-c("code","iso","country") 
#note: rbind can only bind the datasets with the same set of column names
names(country_2)<-names(country_1)
country_list<-unique(rbind(country_1,country_2))  
dim(country_list) #in total we have 190 rows countries, but when we view the data, we may find 'world' is in the data so we need to get rid of it.

country<- d.countries[ , c("a3", "latitude", "longitude", "region")]  
#d.countries is a world countries data from DescTools pacakge 
country_list<- left_join(country_list,  country, by = c("iso" = "a3") ) 

#fix missing countries
country_list[which(country_list$code==535), 4:5] = c(12.183333, -68.233333)
country_list[which(country_list$code==728), 4:5] = c(6.8770, 31.3070)
country_list[which(country_list$code==531), 4:5] = c(12.166667, -68.966667)
country_list[which(country_list$code==581), 4:5] = c(19.2833, 166.6470)
country_list <- na.omit(country_list)
dim(country_list)

#change column order 
raw_data_2018_simplified <-raw_data_2018_extracted[ ,c(2,6,4,8,9)] 
#change the order of columns in raw_data_2018_extracted. As I want to have the iso code of from_country and to_country become the first two columns.
raw_data_2018_simplified2<-raw_data_2018_simplified[
  which(raw_data_2018_simplified$ReporterISO %in% country_list$iso &
          raw_data_2018_simplified$PartnerISO %in% country_list$iso) ,] 
#remove some transaction records as long as one of the country name is not in the country_list
#check the removed records
nr<-nrow(raw_data_2018_extracted)-nrow(raw_data_2018_simplified2)
sprintf('We have removed: %d', nr)

iOTN_edgelist<-raw_data_2018_simplified2 %>%
  rename(from = ReporterISO, to = PartnerISO) %>%
  mutate(newfrom = if_else(FlowCode == "M", to, from),
         newto = if_else(FlowCode == "M", from, to)) %>%
  select(-from, -to) %>%
  rename(from = newfrom, to = newto)

iOTN_edgelist <- iOTN_edgelist[ , c(4,5,2,3)]

#since we have changed sequence of from and to, so no need to have this column

iOTN_edgelist<- iOTN_edgelist[-which(iOTN_edgelist$from == iOTN_edgelist$to), ] 
#lastly, we remove the self-imported edges 
head(iOTN_edgelist)

g.iOTN <-  graph_from_data_frame(iOTN_edgelist, directed = TRUE)  
#As our network has direction, so we set 'directed' argument as 'TRUE'
plot(g.iOTN , edge.arrow.size=.3, vertex.size=4,vertex.label.cex=.4,vertex.color="orange")

#first produce a vertex list from the graph
v.g <- as.data.frame(get.vertex.attribute(g.iOTN)) 
#v.g is a name list, one column data frame. To plot a network with coordinates, we need to add coordinates to v.g 

# create new 'cor.country', which joins the coordinates in an ordering of v.g.
cor.country <- left_join (v.g, country_list, by=c ("name"="iso") ) 
head(cor.country)

g.iOTN <- set.vertex.attribute( g.iOTN, "x", value= cor.country$longitude)
g.iOTN <- set.vertex.attribute( g.iOTN, "y", value= cor.country$latitude) 

map('world',fill=FALSE,col='white',lwd=.7, bg="black")
plot(g.iOTN, vertex.size=200,vertex.label.cex=.05, 
     vertex.frame.color = "transparent", vertex.color= "white",
     edge.alpha = 0.1, edge.width = 0.1, edge.color="yellow",
     edge.arrow.size= 0.01,
     add=TRUE , rescale = FALSE)


iOTN_edgelist_cor <- left_join(iOTN_edgelist, country_list[, c('iso', "latitude",'longitude','region')], by=c("from"='iso')    )
iOTN_edgelist_cor <- left_join(iOTN_edgelist_cor, country_list[, c('iso', "latitude",'longitude','region')], by=c("to"='iso')    )

colnames(iOTN_edgelist_cor)[5:10] <-c( "from_latitude", "from_longitude","from_region", "to_latitude", "to_longitude", "to_region")
head(iOTN_edgelist_cor, 3)

#gglot has a world country map
# set basemap
basemap <- geom_polygon(aes(x = long, y = lat, group = group),
                        data = map_data('world'),
                        fill = "black", color = "#F1FBEE",
                        size = 0.1)

# set map theme (optional)
maptheme <- theme(panel.grid = element_blank(),legend.position = "none")+
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.title = element_blank()) +
  theme(legend.position = "bottom") +
  theme(panel.grid = element_blank()) +
  theme(panel.background = element_rect(fill = "#596673")) +
  theme(plot.margin = unit(c(0, 0, 0.5, 0), 'cm'))

# plot
ggplot(cor.country) + basemap +
  geom_curve(aes(x =from_longitude , y = from_latitude, xend = to_longitude, yend = to_latitude),    
             col="grey",
             data = iOTN_edgelist_cor , curvature = 0.17, #if curvature =0 then we will have a straight line
             alpha = 0.2) +
  geom_point(aes(x = longitude, y = latitude, size = 0.4),          
             shape = 21, fill = '#dd1c77',
             color = 'white', stroke = 0.4) + 
  scale_size_continuous(guide = FALSE, range = c(0.75, 3)) +
  coord_fixed(xlim = c(-150, 180), ylim = c(-55, 80))+
  maptheme

edgelist_gb_in = iOTN_edgelist_cor[which(iOTN_edgelist_cor$from =="GBR" ) ,] 
edgelist_gb_out = iOTN_edgelist_cor[which(iOTN_edgelist_cor$to =="GBR"  ) ,]

cor.GBR = filter(cor.country, cor.country$name=="GBR")

width_in= log(edgelist_gb_in$PrimaryValue) /20
width_out = log(edgelist_gb_out$PrimaryValue) /20

ggplot(cor.country) + basemap +
  geom_curve(aes(x =from_longitude , y = from_latitude, xend = to_longitude, yend = to_latitude),    # draw edges as arcs
             col="#E48CAA",size=1.4*width_in,
             data = edgelist_gb_in , curvature = 0.33,
             alpha = 0.4) +
  geom_curve(aes(x =from_longitude , y = from_latitude, xend = to_longitude, yend = to_latitude),    # draw edges as arcs
             col="#7BAF43",size=1.4*width_out ,
             data = edgelist_gb_out , curvature = 0.33,
             alpha = 0.4)+
  geom_point(aes(x = longitude, y = latitude),  size=0.02,        
             shape = 21, fill = '#dd1c77',
             color = 'white', stroke = 0.4) + 
  geom_point(aes(x = longitude, y = latitude),      
             data= cor.GBR, shape = 21, fill = '#dd1c77', size = 2,
             color = 'black', stroke = 0.2)+
  scale_size_continuous(guide = FALSE, range = c(0.75, 3)) +
  coord_fixed(xlim = c(-150, 180), ylim = c(-55, 80))+
  maptheme

deg_in<- degree (g.iOTN, mode="in") 
max(deg_in)

deg_in_sort = sort(deg_in, decreasing = TRUE)
head(deg_in_sort,3)

hist(deg_in, xlab="Number of importers")

deg_out = degree(g.iOTN, mode="out") 
cor.country$deg_in = deg_in
cor.country$deg_out = deg_out

mean(deg_in) #average degree
mean(deg_out) 

cor.country$from_PrimaryValue =0
cor.country$to_PrimaryValue=0

for (i in 1: nrow(cor.country)){
  iso = cor.country[i,"name"]
  iso_edgelist= filter(iOTN_edgelist, iOTN_edgelist$from == iso)
  sum_from = sum(iso_edgelist$PrimaryValue)
  iso_edgelist2= filter(iOTN_edgelist, iOTN_edgelist$to == iso)
  sum_to = sum(iso_edgelist2$PrimaryValue)
  cor.country[i, "from_PrimaryValue"] = sum_from
  cor.country[i, "to_PrimaryValue"] = sum_to
}

# better way to do it
cor.country = cor.country%>%
  select(-from_PrimaryValue)

import<-iOTN_edgelist%>%
  group_by(from)%>%
  summarise(from_PrimaryValue = sum(PrimaryValue, na.rm = TRUE), .groups = "drop")

cor.country <- cor.country%>%
  left_join(import, join_by("name" == "from"))%>%
  replace_na(list(from_PrimaryValue = 0))


clo_in=closeness(g.iOTN, mode="in")
clo_out=closeness(g.iOTN, mode="out")
cor.country$clo_in= clo_in
cor.country$clo_out= clo_out

bet<-betweenness(g.iOTN, directed= TRUE)
cor.country$bet= bet
#sort(bet, decreasing = TRUE) #We can reorder it.
#View(as.data.frame(bet))

pg = page.rank(g.iOTN, directed =TRUE)
cor.country$pg=pg$vector

palette=hsv(h=1-((log(1+deg_out)/max( log(1+deg_out))*2/3)+1/3),s = 1,v=1)
ggplot(cor.country) + basemap +
  geom_curve(aes(x =from_longitude , y = from_latitude, xend = to_longitude, yend = to_latitude),    
             col="white",
             data = iOTN_edgelist_cor , curvature = 0.17, #if curvature =0 then we will have a straight line
             alpha = 0.1) +
  geom_point(aes(x = longitude, y = latitude ),  size = 5* (0.2+deg_out/max(deg_out)) ,       
             shape = 21, fill = palette,
             color = 'white', stroke = 0.01) + 
  scale_size_continuous(guide = FALSE, range = c(0.75, 3)) +
  coord_fixed(xlim = c(-150, 180), ylim = c(-55, 80))+
  maptheme

# Eff average efficiency
eff<-1/(shortest.paths(g.iOTN))
eff[is.infinite(eff)]<-0
E=mean(eff,na.rm=TRUE)
E=round(E,3)
E  

E_collopase=0.5*E  
v.g$deg_in = deg_in 
v.g_sort= v.g[order(v.g$deg_in, decreasing = TRUE),] #ranking vertex from the hihgest centrality value to the lowest.

#let's model how the eff will change with the removal. We use delete.vertices to remove nodes and their connected edges from the graph (g.iOTN) 
eff.deg=c()
for (i in c(1:140)) {
  eff.i <- 1/(shortest.paths(delete.vertices(g.iOTN,v.g_sort[1:i,"name"])))
  eff.i[!is.finite(eff.i)]<-0
  eff<-mean(eff.i,na.rm=TRUE)
  eff.deg[i]=(eff/E)
}  

#also calculate the random scenario. You can use other way to generate a random country name list and remove. In academic research, for random simulation, we normally generate multiple country name lists and model multiple times to represent the random scenario. Here we just just the original country name list to give an example.
eff.ran=c()
for (i in c(1:140)) {
  eff.i <- 1/(shortest.paths(delete.vertices(g.iOTN,v.g[1:i,"name"])))
  eff.i[!is.finite(eff.i)]<-0
  eff<-mean(eff.i,na.rm=TRUE)
  eff.ran[i]=(eff/E)
}  

eff.deg<-as.data.frame(eff.deg)
eff.deg$eff.ran = eff.ran
eff.deg$x= (1:nrow(eff.deg))/140

# For more about ggolot visulisation arguemtns, see : https://ggplot2-book.org/index.html
ggplot(eff.deg,aes(x=x) )+ 
  geom_line(aes( y = eff.ran, colour="eff.deg") , linetype = "dashed" ) +  #note: if we put colour outside the aes (), there will be no legend.
  geom_line(aes(y = eff.deg,colour="eff.ran"))+
  labs(title="Robustness analysis based on degree",color = "Legend")+ 
  xlab("percentage of node removal")+ 
  ylab("change of network efficiency")+ 
  geom_hline(yintercept=0.5,  linetype="dashed", color = "grey")+
  geom_vline(xintercept=eff.deg[which(eff.deg$eff.deg<0.5)[1],"x"],linetype="dashed", color = "grey")+
  geom_vline(xintercept=eff.deg[which(eff.deg$eff.ran<0.5)[1],"x"],linetype="dashed", color = "grey")+
  theme_linedraw()+
  theme(legend.position = c(0.75, 0.8),legend.background = element_rect(fill = "white"))+    
  theme(legend.text=element_text(size=10))

cz.deg=c()
for (i in c(1:140)) {
  c <-  components (delete.vertices(g.iOTN, v.g_sort[1:i,"name"]))
  cz.deg [i]<- max( c$csize)/nrow(v.g)
}

cz.ran=c()
for (i in c(1:140)) {
  c <-  components (delete.vertices(g.iOTN, v.g[1:i,"name"]))
  cz.ran [i]<- max( c$csize)/nrow(v.g)
}
gc<-as.data.frame(cz.deg)
gc$cz.ran = cz.ran
gc$x= (1:nrow(gc))/140

ggplot(gc,aes(x=x) )+ 
  geom_line(aes( y = cz.deg, colour="cz.deg")  ) + 
  geom_line(aes(y = cz.ran, colour="cz.ran"), linetype = "dashed")+
  labs(title="Robustness analysis based on degree",color = "Legend")+ 
  xlab("Percentage of node removal")+ 
  ylab("Change of network giant component size")+ 
  scale_color_manual(labels = c("Degree_based", "Random_based"), values = c("red", "grey")) +
  geom_hline(yintercept=0.5,  linetype="dashed", color = "grey")+
  geom_vline(xintercept=gc[which(gc$cz.deg<0.5)[1],"x"],linetype="dashed", color = "grey")+
  geom_vline(xintercept=gc[which(gc$cz.ran<0.5)[1],"x"],linetype="dashed", color = "grey")+
  theme_linedraw()+
  theme(legend.position = c(0.75, 0.8),legend.background = element_rect(fill = "white"))+    
  theme(legend.text=element_text(size=10))








