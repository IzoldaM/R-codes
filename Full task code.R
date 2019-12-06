#install.packages("RCurl")
#install.packages("zip")
#install.packages("ggplot2")
#install.packages("ggpubr")
#install.packages("dplyr")
#install.packages("forcats")
#install.packages('hrbrthemes')
#install.packages("gridExtra")
#install.packages("plyr")
#install.packages("igraph")
#install.packages("klaR")


library(ggplot2)
library(ggpubr)
library(dplyr)
library(zip)
library(RCurl)
library(forcats)
library(hrbrthemes)
library(gridExtra)
library(plyr)
library(igraph)
library(klaR)

theme_set(theme_pubr())

########################################
########## DOWNLOADING DATA#############
########################################

link <- "https://offshoreleaks-data.icij.org/offshoreleaks/csv/csv_panama_papers.2018-02-14.zip#_ga=2.159440504.439045129.1575110567-884049020.1575110567"
#create temp file
dir.create(temp <- tempfile())
#download zip file from given URL
download.file(link, paste0(temp, '/PZUURT.zip'), mode = 'wb', exdir = temp)
#unzip file
unzip(paste0(temp, '/PZUURT.zip'), exdir = temp)
#read tables from unziped file
root_folder <- paste0(temp)
## create list of csv files in unziped folder
filelist <- list.files(path = root_folder, pattern = '*.csv', recursive = TRUE)
##clean names for data frames
filenames <- basename(filelist)
table_name <- sub("nodes.","",sub("[a-z]*_[a-z]*.","",sub(".csv", "", filelist)))
##read data frames
table_path <- paste(root_folder, filelist,sep='\\')
for(i in 1:length(table_name)){
  filepath <- table_path[i]
  assign(table_name[i], read.table(filepath,header=TRUE,sep=',',fill=T,stringsAsFactors = FALSE))
}

#####################################################
########## STAND ALONE DATA FRAME GRAPHS#############
#####################################################

####################################################
#Entity
####################################################

#creating Other category for country and jurisdiction of entity (leaving only top 20 countries and top 20 jurisdictions)
entity$group_countries<- fct_lump(entity$countries,n=20)
entity$group_jurisdictions <- fct_lump(entity$jurisdiction_description,n=20)

# frequency tables for country and jurisdiction of entity
ent_country_freq <- entity %>%
  group_by(group_countries) %>%
  dplyr::summarise(counts = n())

ent_jurisdiction_freq <- entity %>%
  group_by(group_jurisdictions) %>%
  dplyr::summarise(counts = n())

# histograms of country and jurisdiction of entity
ent_count_hist <- ggplot(ent_country_freq, aes(x = reorder(group_countries,-counts), counts)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3, size=8) + 
  theme_pubclean()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("Entity country") + ylab("Frequency")


ent_jur_hist <- ggplot(ent_jurisdiction_freq, aes(x = reorder(group_jurisdictions,-counts), counts)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3, size=8) + 
  theme_pubclean()+ theme(axis.text.x = element_text(angle = 90, hjust = 1),size=rel(0.5))+
  xlab("Entity jurisdiction") + ylab("Frequency")

#ploting histograms in one plot
ggarrange(ent_count_hist,ent_jur_hist,
          labels = c("", ""),
          ncol = 2, nrow = 1)

#creating entity country and jurisdiction heatmap
ent_count_juris_freq <- data.frame(table(entity$group_countries,entity$group_jurisdictions))

 ggplot(ent_count_juris_freq, aes(Var1, Var2, fill= Freq)) + 
  geom_tile() +
  scale_fill_continuous(high = "#132B43", low = "#56B1F7") +
  theme_ipsum()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+
   xlab("Entity country") + ylab("Entity jurisdiction")
 
#creating entity incorporation timeseries graph and status of entities at the moment
entity$incorporation_date <- as.Date(entity$incorporation_date,format="%d-%b-%Y")
ent_incorp_date_freq <- data.frame(table(entity$incorporation_date))
ent_incorp_date_freq$Var1 <- as.Date(ent_incorp_date_freq$Var1,format="%Y-%m-%d")
ent_time_series <- ggplot(data = ent_incorp_date_freq, aes(x = Var1, y = Freq))+
  geom_line(color = "#00AFBB", size = 1)+
  xlab("Incorporation Date") + ylab("# of Entities")

ent_status_freq <- data.frame(table(entity$status))
ent_status_hist <- ggplot(ent_status_freq, aes(x = reorder(Var1,-Freq), Freq)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = Freq), vjust = -0.3) + 
  theme_pubclean()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("Entity Status") + ylab("Frequency")

ggarrange(ent_time_series,ent_status_hist,
          labels = c("", ""),
          ncol = 1, nrow = 2)


##############################################################
#Intermediary
##############################################################
#creating intermediary freaquency by country and status of intermediary
intermediary$group_countries<- fct_lump(intermediary$countries,n=20)
ggplot(intermediary, aes(group_countries))+geom_bar(aes(fill = status))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("Intermediary Country") + ylab("Frequency")

##############################################################
#Officer
##############################################################
#creating officer frequency by country

officer$group_countries<- fct_lump(officer$countries,n=20)
off_country_freq <- officer %>%
  group_by(group_countries) %>%
  dplyr::summarise(counts = n())

ggplot(off_country_freq, aes(x = reorder(group_countries,-counts), counts)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3) + 
  theme_pubclean()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("Officer Country") + ylab("Frequency")



##############################################################
#Address
##############################################################
#creating address frequency by country
address$group_countries<- fct_lump(address$countries,n=20)
add_country_freq <- address %>%
  group_by(group_countries) %>%
  dplyr::summarise(counts = n())

ggplot(add_country_freq, aes(x = reorder(group_countries,-counts), counts)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3) + 
  theme_pubclean()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("Adress Country") + ylab("Frequency")


###########################################################
#########Combine information to one data set###############
###########################################################

#creating subset of edges by typpe
edges_address <- edges[edges$TYPE=='registered_address',]
edges_officer <- edges[edges$TYPE=='officer_of',]
edges_intermediary <- edges[edges$TYPE=='intermediary_of',]

##checking connections between edges
unique(substring(edges_address$START_ID,1,3))
unique(substring(edges_address$END_ID,1,3))
unique(substring(edges_officer$START_ID,1,3))
unique(substring(edges_officer$END_ID,1,3))
unique(substring(edges_intermediary$START_ID,1,3))
unique(substring(edges_intermediary$END_ID,1,3))
unique(substring(entity$node_id,1,3))
unique(substring(officer$node_id,1,3))
unique(substring(intermediary$node_id,1,3))
unique(substring(address$node_id,1,3))


#merging address and edges about address information

address_information <- merge(x=edges_address,y=address,by.x="END_ID",by.y="node_id",all.x=TRUE, suffixes = c("",".ed_add"))
address_information <- address_information[,names(address_information) %in% c("START_ID","link","address","countries")]

#merging officer and edges about officer information
officer_information <- merge(x=officer,y=address_information,by.x="node_id",by.y="START_ID",all.x=TRUE, suffixes = c("",".add_off"))
officer_information <- merge(x=edges_officer,y=officer_information,by.x="START_ID",by.y="node_id",all.x=TRUE, suffixes = c("",".ed_off"))
officer_information <- officer_information[,names(officer_information) %in% c("END_ID","link.ed_off","name","countries","address")]

#merging intermediary and edges about intermediary information
intermediary_information <- merge(x=intermediary,y=officer_information,by.x="node_id",by.y="END_ID",all.x=TRUE, suffixes = c("",".off_off"))
intermediary_information <- merge(x=edges_intermediary,y=intermediary_information,by.x="START_ID",by.y="node_id",all.x=TRUE, suffixes = c("",".ed_int"))
intermediary_information <- intermediary_information[,names(intermediary_information) %in% c("END_ID","link.ed_int","name","countries","status","name.off_off")]

#creating one data set where entity information is base
entity_full <- entity[,-c(13:17)]

#adding address information
entity_full <- merge(x=entity_full,y=address_information,by.x="node_id",by.y="START_ID",all.x=TRUE, suffixes = c("",".ed_add"))
names(entity_full)[names(entity_full) == 'link'] <- 'address_type'
names(entity_full)[names(entity_full) == 'countries.ed_add'] <- 'address_country'

#adding officer information
entity_full <- merge(x=entity_full,y=officer_information,by.x="node_id",by.y="END_ID",all.x=TRUE, suffixes = c("",".ed_off"))
names(entity_full)[names(entity_full) == 'link.ed_off'] <- 'officer_type'
names(entity_full)[names(entity_full) == 'countries.ed_off'] <- 'officer_country'
names(entity_full)[names(entity_full) == 'name.ed_off'] <- 'officer_name'
names(entity_full)[names(entity_full) == 'address.ed_off'] <- 'officer_address'

#adding intermediary information
entity_full <- merge(x=entity_full,y=intermediary_information,by.x="node_id",by.y="END_ID",all.x=TRUE, suffixes = c("",".ed_int"))
names(entity_full)[names(entity_full) == 'link.ed_int'] <- 'intermediary_type'
names(entity_full)[names(entity_full) == 'countries.ed_int'] <- 'intermediary_country'
names(entity_full)[names(entity_full) == 'name.ed_int'] <- 'intermediary_name'
names(entity_full)[names(entity_full) == 'status.ed_int'] <- 'intermediary_status'
names(entity_full)[names(entity_full) == 'name.off_off'] <- 'intermediary_officer'

############################################################################
#####################Cross columns information#############################
###########################################################################

#Creating function that identifies domestic/foreign country

group_f <- function(a,b){
  if (is.na(b)) { 
      "Missing"
  } else if (b==a) {
      "Domestic country"
  } else {
      "Foreign country"
  }
}

group_country_f<-Vectorize(group_f)

#filtering data set to have only rows where information about officer exists
cleaned_officer_country <- entity_full[is.null(entity_full$officer_country)==FALSE & is.na(entity_full$officer_country)==FALSE&entity_full$officer_country!="",]

#identifying if entity country is domestic/foreign based on officer country
cleaned_officer_country $officer_entity_group <- group_country_f(cleaned_officer_country$officer_country,cleaned_officer_country$countries)

#creating frequency graph about officer having entities in domestinc/foreign countries
cleaned_officer_country $group_officer_country<- fct_lump(cleaned_officer_country $officer_country,n=20)
ggplot(cleaned_officer_country , aes(group_officer_country))+geom_bar(aes(fill = officer_entity_group))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("Officer country") + ylab("Frequency")

#creating officer country and entity country heatmap
cleaned_officer_country $group_entity_country<- fct_lump(cleaned_officer_country $countries,n=20)
ent_count_off_count_freq <- data.frame(table(cleaned_officer_country$group_entity_country,cleaned_officer_country$group_officer_country))

ggplot(ent_count_off_count_freq, aes(Var1, Var2, fill= Freq)) + 
  geom_tile() +
  scale_fill_continuous(high = "#132B43", low = "#56B1F7") +
  theme_ipsum()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("Entity country") + ylab("Officer country")

#cleaning data to have only rows where information about intermediary exists
cleaned_intermediary_country <- entity_full[is.null(entity_full$intermediary_country)==FALSE & is.na(entity_full$intermediary_country)==FALSE&entity_full$intermediary_country!="",]

#identifying if intermediary works within domestic/foreign country
cleaned_intermediary_country$intermediary_entity_group <- group_country_f(cleaned_intermediary_country$intermediary_country,cleaned_intermediary_country$countries)

#creating frequency graph about intermediary working with entities in domestinc/foreign countries
cleaned_intermediary_country $group_intermediary_country<- fct_lump(cleaned_intermediary_country $intermediary_country,n=20)
ggplot(cleaned_intermediary_country , aes(group_intermediary_country))+geom_bar(aes(fill = intermediary_entity_group))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("Intermediary country") + ylab("Frequency")

# creating intermediary country and entity country heatmap
cleaned_intermediary_country $group_entity_country<- fct_lump(cleaned_intermediary_country $countries,n=20)
ent_count_int_count_freq <- data.frame(table(cleaned_intermediary_country$group_entity_country,cleaned_intermediary_country$group_intermediary_country))

ggplot(ent_count_int_count_freq, aes(Var1, Var2, fill= Freq)) + 
  geom_tile() +
  scale_fill_continuous(high = "#132B43", low = "#56B1F7") +
  theme_ipsum()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("Entity country") + ylab("Intermediary country")

#create list of entity countries and number of different countries of intermediaries working with
ddply(cleaned_intermediary_country,~group_entity_country,summarise,number_of_distinct_intermediary_country=length(unique(group_intermediary_country)))

######################################################
######### SWEDBANK HOME MARKET INVESTIGATION##########
######################################################

######################################################
#Entity
######################################################

#filtering home markets
entityH <- entity[entity$countries %in% c("Lithuania","Latvia","Estonia","Sweden"),]


#creating Other category for country and jurisdiction of entityH (leaving only top 20 countries and top 20 jurisdictions)
entityH$group_countries<- fct_lump(entityH$countries,n=20)
entityH$group_jurisdictions <- fct_lump(entityH$jurisdiction_description,n=20)

# frequency tables for country and jurisdiction of entityH
ent_country_freq_H <- entityH %>%
  group_by(group_countries) %>%
  dplyr::summarise(counts = n())

ent_jurisdiction_freq_H <- entityH %>%
  group_by(group_jurisdictions) %>%
  dplyr::summarise(counts = n())

# histograms of country and jurisdiction of entityH
ent_count_hist_H <- ggplot(ent_country_freq_H, aes(x = reorder(group_countries,-counts), counts)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3) + 
  theme_pubclean()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("Entity Country") + ylab("Frequency")


ent_jur_hist_H <- ggplot(ent_jurisdiction_freq_H, aes(x = reorder(group_jurisdictions,-counts), counts)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3) + 
  theme_pubclean()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("Entity Jurisdiction") + ylab("Frequency")

#ploting histograms in one plot
ggarrange(ent_count_hist_H,ent_jur_hist_H,
          labels = c("", ""),
          ncol = 2, nrow = 1)

#creating entity incorporation timeseries graph and status of entities at the moment
entityH$incorporation_date <- as.Date(entityH$incorporation_date,format="%d-%b-%Y")
ent_incorp_date_freq_H <- data.frame(table(entityH$incorporation_date))
ent_incorp_date_freq_H$Var1 <- as.Date(ent_incorp_date_freq_H$Var1,format="%Y-%m-%d")
ent_time_series_H <- ggplot(data = ent_incorp_date_freq_H, aes(x = Var1, y = Freq))+
  geom_line(color = "#00AFBB", size = 1)+
  xlab("Incorporation Date") + ylab("# of Entities")

ent_status_freq_H <- data.frame(table(entityH$status))
ent_status_hist_H <- ggplot(ent_status_freq_H, aes(x = reorder(Var1,-Freq), Freq)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = Freq), vjust = -0.3) + 
  theme_pubclean()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("Entity Status") + ylab("Frequency")

ggarrange(ent_time_series_H,ent_status_hist_H,
          labels = c("", ""),
          ncol = 1, nrow = 2)

##############################################################
#Intermediary
##############################################################
#filtering home markets
intermediaryH <- intermediary[intermediary$countries %in% c("Lithuania","Latvia","Estonia","Sweden"),]


#creating intermediary freaquency by country and status of intermediary
intermediaryH$group_countries<- fct_lump(intermediaryH$countries,n=20)
ggplot(intermediaryH, aes(group_countries))+geom_bar(aes(fill = status))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("Intermediary Country") + ylab("Frequency")

##############################################################
#Officer
##############################################################
#filtering home markets
officerH <- officer[officer$countries %in% c("Lithuania","Latvia","Estonia","Sweden"),]

#creating officer frequency by country
officerH$group_countries<- fct_lump(officerH$countries,n=20)
off_country_freq_H <- officerH %>%
  group_by(group_countries) %>%
  dplyr::summarise(counts = n())

ggplot(off_country_freq_H, aes(x = reorder(group_countries,-counts), counts)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3) + 
  theme_pubclean()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("Officer Country") + ylab("Frequency")

##############################################################
#Address
##############################################################
#filtering home markets
addressH <- address[address$countries %in% c("Lithuania","Latvia","Estonia","Sweden"),]

#creating address frequency by country
addressH$group_countries<- fct_lump(addressH$countries,n=20)
add_country_freq_H <- addressH %>%
  group_by(group_countries) %>%
  dplyr::summarise(counts = n())

ggplot(add_country_freq_H, aes(x = reorder(group_countries,-counts), counts)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3) + 
  theme_pubclean()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("Adress Country") + ylab("Frequency")



########################################################
############# GRAPH NODES CLUSTERIZATION################
########################################################

-------------#graph as network clusterization#-----------------------

#creating graph from data sets 
#creating edges list
source <- c(edges_address$END_ID, edges_officer$START_ID, edges_intermediary$START_ID)
target <- c(edges_address$START_ID, edges_officer$END_ID, edges_intermediary$END_ID)
linking <- c(edges_address$link, edges_officer$link, edges_intermediary$link)
graph_edges_list <- data.frame(source,target,linking)
#creating nodes list
graph_nodes <- c(entity$node_id,officer$node_id,intermediary$node_id,address$node_id)
#removing nodes from nodes list which don't have any vertices
remove_nodes <- which(!graph_nodes %in% graph_edges_list$target & !graph_nodes %in% graph_edges_list$source)  
graph_nodes_cleaned <- graph_nodes[!graph_nodes %in% remove_nodes]

#creating graph 
g <- graph_from_data_frame(d=graph_edges_list, vertices=graph_nodes_cleaned, directed=FALSE)

#calculating main graph characteristics
#average distance
mean_distance(g, directed=F)
#largest subgraphs
largest_cliques(g)
#betweeness
betweenness(net, directed=T, weights=NA)

#############################################################
#Clusters detection based on edge betweenness (Newman-Girvan)
ceb <- cluster_edge_betweenness(g)
#number of clusters
length(ceb)
#each node and it's cluster 
membership(ceb)
#############################################################

-------------#entity nodes clusterization#-----------------------

#removing dublicated information columns
entity_node_data <- entity_full[!names(entity_full) %in% c("jurisdiction_description","country_codes",
"incorporation_date","inactivation_date","struck_off_date","closed_date","group_countries","group_jurisdictions" )]

#reverting NA values to ""

entity_node_data[is.na(entity_node_data)] <- " "

#making clusterization
cluster.results <-kmodes(entity_node_data[,3:length(entity_node_data)], 3, iter.max = 10, weighted = FALSE )
clusterized_entity_data <- cbind(entity_node_data,cluster.results$cluster)
names(clusterized_entity_data)[18] <- "Cluster"

#merging data with clusters
clusterized_entity_data_freq <- data.frame(table(clusterized_entity_data$Cluster))

#ploting clusters histogram
ggplot(clusterized_entity_data_freq, aes(x = reorder(Var1,-Freq), Freq)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = Freq), vjust = -0.3) + 
  theme_pubclean()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    xlab("Cluster") + ylab("Frequency")


#creating country groupings
clusterized_entity_data$group_countries<- fct_lump(clusterized_entity_data$countries,n=20)
clusterized_entity_data$group_country_officer<- fct_lump(clusterized_entity_data$officer_country,n=20)
clusterized_entity_data$group_country_intermediary<- fct_lump(clusterized_entity_data$intermediary_country,n=20)

#ploting entity histograms based on country (entity/intermediary/officer) and clusters
ggplot(clusterized_entity_data, aes(group_countries))+geom_bar(aes(fill = as.factor(Cluster)))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("Entity Country") + ylab("Frequency")

ggplot(clusterized_entity_data, aes(group_country_officer))+geom_bar(aes(fill = as.factor(Cluster)))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("Entity Officer Country") + ylab("Frequency")

ggplot(clusterized_entity_data, aes(group_country_intermediary))+geom_bar(aes(fill = as.factor(Cluster)))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("Entity Intermediary Country") + ylab("Frequency")









