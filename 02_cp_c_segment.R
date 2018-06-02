# R Version 3.4.3
memory.limit(size=15831)
options(digits = 4, error = NULL, Ncpus=4) #stop
library(pryr); mem_used(); # object_size(testdata); mem_change(rm(testdata)

# coppel.trans <- data.table::fread("sample_transactions_zylotech.txt", sep="|", header=T)
# coppel.trans <- unique(coppel.trans)
# names(coppel.trans)
# coppel.trans <- coppel.trans[,c(1,5,8)]
# str(coppel.trans)
# coppel.trans$transaction_date <- as.Date(coppel.trans$transaction_date, "%m/%d/%Y")
# Hmisc::describe(coppel.trans$transaction_date)
# coppel.trans$recency <- lubridate::as.duration(lubridate::interval(coppel.trans$transaction_date, as.Date("2017-10-13", format="%Y-%m-%d"))) %/% lubridate::as.duration(lubridate::days(1))
# Hmisc::describe(coppel.trans$recency)
# library(dplyr)
# coppel.trans <- ungroup(coppel.trans) %>% group_by(client_id) %>% mutate(frequency=n_distinct(transaction_date)) # how often customer is purchasing
# Hmisc::describe(coppel.trans$frequency)
# coppel.trans <- ungroup(coppel.trans) %>% group_by(client_id) %>% mutate(monetary=sum(amount))
# Hmisc::describe(coppel.trans$monetary)
# ## removal of duplicate customer id's
# # first sort by custid and recency
# coppel.trans <- ungroup(coppel.trans) %>% group_by(client_id) %>% arrange(recency)
# coppel.trans <- ungroup(coppel.trans) %>% filter(!duplicated(.[["client_id"]])) # found 3,425,366 dups # 94054 records
# names(coppel.trans)
# feather::write_feather(coppel.trans[,c(1,4:6)], "coppel.trans.feather")


## loading required data for developing customer segmentation
cp.for.csegment <- feather::read_feather("coppel.trans.feather")
names(cp.for.csegment)


## checking basics
# structures of the file
str(cp.for.csegment)
# check for needed columns such as custid, recency, monetary, and frequency that are needed for customer segmentation
names(cp.for.csegment)

### data preparation step
### preparing scaled matrix for further analysis
set.seed(123)
cp.for.csegment.scaled <- scale(cp.for.csegment[,2:4])

##### finding optimal number of clusters
library(ClusterR) # minimum is the best
system.time(opt <- Optimal_Clusters_KMeans(cp.for.csegment.scaled, max_clusters = 12, plot_clusters = T, criterion = 'distortion_fK', fK_threshold = 0.85, initializer = 'optimal_init', tol_optimal_init = 0.5, seed=12345)) # suggest nine clusters


### som clustering
# generation of 3*3 grid for clustering purposes
library(kohonen)
som.grid <- somgrid(xdim=1, ydim=3, topo="hexagonal", toroidal = T)
som.model.forcpdt <- som(cp.for.csegment.scaled, grid=som.grid, keep.data=T, rlen=100)
# U Matrix
plot(som.model.forcpdt, type = "dist.neighbours", main="SOM neighbour distances" )
sort(table(som.model.forcpdt$unit.classif))
som.output <- as.data.frame(cbind(cp.for.csegment[,1:4], som.model.forcpdt$unit.classif))
names(som.output)[5] <- "clustno"

library(dplyr)
ungroup(som.output) %>% group_by(clustno) %>% summarize(sales=sum(monetary), arec=mean(recency)/7,  afreqm=mean(frequency), spend=mean(monetary), cust_count=n()) %>% mutate(sales_percent=sales/sum(sales)*100, cust_prop=cust_count/sum(cust_count)*100) %>% arrange(arec) # arrange(desc(spend))

sn.segments <- som.output %>% mutate(segments =
                                       case_when(clustno %in% c(3) ~ "High Value",
                                                 clustno %in% c(1) ~ "Promising",
                                                 clustno %in% c(2) ~ "Explorers")) %>% select(-clustno)

# ungroup(sn.segments) %>% group_by(segments) %>% summarize(sales=sum(monetary), arec=mean(recency)/7,  afreqm=mean(frequency), spend=mean(monetary), cust_count=n()) %>% mutate(sales_percent=sales/sum(sales)*100, cust_prop=cust_count/sum(cust_count)*100) %>% arrange(desc(spend))



### preparing scaled matrix for further analysis - recency only
set.seed(123)
names(cp.for.csegment)
cp.for.csegment.scaled.one <- scale(cp.for.csegment[,2])
Hmisc::describe(cp.for.csegment.scaled.one)
##### finding optimal number of clusters
library(ClusterR) # minimum is the best
system.time(opt <- Optimal_Clusters_GMM(cp.for.csegment.scaled.one, max_clusters = 12, dist_mode= "eucl_dist", plot_data = T, criterion = 'BIC', seed=12345)) # suggest nine clusters

### preparing scaled matrix for further analysis - f & M only
set.seed(123)
names(cp.for.csegment)
cp.for.csegment.scaled.two <- scale(cp.for.csegment[,3:4])
Hmisc::describe(cp.for.csegment.scaled.two)
##### finding optimal number of clusters
library(ClusterR) # minimum is the best
system.time(opt <- Optimal_Clusters_KMeans(cp.for.csegment.scaled.two, max_clusters = 12, plot_clusters = T, criterion = 'distortion_fK', fK_threshold = 0.85, initializer = 'optimal_init', tol_optimal_init = 0.5, seed=12345)) # suggest nine clusters
### som clustering
# generation of 3*3 grid for clustering purposes
library(kohonen)
som.grid <- somgrid(xdim=1, ydim=2, topo="hexagonal", toroidal = T)
som.model.forcpdt <- som(cp.for.csegment.scaled.two, grid=som.grid, keep.data=T, rlen=100)
# U Matrix
plot(som.model.forcpdt, type = "dist.neighbours", main="SOM neighbour distances" )
sort(table(som.model.forcpdt$unit.classif))
som.output <- as.data.frame(cbind(cp.for.csegment[,c(1,3:4)], som.model.forcpdt$unit.classif))
names(som.output)[4] <- "clustno"
library(dplyr)
ungroup(som.output) %>% group_by(clustno) %>% summarize(sales=sum(monetary), afreqm=mean(frequency), spend=mean(monetary), cust_count=n()) %>% mutate(sales_percent=sales/sum(sales)*100, cust_prop=cust_count/sum(cust_count)*100) %>% arrange(desc(spend)) # arrange(desc(spend))



