
path <- "future_data"
setwd(path)


## load data
products <- fread("clusterData/cproducts.csv")
tender <- fread("clusterData/ctender.csv")

## check shape of files
dim(products)
dim(tender)

## this data file contains product level information of transactions made by customers
head(products)

## this data file contains payment mode information used by customers in their tranasctions
head(tender)

# Let's build a simple model using products file only. We have to create clusters of customers at store level. We'll use k-means algorithm. We'll set value of k as 3 (you can tune and find optimal value)
# Clustering process is as follows (work-wise):
# Convert the data into numeric / float.
# Since clustering using distance measure, don't forget to scale the data.
# Create a matrix /array which will be feeded to the algorithm.
# Run function.
# We'll ensure that sequence of data remains unaffected. Let's see how this is done.

## fill missing values
products[is.na(promotion_description), promotion_description := "no_promo"]
products[is.na(Gender), Gender := 'no_gender']
products[is.na(State), State := 'no_state']
products[is.na(PinCode), PinCode := -1]
products[is.na(DOB), DOB := "1"]

# convert data into numeric
for(c in colnames(products))
{
  if(!(c %in% c('store_description','customerID','transactionDate','store_code')))
  {
    products[[c]] <- as.numeric(as.factor(products[[c]]))
    
  }
}


## scaling, creating matrix and running k-means

stores <- unique(products$store_code)

cluster_labels <- list()
cluster_store <- list()
cluster_data <- list()
cluster_customers <- list()

library(quantable)
for(x in seq(stores))
{
  cld <- products[store_code == stores[x]]
  cluster_customers[[x]] <- cld[['customerID']]
  
  cld[,c('store_code','customerID','transactionDate', 'store_description') := NULL]
  
  cld2 <- robustscale(cld)
  cld2 <- as.matrix(cld2$data)
  cld2[!is.finite(cld2)] <- 0
  
  algo <- kmeans(cld2, centers = 3)
  
  cluster_data[[x]] <- cld2
  cluster_labels[[x]] <- algo$cluster
  cluster_store[[x]] <- rep(stores[x], nrow(cld))
  rm(cld, cld2)

}

## create submission files

cluster_data <- as.matrix(do.call('rbind', cluster_data))

cluster_customers <- unlist(cluster_customers)
cluster_store <- unlist(cluster_store)
cluster_labels <- unlist(cluster_labels)

sub1 <- data.table(customerID = cluster_customers, store_code = cluster_store, cluster = cluster_labels)
fwrite(sub1, "clusterData/sub01.csv")

write.table(cluster_data, "clusterData/sub02.txt", row.names = F, col.names = F)

