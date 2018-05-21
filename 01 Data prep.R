## set working directory
setwd("C:/Users/harika/Desktop/interview prep/dubbizle/Data")
## install the libraries
library(data.table)
library(kmed)
library(tm)
# install.packages('devtools')
# slam_url <- "https://cran.r-project.org/src/contrib/Archive/slam/slam_0.1-37.tar.gz"
# install_url(slam_url)
library(devtools)
library(Rcpp)
library(rmarkdown)
library(text2vec)

list.files()

## Import the data
query_dt <- fread("za_queries_sample.csv")
listings_dt <- fread("za_sample_listings_incl_cat.csv")

## summary of the datasets
head(query_dt)
nrow(query_dt)
length(unique(query_dt$search_term))
head(listings_dt)
length(unique(listings_dt$category_l1_name_en)) #13
length(unique(listings_dt$category_l2_name_en)) # 55
length(unique(listings_dt$category_l3_name_en)) # 271

listings_dt[,.N, by = category_l2_name_en]

## test on one category
listings_test <- listings_dt[category_l2_name_en == "Musical Instruments"]
# combine the title and the descriptions
listings_test[, listing_title_desc := paste(listing_title, listing_description)]

# a combination of both title and desc gives better results
# corpus <- Corpus(VectorSource(listings_test$listing_title_desc))
corpus <- Corpus(VectorSource(listings_test$listing_title_desc))
dtm <- DocumentTermMatrix(corpus, control = list(removePunctuation = TRUE
                                                 , stopwords = TRUE
                                                 ,weighting = weightTfIdf)
                          )

dim(dtm) # [1] 2489 6745
inspect(dtm)
dtm <- removeSparseTerms(dtm, .999)
dim(dtm)

## try k-means on dtmidf
## join on to the listings data
# k <- round(nrow(dtm_idf)/20)
# kmeans5<- kmeans(dtm_idf, 5)

## Using k-means with text and other factors will not weigh the other factors like price and location well enough
dist_dtm <- sqrt(distNumeric(as.matrix(dtm), as.matrix(dtm), method = "se"))
max(dist_dtm)
min(dist_dtm)
hist(dist_dtm)
quantile(dist_dtm, probs = c(0.99), na.rm = TRUE)
dist_dtm <- dist_dtm/max(dist_dtm)

## distance based on price and location
## scale the data for those columns
## outliers have to be removed or capped
dist_price <- sqrt(distNumeric(as.matrix(listings_test[,.(listing_price)])
                          ,as.matrix(listings_test[,.(listing_price)]), method = "se"))
max(dist_price)
min(dist_price)
dist_price_cap <- quantile(dist_price, probs = c(0.90), na.rm = TRUE)
dist_price <- dist_price/dist_price_cap
# dist_price_scaled <- scale(dist_price)

## distance based on location
dist_loc <- sqrt(distNumeric(as.matrix(listings_test[,.(listing_latitude, listing_longitude)])
                        ,as.matrix(listings_test[,.(listing_latitude, listing_longitude)]), method = "se"))
max(dist_loc)
min(dist_loc)
hist(dist_loc) # doesnt have outliers
dist_loc <- dist_loc/max(dist_loc)
# dist_loc_cap <- quantile(dist_loc, probs = c(0.9), na.rm = TRUE)

## weighted average of the distances
w1 <- 0.4
w2 <- 0.3
w3 <- 0.3
dist_comb <- w1*dist_dtm + w2*dist_price + w3*dist_loc

## number of clusters
ncluster <- round(nrow(dtm)/50)

# k-mediod clustering
result <- fastkmed(dist_comb, ncluster = ncluster, iterate = 10)

# join to the base table
listings_test[, cluster := NULL]
listings_test <- cbind(listings_test, cluster = result$cluster)
listings_test[,.N,by = cluster]

## evaluation of clusters
# naive approach treats all the obs in a single group
listings_test[, cluster_naive := 1]

# similarity
similarity_overall <- sim2(x = as.matrix(dtm), method = "cosine", norm = "l2")
sum(similarity_overall[upper.tri(similarity_overall,diag = FALSE)])
mean(similarity_overall[upper.tri(similarity_overall,diag = FALSE)])

## compute by cluster
dm = cbind(as.matrix(dtm), cluster = listings_test$cluster)
no_cls <- length(unique(listings_test$cluster))
dt_overall <- NULL
for (i in 1:no_cls){
  cluster <- i
  count <- nrow(as.matrix(subset(dm, dm[,"cluster"] == i)))
  similarity <- sim2(x = as.matrix(subset(dm, dm[,"cluster"] == i)), method = "cosine", norm = "l2")
  simi_total <- sum(similarity[upper.tri(similarity,diag = FALSE)])
  simi_avg <- mean(similarity[upper.tri(similarity,diag = FALSE)])
  dt <- data.table(cluster = cluster, count = count, simi_total = simi_total, simi_avg = simi_avg)
  dt_overall <- rbind(dt_overall,dt)
}
