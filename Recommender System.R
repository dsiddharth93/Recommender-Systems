library(recommenderlab)
library(ggplot2)
library(data.table)
library(reshape2)

## Dataset ##

summary(movies)
head(movies)

summary(ratings)
head(ratings)

##Data Pre-Processing##
##Extract A List Of Genres##

genres <- as.data.frame(movies$genres, stringsAsFactors=FALSE)

genres2 <- as.data.frame(tstrsplit(genres[,1], '[|]', 
                                   type.convert=TRUE), 
                         stringsAsFactors=FALSE)
colnames(genres2) <- c(1:10)

genre_list <- c("Action", "Adventure", "Animation", "Children", 
                "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                "Film-Noir", "Horror", "Musical", "Mystery","Romance",
                "Sci-Fi", "Thriller", "War", "Western") # we have 18 genres in total

genre_matrix <- matrix(0,9126,18) #empty matrix, 9126=no of movies+1, 18=no of genres
genre_matrix[1,] <- genre_list #set first row to genre list
colnames(genre_matrix) <- genre_list #set column names to genre list

## Iterate through matrix ##
for (i in 1:nrow(genres2)) {
  for (c in 1:ncol(genres2)) {
    genmat_col = which(genre_matrix[1,] == genres2[i,c])
    genre_matrix[i+1,genmat_col] <- 1
  }
}

## Convert into dataframe ##
genre_matrix2 <- as.data.frame(genre_matrix[-1,], stringsAsFactors=FALSE) #remove first row, which was the genre list
for (c in 1:ncol(genre_matrix2)) {
  genre_matrix2[,c] <- as.integer(genre_matrix2[,c])  #convert from characters to integers
} 

head(genre_matrix2)

## Create a matrix to search a movie by genre ##

search_matrix <- cbind(movies[,1:2], genre_matrix2)
head(search_matrix)

## Convert ratings matrix in a proper format ##

## Create ratings matrix. Rows = userId, Columns = movieId 
ratingmat <- dcast(ratings, userId~movieId, value.var = "rating", na.rm=FALSE)
ratingmat <- as.matrix(ratingmat[,-1])  #remove userIds

## Convert rating matrix into a recommenderlab sparse matrix ##
ratingmat <- as(ratingmat, "realRatingMatrix")
ratingmat

## Exploring parameters of Recommendation models ##

recommender_models <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
names(recommender_models)
lapply(recommender_models, "[[", "description")

recommender_models$IBCF_realRatingMatrix$parameters
recommender_models$UBCF_realRatingMatrix$parameters

## Exploring Similarity Data ##

similarity_users <- similarity(ratingmat[1:4, ], 
                               method = "cosine", 
                               which = "users")
as.matrix(similarity_users)
image(as.matrix(similarity_users), main = "User similarity")

similarity_items <- similarity(ratingmat[, 1:4], method =
                                 "cosine", which = "items")
as.matrix(similarity_items)
image(as.matrix(similarity_items), main = "Movies similarity")

## Further Data Exploration ##

vector_ratings <- as.vector(ratingmat@data)
unique(vector_ratings) # what are unique values of ratings

table_ratings <- table(vector_ratings) # what is the count of each rating value
table_ratings

## Distribution of the ratings ##

vector_ratings <- vector_ratings[vector_ratings != 0] # rating == 0 are NA values
vector_ratings <- factor(vector_ratings)

qplot(vector_ratings) + 
  ggtitle("Distribution of the ratings")

## Number of views of top movies ##

views_per_movie <- colCounts(ratingmat) # count views for each movie

table_views <- data.frame(movie = names(views_per_movie),
                          views = views_per_movie) # create dataframe of views
table_views <- table_views[order(table_views$views, 
                                 decreasing = TRUE), ] # sort by number of views
table_views$title <- NA
for (i in 1:9066){
  table_views[i,3] <- as.character(subset(movies, 
                                          movies$movieId == table_views[i,1])$title)
}

table_views[1:10,]

ggplot(table_views[1:10, ], aes(x = title, y = views)) +
  geom_bar(stat="identity", fill = "orange", color="black") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("Number of views of the top movies")

## Distribution of average movie rating ##

average_ratings <- colMeans(ratingmat)

qplot(average_ratings) + 
  stat_bin(binwidth = 0.1) +
  ggtitle("Distribution of the average movie rating")

average_ratings_relevant <- average_ratings[views_per_movie > 50] 
qplot(average_ratings_relevant) + 
  stat_bin(binwidth = 0.1) +
  ggtitle(paste("Distribution of the relevant average ratings"))

## Heatmap of the rating matrix ##

image(ratingmat, main = "Heatmap of the rating matrix") # hard to read-too many dimensions

image(ratingmat[1:20, 1:25], main = "Heatmap of the first 20 rows and 25 columns")

min_n_movies <- quantile(rowCounts(ratingmat), 0.99)
min_n_users <- quantile(colCounts(ratingmat), 0.99)
print("Minimum number of movies per user:")
min_n_movies
print("Minimum number of users per movie:")
min_n_users

image(ratingmat[rowCounts(ratingmat) > min_n_movies,
                colCounts(ratingmat) > min_n_users], 
      main = "Heatmap of the top users and movies")

## Data Preparation ##

ratings_movies <- ratingmat[rowCounts(ratingmat) > 50,
                            colCounts(ratingmat) > 50]
ratings_movies
#ratingmat

min_movies <- quantile(rowCounts(ratings_movies), 0.98)
min_users <- quantile(colCounts(ratings_movies), 0.98)
image(ratings_movies[rowCounts(ratings_movies) > min_movies,
                     colCounts(ratings_movies) > min_users], 
      main = "Heatmap of the top users and movies")

average_ratings_per_user <- rowMeans(ratings_movies)
qplot(average_ratings_per_user) + stat_bin(binwidth = 0.1) +
  ggtitle("Distribution of the average rating per user")

## Normalizing Data ##

ratings_movies_norm <- normalize(ratings_movies)
sum(rowMeans(ratings_movies_norm) > 0.00001)

image(ratings_movies_norm[rowCounts(ratings_movies_norm) > min_movies,
                          colCounts(ratings_movies_norm) > min_users], 
      main = "Heatmap of the top users and movies")

## Binarizing Data ##

## 1st Option: Define a matrix equal to 1 if the movie has been watched ##

ratings_movies_watched <- binarize(ratings_movies, minRating = 1)
min_movies_binary <- quantile(rowCounts(ratings_movies), 0.95)
min_users_binary <- quantile(colCounts(ratings_movies), 0.95)
image(ratings_movies_watched[rowCounts(ratings_movies) > min_movies_binary,
                             colCounts(ratings_movies) > min_users_binary], 
      main = "Heatmap of the top users and movies")


## 2nd Option: Define a matrix equal to 1 if the cell has a rating above the threshold ##

ratings_movies_good <- binarize(ratings_movies, minRating = 3)
image(ratings_movies_good[rowCounts(ratings_movies) > min_movies_binary, 
                          colCounts(ratings_movies) > min_users_binary], 
      main = "Heatmap of the top users and movies")





## ITEM BASED COLLABORATIVE FILTERING MODEL ##


## Defining Training/Test Sets ##

which_train <- sample(x = c(TRUE, FALSE), 
                      size = nrow(ratings_movies),
                      replace = TRUE, 
                      prob = c(0.8, 0.2))
head(which_train)

recc_data_train <- ratings_movies[which_train, ]
recc_data_test <- ratings_movies[!which_train, ]

#which_set <- sample(x = 1:5, 
#                     size = nrow(ratings_movies), 
#                     replace = TRUE)
#for(i_model in 1:5) {
 #  which_train <- which_set == i_model
  # recc_data_train <- ratings_movies[which_train, ]
   #recc_data_test <- ratings_movies[!which_train, ]
 #}


## Building the Recommendation model ##

recommender_models <- recommenderRegistry$get_entries(dataType ="realRatingMatrix")
recommender_models$IBCF_realRatingMatrix$parameters

recc_model <- Recommender(data = recc_data_train, 
                          method = "IBCF",
                          parameter = list(k = 30))

recc_model
class(recc_model)

## Exploring the Recommender model ##

model_details <- getModel(recc_model)
model_details$description
model_details$k

class(model_details$sim) # this contains a similarity matrix
dim(model_details$sim)

n_items_top <- 20
image(model_details$sim[1:n_items_top, 1:n_items_top],
      main = "Heatmap of the first rows and columns")

row_sums <- rowSums(model_details$sim > 0)
table(row_sums)
col_sums <- colSums(model_details$sim > 0)
qplot(col_sums) + stat_bin(binwidth = 1) + ggtitle("Distribution of the column count")

## Applying Recommender System on the Dataset ##

n_recommended <- 10 # the number of items to recommend to each user

recc_predicted <- predict(object = recc_model, 
                          newdata = recc_data_test, 
                          n = n_recommended)
recc_predicted

class(recc_predicted)
slotNames(recc_predicted)

recc_user_1 <- recc_predicted@items[[1]] # recommendation for the first user
movies_user_1 <- recc_predicted@itemLabels[recc_user_1]
movies_user_2 <- movies_user_1
for (i in 1:10){
  movies_user_2[i] <- as.character(subset(movies, 
                                          movies$movieId == movies_user_1[i])$title)
}
movies_user_2

recc_matrix <- sapply(recc_predicted@items, 
                      function(x){ as.integer(colnames(ratings_movies)[x]) }) # matrix with the recommendations for each user
dim(recc_matrix)
recc_matrix[,1:10]

number_of_items <- factor(table(recc_matrix))

chart_title <- "Distribution of the number of items for IBCF"
qplot(number_of_items) + ggtitle(chart_title)

number_of_items_sorted <- sort(number_of_items, decreasing = TRUE)
number_of_items_top <- head(number_of_items_sorted, n = 4)
table_top <- data.frame(as.integer(names(number_of_items_top)),
                        number_of_items_top)

for (i in 1:4){
  table_top[i,1] <- as.character(subset(movies, 
                                        movies$movieId == table_top[i,1])$title)
}

colnames(table_top) <- c("Movie title", "No of items")
head(table_top)



## USER BASED COLLABORATIVE FILTERING MODEL ##

## Building the recommendation system ##

recommender_models <- recommenderRegistry$get_entries(dataType ="realRatingMatrix")
recommender_models$UBCF_realRatingMatrix$parameters
recc_model <- Recommender(data = recc_data_train, method = "UBCF")
recc_model
model_details <- getModel(recc_model)
names(model_details)
model_details$data

## Applying the Recommender model on the test set ##

n_recommended <- 10
recc_predicted <- predict(object = recc_model,
                          newdata = recc_data_test, 
                          n = n_recommended) 
recc_predicted

## Explore the results ##

recc_matrix <- sapply(recc_predicted@items, 
                      function(x){ as.integer(colnames(ratings_movies)[x]) })
dim(recc_matrix)
recc_matrix[, 1:10]

number_of_items <- factor(table(recc_matrix))

chart_title <- "Distribution of the number of items for UBCF"
qplot(number_of_items) + ggtitle(chart_title)

number_of_items_sorted <- sort(number_of_items, decreasing = TRUE)
number_of_items_top <- head(number_of_items_sorted, n = 4)
table_top <- data.frame(as.integer(names(number_of_items_top)), number_of_items_top)

for (i in 1:4){
  table_top[i,1] <- as.character(subset(movies, 
                                        movies$movieId == table_top[i,1])$title)
}
colnames(table_top) <- c("Movie title", "No of items")
head(table_top)


## EVALUATING THE RECOMMENDER SYSTEM ##

## Preparing the data to Evaluate the model ##

## Splitting the data ##

percentage_training <- 0.8
min(rowCounts(ratings_movies)) 
items_to_keep <- 5 #number of items to generate recommendations
rating_threshold <- 3 # threshold with the minimum rating that is considered good
n_eval <- 1 #number of times to run evaluation

eval_sets <- evaluationScheme(data = ratings_movies, 
                              method = "split",
                              train = percentage_training, 
                              given = items_to_keep, 
                              goodRating = rating_threshold, 
                              k = n_eval) 
eval_sets

getData(eval_sets, "train") # training set
getData(eval_sets, "known") # set with the items used to build the recommendations
getData(eval_sets, "unknown") # set with the items used to test the recommendations

qplot(rowCounts(getData(eval_sets, "unknown"))) + 
  geom_histogram(binwidth = 10) + 
  ggtitle("unknown items by the users")

## Bootstrapping the data ##

eval_sets <- evaluationScheme(data = ratings_movies, 
                              method = "bootstrap", 
                              train = percentage_training, 
                              given = items_to_keep,
                              goodRating = rating_threshold, 
                              k = n_eval)

table_train <- table(eval_sets@runsTrain[[1]])
n_repetitions <- factor(as.vector(table_train))
qplot(n_repetitions) + 
  ggtitle("Number of repetitions in the training set")

## Using cross validation to validate models ##

n_fold <- 4
eval_sets <- evaluationScheme(data = ratings_movies, 
                              method = "cross-validation",
                              k = n_fold, 
                              given = items_to_keep, 
                              goodRating = rating_threshold)
size_sets <- sapply(eval_sets@runsTrain, length)
size_sets

## Evaluating the ratings ##

eval_sets <- evaluationScheme(data = ratings_movies, 
                              method = "cross-validation",
                              k = n_fold, 
                              given = items_to_keep, 
                              goodRating = rating_threshold)

model_to_evaluate <- "IBCF"
model_parameters <- NULL

eval_recommender <- Recommender(data = getData(eval_sets, "train"),
                                method = model_to_evaluate, 
                                parameter = model_parameters)

items_to_recommend <- 10
eval_prediction <- predict(object = eval_recommender, 
                           newdata = getData(eval_sets, "known"), 
                           n = items_to_recommend, 
                           type = "ratings")

qplot(rowCounts(eval_prediction)) + 
  geom_histogram(binwidth = 10) +
  ggtitle("Distribution of movies per user")

eval_accuracy <- calcPredictionAccuracy(x = eval_prediction, 
                                        data = getData(eval_sets, "unknown"), 
                                        byUser = TRUE)
head(eval_accuracy)

qplot(eval_accuracy[, "RMSE"]) + 
  geom_histogram(binwidth = 0.1) +
  ggtitle("Distribution of the RMSE by user")

eval_accuracy <- calcPredictionAccuracy(x = eval_prediction, 
                                        data = getData(eval_sets, "unknown"), 
                                        byUser = FALSE) 
eval_accuracy


## Evaluating the recommendations ##

results <- evaluate(x = eval_sets, 
                    method = model_to_evaluate, 
                    n = seq(10, 100, 10))

head(getConfusionMatrix(results)[[1]])

columns_to_sum <- c("TP", "FP", "FN", "TN")
indices_summed <- Reduce("+", getConfusionMatrix(results))[, columns_to_sum]
head(indices_summed)

plot(results, annotate = TRUE, main = "ROC curve")

plot(results, "prec/rec", annotate = TRUE, main = "Precision-recall")



## Comparing Models ##

models_to_evaluate <- list(
  IBCF_cos = list(name = "IBCF", 
                  param = list(method = "cosine")),
#  IBCF_cor = list(name = "IBCF", 
 #                 param = list(method = "pearson")),
  UBCF_cos = list(name = "UBCF", 
                  param = list(method = "cosine")),
  #UBCF_cor = list(name = "UBCF", 
   #               param = list(method = "pearson")),
  random = list(name = "RANDOM", param=NULL)
)

n_recommendations <- c(1, 5, seq(10, 100, 10))
list_results <- evaluate(x = eval_sets, 
                         method = models_to_evaluate, 
                         n = n_recommendations)

sapply(list_results, class) == "evaluationResults"

avg_matrices <- lapply(list_results, avg)
avg_matrices$IBCF_cos[, 5:8]
avg_matrices$IBCF_cos
avg_matrices$UBCF_cos[, 5:8]
avg_matrices$UBCF_cos

## Identifying the most suitable model ##

plot(list_results, annotate = 1, legend = "topleft") 
title("ROC curve")

plot(list_results, "prec/rec", annotate = 1, legend = "bottomright")
title("Precision-recall")


## Optimizing a Numeric Parameter ##

vector_k <- c(5, 10, 20, 30, 40)
models_to_evaluate <- lapply(vector_k, function(k){
  list(name = "IBCF",
       param = list(method = "cosine", k = k))
})
names(models_to_evaluate) <- paste0("IBCF_k_", vector_k)

n_recommendations <- c(1, 5, seq(10, 100, 10))
list_results <- evaluate(x = eval_sets, 
                         method = models_to_evaluate, 
                         n = n_recommendations)

plot(list_results, annotate = 1, legend = "topleft") 
title("ROC curve")

plot(list_results, "prec/rec", annotate = 1, legend = "bottomright")
title("Precision-recall")


