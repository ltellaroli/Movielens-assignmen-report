#install required packages if needed
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(broom)) install.packages("broom", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(recosystem)) install.packages("recosystem", repos = "http://cran.us.r-project.org")

#download the data as specified in the assessment instructions
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies, stringsAsFactors=TRUE) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],  #explicltly mentioning stringasfactors=TRUE because from R v4.0.0 the default of this argument changed from TRUE to FALSE
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

#number of movies in the dataset
nmovies<-(length(unique(edx$movieId)))
#number of users in the dataset
nusers<-(length(unique(edx$userId)))

set.seed(1, sample.kind="Rounding") #setting the seed for the next sample taking

#taking a sample of 500 user ids to perfor data exploration
library(dplyr)
library(broom)
set.seed(1, sample.kind="Rounding")
users_sample <- sample(unique(edx$userId), 500)
edx_small<-edx%>%filter(userId %in% users_sample)

#creating a matrix with movies as columns and users as rows
edx_small_matrix<-edx_small%>%select(userId, movieId, rating)%>%spread(movieId, rating)%>% select(sample(ncol(.), 500))%>%do(tidy(is.na(.)))%>% as.matrix() %>% t(.)

#plotting non NA values of the previous matrix to show that the data is very sparse
non_NA_matrix<-edx_small_matrix%>%as.vector %>%  tibble(value = ., row = rep(1:nrow(edx_small_matrix), times = ncol(edx_small_matrix)), col = rep(1: ncol(edx_small_matrix), each = nrow(edx_small_matrix))) %>%ggplot(aes(x = row, y = col, colour = value)) + geom_point(size = 2) +scale_color_manual(values = c('black','white'), name="", breaks=c(FALSE, TRUE), labels=c("Not NA", "NA"))+xlab("Movies")+ylab("Users")

#taking a bigger sample for the next data explortion steps
set.seed(1, sample.kind="Rounding")
users_sample <- sample(unique(edx$userId), 5000)
edx_small<-edx%>%filter(userId %in% users_sample)

#extracting release years from movie titles
release_years<-str_extract(str_trim(edx_small$title),  "[(]([0-9]{4})[)]")  #extract years of release from the dataset
release_years<-as.numeric(str_replace(release_years, "[(]([0-9]{4})[)]", "\\1")) #removes () and converts to numeric

#investigating movie effect by plotting mean rating vs the number of movies with a given mean rating
edx_small<-edx_small%>%mutate(release_year=release_years) #adds release year column into dataframe
movie_effect_plot<-edx_small%>%group_by(movieId)%>%summarize(mean=mean(rating))%>%qplot(mean, geom="histogram", bins=10, data=.)+xlab("Average Rating")+ylab("N of Movies") + ggtitle("Movies grouped by average rating")  #plots mean rating for each movie

#investigating user effect by by plotting mean rating vs the number of users with a given mean rating
user_effect_plot<-edx_small%>%group_by(userId)%>%summarize(mean=mean(rating))%>%qplot(mean, geom="histogram", bins=10, data=.)+xlab("Average Rating")+ylab("N of Users")+ggtitle("Users grouped by average rating")  #plots mean rating for each user

#investigating relese year effect by plotting mean rating for each year of release and running a correlation test between release yer and rating
year_effect_plot<-edx_small%>%group_by(release_year)%>%summarize(mean=mean(rating))%>%ggplot(aes(x=release_year, y=mean))+geom_point()+geom_smooth()+xlab("Release Year")+ylab("Average Rating of Movies")+ggtitle("Release year effect")  #plots mean rating for each release year
year_effect_corr<-cor.test(edx_small$release_year, edx_small$rating)

#investigating  number of ratings effect plotting number of ratings vs average rating and running correlation test between those 2 variables
nratings_effect_plot<-edx_small%>%group_by(movieId)%>%summarize(mean=mean(rating), nratings=n())%>%ggplot(aes(x=nratings, y=mean))+geom_smooth()+geom_point()+xlab("Number of ratings")+ylab("Average Rating") #plots number of rating for each movie vs mean rating
nratings_effect_cor<-edx_small%>%group_by(movieId)%>%summarize(mean=mean(rating), nratings=n())
nratings_effect_cor<-cor.test(nratings_effect_cor$nratings, nratings_effect_cor$mean)  #testing correlation between number of ratings per movie and mean rating

#investigating review date effect 
library(lubridate)
time_effect_plot<-edx_small%>%mutate(date=round_date(as_datetime(timestamp), unit="week"))%>%group_by(date)%>%summarize(mean=mean(rating))%>%ggplot(aes(x=date, y=mean))+geom_point()+geom_smooth()+xlab("Rating Date")+ylab("Average Rating") #plots week of the review vs mean rating for the week 


#investigating genre effect by plotting average rating and error bars for genres with mmore than 500 movies
genre_effect_plot<-edx_small%>%group_by(genres)%>%summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>% filter(n >= 5000) %>%mutate(genres = reorder(genres, avg)) %>% ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + geom_point() + geom_errorbar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))+xlab("")+ylab("Genre average rating")  #error barplot comparing the mean an sd of rating grouped by genre

#investigating the variability of ratings for movies with a low number of ratings we see that movies and users with a low number of ratings have a greater variability of the sd
nratings_movie_sd_plot<-edx_small%>%group_by(movieId)%>%filter(n()>2)%>%summarize(sd=sd(rating), nratings=n())%>%ggplot(aes(x=nratings, y=sd))+geom_point()+xlab("Number of Ratings by movie")+ylab("Standard deviation of ratings")+ggtitle("Ratings Std Dev vs number of ratings per movie")
nratings_user_sd_plot<-edx_small%>%group_by(userId)%>%filter(n()>2)%>%summarize(sd=sd(rating), nratings=n())%>%ggplot(aes(x=nratings, y=sd))+geom_point()+xlab("Number of Ratings by user")+ylab("Standard deviation of ratings")+ggtitle("Ratings Std Dev vs number of ratings per user")
nratings_movie_vs_mean<-edx_small%>%group_by(movieId)%>%filter(n()>2)%>%summarize(mean=mean(rating), nratings=n())%>%ggplot(aes(x=mean, y=nratings))+geom_point()+xlab("Average rating")+ylab("Number of Ratings by Movie")+ggtitle("Nuber of ratings by movie vs Movie average rating")
nratings_user_vs_mean<-edx_small%>%group_by(userId)%>%filter(n()>2)%>%summarize(mean=mean(rating), nratings=n())%>%ggplot(aes(x=mean, y=nratings))+geom_point()+xlab("User average rating")+ylab("Number of Ratings by user")


#model development



#first we add release year and number of ratings per movie to our edx dataframe
release_years<-str_extract(str_trim(edx$title),  "[(]([0-9]{4})[)]")  #extract years of release from the dataset
release_years<-as.numeric(str_replace(release_years, "[(]([0-9]{4})[)]", "\\1")) #removes () and converts to numeric
edx<-edx%>%mutate(release_year=release_years) #adds relese years to the data
edx<-edx%>%group_by(movieId)%>%mutate(n_ratings_movie=n())%>%ungroup() #adds number of ratings by movie 
edx<-edx%>%group_by(userId)%>%mutate(n_ratings_user=n())%>%ungroup() #adds number of ratings by user

#then the edx dataset is splitted in a train dataset and a test dataset to optimize prameters, test dataset formed by 10% of data from edx

set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index2 <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
edx_train <- edx[-test_index2,]
temp <- edx[test_index2,]
edx_test <- temp %>% semi_join(edx_train, by = "movieId") %>% semi_join(edx_train, by = "userId")
removed <- anti_join(temp, edx_test)
edx_train <- rbind(edx, removed)
validation<-validation%>%semi_join(edx_train, by= "movieId" ) %>% semi_join(edx_train, by = "userId")


#choosing lambda parameter for the regularized linear reg
wmean<-weighted.mean(edx_train$rating, edx_train$n_ratings_movie) #average rating of all the movies wegihted by number of ratings per movie 
rmse<-function(x, y){sqrt(mean((x-y)^2))} #function to find residual mean standard error
lambdamov<-seq(0, 3, 0.15)  #test values for lambda regularization parameter for movie effect
lambdauser<-seq(0, 3, 0.15) #test values for lambda regularization parameter for user effect
rmsematrix<-matrix(rep(0, (length(lambdamov)*length(lambdauser))), nrow=length(lambdauser), ncol=length(lambdamov)) #initialize matrix containing rmse for various combination of lambdas
for (i in seq(1:length(lambdamov))) {    #2 nested for cycle to generate rmse for each value of lambauser and lambdamovie
lambdai<-lambdamov[i]
b_mov<-edx_train%>%group_by(movieId)%>%summarize(b_mov=sum(rating-wmean)/(n()+lambdai))
for (j in seq(1:length(lambdauser))) {
lambdaj<-lambdauser[j] 
b_user<-edx_train%>%left_join(b_mov, by="movieId")%>%group_by(userId)%>%summarize(b_user=sum(rating-wmean-b_mov)/(n()+lambdaj))
edx_linear_prediction<-edx_test%>%left_join(b_mov, by='movieId')%>%left_join(b_user, by='userId')%>%mutate(prediction=wmean+b_mov+b_user)%>%mutate(prediction=ifelse(prediction>5, 5, prediction))%>%mutate(prediction=ifelse(prediction<0, 0, prediction))  #last part of the line sets to 5 predictions that are higher than 5 and to 0 preditions that are lower than 0
rmsematrix[j, i]<-rmse(edx_linear_prediction$rating, edx_linear_prediction$prediction)}}
min(rmsematrix) #picks minimum value of rmse obtained
best_indexes<-which(rmsematrix==min(rmsematrix), arr.ind=TRUE) #picks indices of the minimum values of rmse obtained, the column index relates to best lambdamovie value, the row index relates to best lambdauser value

lambdamovie_best<-lambdamov[best_indexes[2]]  
lambdauser_best<-lambdauser[best_indexes[1]]

plot(lambdauser, rmsematrix[ , best_indexes[2]]) #plots lambdauser vs rmse
plot(lambdamov, rmsematrix[ best_indexes[1] ,])  #plots lambdamovie vs rmse

#use the choosen values of lambda on the test set 
b_mov<-edx_train%>%group_by(movieId)%>%summarize(b_mov=sum(rating-wmean)/(n()+lambdamovie_best))
b_user<-edx_train%>%left_join(b_mov, by="movieId")%>%group_by(userId)%>%summarize(b_user=sum(rating-wmean-b_mov)/(n()+lambdauser_best))
regularized_linear_prediction<-edx_test%>%left_join(b_mov, by='movieId')%>%left_join(b_user, by='userId')%>%mutate(prediction=wmean+b_mov+b_user)%>%mutate(prediction=ifelse(prediction>5, 5, prediction))%>%mutate(prediction=ifelse(prediction<0, 0, prediction))   #last part of the line sets to 5 predictions that are higher than 5 and to 0 preditions that are negative
rmse_user_and_movie_regularized<-rmse(regularized_linear_prediction$rating, regularized_linear_prediction$prediction)

#now release year  effects are added
b_release_year<-edx_train%>%left_join(b_mov, by="movieId")%>%left_join(b_user, by="userId")%>%group_by(release_year)%>%summarize(b_release_year=mean(rating-b_mov-b_user-wmean))


#testing model with  the year effect
year_effect_linear_prediction<-edx_test%>%left_join(b_mov, by='movieId')%>%left_join(b_user, by='userId')%>%left_join(b_release_year, by="release_year")%>%mutate(prediction=wmean+b_mov+b_user+b_release_year)%>%mutate(prediction=ifelse(prediction>5, 5, prediction))%>%mutate(prediction=ifelse(prediction<0, 0, prediction))
rmse_user_and_movie_regularized_plus_releaseyear<-rmse(year_effect_linear_prediction$rating, year_effect_linear_prediction$prediction)
#as we can see from plot below adding year effect improves the RMSE calculated on the test set


#setting seed again to prepare for next analysis using the recosystem package
set.seed(1, sample.kind="Rounding")

train_reco<- data_memory(user_index = edx_train$userId, item_index = edx_train$movieId, rating = edx_train$rating) #converting train and test set in format compatible with recosystem package
test_reco<-data_memory(user_index = edx_test$userId, item_index = edx_test$movieId, rating = edx_test$rating)

recomodel<-Reco() #creating recosystem model as indicated in the package docs


#optimizing parameters for recosystem
opts = recomodel$tune(train_reco, opts = list(nthread = 4, niter = 5)) #nthread is the number of CPU threads available and niter the number of iterations
 
#training the model                                    
recomodel$train(train_reco, opts = c(opts$min, nthread = 4, niter = 10))  #train model
recopred = recomodel$predict(test_reco, out_memory())  #make predictions on the test set from the model
rmsereco<-rmse(edx_test$rating, recopred)  #computes rmse






#now we apply the best method to the validation set to find the final rmse

validation_reco<-data_memory(user_index = validation$userId, item_index = validation$movieId, rating = validation$rating) #converts validation set to package specific format

recopred_validation = recomodel$predict(validation_reco, out_memory())  #make predictions on the validation set from the model
rmse_final<-rmse(validation$rating, recopred_validation)  #computes rmse of the model on validation set


#out of crusiosity we test also the regularized linear model on the validation set
reglinear_validation<-validation%>%left_join(b_mov, by='movieId')%>%left_join(b_user, by='userId')%>%mutate(prediction=wmean+b_mov+b_user)%>%mutate(prediction=ifelse(prediction>5, 5, prediction))%>%mutate(prediction=ifelse(prediction<0, 0, prediction))
rmse_user_and_movie_regularized_validation<-rmse(reglinear_validation$rating, reglinear_validation$prediction)

 #and also the model with the added year effect, but first we have to addd a column to the validation set including release years of movies in that dataset
release_years<-str_extract(str_trim(validation$title),  "[(]([0-9]{4})[)]")  #extract years of release from the dataset
release_years<-as.numeric(str_replace(release_years, "[(]([0-9]{4})[)]", "\\1")) #removes () and converts to numeric
validation<-validation%>%mutate(release_year=release_years) #adds relese years to the validation dataframe

year_effect_linear_prediction<-validation%>%left_join(b_mov, by='movieId')%>%left_join(b_user, by='userId')%>%left_join(b_release_year, by="release_year")%>%mutate(prediction=wmean+b_mov+b_user+b_release_year)%>%mutate(prediction=ifelse(prediction>5, 5, prediction))%>%mutate(prediction=ifelse(prediction<0, 0, prediction))
rmse_user_and_movie_regularized_plus_releaseyear_validation<-rmse(year_effect_linear_prediction$rating, year_effect_linear_prediction$prediction)


#plot of the results
rmse_values<-data.frame(method=c("Regularized linear regression", "Regularized linear reg + year eff.", "Matrix Factorization" ),
                       value<-c(rmse_user_and_movie_regularized_validation, rmse_user_and_movie_regularized_plus_releaseyear_validation ,rmse_final ))
library(scales)
p <- ggplot(rmse_values, aes(method, value, fill=method)) + geom_bar(position="dodge", stat="identity") + ylab("RMSE")+theme(axis.text.x = element_text(angle = 90))+theme(legend.position = "none")+xlab("")






