# 15.780 - Fall 2022
# Data cleaning
data = read.csv("/Users/jasonlee/Downloads/train_with_actors.csv")

df2 <- subset(data, substr(production_countries, 18, 19) == "US")
df2 <- subset(df2, budget > 10000)
df2 <- subset(df2, revenue > 10000)

df2$comedy <- ifelse(grepl("Comedy", df2$genres, fixed = TRUE), 1, 0)
df2$action <- ifelse(grepl("Action", df2$genres, fixed = TRUE), 1, 0)
df2$drama <- ifelse(grepl("Drama", df2$genres, fixed = TRUE), 1, 0)
df2$horror <- ifelse(grepl("Horror", df2$genres, fixed = TRUE), 1, 0)
df2$thriller <- ifelse(grepl("Thriller", df2$genres, fixed = TRUE), 1, 0)
df2$music <- ifelse(grepl("Music", df2$genres, fixed = TRUE), 1, 0)
df2$science_fic <- ifelse(grepl("Science Fiction", df2$genres, fixed = TRUE), 1, 0)
df2$mystery <- ifelse(grepl("Mystery", df2$genres, fixed = TRUE), 1, 0)
df2$western <- ifelse(grepl("Western", df2$genres, fixed = TRUE), 1, 0)
df2$animation <- ifelse(grepl("Animation", df2$genres, fixed = TRUE), 1, 0)
df2$adventure <- ifelse(grepl("Adventure", df2$genres, fixed = TRUE), 1, 0)
df2$romance <- ifelse(grepl("Romance", df2$genres, fixed = TRUE), 1, 0)
df2$war <- ifelse(grepl("War", df2$genres, fixed = TRUE), 1, 0)
df2$history <- ifelse(grepl("History", df2$genres, fixed = TRUE), 1, 0)
df2$crime <- ifelse(grepl("Crime", df2$genres, fixed = TRUE), 1, 0)
df2$documentary <- ifelse(grepl("Documentary", df2$genres, fixed = TRUE), 1, 0)
df2$fantasy <- ifelse(grepl("Fantasy", df2$genres, fixed = TRUE), 1, 0)
df2$family <- ifelse(grepl("Family", df2$genres, fixed = TRUE), 1, 0)
df2$franchise <- ifelse(df2$belongs_to_collection == "", 0, 1)

df2$release_year <- substring(df2$release_date, nchar(df2$release_date) - 1, nchar(df2$release_date))
df2$release_year_num <- df2$release_year
df2$release_year = as.numeric(df2$release_year)
df2$release_year_num <- ifelse(df2$release_year > 20, paste("19", df2$release_year_num, sep=""), paste("20", df2$release_year_num, sep=""))
df2$release_year = as.numeric(df2$release_year_num)

#data_US <- data_US[-c(5,6,7,8,9,11,12,15,16,17,19,21)]

df2$lnrevenue = log(df2$revenue)
df2$lnbudget = log(df2$budget)

train = df2[1:958,]
test = df2[-(1:958),]

# Linear regression basic (no logs)
lm.msrp = lm(revenue~budget + runtime + comedy + action + drama + horror + thriller + music + science_fic + mystery 
             + western + animation + adventure + romance + war + history + crime + documentary + fantasy 
             + family + franchise + release_year + hasTopStar + Maggie.Smith + Tom.Hanks + Bruce.Willis + Orlando.Bloom + Willem.Dafoe
             + Alan.Rickman + Scarlett.Johansson + Leonardo.DiCaprio + Chris.Evans + Whoopi.Goldberg, data=train)
summary(lm.msrp)

train_pred = lm.msrp$fitted.values
test_pred = predict(lm.msrp, test)
test$predicted_revenue <- test_pred
test$error <- test$revenue - test$predicted_revenue

# Calculate MAPE
mean(abs(train_pred - train$revenue) / abs(train$revenue))
mean(abs(test_pred - test$revenue) / abs(test$revenue))

plot(test$predicted_revenue, test$revenue,
     col='blue', pch=16, type="p", 
     main = "true vs. predicted revenue", 
     ylab = "true_revenue",
     xlab = "predicted_revenue",
     xlim = c(0, 5e+8),
     ylim = c(0, 5e+8))

# Linear regression (with log revenue/budget)
train$revenue <- log(train$revenue)
test$revenue <- log(test$revenue)
train$budget <- log(train$budget)
test$budget <- log(test$budget)
lm.msrp = lm(revenue~budget + runtime + comedy + action + drama + horror + thriller + music + science_fic + mystery 
             + western + animation + adventure + romance + war + history + crime + documentary + fantasy 
             + family + franchise + popularity, data=train)
summary(lm.msrp)

train_pred = lm.msrp$fitted.values
test_pred = predict(lm.msrp, test)
test$predicted_revenue <- test_pred
test$error <- test$revenue - test$predicted_revenue

mean(abs(train_pred - train$revenue) / abs(train$revenue))
mean(abs(test_pred - test$revenue) / abs(test$revenue))

plot(test$predicted_revenue, test$revenue,
     col='blue', pch=16, type="p", 
     main = "true vs. predicted revenue", 
     ylab = "true_revenue",
     xlab = "predicted_revenue")
