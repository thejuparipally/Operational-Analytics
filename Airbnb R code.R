library(readxl)
Airbnb_NYC <- read_excel("Desktop/Op Analytics Project/Airbnb NYC.xlsx")
View(Airbnb_NYC) 

# general visualisation
library(RColorBrewer)
library(ggridges)
install.packages("cowplot", type = "source")
library(cowplot)
library(ggplot2)

# general data manipulation
library(dplyr)
library(glue)

# specific visualisation
library(hrbrthemes)
install.packages("ggthemes")
library(ggthemes)

# Statistics
install.packages("DescTools")
library(DescTools)

library(tidyr)
library(corrplot)


# Loading the database
airbnb <- read.csv("/Users/theju/Desktop/Op Analytics Project/Airbnb NYC.csv")
airbnb <- select(airbnb, neighbourhood_group, neighbourhood, latitude, longitude, room_type, price, minimum_nights, number_of_reviews, last_review, reviews_per_month, calculated_host_listings_count, availability_365)
# Viewing the first DataFrame records
head(airbnb,6)

# Generating a summary of all data attributes with the summary() function
summary(airbnb)

# Variable types
c(unique(airbnb["neighbourhood_group"]))

c(unique(airbnb["neighbourhood"]))

c(unique(airbnb["room_type"]))

c(unique(airbnb["minimum_nights"]))

glue("Price Minimum: {min(airbnb$price)} | Price Maximum: {max(airbnb$price)}")

glue("Minimum Longitude : {min(airbnb$longitude)} | Maximum Latitude: {max(airbnb$longitude)}")

glue("Minimum Latitude : {min(airbnb$latitude)} | Maximum Latitude: {max(airbnb$latitude)}")

# Frequency Distribution
freq_location <- data.frame(cbind(Frequency = table(airbnb$neighbourhood_group), Percent = prop.table(table(airbnb$neighbourhood_group)) * 100))
freq_location <- freq_location[order(freq_location$Frequency),]
freq_location

freq_area <- data.frame(cbind(Frequency = table(airbnb$neighbourhood), Percent = prop.table(table(airbnb$neighbourhood)) * 100))
freq_area <- freq_area[order(freq_area$Frequency),]
freq_area

freq_type <- data.frame(cbind(Frequency = table(airbnb$room_type), Percent = prop.table(table(airbnb$room_type)) * 100))
freq_type <- freq_type[order(freq_type$Frequency),]
freq_type

#Frequency Distribution Tables for quantitative variables
n <- nrow(airbnb)
k <- round(1 + ((10/3) * log10(n)))

labels <- c(
  '      1   —  73,470', 
  ' 73,470   —  146,940', 
  ' 146,940  —  220,411', 
  ' 220,411  —  293,881', 
  ' 293,881  —  367,352', 
  ' 367,352  —  440,822', 
  ' 440,822  — 514,293', 
  ' 514,293  — 587,764', 
  ' 587,764  — 661,234', 
  ' 661,234  — 734,705', 
  ' 734,705  — 808,175', 
  ' 808,175  — 881,646', 
  ' 881,646  — 955,117', 
  ' 955,117  — 1028,587', 
  ' 1028,587 — 1102,058', 
  ' 1102,058 — 1175,528', 
  ' 1175,528 — 1250')

freq_nights <- cbind(Frequency=table(cut(x = airbnb$minimum_nights, breaks = k, labels = labels, include.lowest=T)), Percent = prop.table(table(cut(x = airbnb$minimum_nights, breaks = k, labels = labels, include.lowest=T))) * 100)
freq_nights

labels <- c(
  '       0  —  58,823', 
  '  58,823  —  117,647', 
  ' 117,647  —  176,470', 
  ' 176,470  —  235,294', 
  ' 235,294  —  294,117', 
  ' 294,117  —  352,941', 
  ' 352,941  —  411,764', 
  ' 411,764  —  470,588', 
  ' 470,588  —  529,411', 
  ' 529,411  —  588,235', 
  ' 588,235  —  647,058', 
  ' 647,058  —  705,882', 
  ' 705,882  —  764,705', 
  ' 764,705  —  823,529', 
  ' 823,529  —  882,352', 
  ' 882,352  —  941,176', 
  ' 941,176  —  10 000')

freq_price <- cbind(Frequency=table(cut(x = airbnb$price, breaks = k, labels = labels, include.lowest=T)), Percent = prop.table(table(cut(x = airbnb$price, breaks = k, labels = labels, include.lowest=T))) * 100)
freq_price

#Correlation Matrix
airbnb_cor <- airbnb[, sapply(airbnb, is.numeric)]
airbnb_cor <- airbnb_cor[complete.cases(airbnb_cor), ]
correlation_matrix <- cor(airbnb_cor, method = "spearman")
corrplot(correlation_matrix, method = "color")

#Boxplot of price by room type
th <- theme_fivethirtyeight() + theme(axis.title = element_text(), axis.title.x = element_text()) # global theme for ggplot2 objects

ggplot(airbnb, aes(x = room_type, y = price)) +
  geom_boxplot(aes(fill = room_type)) + scale_y_log10() +
  th + 
  xlab("Room type") + 
  ylab("Price") +
  ggtitle("Boxplots of price by room type",
          subtitle = "Entire homes and apartments have the highest avg price") +
  geom_hline(yintercept = mean(airbnb$price), color = "purple", linetype = 2)

#Missing data
th <- theme_fivethirtyeight() + theme(axis.title = element_text(), axis.title.x = element_text()) # global theme for ggplot2 objects

missing_airbnb <- airbnb %>% summarise_all(~(sum(is.na(.))/n()))
missing_airbnb <- gather(missing_airbnb, key = "variables", value = "percent_missing")
missing_airbnb <- missing_airbnb[missing_airbnb$percent_missing > 0.0, ] 
ggplot(missing_airbnb, aes(x = reorder(variables, percent_missing), y = percent_missing)) +
  geom_bar(stat = "identity", fill = "red", aes(color = I('white')), size = 0.3)+
  xlab('variables')+
  coord_flip() + 
  th  +
  ggtitle("Missing Data") +
  xlab("Column name") +
  ylab("Percentage missing") +
  annotate("text", x = 1.5, y = 0.1,label = "host_name and name have less than 0.001\n percentage missing", color = "slateblue", size = 5)

#Histograms

tema <- theme(plot.background = element_rect(fill = "#FFFAFA", color = "#FFFAFA"),
              plot.title = element_text(size = 23, hjust = .5),
              axis.text.x = element_text(size = 19, face = "bold"),
              axis.text.y = element_text(size = 19, face = "bold"),
              axis.title.x = element_text(size = 19),
              axis.title.y = element_text(size = 19),
              legend.position = "none")

options(repr.plot.width=14, repr.plot.height=6)
a <- ggplot(data = airbnb, mapping = aes(x = price)) +
  geom_histogram(fill = "cyan", bins = 70, size = 1.3, color = "black") +
  theme_minimal() +
  ylab("Frequency") +
  xlab("Price") +
  ggtitle("Price Histogram") +
  tema


df <- data.frame(price = airbnb["price"][airbnb["price"] <= 1000])
b <- ggplot(data = df, mapping = aes(x = price)) +
  geom_histogram(fill = "cyan", bins = 70, size = 1.3, color = "black") +
  theme_minimal() +
  ylab("Frequency") +
  xlab("Price") +
  ggtitle("Price <= 1000 | Histogram") +
  tema

plot_grid(a, b, ncol=2, nrow=1)

a <- ggplot(data = airbnb, mapping = aes(x = minimum_nights)) +
  geom_histogram(fill = "yellow", bins = 70, size = 1.3, color = "black") +
  theme_minimal() +
  ylab("Frequency") +
  xlab("Minimum nights") +
  ggtitle("Minimum nights Histogram") +
  tema


df <- data.frame(minimum_nights = airbnb["minimum_nights"][airbnb["minimum_nights"] <= 40])
b <- ggplot(data = df, mapping = aes(x = minimum_nights)) +
  geom_histogram(fill = "yellow", bins = 70, size = 1.3, color = "black") +
  theme_minimal() +
  ylab("Frequency") +
  xlab("Minimum nights") +
  ggtitle("Minimum nights <= 40 | Histogram") +
  tema

plot_grid(a, b, ncol=2, nrow=1)

#Bar graphs

# Room type
tema <- theme(plot.background = element_rect(fill = "#EEE8AA", color = "yellow"),
              plot.title = element_text(size = 23, hjust = .5),
              axis.text.x = element_text(size = 19, face = "bold"),
              axis.text.y = element_text(size = 19, face = "bold"),
              axis.title.x = element_text(size = 19),
              axis.title.y = element_text(size = 19),
              legend.position = "none")

tema1 <- theme(plot.background = element_rect(fill = "#EEE8AA", color = "yellow"),
               plot.title = element_text(size = 23, hjust = .5),
               axis.text.x = element_text(size = 12, face = "bold"),
               axis.text.y = element_text(size = 12, face = "bold"),
               axis.title.x = element_text(size = 12),
               axis.title.y = element_text(size = 12),
               legend.position = "none")

options(repr.plot.width=15, repr.plot.height=6)
a <- ggplot(data = freq_type, mapping = aes(x = Frequency, y = row.names(freq_type))) +
  geom_bar(stat = "identity", mapping = aes(fill = row.names(freq_type), color = row.names(freq_type)), alpha = .7, size = 1.1) +
  geom_label(mapping = aes(label=Frequency), fill = "#006400", size = 6, color = "white", fontface = "bold", hjust=.7) +
  ylab("") +
  ggtitle("Room type distribution") +
  theme_economist() +
  tema

b <- ggplot(data = freq_type, aes(x = row.names(freq_type), y = Frequency)) +
  geom_bar(stat = "identity", mapping = aes(fill = row.names(freq_type), color = row.names(freq_type)), alpha = .7, size = 1.1) +
  theme_economist() +
  xlab("") +
  ylab("") +
  ggtitle("Room type - Polar") +
  tema1


plot_grid(a, b + coord_polar(), ncol=2, nrow=1)

#The 10 most frequent neighbourhood
tema <- theme(plot.title = element_text(size = 23, hjust = .5),
              axis.text.x = element_text(size = 19, angle=15, face = "bold"),
              axis.text.y = element_text(size = 19, angle=10, face = "bold"),
              axis.title.x = element_text(size = 19),
              axis.title.y = element_text(size = 19),
              legend.text = element_text(size = 14, face = "bold"))

df <- data.frame(neighbourhood = row.names(tail(freq_area, 10)), Frequency = tail(freq_area, 10)$Frequency)

options(repr.plot.width=15, repr.plot.height=6)
ggplot(data = df, mapping = aes(x = neighbourhood, y = Frequency)) +
  theme_minimal() + 
  geom_point(size = 6, color = "green") +
  ggtitle("The 10 most frequent neighbourhood") +
  xlab("") +
  geom_line(color = "black", size = 2, linetype= 16, group = 1, alpha = .5) + 
  geom_bar(stat = "identity", mapping = aes(fill = neighbourhood, color = neighbourhood), alpha = .8, size = 1.5) +   
  tema

#neighbourhood_group
tema <- theme(plot.background = element_rect(fill = "#DCFCE6", color = "#66CDAA"),
              plot.title = element_text(size = 23, hjust = .5),
              axis.text.x = element_text(size = 19, face = "bold"),
              axis.text.y = element_text(size = 19, face = "bold"),
              axis.title.x = element_text(size = 19),
              axis.title.y = element_text(size = 19),
              legend.position = "none")

tema1 <- theme(plot.background = element_rect(fill = "#DCFCE6", color = "#66CDAA"),
               plot.title = element_text(size = 23, hjust = .5),
               axis.text.x = element_text(size = 15, face = "bold"),
               axis.text.y = element_text(size = 15, face = "bold"),
               axis.title.x = element_text(size = 15),
               axis.title.y = element_text(size = 15),
               legend.position = "none")

options(repr.plot.width=14, repr.plot.height=6)
a<- ggplot(data = freq_location, aes(x=row.names(freq_location), y=Frequency)) +
  geom_bar(stat = "identity", mapping = aes(fill = row.names(freq_location), color = row.names(freq_location)), alpha = .7, size = 1.1) +
  coord_flip() +
  theme_economist() +
  xlab("") +
  ggtitle("Location Distribution") +
  tema

b <- ggplot(data = freq_location, aes(x=row.names(freq_location), y=Frequency)) +
  geom_bar(stat = "identity", mapping = aes(fill = row.names(freq_location), color = row.names(freq_location)), alpha = .7, size = 1.1) +
  theme_economist() +
  xlab("") +
  ylab("") +
  ggtitle("Location Distribution - Polar") +
  tema1

plot_grid(a, b + coord_polar(), ncol=2, nrow=1)

#Central Trend Measures

#Mean
means <- data.frame(Mean = c(mean(airbnb$price), mean(airbnb$minimum_nights)))
row.names(means) <- c("Price", "Minimum nights")
means

#Median
medians <- data.frame(Median = c(median(airbnb$price), median(airbnb$minimum_nights)))
row.names(medians) <- c("Price", "Minimum nights")
medians

#Mode
Moda <- function(x){
  
  freq <- table(x)
  return(names(freq)[freq == max(freq)])
}

mod <- data.frame(Mode = c(Moda(airbnb$price), Moda(airbnb$minimum_nights)))
row.names(mod) <- c("Price", "Minimum nights")
mod

#Dispersion Measures

#Mean Absolute Deviation
md <- data.frame(DM = c(MeanAD(airbnb$price), MeanAD(airbnb$minimum_nights)))
row.names(md) <- c("Price", "Minimum nights")
md

#Variance
v <- data.frame("Variance" = c(var(airbnb$price), var(airbnb$minimum_nights)))
row.names(v) <- c("Price", "Minimum nights")
v

#Standard deviation
std <- data.frame("Standard deviation" = c(sqrt(var(airbnb$price)), sqrt(var(airbnb$minimum_nights))))
row.names(std) <- c("Price", "Minimum nights")
std

# Identify outliers using z-scores
z_scores <- scale(airbnb$price) 
z_cutoff <- 3 
outliers_z <- which(abs(z_scores) > z_cutoff)

# Identify outliers using the 1.5 * IQR method
Q1 <- quantile(airbnb$price, 0.25)
Q3 <- quantile(airbnb$price, 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
outliers_iqr <- which(airbnb$price < lower_bound | airbnb$price > upper_bound)

# Combine the two outlier sets
outliers <- union(outliers_z, outliers_iqr)

# Print the indices of identified outliers
print(outliers)

# Resolve outliers (either remove or retain, with an explanation)
# For example, to remove outliers:
# cleaned_data <- airbnb[-outliers, ]

# To retain outliers:
# retained_data <- airbnb[outliers, ]

