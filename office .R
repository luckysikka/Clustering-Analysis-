#INSTALLING PACKAGES 

install.packages("tidyverse")
install.packages("flexclust")
install.packages("NbClust")
install.packages("janitor")


library(tidyverse)
library(NbClust)
library(flexclust)
library(janitor)


#Task 1 - Read the data 
df <- read.csv("office.csv")
#Checking the Summary of the data 
glimpse(df)
# Descriptive Analysis 
summary(df)

#Task 2 - Creating a data frame for attitudinal variable 
df1<- df[-c(1,8,9,10) ]
# Converting the data as normalised (Z Score)
df1_std <- scale(df1[, c("variety_of_choice",
                                     "electronics",
                                     "furniture",
                                     "quality_of_service",
                                     "low_prices",
                                     "return_policy")]) %>% 
  as_tibble()
#Checking the minumum & Maximum values in r data frame 
summary(df1_std)
min(df1_std)
max(df1_std)

# Task 3- Computing euclidean distances
dist <- dist(df1_std,method = "euclidean")
#Doing Randmoise (for repreducibility )
set.seed(123)
#Doing the herarical Clustering algorithm 
hc <- hclust(dist, method = "ward.D2")
print(hc)
#Plotting the Dendogram 
plot(hc)

# Task 4- creates a Six cluster solution
hc_6 <- cutree(hc, k = 6) 
plot(hc_6)
#checking the number of observation in each cluster
table(hc_6)

# Task5- Checking the mean on each cluster & Bar chart segment Profile
df1_std %>%
  mutate(hc_6 = factor(hc_6)) %>% # add the cluster assignment
  group_by(hc_6) %>% # group by cluster
  mutate(n = n()) %>% # calculate the n per group 
  summarise_all(~ mean(.x)) %>% # calculate the mean per group 
  mutate(prop = n/sum(n)) %>% # calculate the Proportion of each 
  print(width = Inf) # print all columns

#Segment Profile Plot
hc_6flex <- as.kcca(hc, df1_std, k = 6)

#Bar chart for Six Cluster 
barchart(hc_6flex , main= "Cluster-Segment Profile")

# checking the Concordance between Kcca and hierarchical cluster 
table(hc_6, clusters(hc_6flex))

# Task8- Creating the Five cluster solution
hc_5 <- cutree(hc, k = 5) 
table(hc_5)

# Task9- Checking the mean on each 5 cluster on each attitudinal variable and checking their Proportion of each cluster  
df1_std %>%
  mutate(hc_5 = factor(hc_5)) %>% # add the cluster assignment
  group_by(hc_5) %>% # group by cluster
  mutate(n = n()) %>% # calculate the n per group 
  summarise_all(~ mean(.x)) %>% # calculate the mean per group 
  mutate(prop = n/sum(n)) %>% # calculate the Proportion of each 
  print(width = Inf) # print all columns

# Segmentation plot for 5 clusters 
hc_5flex <- as.kcca(hc, df1_std, k = 5)

#Bar chart for Five Cluster 
barchart(hc_5flex , main= "Five Cluster-Segment Profile")

# checking the Concordance between Kcca and hierarchical cluster on 5  
table(hc_5, clusters(hc_5flex))

# labeling the Clusters 
hc_5<- factor(hc_5, levels = c(1,2,3,4,5), 
              labels = c("Furnishings Shoppers" , "Bargain Hunters"  , "Policy Pickers" , "Geeky Shoppers",  "Quality Pickers"))
levels(hc_5)

# Task 11 - Targeting the Segments 
df <- df %>% mutate(hc_5 = hc_5)
df %>%
  tabyl(hc_5, professional) %>% 
  adorn_totals(c("row", "col")) %>% 
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns() 

df %>%
  tabyl(hc_5, income) %>% 
  adorn_totals(c("row", "col")) %>% 
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns() 

df %>%
  tabyl(hc_5, age) %>% 
  adorn_totals(c("row", "col")) %>% 
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns() 

# Task 12- for reproducibility
set.seed(123)
# Running the kmeans with 5 cluster 
km <- kmeans(df1_std, centers = 5,iter.max = 1000, nstart = 100)
# print the cluster object
table(km$cluster)


#Task 13- concordance between the hclust-5 and kmeans procedures
table(hc_5, km$cluster)

#Hit rate 
(60+17+33+29+59)/200 






