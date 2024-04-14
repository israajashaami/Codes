library(readr)
library(skimr)
library(ggplot2)
library(dplyr)
library(GGally)
library(reshape2)

df <- read_csv('insurance_data.csv')

df <- df %>%
  mutate(across(c(1, 4:64, 86), as.factor))


# check the number of columns and rows and check if there are any missing values 

skim(df)
summary(df)

# check the outliers in the continuous variables

boxplot(df$num_houses ~ df$response, ylab="Number of Houses")
boxplot(df$hhsize ~ df$response, ylab="Avg size of household")

numeric_variables <- c("num_privatethirdparty", "num_thirdpartyfirms", "num_thirdpartyagriculture",
                       "num_car", "num_van", "num_motorcycle", "num_truck", "num_trailer",
                       "num_tractor", "num_agmachines", "num_scooter", "num_life",
                       "num_privateaccident", "num_familyaccidents", "num_disability",
                       "num_fire", "num_surfboard", "num_boat", "num_bicycle",
                       "num_property", "num_socialsecurity")


par(mfrow = c(5, 5), mar = c(2, 2, 2, 2))  # Adjust the rows, columns, and margins as needed

for (variable in numeric_variables) {
  boxplot(df[[variable]], main = variable, col = "skyblue", border = "black")
}

par(mfrow = c(1, 1))

# check if there are inconsistencies in the continuous variables

if (any(df$num_houses < 0) || any(df$hhsize < 0)) {
  print("There are negative values in 'num_houses' or 'hhsize'.")
} else {
  print("There are no negative values in 'num_houses' or 'hhsize'.")
}

# distribution of the response

barplot(table(df$response), col=c("red", "blue"), 
        main="Distribution of Response Variable",
        xlab="Response Variable", ylab="Frequency")
table(df$response)

# checking the relationship between Customer Sub and the response variable

ggplot(df, aes(x = customer_sub, fill = response)) +
  geom_bar(position = "dodge") +
  labs(title = "Count Plot of Response by Customer Sub",
       x = "Customer Sub",
       y = "Count",
       fill = "Response")
colnames(df)


#################################################################

# trial to eliminate the features
# Create a new variable for aggregated religious affiliation
df$agg_religion <- apply(df[, c("perc_catholic", "perc_protestant", "perc_otherreligion", "perc_noreligion")], 1, function(row) which.max(row) - 1)

# Mapping the aggregated values to corresponding labels
religion_labels <- c("Catholic", "Protestant", "Other Religion", "No Religion")

df$agg_religion <- factor(df$agg_religion, levels = 0:3, labels = religion_labels)

# visualize the relationship

ggplot(df, aes(x = agg_religion, fill = factor(response))) +
  geom_bar(position = "stack", stat = "count", prob = TRUE) +
  labs(title = "Stacked Bar Plot of Response by Aggregated Religion",
       x = "Aggregated Religion",
       y = "Count",
       fill = "Response") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Density plot


ggplot(df, aes(x = agg_religion, fill = factor(response))) +
  geom_bar(position = "fill", stat = "count") +
  labs(title = "Probability Plot of Response by Aggregated Religion",
       x = "Aggregated Religion",
       y = "Proportion",
       fill = "Response") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#548959859573687496874569874687 FROM THE LECTURE

# CALCULATE THE PERCENTAGES

resp_Religion<-table(df$response , df$agg_religion)
resp_Religion

round(prop.table(resp_Religion, 1),2)

#create data frame to plot percentages

AggRe <- df %>% 
  group_by(response,agg_religion)  %>%
  summarise (n = n()) %>%
  mutate(pct = 100*n / sum(n)) 

#differences broken out by category

ggplot(data=AggRe, mapping=aes(x=response, y=pct)) + 
  geom_bar(aes(fill=response),stat="identity") +
  facet_wrap( ~ agg_religion) +
  labs(x="Religion", y="Percent")

#stacked bar chart of percentages
ggplot(data=AggRe, mapping=aes(x=agg_religion,y=pct,fill=response)) + 
  geom_col() +
  labs(x="Religion", y="Percent") 

##############################################################################




## filter the response 

ggplot(filter(df, response == 1), aes(x = agg_religion, fill = factor(response))) +
  geom_bar(stat = "count") +
  labs(title = "Bar Plot of Response by Aggregated Religion (Response = 1)",
       x = "Aggregated Religion",
       y = "Count",
       fill = "Response") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Create a stacked bar plot for Age and Response
ggplot(df, aes(x = age, fill = response)) +
  geom_bar(position = "stack", stat = "count") +
  labs(title = "Stacked Bar Plot of Response by Age",
       x = "Age",
       y = "Count",
       fill = "Response") 

## filter the response

ggplot(filter(df, response == 1), aes(x = age, fill = response)) +
  geom_bar(position = "stack", stat = "count") +
  labs(title = "Bar Plot of Response by Age",
       x = "Age",
       y = "Count",
       fill = "Response") 

# distribution of home owners

ggplot(df, aes(x = perc_homeowners)) +
  geom_bar(position = "stack", stat = "count")+
  labs(title = "Stacked Bar Plot of Percentage of Home Owners")

# Create a stacked bar plot for the Average income and Response

ggplot(df, aes(x = perc_avgincome, fill = response)) +
  geom_bar(position = "stack", stat = "count") +
  labs(title = "Stacked Bar Plot of Response by  Average income",
       x = " Average income",
       y = "Count",
       fill = "Response") 





# Class work 

df <- df %>% mutate_at(4:86, as.factor)
df$customer_sub <- as.factor(df$customer_sub)

skim(df)

boxplot(df$num_houses ~ df$response, ylab="Number of Houses")
boxplot(df$hhsize ~ df$response, ylab="Avg size of household")




# scatter plot for the relationship between the continuous variables and the response variable

ggplot(df, aes(x = num_houses, y = hhsize, color = response)) +
  geom_point() +
  labs(title = "Scatter Plot of Numerical Variables with Response")

ggplot(df, aes(x = num_houses, y = hhsize, color = response)) +
  geom_point() +
  labs(title = "Scatter Plot of Numerical Variables with Response") +
  facet_wrap(~ response)

# create a heatmap to explore the relationship between the numerical variables at the customer level

numeric_features <- df %>%
  select( num_privatethirdparty, num_thirdpartyfirms, 
          num_thirdpartyagriculture, num_car, num_van, num_motorcycle, num_truck, num_trailer,
         num_tractor, num_agmachines, num_scooter, num_life,
         num_privateaccident, num_familyaccidents, num_disability,
         num_fire, num_surfboard, num_boat, num_bicycle,
         num_property, num_socialsecurity)
summary(numeric_features)

correlation_matrix <- cor(numeric_features)
melt_corr <- melt(correlation_matrix)

ggplot(melt_corr, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0)

# customer main type

ggplot(df, aes(x = customer_main, fill = factor(response))) +
  geom_bar(position = "dodge", color = "black", show.legend = FALSE) +
  labs(
    title = "Distribution of Customer Main Type with Response",
    x = "Customer Main Category",
    y = "Count"
  ) +
  facet_wrap(~ response)

# pie chart for age
ggplot(df, aes(x = "", fill = age)) +
  geom_bar(width = 1, stat = "count") +
  coord_polar("y") +
  labs(title = "Pie Chart of Age",
       fill = "age")+ 
  theme_minimal()

ggplot(df, aes(x = num_life, fill = num_familyaccidents)) +
  geom_bar(position = "stack") +
  labs(title = "Stacked Bar Plot of Two Categorical Variables",
       x = " Number of life insurances",
       fill = " num_familyaccidents")


ggplot(df, aes(x = perc_highstatus)) +
  geom_bar(position = "stack", stat = "count")+
  labs(title = "Bar Plot of Percentage of  High status")






#############################################################


# Function to create a matrix of boxplots
create_boxplot_matrix <- function(data, numeric_vars, response_var, rows, cols) {
  par(mfrow = c(rows, cols), mar = c(2, 2, 2, 2))  # Adjust the rows, columns, and margins as needed
  
  for (numeric_var in numeric_vars) {
    boxplot(data[[numeric_var]] ~ data[[response_var]], main = numeric_var, col = "skyblue", border = "black")
  }
  
  # Reset the layout
  par(mfrow = c(1, 1))
}

# List of numeric variables
numeric_variables <- c("num_privatethirdparty", "num_thirdpartyfirms", "num_thirdpartyagriculture",
                       "num_car", "num_van", "num_motorcycle", "num_truck", "num_trailer",
                       "num_tractor", "num_agmachines", "num_scooter", "num_life",
                       "num_privateaccident", "num_familyaccidents", "num_disability",
                       "num_fire", "num_surfboard", "num_boat", "num_bicycle",
                       "num_property", "num_socialsecurity")

# Response variable
response_variable <- "response"  

# Create boxplot matrix
create_boxplot_matrix(df, numeric_variables, response_variable, 5, 5)


################################################




################################################

# split the numeric features and explore the correlation with the response

numeric_features_1 <- df %>%
  select( num_privatethirdparty, num_thirdpartyfirms, 
          num_thirdpartyagriculture, num_car, num_van, num_motorcycle, num_truck, num_trailer,
          num_tractor, num_agmachines)

ggpairs(df, columns = names(numeric_features_1), aes(color = response), legend = 1 
) +
  theme(legend.position = "bottom") 


numeric_features_2 <- df %>%
  select( num_scooter, num_life,
          num_privateaccident, num_familyaccidents, num_disability,
          num_fire, num_surfboard, num_boat, num_bicycle,
          num_property, num_socialsecurity)

ggpairs(df, columns = names(numeric_features_2), aes(color = response), legend = 1 
) +
  theme(legend.position = "bottom") 

################################################


## count and plot the total number of insurances

insurance_columns <- c("num_privatethirdparty", "num_thirdpartyfirms", "num_thirdpartyagriculture",
                       "num_car", "num_van", "num_motorcycle", "num_truck", "num_trailer",
                       "num_tractor", "num_agmachines", "num_scooter", "num_life",
                       "num_privateaccident", "num_familyaccidents", "num_disability", "num_fire",
                       "num_surfboard", "num_boat", "num_bicycle", "num_property", "num_socialsecurity")

# Create the new column 'total_num_insurance'

df$total_num_insurance <- rowSums(df[insurance_columns], na.rm = TRUE)


## plot the proportion 

ggplot(df, aes(x = total_num_insurance, fill = factor(response))) +
  geom_bar(position = "fill", stat = "count") +
  labs(title = "Proportion Plot of Response by Total Number of Insurances",
       x = "Total Number of Insurances",
       y = "Proportion",
       fill = "Response") +
  scale_x_continuous(breaks = seq(1, 16, by = 1))

## plot the probability 

# Calculate probabilities
probabilities <- df %>%
  group_by(total_num_insurance, response) %>%
  summarise(count = n()) %>%
  mutate(probability = count / sum(count))

# Create a bar plot with probabilities
ggplot(probabilities, aes(x = total_num_insurance, y = probability, fill = factor(response))) +
  geom_bar(position = "fill", stat = "identity") +
  geom_text(aes(label = scales::percent(probability)), position = position_fill(vjust = 0.5), color = "black") +
  labs(title = "Probability Plot of Response by Total Number of Insurances",
       x = "Total Number of Insurances",
       y = "Probability",
       fill = "Response") +
  scale_x_continuous(breaks = seq(1, 16, by = 1))

# calculate the percantages

agg_status <- table(df$response, df$total_num_insurance)
agg_status

status_perc <- round((agg_status/8840)*100,3)
status_perc

df <- as.data.frame.matrix(status_perc)

df2 <- df
df2$base <- rownames(df)
df2 <- melt(df2)

ggplot(df2, aes(fill = base, y = value, x = variable)) + geom_bar(stat = "identity") +
  labs(title = "Stacked Bar Plot of Response by Aggregated Total Insurances",
       x = "Total Insurances",
       y = "Percentage (%)",
       fill = "Response")


#548959859573687496874569874687 FROM THE LECTURE

# CALCULATE THE PERCENTAGES

resp_total_insurance<-table(df$response , df$total_num_insurance)
resp_total_insurance

round(prop.table(resp_total_insurance, 1),2)

#create data frame to plot percentages

totalnumofinsurances <- df %>% 
  group_by(response,total_num_insurance)  %>%
  summarise (n = n()) %>%
  mutate(pct = 100*n / sum(n)) 

#differences broken out by category

ggplot(data=totalnumofinsurances, mapping=aes(x=response, y=pct)) + 
  geom_bar(aes(fill=response),stat="identity") +
  facet_wrap( ~ total_num_insurance) +
  labs(x="Total Number of Insurances", y="Percent")

#stacked bar chart of percentages
ggplot(data=totalnumofinsurances, mapping=aes(x=total_num_insurance,y=pct,fill=response)) + 
  geom_col() +
  labs(x="Total Number of Insurances", y="Percent") +
  scale_x_continuous(breaks = seq(0, 16, by = 1))

##########################################################################################################

theme_set(theme_bw())  # pre-set the bw theme.
g <- ggplot(df, aes(total_num_insurance, age))
g + geom_count(col="tomato3", show.legend=F) +
  labs(subtitle="", 
       y="age", 
       x="total_num_insurance", 
       title="Counts Plot") +
  scale_x_continuous(breaks = seq(0, 15, by = 1))


g <- ggplot(df, aes(num_houses, hhsize, color = response))
g + geom_count(show.legend=F) +
  labs(subtitle="", 
       y="hhsize", 
       x="num_houses", 
       title="Counts Plot") +
  facet_wrap(~ response)


# to visualize 2 categorical variables 


df %>% 
  count(perc_homeowners, total_num_insurance) %>%  
  ggplot(mapping = aes(x = perc_homeowners, y = total_num_insurance)) +
  geom_tile(mapping = aes(fill = n)) +
  scale_y_continuous(breaks = seq(0, 15, by = 1))

###########################################################################################################
