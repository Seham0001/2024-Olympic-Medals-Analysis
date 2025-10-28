## import file to R program 
medals <- read.csv("C:/Users/DELL/Downloads/medals.csv")
View(medals)

## Check for missing values and handle them appropriately (if any)
missing_values= is.na(medals)
print(missing_values)
count_null= sum(missing_values); count_null

# there is one Null value in the data
# Filter rows with NA values to find which column contains the null 

library(dplyr)
rows_with_na_dplyr <- medals %>% filter(if_any(everything(), is.na))
print(rows_with_na_dplyr)

# I found a null value in the column (medal_code). Since it pertains to a bronze medal, I will assign the missing value as "3," which is the correct code for bronze. 
medals[470,2]<- 3
medals[470,]
# 	Ensure data types are correct for each column.
str(medals)

# I found out the the column(medal_date) is 'character' and need to change into daytime
class(medals$medal_date)
medals$medal_date<- as.Date(medals$medal_date, format = "%Y-%m-%d")


# check the type of column(medal_date)
str(medals$medal_date)

#	Generate a summary of the dataset's statistics.

summary(medals)


#Find the total number of record in the dataset.

nrow(medals)

#Identify which countries has obtained highest number of Gold, Silver and Bronze medals
# Count medals by country and medal type
library(dplyr)
medal_summary <- medals %>%
  group_by(country, medal_type) %>%
  summarize(count = n(), .groups = 'drop') %>%
  arrange(country, medal_type)

install.packages("tidyr")
library(tidyr)

# Pivot the data to have medals as columns
medal_counts <- medal_summary %>%
  pivot_wider(names_from = medal_type, values_from = count, values_fill = 0)

# Rename columns for clarity 
colnames(medal_counts) <- c("country", "Bronze", "Gold", "Silver")


# Identify countries with the most medals for each type
top_medals <- medal_counts %>%
  summarise(
    Top_Gold = country[which.max(Gold)],
    Top_Silver = country[which.max(Silver)],
    Top_Bronze = country[which.max(Bronze)],
    Max_Gold = max(Gold),
    Max_Silver = max(Silver),
    Max_Bronze = max(Bronze)
  )

# Display the results
print(top_medals)



#Identify the top 3 medallists

top_medalist<- medals%>%group_by(name)%>%summarise(top_medalist= n())%>%arrange(desc(top_medalist))%>%head(3)
cat('Top 3 medalists:\n')
print(top_medalist)

## I notice that the column(name) doesn't have exactly the medalist name but it was with their country's name


#Create visualizations to show the distribution of medals (you may choose any of the columns as you wish)


 # Create a table of medal counts
 medal_counts <- table(medals$medal_type)
 
 # Calculate percentages
 medal_percentage <- round(100 * medal_counts / sum(medal_counts), 1)
 
 # Create labels for the pie chart
 labels <- paste(names(medal_counts), "(", medal_percentage, "%)", sep = "")
 
 # Specify custom colors
 custom_colors <- c("pink", "gray", "white")  
 
 # Create the pie chart with custom colors
 pie(medal_counts, labels = labels, main = 'Medals Distribution by Medal Type', col = custom_colors)
 
        
#	Aggregate the data to calculate total Gold medals obtained by “Team” category for France

total_gold_team_france <- medals %>%
  filter(country == "France", medal_type == "Gold Medal", grepl("TEAM", event_type)) %>%  # Filter for France, Gold, and Team
  summarise(total_gold = n())  # Count the number of rows

# Display the result
filter(total_gold_team_france)


#Update the gender abbreviation for female from ‘W’ to ‘F’
medals$gender<- sub('W', 'F', medals$gender)

##Which country has the highest female medallists?

highest_female_medallists <- medals %>%
  filter(gender == "F") %>%  # Filter for female medallists
  group_by(country) %>%           # Group by country
  summarise(Total_Female_Medallists = n_distinct(name), .groups = 'drop') %>%  # Count unique female medallists
  slice_max(order_by = Total_Female_Medallists, n = 1)

print(highest_female_medallists)

##Which country come out as the top achiever in Athletics discipline

top_athletics_country <- medals %>%    
  filter(event_type == "ATH") %>%  
  group_by(country) %>%                  
  summarise(Total_Medals = n(), .groups = 'drop') %>%  
  slice_max(order_by = Total_Medals, n = 1) 
print(top_athletics_country)

##Calculate the total medals obtained by the country for man and.  woman category. 
total_medals_by_gender <- medals %>%
  group_by(country, gender) %>%          
  summarise(Total_Medals = n(), .groups = 'drop')  
print(total_medals_by_gender)

##	Visualize your result.
# Load necessary libraries
library(dplyr)
library(ggplot2)

total_medals_by_gender <- medals %>%
  group_by(gender) %>%          
  summarise(Total_Medals = n(), .groups = 'drop')  

# Print the summary data frame
print(total_medals_by_gender)

# Create a pie chart
ggplot(total_medals_by_gender, aes(x = "", y = Total_Medals, fill = gender)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  theme_void() +  # Removes the background and axes
  labs(title = "Total Medals by Gender") +
  theme(legend.title = element_blank())

##Calculate and present the Gold Metals performance for top 10 countries.
top_gold_medals <- medals %>%
  filter(trimws(medal_type) == "Gold Medal") %>%  
  group_by(country) %>%
  summarise(Total_Gold_Medals = n(), .groups = 'drop') %>%
  arrange(desc(Total_Gold_Medals)) %>%
  slice_head(n = 10)
print(top_gold_medals)

#visulaize the result


# Create a bar chart with countries ordered from highest to lowest gold medals
ggplot(top_gold_medals, aes(x= reorder(country, -Total_Gold_Medals), y = Total_Gold_Medals)) +
  geom_bar(stat = "identity", fill = "gold") +
  labs(title = "Top 10 Countries by Gold Medals",
       x = "Total Gold Medals",
       y = "Country") +
  theme_minimal()



