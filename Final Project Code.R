plastic <- read.csv("https://raw.githubusercontent.com/C-DeLeon/BUAN-314-Project/refs/heads/main/Plastic%20Waste%20Around%20the%20World%20-%20Plastic%20Waste%20Around%20the%20World.csv")

gdp <- read.csv ("https://raw.githubusercontent.com/C-DeLeon/BUAN-314-Project/refs/heads/main/gdp_pcap.csv")

population <- read.csv("https://raw.githubusercontent.com/C-DeLeon/BUAN-314-Project/refs/heads/main/population_density_per_square_km.csv")

urban <- read.csv("https://raw.githubusercontent.com/C-DeLeon/BUAN-314-Project/refs/heads/main/urban_population.csv")

child_mort<- read.csv("https://raw.githubusercontent.com/C-DeLeon/BUAN-314-Project/refs/heads/main/child_mortality_0_5_year_olds_dying_per_1000_born.csv")

library(tidyverse)
library(sqldf)
#############CLEANING DATA##########################
#cleaning both larger dataset to get a table of only the country names and the data from 2023
gdp_2023 <- gdp %>%
  select(country, X2023)

pop_2023 <- population %>%
  select(country, X2023)


#changing any abbreviated names to match the names of countries in the plastic waste dataset
gdp_2023_clean <- gdp_2023 %>% 
  mutate(country = str_replace_all(country, "USA", "United States"))%>%
  mutate(country = str_replace_all(country, "UK", "United Kingdom"))%>%
  mutate(country = str_replace_all(country, "Congo, Dem. Rep.", "Democratic Republic of Congo"))%>%
  mutate(country = str_replace_all(country, "Congo, Rep.", "Republic of Congo"))

pop_2023_clean <- pop_2023 %>% 
  mutate(country = str_replace_all(country, "USA", "United States"))%>%
  mutate(country = str_replace_all(country, "UK", "United Kingdom"))%>%
  mutate(country = str_replace_all(country, "Congo, Dem. Rep.", "Democratic Republic of Congo"))%>%
  mutate(country = str_replace_all(country, "Congo, Rep.", "Republic of Congo"))

#merging gdp per capita and population per capita together

table1 <- "SELECT gdp_2023_clean.country AS Country,
              gdp_2023_clean.x2023 AS gdp_percap,
              pop_2023_clean.x2023 AS pop_percap
       FROM gdp_2023_clean
       INNER JOIN pop_2023_clean
       USING (country)"

gdp_pop <- sqldf(table1)

#merging the new gdp_pop dataset with the plastic waste dataset 

merge1 <- "SELECT gdp_pop.Country AS Country,
              gdp_pop.gdp_percap AS Gdp_percap,
              gdp_pop.pop_percap AS Pop_percap,
              plastic.Total_Plastic_Waste_MT AS Total_Plastic_Waste_MT,
              plastic.Main_Sources AS Main_Sources,
              plastic.Recycling_Rate AS Recylcing_Rate,
              plastic.Per_Capita_Waste_KG AS Per_Capita_Waste_KG,
              plastic.Coastal_Waste_Risk AS Coastal_Waste_Risk
       FROM gdp_pop
       INNER JOIN plastic
       USING (Country)"

plastic_full <- sqldf(merge1)

#reformatting the GDP per capita values to be all numeric values
#larger values have a k after the number 

plastic_full1 <- plastic_full
plastic_full1$Gdp_percap <- as.numeric(gsub("k", "", plastic_full$Gdp_percap)) * 1000


#converting total waste and per capita waste into the same units of kilograms 

plastic_full2 <- plastic_full1
plastic_full2$Total_Plastic_Waste_MT <- plastic_full2$Total_Plastic_Waste_MT * 1000

plastic_clean <- plastic_full2

#adding more data

urban.1 <- urban%>%
  select(Country.Name, X2023..YR2023.)

urban_2 <- urban.1%>%
  rename('2023' = X2023..YR2023.)%>%
  rename('Country'= Country.Name)%>%
  mutate(Country = str_replace_all(Country, "USA", "United States"))%>%
  mutate(Country = str_replace_all(Country, "UK", "United Kingdom"))%>%
  mutate(Country = str_replace_all(Country, "Congo, Dem. Rep.", "Democratic Republic of Congo"))%>%
  mutate(Country = str_replace_all(Country, "Congo, Rep.", "Republic of Congo"))%>%
  mutate(Country = str_replace_all(Country, "Viet Nam", "Vietnam"))
  

merge2 <- "SELECT urban_2.Country AS Country,
              urban_2.'2023' AS Urban_population,
              plastic_full2.Total_Plastic_Waste_MT AS Total_Plastic_Waste_MT,
              plastic_full2.Main_Sources AS Main_Sources,
              plastic_full2.Recylcing_Rate AS Recycling_Rate,
              plastic_full2.Per_Capita_Waste_KG AS Per_Capita_Waste_KG,
              plastic_full2.Coastal_Waste_Risk AS Coastal_Waste_Risk,
              plastic_full2.Gdp_percap,
              plastic_full2.pop_percap AS Population_density
       FROM urban_2
       LEFT JOIN plastic_full2
       USING (Country)"

plastic_full3 <- sqldf(merge2)

#adding child mortality rate to data
child_mort1 <- child_mort%>%
  select(country,X2023)

child_mort2 <- child_mort1%>%
  rename('Child_mortality_rate' = X2023)%>%
  rename('Country'= country)%>%
  mutate(Country = str_replace_all(Country, "USA", "United States"))%>%
  mutate(Country = str_replace_all(Country, "UK", "United Kingdom"))%>%
  mutate(Country = str_replace_all(Country, "Congo, Dem. Rep.", "Democratic Republic of Congo"))%>%
  mutate(Country = str_replace_all(Country, "Congo, Rep.", "Republic of Congo"))


merge4 <- "SELECT child_mort2.Country,
              child_mort2.Child_mortality_rate ,
              plastic_full3.Total_Plastic_Waste_MT AS Total_Plastic_Waste_MT,
              plastic_full3.Main_Sources AS Main_Sources,
              plastic_full3.Recylcing_Rate AS Recycling_Rate,
              plastic_full3.Per_Capita_Waste_KG AS Per_Capita_Waste_KG,
              plastic_full3.Coastal_Waste_Risk AS Coastal_Waste_Risk,
              plastic_full3.Gdp_percap,
              plastic_full3.pop_percap AS Population_density,
              plastic_full3.Urban_population
       FROM child_mort2
       INNER JOIN plastic_full3
       USING (Country)"

plastic_full4 <- sqldf(merge4)

#adding Continents
install.packages("countrycode")
library(countrycode)

plastic_full4$continent <- countrycode(plastic_full4$Country, "country.name", "continent")

#updating that all category names are uniform un Main_Sources variable
plastic_cleaned <- plastic_full4%>%
  rename('Total_Plastic_Waste_KG'= Total_Plastic_Waste_MT)%>%
  mutate(Main_Sources = str_replace_all(Main_Sources, "Packaging_Consumer", "Consumer_Packaging"))%>%
  mutate(Main_Sources = str_replace_all(Main_Sources, "Packaging_Electronics", "Electronics_Packaging"))%>%
  mutate(Main_Sources = str_replace_all(Main_Sources, "Packaging_Industrial", "Industrial_Packaging"))
  


write.csv(plastic_cleaned, "/Users/brianna/Desktop/BUAN-314-Project/Cleaned Plastic Data.csv", row.names = FALSE)

#############################
#####Query + Visualization 1####
 







#####Query + Visualization 2####







#####Query + Visualization 3####






#####Query + Visualization 4####








#####Query + Visualization 5####

total_waste <-"SELECT continent, SUM(Total_Plastic_Waste_KG) AS Total_Waste
                FROM plastic_cleaned
                GROUP BY continent" 

total_waste<-sqldf(total_waste)

waste_by_source <- "SELECT continent, Main_Sources, SUM(Total_Plastic_Waste_KG) AS Continent_Waste
                FROM plastic_cleaned
                GROUP BY continent, Main_sources"
waste_by_source <- sqldf(waste_by_source)

proportion_breakdown <-  total_waste%>%
  left_join(waste_by_source, by = "continent")%>%
  mutate(Proportion = Continent_Waste/Total_Waste)

ggplot(plastic_cleaned, aes(x = continent, y = Total_Plastic_Waste_KG, fill = Main_Sources)) +
  geom_bar(stat = "identity", position = "fill") +  
  labs(title = "Proportional Plastic Waste Breakdown by Continent",
       y = "Proportion",
       x = "Continent") 


######Query + Visualization 6###
library(ggplot2)

Very_high <- 'SELECT Gdp_percap, Per_capita_waste_KG, Urban_population
      FROM plastic_cleaned
      WHERE Coastal_Waste_Risk = "Very_High"'

High <- 'SELECT Gdp_percap, Per_capita_waste_KG, Urban_population
      FROM plastic_cleaned
      WHERE Coastal_Waste_Risk = "High"'

Medium <- 'SELECT Gdp_percap, Per_capita_waste_KG, Urban_population
      FROM plastic_cleaned
      WHERE Coastal_Waste_Risk = "Medium"'

Low <- 'SELECT Gdp_percap, Per_capita_waste_KG, Urban_population
      FROM plastic_cleaned
      WHERE Coastal_Waste_Risk = "Low"'

sqldf(Very_high)

print((sqldf(Very_high)))

sqldf(High)
sqldf(Medium)
sqldf(Low)



ggplot(plastic_cleaned, aes(x = Gdp_percap, y = Per_Capita_Waste_KG)) +
  geom_point(aes(size = Urban_population, color = Coastal_Waste_Risk), alpha = 0.7) +
  scale_x_log10() + 
  scale_y_log10() + 
  labs(
    title = "GDP per Capita vs. Per Capita Waste",
    x = "GDP Per Capita (Log Scale)",
    y = "Per Capita Waste (Log Scale)") 



########QUERY + VISUALIZATION 7############

str(plastic_cleaned)


numeric_plastic <- plastic_cleaned %>%
  select_if(is.numeric)

install.packages("reshape 2")
library(reshape2)

cor_matrix <- cor(numeric_plastic)
cor_matrix_melted <- melt(cor_matrix)

ggplot(cor_matrix_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "#619CFF", high = "#F8766D", mid = "white", midpoint = 0,
                       limit = c(-1, 1), name = "Correlation") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3)+
  theme_minimal() +
  labs(title = "Correlation Heatmap") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

waste_urban <- 'SELECT continent,SUM(Urban_population) AS Total_Urban_Population,
                SUM(Total_Plastic_Waste_KG) AS Total_Plastic_Waste
                FROM plastic_cleaned
                GROUP BY continent
                ORDER BY Total_Plastic_Waste DESC;'

sqldf(waste_urban)

########QUERY + VISUALIZATION 8############

ggplot(plastic_cleaned, aes(x = Population_density, y = Total_Plastic_Waste_KG, group = continent, color = continent)) +
  geom_line() +  
  geom_smooth(method = "lm", aes(color = continent), linetype = "solid") +  
  scale_x_log10() +  
  scale_y_log10() +  
  labs(title = "Trend of Plastic Waste by Continent") +
  facet_wrap(~ continent)  

top_waste <- 'SELECT Country, Population_density, Total_Plastic_Waste_KG, continent
            FROM plastic_cleaned
            ORDER BY Total_Plastic_Waste_KG DESC
            LIMIT 10;'

sqldf(top_waste)

top_pop <- 'SELECT Country, Population_density, Total_Plastic_Waste_KG, continent
            FROM plastic_cleaned
            ORDER BY Population_density DESC
            LIMIT 10;'
sqldf(top_pop)


########QUERY + VISUALIZATION 9############
#Completed by Talia


#impact of child morality rate and coastal waste risk on plastic waste
#Do countries with high child mortality rates and coastal waste risks generate more plastic waste?

Q9 <- "SELECT Country, Child_mortality_rate, Coastal_Waste_Risk, SUM(Total_Plastic_Waste_KG) AS total_plastic
FROM plastic
GROUP BY Country, Child_mortality_rate, Coastal_Waste_Risk
ORDER BY Child_mortality_rate DESC, Coastal_Waste_Risk DESC;"
Q9 <- sqldf(Q9)

#Q9 USE THIS ONE
ggplot(Q9, aes(x = Child_mortality_rate, y = Coastal_Waste_Risk, size = total_plastic, color = total_plastic)) +
  geom_point(alpha = 0.7) +
  scale_color_gradient(low = "green", high = "red") +
  labs(
    title = "Impact of Child Mortality and Coastal Waste Risk on Plastic Waste",
    x = "Child Mortality Rate",
    y = "Coastal Waste Risk",
    size = "Total Plastic Waste (KG)"
  ) +
  theme_minimal()


########QUERY + VISUALIZATION 10############
#Completed by Talia

#child mortality and plastic waste 
#Do countries with higher child mortality rates produce more plastic waste, and are they less likely to recycle?
Q10 <- "SELECT country, Child_mortality_rate, Total_Plastic_Waste_KG, Recycling_Rate
FROM plastic
ORDER BY Child_mortality_rate DESC;"
Q10 <- sqldf(Q10)


# USE THIS VERSION Q10
ggplot(Q10_correlation, aes(x = reorder(Country, Child_mortality_rate), y = Total_Plastic_Waste_KG, fill = Recycling_Rate)) +
  geom_bar(stat = "identity", color = "black") +
  labs(
    title = "Top 25 Countries: Child Mortality and Plastic Waste Correlation",
    x = "Country",
    y = "Total Plastic Waste (KG)",
    fill = "Recycling Rate (%)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

########QUERY + VISUALIZATION 11###########
#Completed by Talia


#Urbanization and Recycling Effectiveness #Do countries with higher urban populations recycle more effectively?
Q11 <- "SELECT country, Urban_population, Recycling_Rate
FROM plastic
ORDER BY Urban_population DESC;"
Q11 <- sqldf(Q11)

#TOP 25 Q7 USE THIS ONE
Q11_top25 <- Q11 %>%
  arrange(desc(Urban_population)) %>%
  head(25)

ggplot(Q11_top25, aes(x = reorder(Country, -Urban_population), y = Recycling_Rate, group = 1)) +
  geom_line(color = "darkgreen", linewidth = 1) +
  geom_point(color = "blue", size = 2) +
  labs(
    title = "Top 25 Countries: Urbanization and Recycling Effectiveness",
    x = "Country",
    y = "Recycling Rate (%)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

########QUERY + VISUALIZATION 12############
#Completed by Talia

#Child Mortality Rate, population density, and Main Sources of Plastic Waste
#Are countries with high child mortality rates and high population densities more dependent on specific sources of plastic waste?

Q12 <- "SELECT Country, Child_mortality_rate, Population_density, Main_Sources, SUM(Total_Plastic_Waste_KG) AS total_waste
FROM plastic_cleaned
GROUP BY Country, Child_mortality_rate, Population_density, Main_Sources
ORDER BY Child_mortality_rate DESC, Population_density DESC;"

Q12 <- sqldf(Q12)

#Q12 
ggplot(Q12, aes(x = Population_density, y = Child_mortality_rate, size = total_waste, color = Main_Sources)) +
  geom_point(alpha = 0.7) +  # Add bubbles with transparency
  facet_wrap(~ Main_Sources, scales = "free") +  # Facet by Main Sources of Plastic Waste
  scale_size_continuous(name = "Total Plastic Waste (KG)") +  # Bubble size for total waste
  scale_color_brewer(palette = "Set2", name = "Main Sources") +  # Add color for main sources
  labs(
    title = "Correlation Between Main Sources of Plastic Waste and Child Mortality",
    x = "Population Density",
    y = "Child Mortality Rate (0-5)"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 10),  # Adjust facet label size
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
  )


