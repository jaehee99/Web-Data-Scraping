# load packages
library(rvest)
library(tidyverse)
library(stringr)
library(keyring)
library(httr)

# Use read_html() to save an HTML file to a variable.
html_obj <- read_html("https://www.imdb.com/search/title/?count=100&groups=oscar_best_picture_winners&sort=year%2Cdesc&ref_=nv_ch_osc")
class(html_obj)

Data <- html_nodes(html_obj,
                   css = ".ghost~ .text-muted+ span , .genre , .runtime ,
.certificate , strong , .favorable , .unbold , .lister-item-header a")

Data_text <- html_text(Data)

datadf <- tibble(text = Data_text)

datadf %>%
  mutate(ismovierank = str_detect(text, "^\\d+\\.$")) -> datadf

datadf %>%
  mutate(movienum = cumsum(ismovierank)) %>%
  filter(movienum > 0) -> datadf

datadf %>%
  mutate(isyear = str_detect(text, "\\(\\d{4}\\)")) -> datadf

datadf %>%
  mutate(isgenre = str_detect(text, "^\\n")) -> datadf

datadf %>% 
  mutate(isStarRating = str_detect(text, "^\\d+\\.+\\d$")) -> datadf

datadf %>% 
  mutate(isGross_Receipts = str_detect(text, "^\\$")) -> datadf

data0 <- html_nodes(html_obj, ".lister-item-header a")
data0_text <- html_text(data0)
datadf %>%
  mutate(isname = text %in% data0_text) -> datadf

datadf %>%  
  mutate(islength = str_detect(text,"^\\d{1,3}+\\s+min$")) -> datadf

datadf %>%
  mutate(isMetascore_Rating = str_detect(text, "^\\d{1,3}+\\s+$")) -> datadf

datadf %>%
  mutate(isMPAA = !ismovierank & !isyear & !isgenre & !isStarRating & !isGross_Receipts & !isname &!isMetascore_Rating &!islength) ->datadf
head(datadf)
## make sure we have 93 movies:
sum(datadf$isname)

datadf %>%
  mutate(key = case_when(ismovierank ~ "rank",
                         isyear ~ "year",
                         isgenre ~ "genre",
                         islength ~ "length",
                         isMetascore_Rating ~ "metacritic", 
                         isGross_Receipts ~ "gross_receipts", 
                         isStarRating ~ "star_rating", 
                         isMPAA ~ "MPAA",
                         isname ~ "name")) %>%
  select(key, text, movienum) %>%
  pivot_wider(names_from = key, values_from = text) -> datawide
head(datawide)

datawide %>%
  mutate(length = str_extract_all(length, "[0-9]+")) %>% 
  mutate(year = str_extract_all(year, "[0-9]+")) %>% 
  mutate(gross_receipts = str_sub(gross_receipts, 2, -2)) %>% 
  mutate(genre = str_replace_all(genre, "\\n", ""),
         genre = str_squish(genre)) %>% 
  mutate(movienum=NULL) -> datawide
head(datawide)

# change the datatype 
datawide$year <- as.numeric(datawide$year)
datawide$length <- as.numeric(datawide$length)
datawide$star_rating <- as.numeric(datawide$star_rating)
datawide$metacritic <- as.double(datawide$metacritic)
datawide$gross_receipts <- as.numeric(datawide$gross_receipts)

# see the summary and str of the data
str(datawide)
summary(datawide)

# Show a summary of the number of data elements for each movie that are not NA. Which two elements are missing the most from the movies?
# 1 method
datawide %>% 
  summarise_all(funs(sum(!is.na(.))))
# 2 method
sum(is.na(datawide$rank))
sum(is.na(datawide$name))
sum(is.na(datawide$MPAA))
sum(is.na(datawide$length))
sum(is.na(datawide$genre))
sum(is.na(datawide$star_rating))
sum(is.na(datawide$metacritic))
sum(is.na(datawide$gross_receipts))
# The two elements that are missing the most from the movies: metaScore_Rating and gross_receipts

# Create a plot of the length of a film and its gross, color coded by rating. Show linear smoothers for each rating.
# - Does MPAA rating matter?  
sum(datadf$islength)
sum(datadf$isGross_Receipts)

ggplot(data = datawide, aes(x = length, y = gross_receipts, color = MPAA)) +
  geom_smooth(method = "lm", se = FALSE)+
  geom_point()

# Does MPAA rating matter?
# Yes and No. When MPAA is "PG-13", length and gross_receipts have a positive correlation, so MPAA rating matters. Also when MPAA is "Passed", length and gross_receipts have a positive correlation, so MPAA rating matters. However, other MPAA show that the lines are almost horizontal to x-axis, therefore, there is no correlation between length and gross_receipts, so it doesn't matter for other MPAA.


# Create a plot with a single Ordinary Least Squares smoothing line with no standard errors showing for predicting stars rating based on metacritic scores for those movies that have metacritic scores. 
# - Use a linear model to assess if there is there a meaningful relationship. Show the summary of the output and interpret in terms of the $p$-value and the adjusted R-Squared?

datawide %>% 
  ggplot(aes(y = star_rating, x = metacritic)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

data_3 <- lm(metacritic ~ star_rating, data = datawide)
summary(data_3)

# Interpretation: 
# p-value is 0.007443 which is smaller than significance level( =0.01),this will reject the null hypothesis and this tells us that this is statistically significant. Adusted R-squared is 0.08059, this means that we can only explain 8% of this model, indicating that this is not a good model. Since Adjusted R2 indicates how well terms fit a curve or line. When we see the graph above, we can easily see that a lot of points do not well align with the blue line. Therefore, I feel this is not a good model: This linear model doesn't show that there is a meaningful relationship between star_rating and metaScore_rating. 

# Use an appropriate plot to compare the gross receipts by MPAA rating.
str(datawide)
datawide %>%  
  group_by(as.factor(MPAA)) %>% 
  ggplot(aes(y = gross_receipts, x = MPAA)) +
  geom_boxplot() 

# Which MPAA rating has the highest median gross receipts? 
# The black line inside the box is the median. We can see that when MPAA is "PG-13", the median gross_receipts is the highest.

# Which R-rated movies are in the overall top 10 of gross receipts?
datawide %>%  
  filter(MPAA == "R") %>%  
  arrange(desc(gross_receipts)) %>%  
  head(10) %>%  
  select(name)

# Use one-way analysis of variance to assess the level of evidence for whether all ratings have the same mean gross receipts. Show the summary of the results and provide your interpretation of the results.
# Compute the analysis of variance
one.way <- aov(gross_receipts ~ MPAA, data = datawide)
# Summary of the analysis
summary(one.way)

# Interpretation: 
# As the p-value is bigger than the significance level 0.05, we can conclude that there are not significant differences between the groups.
