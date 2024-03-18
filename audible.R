#load tidyverse package
library(tidyverse)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(RColorBrewer)

#read in audible csv 
audible<- read_csv("C:/Users/SAMMY/Desktop/Audible/audible_uncleaned.csv")


#examine dataset 
view(audible)
head(audible,20)



#author and narrator column cleaned of unwanted characters ('Writtenby:' and 'Narratedby:') respectively
#and content of the columns properly formatted
audible<-audible %>%mutate_at(vars(author,narrator),
                              ~if_else(str_detect(.,"(Writtenby:)|(Narratedby:)"),
                                       str_replace_all(str_remove(.,"(Writtenby:)|(Narratedby:)|\\d+"),"(?<=[a-z])(?=[A-Z])", " "),
                                       .)
) 

# Create new columns off the time column and typecast to numeric 
audible <- audible %>%
  mutate(
    # Extract hours and minutes into separate columns
    hours = as.numeric(str_extract(time, "\\d+(?=\\s*hr)")),
    minutes = as.numeric(str_extract(time, "\\d+(?=\\s*min)")),
    
    # Convert time to seconds
    time_seconds = case_when(
      str_detect(time, "hrs|hr") & !str_detect(time, "mins|min") ~
        as.numeric(str_extract(time, "\\d+(?=\\s*hr)")) * 3600,
      str_detect(time, "hrs|hr") & str_detect(time, "min|mins") ~
        as.numeric(str_extract(time, "\\d+(?=\\s*hr)")) * 3600 +
        as.numeric(str_extract(time, "\\d+(?=\\s*min)")) * 60,
      str_detect(time, "mins|min") ~
        as.numeric(str_extract(time, "\\d+")) * 60,
      TRUE ~ as.numeric(time)
    )
  )
# Handle NA values (if there are cases like "2 hours" with no mins or "45 mins" with no hours)
audible$hours[is.na(audible$hours)] <- 0
audible$minutes[is.na(audible$minutes)] <- 0 



#converted releasedate column of data type string to date as should be
audible$releasedate<- as.Date(audible$releasedate,format="%d-%m-%y")
#extract year column
audible<-audible %>% mutate(Year=year(audible$releasedate))


#split stars column into Stars and Rating Column,remove unwanted characters and convert columns to numeric 
audible <- audible %>% separate(
  stars, into = c("Stars", "Rating"), sep = "out of 5 stars") %>% 
  mutate(
    Stars = as.numeric(str_remove_all(Stars, " ")),
    Rating = as.numeric(str_remove_all(Rating, ",|ratings"))
  ) %>% 
  mutate_at(
    vars(Stars, Rating),
    ~if_else(is.na(.), 0, .)
  )


#Clean price column and type cast to numeric
audible <- audible %>% mutate(
  price = case_when(
    str_detect(price, ",") ~ as.numeric(str_remove(price, ",")),
    str_detect(price, "Free") ~ as.numeric(str_remove(price, "Free")),
    TRUE ~ as.numeric(price)
  ),price=if_else(is.na(price),0,price)
)

 
#subsetting the data
audible <-audible %>% select(name:time,hours:time_seconds,releasedate,language,Stars:price,Year)

#saving the data
write.csv(audible,"C:/Users/SAMMY/Desktop/Audible/audible_cleaned.csv")


#Some Data Vizzes
#Top 5 Audible Authors
# Filter top 5 authors
top_authors <- audible %>%
  group_by(author) %>%
  summarise(n = n()) %>%
  arrange(n) %>%
  top_n(5)

# Bar plot of Top 5 Audible Authors 
ggplot(top_authors, aes(x = reorder(author, n), y = n, fill = author)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), hjust = -0.1, color = "black", size = 3.5) +
  scale_fill_manual(values = c("矢島雅弘,石橋遊"="#FFA000","Smart Reading"="#FFB347","中西貴之,BJ"="#FFC680","div."="#FFDAB3","Online Studio Productions"="#FFEFE0")) +
  coord_flip() +
  labs(x = "Author", y = "Book Count", title = "Top 5 Authors in Audible Dataset") +
  theme_classic() +
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5))


#Distribution of Star Ratings
# Filter Star Ratings
star_summary <- audible %>%
  group_by(Stars) %>%
  summarise(count = n()) %>%
  arrange(desc(Stars))

# bar plot of the distribution
ggplot(star_summary, aes(x =reorder(Stars,Stars), y = count)) +
  geom_bar(stat = "identity", fill = "#FFA000", color = "#000000") +
  geom_text(aes(label = count), vjust = -.5,hjust=0.5, color= "black", size = 3.5)+
  labs(x = "Star Rating", y = "Count", title = "Distribution of Star Ratings") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))


#Heatmap of Books released per year
# Group books by year
unique_years <- unique(audible$Year)
year_summary <- audible %>%
  group_by(Year) %>%
  summarise(count = n()) %>%
  arrange(Year)

# Create the heatmap of books per year
ggplot(year_summary, aes(x = "", y = Year, fill = count)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "linen", high = "#FFA000") +
  geom_text(aes(label = count), vjust = 0.5, hjust = 1,color = "black", size = 2.8,alpha=1)+
  scale_y_continuous(breaks = unique_years)+
  labs(x = "Book Count", y = "Year", title = "Heatmap of Books Released Each Year") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))












