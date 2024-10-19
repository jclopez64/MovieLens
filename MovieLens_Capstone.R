
# 2.1 Data Download and Preparation

##########################################################
# Create edx and final_holdout_test sets 
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) 
  install.packages("tidyverse"
                   , repos = "http://cran.us.r-project.org")
if(!require(caret)) 
  install.packages("caret"
                   , repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

options(timeout = 120)

dl <- "ml-10M100K.zip"
if(!file.exists(dl))
  download.file(paste0("https://files.grouplens.org/datasets/", 
                       "movielens/", 
                       "ml-10m.zip"), dl)
ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
  unzip(dl, ratings_file)

movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)
ratings <- as.data.frame(str_split(read_lines(ratings_file), 
                                   fixed("::"), 
                                   simplify = TRUE), 
                         stringsAsFactors = FALSE)
colnames(ratings) <- c("userId", "movieId", 
                       "rating", "timestamp")
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))
movies <- as.data.frame(str_split(read_lines(movies_file), 
                                  fixed("::"), 
                                  simplify = TRUE), 
                        stringsAsFactors = FALSE)

colnames(movies) <- c("movieId", "title", "genres")
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

movielens <- left_join(ratings, movies, by = "movieId")

# Final hold-out test set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
# set.seed(1) # if using R 3.5 or earlier
test_index <- createDataPartition(y = movielens$rating, 
                                  times = 1, 
                                  p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in final 
# hold-out test set are also in edx set
final_holdout_test <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from final hold-out test
# set back into edx set
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)


# Adding additional packages
if(!require(data.table))
  install.packages("data.table"
                   , repos = "http://cran.us.r-project.org")
if(!require(knitr))
  install.packages("knitr"
                   , repos = "http://cran.us.r-project.org")
if (!require(scales))
  install.packages("scales"
                   , repos = "http://cran.us.r-project.org")
if (!require(ggthemes))
  install.packages("ggthemes"
                   , repos = "http://cran.us.r-project.org")
if (!require(recosystem))
  install.packages("recosystem"
                   , repos = "http://cran.us.r-project.org")
if(!require(formatR))
  install.packages("formatR"
                   , repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) 
  install.packages("kableExtra", repos = "http://cran.us.r-project.org")




library(data.table)
library(knitr)
library(scales)
library(ggthemes)
library(recosystem)
library(formatR)
library(kableExtra)



# 2.2 Preliminary Exploration

# Displaying the first 5 rows of edx dataset
edx_head <- head(edx, 5)
print(head(edx, 5))

# Generating the table 
edx_head %>%
  kable("html", caption = NULL, booktabs = TRUE, row.names = FALSE, align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = FALSE, position = "center") %>%
  column_spec(1, width = "1.5cm") %>%
  column_spec(2, width = "1.5cm") %>%
  column_spec(3, width = "1.5cm") %>%
  column_spec(4, width = "2.5cm") %>%
  column_spec(5, width = "4.5cm") %>%
  column_spec(6, width = "6cm") %>%
  row_spec(0, bold = TRUE, color = "#FFFFFF", background = "#4472C4") %>%
  add_header_above(c("First 5 Rows of the edx Dataset" = 6), bold = TRUE, color = "orange")

# About dimensions
# Creating a data frame with dimensions
dim_df <- data.frame(
  Description = c("Number of Rows", "Number of Columns"),
  Value = c(9000055, 6)
)

# Generating the table
dim_df %>%
  kable("html", caption = NULL, booktabs = TRUE, row.names = FALSE, align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = FALSE, position = "center") %>%
  column_spec(1, width = "10em") %>%  # Adjusting the column width 'Description'
  column_spec(2, width = "5em") %>%  # Adjusting the column width 'Value'
  row_spec(0, bold = TRUE, color = "#FFFFFF", background = "#4472C4") %>%  # Orange header and blue background
  add_header_above(c("Dimensions Table" = 2), bold = TRUE, color = "orange")

# Missing values
# Checking for missing values in all the columns of edx
na_check <- sapply(edx, function(x) sum(is.na(x)))

# Converting the results to a data frame
na_check_df <- data.frame(
  Column = names(na_check),
  Missing_Values = na_check,
  row.names = NULL # To avoid an extra column
)

# Generating the table
na_check_df %>%
  kable("html", caption = NULL, booktabs = TRUE, row.names = FALSE, align = "c", 
        linesep = "") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = FALSE, position = "center") %>%
  column_spec(1, width = "15em") %>%
  column_spec(2, width = "10em") %>%
  row_spec(0, bold = TRUE, color = "#FFFFFF", background = "#4472C4") %>%
  add_header_above(c("Missing Values in edx Dataset" = 2), bold = TRUE, color = "orange") %>%
  row_spec(1:nrow(na_check_df), extra_css = "padding-top: 1px; padding-bottom: 1px;")


# Unique values:
# Getting the number of unique values in all columns of edx
unique_counts <- edx %>%
  summarise_all(n_distinct)

# Converting the results into a good format for kable
unique_counts_df <- data.frame(
  Column = colnames(unique_counts),
  Unique_Values = as.vector(t(unique_counts)),
  row.names = NULL
)

# Generating the table
unique_counts_df %>%
  kable("html", caption = NULL, booktabs = TRUE, row.names = FALSE, align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = FALSE, position = "center") %>%
  column_spec(1, width = "12em") %>%
  column_spec(2, width = "7em") %>%
  row_spec(0, bold = TRUE, color = "#FFFFFF", background = "#4472C4") %>%
  add_header_above(c("Unique Values in edx Dataset" = 2), bold = TRUE, color = "orange") %>%
  row_spec(1:nrow(unique_counts_df), extra_css = "padding-top: 1px; padding-bottom: 1px;")


# Variable type
# Getting the class of each column in the edx dataset
class_check <- sapply(edx, class)

# Converting the results into a data frame
class_check_df <- data.frame(
  Column = names(class_check),
  Data_Type = class_check,
  row.names = NULL
)

# Generating the table
class_check_df %>%
  kable("html", caption = NULL, booktabs = TRUE, row.names = FALSE, align = "c", 
        linesep = "") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = FALSE, position = "center") %>%
  column_spec(1, width = "15em") %>%
  column_spec(2, width = "10em") %>%
  row_spec(0, bold = TRUE, color = "#FFFFFF", background = "#4472C4") %>%
  add_header_above(c("Data Types in edx Dataset" = 2), bold = TRUE, color = "orange") %>%
  row_spec(1:nrow(class_check_df), extra_css = "padding-top: 1px; padding-bottom: 1px;")

# Wrangling timestamp
# Converting timestamp to a date and then extracting only the year
edx <- edx %>%
  mutate(Rating_Year = format(as.POSIXct(timestamp, origin = "1970-01-01", tz = "UTC"), "%Y"))

# Eliminating the column timestamp
edx <- edx %>%
  select(-timestamp)

# Displaying the first rows for checking the changes
edx_head_updated <- head(edx, 5)

# Generating the table 
edx_head_updated %>%
  kable("html", caption = NULL, booktabs = TRUE, row.names = FALSE, align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = FALSE, position = "center") %>%  # Table aligned to center
  column_spec(1, width = "2.2em") %>%  # Adjusting the column width 'userId'
  column_spec(2, width = "3em") %>%  # Adjusting the column width 'movieId'
  column_spec(3, width = "2.8em") %>%  # Adjusting the column width 'rating'
  column_spec(4, width = "14em") %>%  # Adjusting the column width 'title'
  column_spec(5, width = "16em") %>%  # Adjusting the column width 'genres'
  column_spec(6, width = "5em") %>%  # Adjusting the column width 'Rating_Year'
  row_spec(0, bold = TRUE, color = "#FFFFFF", background = "#4472C4") %>%  # Header with blue background
  row_spec(1:5, extra_css = "padding: 1px;") %>%  # Uniform padding
  add_header_above(c("First 5 Rows of the edx Dataset (After Timestamp Conversion)" = 6), bold = TRUE, color = "orange")

# Wrangling title
# Using mutate and regular expressions to split the title and release year
edx <- edx %>%
  mutate(Release_Year = str_extract(title, "\\(\\d{4}\\)"),    # Extracting the release year (with parentheses)
         Release_Year = as.numeric(str_remove_all(Release_Year, "[\\(\\)]")),  # Removing the parentheses and converting to numeric
         title = str_remove(title, "\\(\\d{4}\\)"))             # Eliminating year from title

# Eliminating the White spaces around title
edx <- edx %>%
  mutate(title = str_trim(title))

# Displaying first rows to verify the change
edx_head_updated <- head(edx, 5)

# Generating the table 
edx_head_updated %>%
  kable("html", caption = NULL, booktabs = TRUE, row.names = FALSE, align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = FALSE, position = "left") %>%
  column_spec(1, width = "2.2em") %>%  # Adjusting the column width 'userId'
  column_spec(2, width = "3em") %>%  # Adjusting the column width 'movieId'
  column_spec(3, width = "2.8em") %>%  # Adjusting the column width 'rating'
  column_spec(4, width = "10em") %>%  # Adjusting the column width 'title'
  column_spec(5, width = "16em") %>%  # Adjusting the column width 'genres'
  column_spec(6, width = "5em") %>%  # Adjusting the column width 'Rating_Year'
  column_spec(7, width = "5em") %>%  # Adjusting the column width 'Release_Year'
  row_spec(0, bold = TRUE, color = "#FFFFFF", background = "#4472C4") %>%  # Blue header with white text
  row_spec(1:5, extra_css = "padding: 1px;") %>%  # Uniform padding
  add_header_above(c("First 5 Rows of the edx Dataset (After Release Year Extraction)" = 7), bold = TRUE, color = "orange")


# Wrangling genres
# Separating the compound genres into individual genres (one row per genre)
edx <- edx %>%
  separate_rows(genres, sep = "\\|")

# Displaying first rows for checking the changes
edx_head_updated <- head(edx, 5)

# Generating the table
edx_head_updated %>%
  kable("html", caption = NULL, booktabs = TRUE, row.names = FALSE, align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = FALSE, position = "center") %>%
  column_spec(1, width = "2.2em") %>%  # Adjusting the column width 'userId'
  column_spec(2, width = "3em") %>%  # Adjusting the column width 'movieId'
  column_spec(3, width = "2.8em") %>%  # Adjusting the column width 'rating'
  column_spec(4, width = "10em") %>%  # Adjusting the column width 'title'
  column_spec(5, width = "12em") %>%  # Adjusting the column width 'genres'
  column_spec(6, width = "8em") %>%  # Assuming another column exists
  column_spec(7, width = "8em") %>%  # Assuming another column exists
  row_spec(0, bold = TRUE, color = "#FFFFFF", background = "#4472C4") %>%  # Blue header with white text
  row_spec(1:5, extra_css = "padding: 1px;") %>%  # Uniform padding for rows
  add_header_above(c("First 5 Rows of the edx Dataset (After Genres Separation)" = 7), bold = TRUE, color = "orange")


# New dimensions dataset
# Obtaining dimensions of dataset edx
edx_dim <- data.frame(
  Description = c("Number of Rows", "Number of Columns"),
  Value = dim(edx)
)

# Generating the table 
edx_dim %>%
  kable("html", caption = NULL, booktabs = TRUE, row.names = FALSE, align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = FALSE, position = "center", font_size = 8) %>%  # Adjust font size
  column_spec(1, width = "12em") %>%  # Adjusting the column width 'Description'
  column_spec(2, width = "5em") %>%  # Adjusting the column width 'Value'
  row_spec(0, bold = TRUE, color = "#FFFFFF", background = "#4472C4") %>%  # Blue header
  row_spec(1:2, extra_css = "padding: 1px;") %>%  # Uniform padding for rows
  add_header_above(c("Dimensions of the edx Dataset" = 2), bold = TRUE, color = "orange")  # Custom header above

# Wrangling final_holdout_test
# Converting timestamp to a date and then extracting just the year for final_holdout_test
final_holdout_test <- final_holdout_test %>%
  mutate(Rating_Year = format(as.POSIXct(timestamp, origin = "1970-01-01", tz = "UTC"), "%Y"))

# Eliminating the timestamp column in final_holdout_test
final_holdout_test <- final_holdout_test %>%
  select(-timestamp)

# Separating the compound genres into individual genres (one row per genre)
final_holdout_test <- final_holdout_test %>%
  separate_rows(genres, sep = "\\|")

# Using mutate and regular expressions to split the title and release year 
final_holdout_test <- final_holdout_test %>%
  mutate(Release_Year = str_extract(title, "\\(\\d{4}\\)"),    # Extracting Release Year
         Release_Year = as.numeric(str_remove_all(Release_Year, "[\\(\\)]")),  # Removing the parentheses and converting to numeric
         title = str_remove(title, "\\(\\d{4}\\)"))             # Eliminating the year from title

# Eliminating possible blank spaces around title
final_holdout_test <- final_holdout_test %>%
  mutate(title = str_trim(title))

# Displaying the first rows of the modified dataset
final_holdout_test_head <- head(final_holdout_test, 5)

# Generating the table for final_holdout_test_head
final_holdout_test_head %>%
  kable("html", caption = NULL, booktabs = TRUE, row.names = FALSE, align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = FALSE, position = "center") %>%
  column_spec(1, width = "2.5em") %>%
  column_spec(2, width = "3em") %>%
  column_spec(3, width = "4em") %>%
  column_spec(4, width = "12em") %>%
  column_spec(5, width = "4em") %>%
  column_spec(6, width = "6em") %>%
  column_spec(7, width = "6em") %>%
  row_spec(0, bold = TRUE, color = "#FFFFFF", background = "#4472C4") %>%
  row_spec(1:5, extra_css = "padding: 1px;") %>%
  add_header_above(c("First 5 Rows of the final_holdout_test Dataset" = 7), bold = TRUE, color = "orange")


# Analysis - rating
# Unique values of the rating column ordered from lowest to highest
unique_ratings_sorted <- sort(unique(edx$rating))

# Converting the unique values into a one-row data frame 
unique_ratings_df <- as.data.frame(t(unique_ratings_sorted))

# Renaming columns "Rating Values"
colnames(unique_ratings_df) <- paste("Rating", 1:ncol(unique_ratings_df), sep = "_")

# Generating the table
unique_ratings_df %>%
  kable("html", caption = NULL, booktabs = TRUE, row.names = FALSE, align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = FALSE, position = "center") %>%
  column_spec(1, width = "6em") %>%  # Adjusting column width 
  column_spec(2, width = "6em") %>%  # Adjusting the other
  row_spec(0, bold = TRUE, color = "#FFFFFF", background = "#4472C4")  # Blue header without row shading

# Calculating the mean by removing duplicates based on userId and movieId
mean_rating_original <- edx %>%
  distinct(userId, movieId, .keep_all = TRUE) %>%
  summarise(mean_rating = mean(rating))

# Converting the result into a data frame to generate the table
mean_rating_df <- as.data.frame(mean_rating_original)
colnames(mean_rating_df) <- "Mean Rating"

# Generating the table 
mean_rating_df %>%
  kable("html", caption = NULL, booktabs = TRUE, row.names = FALSE, align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = FALSE, position = "center") %>%
  column_spec(1, width = "6em") %>%  # Adjusting column width again
  row_spec(0, bold = TRUE, color = "#FFFFFF", background = "#4472C4")  # Blue header without row shading

# Calculating the relative frequency
rating_freq <- edx %>%
  group_by(rating) %>%
  summarise(frequency = n() / nrow(edx))

# Generating the chart
ggplot(rating_freq, aes(x = rating, y = frequency)) +
  geom_bar(stat = "identity", fill = "orange") +  
  labs(title = "Relative Frequency of Ratings by Rating Value",
       x = "Rating Value",
       y = "Relative Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Calculating the number of ratings with and without decimals
rating_type_counts <- edx %>%
  mutate(is_integer = rating == floor(rating)) %>%  # Verifying no decimals
  group_by(is_integer) %>%
  summarise(count = n())

# Modifying the results to add the rating type (integers or not)
rating_type_counts <- rating_type_counts %>%
  mutate(rating_type = ifelse(is_integer, "Integer Ratings", "Non-integer Ratings")) %>%
  select(rating_type, count)

# Generating the table with HTML format for rating_type_counts
rating_type_counts %>%
  kable("html", caption = NULL, booktabs = TRUE, row.names = FALSE, align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = FALSE, position = "center") %>%
  column_spec(1, width = "10em") %>%  # Width first column
  column_spec(2, width = "10em") %>%  # Width second one
  row_spec(0, bold = TRUE, color = "#FFFFFF", background = "#4472C4") %>%  # Same header
  row_spec(1:nrow(rating_type_counts), extra_css = "padding-bottom: 5px; padding-top: 5px;")  # Uniform spacing

# Analysis user
# Calculating the number of distinct users
distinct_users_count <- data.frame(Users = n_distinct(edx$userId))

# Generating the table 
distinct_users_count %>%
  kable("html", caption = NULL, booktabs = TRUE, row.names = FALSE, align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = FALSE, position = "center") %>%
  column_spec(1, width = "10em") %>%  # Adjusts column width
  row_spec(0, bold = TRUE, color = "#FFFFFF", background = "#4472C4") %>%  # Blue header with white text
  row_spec(1:nrow(distinct_users_count), extra_css = "padding-bottom: 5px; padding-top: 5px;")  # Uniform spacing

# Distribution of ratings
# Grouping by userId and counting the number of ratings each user has made.
user_ratings_count <- edx %>%
  group_by(userId) %>%
  summarise(n = n())

# Making the chart - logarithmic scale on the x-axis
ggplot(user_ratings_count, aes(n)) +
  geom_histogram(color = "black", fill = "orange", bins = 30) +  
  scale_x_log10() +  # Logarithmic scale on the x-axis
  scale_y_continuous(labels = scales::comma, breaks = seq(0, 7000, 1000), limits = c(0, 7000)) +  # Axis y limited
  ggtitle("Distribution of Users") +
  xlab("Number of Ratings (Log Scale)") +
  ylab("Number of Users") +
  theme_minimal() +  # Minimal theme
  theme(plot.title = element_text(hjust = 0.5))  # Centering title

# Plotting average rating by user for the edx dataset
edx %>%
  mutate(rating_numeric = as.numeric(as.character(rating))) %>%  # Converting 'rating'
  group_by(userId) %>%
  summarise(ave_rating = mean(rating_numeric, na.rm = TRUE)) %>%  # Calculating average de 'rating_numeric'
  ggplot(aes(ave_rating)) +
  geom_histogram(bins = 30, color = "black", fill = "orange") +  
  labs(x = "Average Rating", y = "Number of Users") +  
  theme_minimal() +  # Basic theme
  theme(plot.title = element_text(hjust = 0.5))  # Centerng title

# Analysis title
# Movies with more ratings
# Removing duplicates based on userId and movieId
ratings_per_movie <- edx %>%
  distinct(userId, movieId, .keep_all = TRUE) %>%
  group_by(title) %>%
  summarise(
    avg_rating = mean(rating),       # Calculating mean
    rating_count = n()               # Counting ratings
  ) %>%
  arrange(desc(rating_count))        # Ordering by number of ratings

# Filter the movies with the most ratings
top_rated_movies <- ratings_per_movie %>%
  top_n(10, rating_count)

# Making the chart
ggplot(top_rated_movies, aes(x = rating_count, y = reorder(title, rating_count))) +
  geom_point(color = "orange", size = 3) +
  labs(title = "Top 10 Movies by Number of Ratings",
       x = "Number of Ratings",
       y = "Movie Title") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),          # Centring the title
    axis.text.y = element_text(size = 8),            # Changing font size for y-axis
    axis.text.x = element_text(size = 10)            # Changing font size for x-axis
  )

# Movies with highest ratings
# Removing duplicates based on userId and movieId
ratings_per_movie <- edx %>%
  distinct(userId, movieId, .keep_all = TRUE) %>%
  group_by(title) %>%
  summarise(
    avg_rating = mean(rating, na.rm = TRUE),  # Calculating mean of ratings
    rating_count = n()                        # Counting number of ratings
  ) %>%
  arrange(desc(avg_rating))                   # Ordering by average rating

# Filtering the top 10 movies with the highest average rating
top_avg_rated_movies <- ratings_per_movie %>%
  top_n(10, avg_rating)

# Creating the chart
ggplot(top_avg_rated_movies, aes(x = avg_rating, y = reorder(title, avg_rating))) +
  geom_point(color = "orange", size = 3) +
  labs(title = "Top 10 Movies by Average Rating",
       x = "Average Rating",
       y = "Movie Title") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),          # Centring the title
    axis.text.y = element_text(size = 8),            # Changing font size for y-axis
    axis.text.x = element_text(size = 10)            # Changing font size for x-axis
  )

# Correlation
# Removing duplicates based on userId and movieId
ratings_per_movie <- edx %>%
  distinct(userId, movieId, .keep_all = TRUE) %>%
  group_by(title) %>%
  summarise(
    avg_rating = mean(rating, na.rm = TRUE),  # Calculating mean of ratings
    rating_count = n()                        # Counting number of ratings
  ) %>%
  filter(!is.na(avg_rating), !is.na(rating_count))  # Filtering NAs if needed

# Creating the scatter plot 
ggplot(ratings_per_movie, aes(x = rating_count, y = avg_rating)) +
  geom_point(color = "orange", alpha = 0.7) +
  labs(title = "Correlation between Number of Ratings and Average Rating",
       x = "Number of Ratings",
       y = "Average Rating") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 10),  # Centring and reducing title size
    axis.text.x = element_text(size = 9),               # Changing font size for x-axis
    axis.text.y = element_text(size = 9)                # Reducing font size for y-axis
  ) +
  geom_smooth(method = "lm", color = "blue", se = FALSE)  # Linear trendline

# Analysis Release year
# Relationship between the release year and the number of ratings 
# Removing duplicates based on userId and movieId
ratings_clean <- edx %>%
  distinct(userId, movieId, .keep_all = TRUE)

# Grouping by Release_Year
ratings_per_release_year <- ratings_clean %>%
  group_by(Release_Year) %>%
  summarise(count = n())  # Counting ratings by year

# Creating the chart
ggplot(ratings_per_release_year, aes(x = Release_Year, y = count)) +
  geom_bar(stat = "identity", fill = "orange") +  # Bar chart with orange fill
  labs(title = "Number of Ratings by Release Year",
       x = "Release Year",
       y = "Number of Ratings") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Centring the title

# Analysis Rating year
# Calculating the number of ratings per year
ratings_per_year <- edx %>%
  group_by(Rating_Year) %>%
  summarise(count = n())

# Generating the chart with smaller font for x-axis labels
ggplot(ratings_per_year, aes(x = Rating_Year, y = count)) +
  geom_bar(stat = "identity", fill = "orange") +  # Bar chart in orange
  labs(title = "Number of Ratings by Year",
       x = "Year of Rating",
       y = "Number of Ratings") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),  # Centring title
    axis.text.x = element_text(size = 5)  # Reducing font size for x-axis labels
  )

# Grouping by Release_Year and calculating the average rating per year
average_ratings_per_year <- ratings_clean %>%
  group_by(Release_Year) %>%
  summarise(avg_rating = mean(rating, na.rm = TRUE))  # Calculating the average of rating, ignoring NAs

# Filtering to remove rows with NA in avg_rating or Release_Year
average_ratings_per_year <- average_ratings_per_year %>%
  filter(!is.na(avg_rating), !is.na(Release_Year))

# Creating the chart
ggplot(average_ratings_per_year, aes(x = Release_Year, y = avg_rating)) +
  geom_line(color = "orange", linewidth = 1) +  # Line in orange
  geom_point(color = "orange", size = 2) +  # Adding orange points for years
  labs(title = "Average Rating by Release Year",
       x = "Release Year",
       y = "Average Rating") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Centring title

# Average per Rating year
# Keeping all columns except genres when removing duplicates
ratings_clean_temp <- edx %>%
  distinct(userId, movieId, rating, title, Release_Year, Rating_Year, .keep_all = TRUE)

# Calculating the average of ratings by Rating_Year
average_ratings_per_rating_year <- ratings_clean_temp %>%
  group_by(Rating_Year) %>%
  summarise(avg_rating = mean(rating, na.rm = TRUE))  # Calculating mean, ignoring NAs

# Filtering rows with NA in 'avg_rating' or 'Rating_Year'
average_ratings_per_rating_year <- average_ratings_per_rating_year %>%
  filter(!is.na(avg_rating), !is.na(Rating_Year))

# Generating the chart with smaller font for x-axis labels
ggplot(average_ratings_per_rating_year, aes(x = Rating_Year, y = avg_rating)) +
  geom_bar(stat = "identity", fill = "orange") +  # Orange color for bars
  labs(title = "Average Rating by Rating Year",
       x = "Year of Rating",
       y = "Average Rating") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),  # Centring title
    axis.text.x = element_text(size = 5)  # Reducing font size for x-axis labels
  )

# Analysis genres
# Unique movies
# Counting the number of unique movies by genre
genre_counts <- edx %>%
  distinct(movieId, genres) %>%
  group_by(genres) %>%
  summarise(movie_count = n())    # Counting unique movies by genre

# Generating the horizontal bar chart with numbers next to each bar
ggplot(genre_counts, aes(x = movie_count, y = reorder(genres, movie_count))) + 
  geom_bar(stat = "identity", fill = "orange", color = "black") + 
  geom_text(aes(label = movie_count), 
            hjust = -0.2,  # Positioning the numbers outside the bars
            color = "black", 
            size = 3) +  # Adjusting the size of the numbers
  labs(title = "Number of Unique Movies per Genre", 
       x = "Number of Movies", 
       y = "Genres") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Centring the title

# Calculating the mean of ratings per each genre
average_rating_by_genre <- edx %>%
  group_by(genres) %>%
  summarise(avg_rating = mean(rating, na.rm = TRUE))  # Calculating average of ratings, ignoring NAs

# Filtering genres with NA in 'avg_rating'
average_rating_by_genre <- average_rating_by_genre %>%
  filter(!is.na(avg_rating))

# Creating the chart
ggplot(average_rating_by_genre, aes(x = avg_rating, y = reorder(genres, avg_rating))) +
  geom_bar(stat = "identity", fill = "orange") +    # Orange bars
  labs(title = "Average Rating by Genre",
       x = "Average Rating",
       y = "Genre") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))      # Centring the title

# Calculating the number of ratings for each genre
ratings_count_by_genre <- edx %>%
  group_by(genres) %>%                  # Grouping by genres
  summarise(rating_count = n())         # Counting the number of ratings per each genre

# Creating the chart
ggplot(ratings_count_by_genre, aes(x = rating_count, y = reorder(genres, rating_count))) +
  geom_bar(stat = "identity", fill = "orange") +    # Orange bars
  labs(title = "Number of Ratings by Genre",
       x = "Number of Ratings",
       y = "Genre") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))      # Centring the title

# Modeling
# Partition edx dataset
# Setting seed for reproducibility of results
set.seed(1, sample.kind = "Rounding")  # For R version 3.6 or later

# Creating an index to split the edx dataset into 90% for edx2 and 10% for testing
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)

# Creating the edx2 and a temporary subset (temp) for 10% of the data
edx2 <- edx[-test_index,]  # Training (90%)
temp <- edx[test_index,]   # Temp (10%)

# Ensuring that all elements in the testing set have a match in edx2
testing <- temp %>%
  semi_join(edx2, by = "movieId") %>%  # Just movies in edx2
  semi_join(edx2, by = "userId")       # Just users in edx2

# Rows that were removed from the testing set because they did not meet the matching criteria between userId and movieId are added back to edx2
removed <- anti_join(temp, testing)
edx2 <- rbind(edx2, removed)

# Displaying first rows of edx2
head(edx2)

# Clearing temporary variables to avoid occupying unnecessary space
rm(test_index, temp, removed)

# Displaying the first 5 rows of edx2
edx2_head <- head(edx2, 5)  # Extracting the first 5 rows

# Generating the table with kableExtra in HTML format
edx2_head %>%
  kable("html", caption = NULL, booktabs = TRUE, row.names = FALSE, align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = FALSE, position = "center") %>%
  column_spec(1, width = "2.2em") %>%  # userId
  column_spec(2, width = "3em") %>%  # movieId
  column_spec(3, width = "2.8em") %>%  # rating
  column_spec(4, width = "10em") %>%  # title
  column_spec(5, width = "12em") %>%  # genres
  column_spec(6, width = "7em") %>%  # Rating_Year
  column_spec(7, width = "7em") %>%  # Release_Year
  row_spec(0, bold = TRUE, color = "#FFFFFF", background = "#4472C4") %>%  # Formatting the header
  row_spec(1:5, extra_css = "padding: 1px;") %>%  # Formatting the rows
  add_header_above(c("First 5 Rows of the edx2 Dataset" = 7), bold = TRUE, color = "orange")

# Displaying the first 5 rows of testing
testing_head <- head(testing, 5)

# Generating the table with kableExtra in HTML format
testing_head %>%
  kable("html", caption = NULL, booktabs = TRUE, row.names = FALSE, align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = FALSE, position = "center") %>%
  column_spec(1, width = "2.2em") %>%  # userId
  column_spec(2, width = "3em") %>%  # movieId
  column_spec(3, width = "2.8em") %>%  # rating
  column_spec(4, width = "10em") %>%  # title
  column_spec(5, width = "12em") %>%  # genres
  column_spec(6, width = "7em") %>%  # Rating_Year
  column_spec(7, width = "7em") %>%  # Release_Year
  row_spec(0, bold = TRUE, color = "#FFFFFF", background = "#4472C4") %>%  # Formatting the header
  row_spec(1:5, extra_css = "padding: 1px;") %>%  # Formatting the rows
  add_header_above(c("First 5 Rows of the testing Dataset" = 7), bold = TRUE, color = "orange")

# Dimensions all the datasets
# Creating a data frame with the dimensions of the datasets
dataset_dimensions <- data.frame(
  Dataset = c("edx original", "edx modified", "edx2", "testing"),
  Rows = c(9000055, nrow(edx), nrow(edx2), nrow(testing)),
  Columns = c(6, ncol(edx), ncol(edx2), ncol(testing))
)

# Generating the table with kableExtra in HTML format
dataset_dimensions %>%
  kable("html", caption = NULL, booktabs = TRUE, row.names = FALSE, align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = FALSE, 
                position = "center", font_size = 8) %>%
  column_spec(1, width = "9em") %>%  # Adjusting the column width 'Dataset'
  column_spec(2, width = "4em") %>%  # Adjusting the column width 'Rows'
  column_spec(3, width = "4em") %>%  # Adjusting the column width 'Columns'
  row_spec(0, bold = TRUE, color = "#FFFFFF", background = "#4472C4") %>%  # Formatting the header
  row_spec(1:4, extra_css = "padding: 1px;") %>%  # Formatting the rows
  add_header_above(c("Dataset Dimensions" = 3), bold = TRUE, color = "orange")

# Mean model
# Calculating the mean of ratings eliminating duplicates
mean_rating_edx2 <- edx2 %>% 
  distinct(userId, movieId, .keep_all = TRUE) %>% 
  summarise(mean_rating = mean(rating)) %>% 
  pull(mean_rating)

# Initializing a list to store the RMSEs of each model
rmse_values <- list()

# Eliminating duplicates in testing
testing_clean <- testing %>% 
  distinct(userId, movieId, .keep_all = TRUE) %>% 
  mutate(predicted_rating = mean_rating_edx2)

# Calculating RMSE comparing predictions with real values
rmse_mean_based <- sqrt(mean((testing_clean$rating - testing_clean$predicted_rating)^2))

# Saving RMSE in rmse_values
rmse_values$mean_based <- round(rmse_mean_based, 5)

# Creating a data frame with the RMSE
rmse_table <- data.frame(
  Model = "Mean_Based Model",  # Header 
  RMSE = rmse_values$mean_based  # Using saved value in rmse_values
)

# Generating table
rmse_table %>%
  kable("html", booktabs = TRUE, row.names = FALSE, align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = FALSE, position = "center") %>%
  column_spec(1, width = "15em") %>%  # Adjusting the column width for model name
  column_spec(2, width = "10em") %>%  # Adjusting the column width for RMSE
  row_spec(0, bold = TRUE, color = "orange", background = "blue") %>%  # Blue header with orange text
  row_spec(1, extra_css = "padding: 1px;") %>%  # Uniform padding for rows
  kable_styling(font_size = 12, position = "center")  # Setting font size


# Movie effect model
# Calculating mean rating 
mean_rating_edx2 <- edx2 %>% 
  distinct(userId, movieId, .keep_all = TRUE) %>% 
  summarise(mean_rating = mean(rating)) %>% 
  pull(mean_rating)

# Calculating movie bias
movie_bias <- edx2 %>% 
  group_by(movieId) %>% 
  summarise(b_i = mean(rating) - mean_rating_edx2)  

# Cleanning testing set and add movie bias
testing_clean <- testing %>% 
  distinct(userId, movieId, .keep_all = TRUE) %>% 
  left_join(movie_bias, by = "movieId") %>% 
  mutate(b_i = ifelse(is.na(b_i), 0, b_i),  # Replace NA with 0 if no movie bias
         predicted_rating = mean_rating_edx2 + b_i)  # Prediction = μ + b_i

# Calculating RMSE
rmse_movie_effect <- sqrt(mean((testing_clean$rating - testing_clean$predicted_rating)^2))

# Saving RMSE in rmse_values
rmse_values$movie_effect <- round(rmse_movie_effect, 5)

# Creating RMSE table
rmse_table_combined <- data.frame(
  Model = c("Mean_Based Model", "Movie_Effect Model"),  
  RMSE = c(rmse_values$mean_based, rmse_values$movie_effect)
)

# Generating the table
rmse_table_combined %>% 
  kable("html", booktabs = TRUE, row.names = FALSE, align = "c") %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = FALSE, position = "center") %>% 
  column_spec(1, width = "22em") %>%  # Fixed width for 'Model'
  column_spec(2, width = "6em") %>%   # Reduced width for 'RMSE'
  row_spec(0, bold = TRUE, color = "orange", background = "blue") %>%  # Blue header with orange text
  row_spec(1:2, extra_css = "padding: 1px;") %>%  # Uniform padding for rows
  kable_styling(font_size = 12, position = "center")  # Font size


# Visualization
# Calculating the bias of each movie (b_i) with respect to the global mean in edx2
movie_bias <- edx2 %>%
  distinct(userId, movieId, .keep_all = TRUE) %>%  # Eliminating duplicates
  group_by(movieId) %>%       # Grouping by movie
  summarise(b_i = mean(rating - mean_rating_edx2)) # Calculating the bias

# Generating the chart with ggplot
ggplot(movie_bias, aes(x = b_i)) +
  geom_histogram(binwidth = 0.1, fill = "orange", color = "black") +  # Creating the chart
  labs(title = "Distribution of Movies by Movie Bias (b_i)",   # Title
       x = "Movie Bias (b_i)",   # X label
       y = "Number of Movies") +    # Y label
  theme_minimal() +                                                  
  theme(plot.title = element_text(hjust = 0.5))  # Centring title


# Movie & user effects model
# Calculating the bias of each user (b_u) with respect to the global mean and the bias of each movie
user_bias <- edx2 %>%
  distinct(userId, movieId, .keep_all = TRUE) %>% 
  left_join(movie_bias, by = "movieId") %>% 
  group_by(userId) %>% 
  summarise(b_u = mean(rating - mean_rating_edx2 - b_i))

# Merging the movie and user biases with the testing set, removing duplicates
testing_clean <- testing %>%
  distinct(userId, movieId, .keep_all = TRUE) %>% 
  left_join(movie_bias, by = "movieId") %>% 
  left_join(user_bias, by = "userId") %>% 
  mutate(b_i = ifelse(is.na(b_i), 0, b_i),  # Replacing NA with 0 if no movie bias
         b_u = ifelse(is.na(b_u), 0, b_u),  # Replacing NA with 0 if no user bias
         predicted_rating = mean_rating_edx2 + b_i + b_u)  # Prediction = μ + b_i + b_u

# Calculating the RMSE by comparing predictions with actual values in the testing set
rmse_movie_user_effect <- sqrt(mean((testing_clean$rating - testing_clean$predicted_rating)^2))

# Saving RMSE in rmse_values
rmse_values$movie_user_effect <- round(rmse_movie_user_effect, 5)

# Creating a data frame with RMSE of all the models
rmse_table_combined <- data.frame(
  Model = c("Mean_Based Model", "Movie_Effect Model", "Movie+User_Effect Model"),  
  RMSE = c(rmse_values$mean_based, rmse_values$movie_effect, rmse_values$movie_user_effect)  
)

# Generating the table width for the 'Model' column to fit longer model names
rmse_table_combined %>% 
  kable("html", booktabs = TRUE, row.names = FALSE, align = "c") %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = FALSE, position = "center") %>% 
  column_spec(1, width = "22em") %>%  # Fixed width for 'Model' to accommodate longer names
  column_spec(2, width = "6em") %>%   # Reduced width for 'RMSE'
  row_spec(0, bold = TRUE, color = "orange", background = "blue") %>%  # Blue header with orange text
  row_spec(1:3, extra_css = "padding: 1px;") %>%  # Uniform padding for rows
  kable_styling(font_size = 12, position = "center")  # Font size adjusted

# Visually:
# Calculate the bias of each user (b_u) with respect to the global mean and the bias of each movie in edx2
user_bias <- edx2 %>%
  distinct(userId, movieId, .keep_all = TRUE) %>%  # Eliminating duplicates
  left_join(movie_bias, by = "movieId") %>% # Joining bias
  group_by(userId) %>%  # Grouping by user
  summarise(b_u = mean(rating - mean_rating_edx2 - b_i)) # Calculating user bias

# Generating chart with ggplot
ggplot(user_bias, aes(x = b_u)) +
  geom_histogram(binwidth = 0.1, fill = "orange", color = "black") +  # Creating the chart
  labs(title = "Distribution of User Bias (b_u)",  # Title
       x = "User Bias (b_u)",  # X label
       y = "Count") +  # Y label
  theme_minimal() +                                                   
  theme(plot.title = element_text(hjust = 0.5))  # Centring the title


# Movie, user & Release year effects model
# Calculating the bias by release year (b_r) with respect to the global mean, movie bias, and user bias
release_year_bias <- edx2 %>%
  distinct(userId, movieId, .keep_all = TRUE) %>% 
  left_join(movie_bias, by = "movieId") %>% 
  left_join(user_bias, by = "userId") %>% 
  group_by(Release_Year) %>% 
  summarise(b_r = mean(rating - mean_rating_edx2 - b_i - b_u))

# Merging the biases of movies, users, and release year with the testing set, removing duplicates
testing_clean <- testing %>%
  distinct(userId, movieId, .keep_all = TRUE) %>% 
  left_join(movie_bias, by = "movieId") %>% 
  left_join(user_bias, by = "userId") %>% 
  left_join(release_year_bias, by = "Release_Year") %>% 
  mutate(b_i = ifelse(is.na(b_i), 0, b_i),   
         b_u = ifelse(is.na(b_u), 0, b_u), 
         b_r = ifelse(is.na(b_r), 0, b_r),  
         predicted_rating = mean_rating_edx2 + b_i + b_u + b_r)

# Calculating the RMSE by comparing predictions with real values
rmse_movie_user_release_effect <- sqrt(mean((testing_clean$rating - testing_clean$predicted_rating)^2))

# Saving the RMSE in rmse_values
rmse_values$movie_user_release_effect <- round(rmse_movie_user_release_effect, 5)

# Creating a data frame with all models' RMSE
rmse_table_combined <- data.frame(
  Model = c("Mean_Based Model", "Movie_Effect Model", "Movie+User_Effect Model", "Movie+User+Release_Year Model"),  
  RMSE = c(rmse_values$mean_based, rmse_values$movie_effect, rmse_values$movie_user_effect, rmse_values$movie_user_release_effect)
)

# Generating the table
rmse_table_combined %>%
  kable("html", booktabs = TRUE, row.names = FALSE, align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = FALSE, position = "center") %>% 
  column_spec(1, width = "22em") %>%  # Adjusting width for 'Model'
  column_spec(2, width = "6em") %>%   # Adjusting width for 'RMSE'
  row_spec(0, bold = TRUE, color = "orange", background = "blue") %>%  # Styling header row
  row_spec(1:4, extra_css = "padding: 1px;") %>%  # Uniform padding for rows
  kable_styling(font_size = 12, position = "center")  # Font size adjusted

# Visually
# Chart of Release_Year bias (b_r)
ggplot(release_year_bias, aes(x = b_r)) +
  geom_histogram(binwidth = 0.1, fill = "orange", color = "black") +  # Creating chart
  labs(title = "Distribution of Release Year Bias (b_r)",  # Title
       x = "Release Year Bias (b_r)",   # X label
       y = "Count") +    # Y label
  theme_minimal() +    
  theme(plot.title = element_text(hjust = 0.5))   # Centring the title

# Movie, user, Release year & genres effects model
# Calculating the bias by genre (b_g) using unique combinations of userId, movieId, and genres
genre_bias <- edx2 %>%
  distinct(userId, movieId, genres, .keep_all = TRUE) %>% 
  left_join(movie_bias, by = "movieId") %>% 
  left_join(user_bias, by = "userId") %>% 
  left_join(release_year_bias, by = "Release_Year") %>% 
  group_by(genres) %>% 
  summarise(b_g = mean(rating - mean_rating_edx2 - b_i - b_u - b_r))

# Merging all the effects (movie, user, release year, and genre) with the testing set
testing_clean <- testing %>%
  distinct(userId, movieId, genres, .keep_all = TRUE) %>% 
  left_join(movie_bias, by = "movieId") %>% 
  left_join(user_bias, by = "userId") %>% 
  left_join(release_year_bias, by = "Release_Year") %>% 
  left_join(genre_bias, by = "genres") %>% 
  mutate(b_i = ifelse(is.na(b_i), 0, b_i),
         b_u = ifelse(is.na(b_u), 0, b_u),
         b_r = ifelse(is.na(b_r), 0, b_r),
         b_g = ifelse(is.na(b_g), 0, b_g),
         predicted_rating = mean_rating_edx2 + b_i + b_u + b_r + b_g)

# Calculating the RMSE for the model with genres effect
rmse_genre_effect <- sqrt(mean((testing_clean$rating - testing_clean$predicted_rating)^2))

# Saving the RMSE in rmse_values
rmse_values$genre_effect <- round(rmse_genre_effect, 5)

# Creating a data frame with all the models' RMSE
rmse_table_combined <- data.frame(
  Model = c("Mean_Based Model", 
            "Movie_Effect Model", 
            "Movie+User_Effect Model", 
            "Movie+User+Release_Year Model", 
            "Movie+User+Release_Year+Genre Model"),
  RMSE = c(rmse_values$mean_based, 
           rmse_values$movie_effect, 
           rmse_values$movie_user_effect, 
           rmse_values$movie_user_release_effect,
           rmse_values$genre_effect)
)

# Generating the table
rmse_table_combined %>% 
  kable("html", booktabs = TRUE, row.names = FALSE, align = "c") %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = FALSE, position = "center") %>% 
  column_spec(1, width = "22em") %>%  # Fixed width for 'Model'
  column_spec(2, width = "6em") %>%   # Reduced width for 'RMSE'
  row_spec(0, bold = TRUE, color = "orange", background = "blue") %>%  # Old header row
  row_spec(1:5, extra_css = "padding: 1px;") %>%  # Uniform row padding
  kable_styling(font_size = 12, position = "center")

# Visualization bias:
# Generating the chart for genres bias (b_g)
ggplot(genre_bias, aes(x = b_g)) +
  geom_histogram(binwidth = 0.05, fill = "orange", color = "black") +  # Creating the chart
  labs(title = "Distribution of Genre Bias (b_g)",  # Title
       x = "Genre Bias (b_g)",   # X label
       y = "Count") +  # Y label
  theme_minimal() +  
  theme(plot.title = element_text(hjust = 0.5))  # Centring the title

# Regularization
# Defining the function to calculate the regularized RMSE
calculate_rmse <- function(lambda, edx2, testing, mean_rating) {
  
  # Calculating the regularized bias of each movie (b_i)
  movie_bias_reg <- edx2 %>%
    distinct(userId, movieId, .keep_all = TRUE) %>% 
    group_by(movieId) %>%
    summarise(b_i = sum(rating - mean_rating) / (n() + lambda), .groups = 'drop')
  
  # Calculating the regularized bias of each user (b_u)
  user_bias_reg <- edx2 %>%
    distinct(userId, movieId, .keep_all = TRUE) %>%
    left_join(movie_bias_reg, by = "movieId") %>% 
    group_by(userId) %>%
    summarise(b_u = sum(rating - mean_rating - b_i) / (n() + lambda), .groups = 'drop')
  
  # Calculating the regularized bias for Release_Year (b_r)
  release_year_bias_reg <- edx2 %>%
    distinct(userId, movieId, .keep_all = TRUE) %>%
    left_join(movie_bias_reg, by = "movieId") %>% 
    left_join(user_bias_reg, by = "userId") %>% 
    group_by(Release_Year) %>%
    summarise(b_r = sum(rating - mean_rating - b_i - b_u) / (n() + lambda), .groups = 'drop')
  
  # Calculating the regularized bias of each genre(b_g)
  genre_bias_reg <- edx2 %>%
    distinct(userId, movieId, genres, .keep_all = TRUE) %>%
    left_join(movie_bias_reg, by = "movieId") %>% 
    left_join(user_bias_reg, by = "userId") %>% 
    left_join(release_year_bias_reg, by = "Release_Year") %>% 
    group_by(genres) %>%
    summarise(b_g = sum(rating - mean_rating - b_i - b_u - b_r) / (n() + lambda), .groups = 'drop')
  
  # Merging the regularized bias - testing
  testing_reg <- testing %>%
    distinct(userId, movieId, genres, .keep_all = TRUE) %>%
    left_join(movie_bias_reg, by = "movieId") %>% 
    left_join(user_bias_reg, by = "userId") %>% 
    left_join(release_year_bias_reg, by = "Release_Year") %>% 
    left_join(genre_bias_reg, by = "genres") %>% 
    mutate(b_i = ifelse(is.na(b_i), 0, b_i),
           b_u = ifelse(is.na(b_u), 0, b_u),
           b_r = ifelse(is.na(b_r), 0, b_r),
           b_g = ifelse(is.na(b_g), 0, b_g),
           predicted_rating = mean_rating + b_i + b_u + b_r + b_g)
  
  # Calculating the RMSE with the regularized predictions
  rmse <- sqrt(mean((testing_reg$rating - testing_reg$predicted_rating)^2))
  
  return(rmse)
}

# Calculating global mean of edx2
mean_rating_edx2 <- edx2 %>%
  distinct(userId, movieId, .keep_all = TRUE) %>%
  summarise(mean_rating = mean(rating)) %>% 
  pull(mean_rating)

# Defining the range of lambda to test
lambdas <- seq(0, 10, 0.5)

# Calculating the RMSE for each value of lambda
rmse_results <- sapply(lambdas, calculate_rmse, edx2 = edx2, testing = testing, mean_rating = mean_rating_edx2)

# Creating a dataframe with the results
rmse_table <- data.frame(
  Lambda = lambdas,
  RMSE = rmse_results
)

# Saving the minimun RMSE in rmse_values
optimal_lambda <- lambdas[which.min(rmse_results)]
rmse_values$regularized <- round(min(rmse_results), 5)

# Displaying the optimal lambda and corresponding RMSE
print(optimal_lambda)
print(rmse_values$regularized)

# Generating the chart RMSE vs Lambda
ggplot(rmse_table, aes(x = Lambda, y = RMSE)) +
  geom_line(color = "blue") +  # Line chart
  geom_point(color = "orange", size = 2) +  # Adding points in orange
  labs(title = "RMSE vs Lambda",  # Title
       x = "Lambda",              # X-axis label
       y = "RMSE") +              # Y-axis label
  theme_minimal() +  
  theme(plot.title = element_text(hjust = 0.5))  # Centring the title

# Finding the value of lambda that minimizes the RMSE
optimal_lambda <- lambdas[which.min(rmse_results)]

# Displaying the optimal lambda value and its RMSE
cat("The optimal value of lambda is:", optimal_lambda, "\n")
cat("The minimum associated RMSE is:", min(rmse_results), "\n")

# Applying the optimal lambda to the model:
# Calculating the regularized bias of each movie (b_i)
movie_bias_reg_opt <- edx2 %>%
  distinct(userId, movieId, .keep_all = TRUE) %>% 
  group_by(movieId) %>%
  summarise(b_i = sum(rating - mean_rating_edx2) / (n() + optimal_lambda), .groups = 'drop')

# Calculating the regularized bias of each user (b_u)
user_bias_reg_opt <- edx2 %>%
  distinct(userId, movieId, .keep_all = TRUE) %>%
  left_join(movie_bias_reg_opt, by = "movieId") %>% 
  group_by(userId) %>%
  summarise(b_u = sum(rating - mean_rating_edx2 - b_i) / (n() + optimal_lambda), .groups = 'drop')

# Calculating the regularized bias for Release_Year (b_r)
release_year_bias_reg_opt <- edx2 %>%
  distinct(userId, movieId, .keep_all = TRUE) %>%
  left_join(movie_bias_reg_opt, by = "movieId") %>% 
  left_join(user_bias_reg_opt, by = "userId") %>% 
  group_by(Release_Year) %>%
  summarise(b_r = sum(rating - mean_rating_edx2 - b_i - b_u) / (n() + optimal_lambda), .groups = 'drop')

# Calculating the regularized bias for genres (b_g)
genre_bias_reg_opt <- edx2 %>%
  distinct(userId, movieId, genres, .keep_all = TRUE) %>%
  left_join(movie_bias_reg_opt, by = "movieId") %>% 
  left_join(user_bias_reg_opt, by = "userId") %>% 
  left_join(release_year_bias_reg_opt, by = "Release_Year") %>% 
  group_by(genres) %>%
  summarise(b_g = sum(rating - mean_rating_edx2 - b_i - b_u - b_r) / (n() + optimal_lambda), .groups = 'drop')

# Merging the regularized bias - testing
testing_reg_opt <- testing %>%
  distinct(userId, movieId, genres, .keep_all = TRUE) %>%
  left_join(movie_bias_reg_opt, by = "movieId") %>% 
  left_join(user_bias_reg_opt, by = "userId") %>% 
  left_join(release_year_bias_reg_opt, by = "Release_Year") %>% 
  left_join(genre_bias_reg_opt, by = "genres") %>% 
  mutate(b_i = ifelse(is.na(b_i), 0, b_i),
         b_u = ifelse(is.na(b_u), 0, b_u),
         b_r = ifelse(is.na(b_r), 0, b_r),
         b_g = ifelse(is.na(b_g), 0, b_g),
         predicted_rating = mean_rating_edx2 + b_i + b_u + b_r + b_g)

# Calculating the final RMSE with optimal lambda
rmse_final <- sqrt(mean((testing_reg_opt$rating - testing_reg_opt$predicted_rating)^2))

# Saving the RMSE in rmse_values
rmse_values$regularized <- round(rmse_final, 5)

# Displaying final RMSE
cat("RMSE of final model with optimal lambda is:", rmse_final, "\n")

# Comparing RMSE
# Rounding RMSE values to 5 decimal places
mean_based_val <- round(rmse_values$mean_based, 5)
movie_effect_val <- round(rmse_values$movie_effect, 5)
movie_user_effect_val <- round(rmse_values$movie_user_effect, 5)
movie_user_release_val <- round(rmse_values$movie_user_release_effect, 5)
genre_effect_val <- round(rmse_values$genre_effect, 5)
regularized_val <- round(rmse_values$regularized, 5)

# Creating a data frame for the RMSE values
rmse_table <- data.frame(
  Model = c("Mean_Based Model", 
            "Movie_Effect Model", 
            "Movie+User_Effect Model", 
            "Movie+User_Release_Year Model", 
            "Movie+User_Release_Year+Genre Model", 
            "Regularized Model"),
  RMSE = c(mean_based_val, 
           movie_effect_val, 
           movie_user_effect_val, 
           movie_user_release_val, 
           genre_effect_val, 
           regularized_val)
)

# Generating the table with kableExtra in HTML format
rmse_table %>%
  kable("html", booktabs = TRUE, row.names = FALSE, align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = FALSE, position = "center", font_size = 12) %>%
  column_spec(1, width = "22em", color = "black") %>%
  column_spec(2, width = "10em", color = "black") %>%
  row_spec(0, bold = TRUE, color = "#FFFFFF", background = "#4472C4") %>%
  row_spec(1:6, extra_css = "padding: 5px;")


# Analyzing the model's performance through the distribution of residuals
# Calculating residuals
residuals <- testing_clean$rating - testing_clean$predicted_rating

# Density chart
ggplot(data.frame(residuals), aes(x = residuals)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "orange", color = "black") +  # Histogram with density on y-axis
  geom_density(color = "blue") +  # Adding a density line in blue
  labs(title = "Distribution of Residuals",  # Title
       x = "Residuals",                      # X-axis label
       y = "Density") +                      # Y-axis label
  theme_minimal() +  
  theme(plot.title = element_text(hjust = 0.5))  # Centering the title

# Matrix factorization
# Suppressing any unnecessary output
invisible(capture.output({
  suppressMessages({
    suppressWarnings({
      
      # Converting the data from edx2 into the format required by recosystem (user, item, rating)
      edx2_recosystem <- edx2 %>%
        select(userId, movieId, rating)
      
      # Saving the training data to a file to be used by the recosystem model
      write.table(edx2_recosystem, file = "edx2_recosystem.txt", sep = " ", row.names = FALSE, col.names = FALSE)
      
      # Preparing the test set in a similar way
      testing_recosystem <- testing %>%
        select(userId, movieId, rating)
      write.table(testing_recosystem, file = "testing_recosystem.txt", sep = " ", row.names = FALSE, col.names = FALSE)
      
      # Creating the Reco model object
      r <- Reco()
      
      # Reading the training set
      train_set <- data_file("edx2_recosystem.txt")
      
      # Reading the test set
      test_set <- data_file("testing_recosystem.txt")
      
      # Tuning the matrix factorization model to find optimal hyperparameters
      # Reducing number of iterations (niter) and dimensions (dim) to speed up the process
      opts <- r$tune(train_set, opts = list(dim = c(10, 20), lrate = c(0.1), costp_l2 = c(0.1), costq_l2 = c(0.1), nthread = 2, niter = 5))
      
      # Training the model with the best parameters found, further reducing iterations
      r$train(train_set, opts = c(opts$min, niter = 10))
      
      # Creating a temporary file for storing predictions
      pred_file <- tempfile()
      
      # Making predictions on the test set
      r$predict(test_set, out_file(pred_file))
      
      # Reading the predictions
      predictions <- scan(pred_file)
      
      # Adding predictions to the testing set
      testing_predictions <- testing
      testing_predictions$predicted_rating <- predictions
      
      # Calculating RMSE for the matrix factorization model
      rmse_factorization <- sqrt(mean((testing_predictions$rating - testing_predictions$predicted_rating)^2))
      
      # Saving the RMSE of the matrix factorization model in rmse_values
      rmse_values$factorization <- round(rmse_factorization, 5)
      
      # Displaying the RMSE of the new model
      cat("The RMSE of the matrix factorization model is:", round(rmse_factorization, 5), "\n")
      
      # Cleaning up temporary files
      file.remove("edx2_recosystem.txt", "testing_recosystem.txt", pred_file)
      
    })
  })
}))

# Formatting the rounded RMSE values for text output
mean_based_val <- round(rmse_values$mean_based, 5)
movie_effect_val <- round(rmse_values$movie_effect, 5)
movie_user_effect_val <- round(rmse_values$movie_user_effect, 5)
movie_user_release_val <- round(rmse_values$movie_user_release_effect, 5)
genre_effect_val <- round(rmse_values$genre_effect, 5)
regularized_val <- round(rmse_values$regularized, 5)
factorization_val <- round(rmse_values$factorization, 5)

# Display the rounded values for use in further reporting or output
print(mean_based_val)
print(movie_effect_val)
print(movie_user_effect_val)
print(movie_user_release_val)
print(genre_effect_val)
print(regularized_val)
print(factorization_val)

# Formatting the rounded RMSE values to text to use them in the table
mean_based_val <- round(rmse_values$mean_based, 5)
movie_effect_val <- round(rmse_values$movie_effect, 5)
movie_user_effect_val <- round(rmse_values$movie_user_effect, 5)
movie_user_release_val <- round(rmse_values$movie_user_release_effect, 5)
genre_effect_val <- round(rmse_values$genre_effect, 5)
regularized_val <- round(rmse_values$regularized, 5)
factorization_val <- round(rmse_values$factorization, 5)

# Creating a data frame for the RMSE values
rmse_table <- data.frame(
  Model = c("Mean_Based Model", 
            "Movie_Effect Model", 
            "Movie+User_Effect Model", 
            "Movie+User_Release_Year Model", 
            "Movie+User_Release_Year+Genre Model", 
            "Regularized Model", 
            "Matrix Factorization Model"),
  RMSE = c(mean_based_val, 
           movie_effect_val, 
           movie_user_effect_val, 
           movie_user_release_val, 
           genre_effect_val, 
           regularized_val, 
           factorization_val)
)

# Generating the table
rmse_table %>%
  kable("html", booktabs = TRUE, row.names = FALSE, align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = FALSE, position = "center", font_size = 12) %>%
  column_spec(1, width = "22em", color = "black") %>%
  column_spec(2, width = "10em", color = "black") %>%
  row_spec(0, bold = TRUE, color = "#FFFFFF", background = "#4472C4") %>%
  row_spec(1:7, extra_css = "padding: 5px;")

# Final validation
# Suppressing any unnecessary output
invisible(capture.output({
  suppressMessages({
    suppressWarnings({
      
      # Converting the data from edx and final_holdout_test into the required format for recosystem (user, item, rating)
      edx_recosystem <- edx %>%
        select(userId, movieId, rating)
      
      final_holdout_test_recosystem <- final_holdout_test %>%
        select(userId, movieId, rating)
      
      # Saving the data to temporary files to pass them to the model
      write.table(edx_recosystem, file = "edx_recosystem.txt", sep = " ", row.names = FALSE, col.names = FALSE)
      write.table(final_holdout_test_recosystem, file = "final_holdout_test_recosystem.txt", sep = " ", row.names = FALSE, col.names = FALSE)
      
      # Creating the model object and loading the data
      r <- Reco()
      
      # Reading training data (edx)
      train_set <- data_file("edx_recosystem.txt")
      
      # Reading testing data (final_holdout_test)
      test_set <- data_file("final_holdout_test_recosystem.txt")
      
      # Fitting the model with the best hyperparameters found during the factorization phase
      r$train(train_set, opts = c(opts$min, niter = 10))
      
      # Creating a temp file for predictions
      pred_file <- tempfile()
      
      # Making predictions
      r$predict(test_set, out_file(pred_file))
      
      # Reading predictions
      final_predictions <- scan(pred_file)
      
      # Adding predictions to final_holdout_test
      final_holdout_test_predictions <- final_holdout_test
      final_holdout_test_predictions$predicted_rating <- final_predictions
      
      # Calculating RMSE for final validation
      rmse_final_validation <- sqrt(mean((final_holdout_test_predictions$rating - final_holdout_test_predictions$predicted_rating)^2))
      
      # Displaying the RMSE for the final validation
      cat("The RMSE of the matrix factorization model is:", round(rmse_final_validation, 5), "\n")
      
      # Cleaning up temp files
      file.remove("edx_recosystem.txt", "final_holdout_test_recosystem.txt", pred_file)
      
      # Storing RMSE of final validation in rmse_values
      rmse_values$final_validation <- round(rmse_final_validation, 5)
      
    })
  })
}))

# Formatting the RMSE values for display
mean_based_val <- round(rmse_values$mean_based, 5)
movie_effect_val <- round(rmse_values$movie_effect, 5)
movie_user_effect_val <- round(rmse_values$movie_user_effect, 5)
movie_user_release_val <- round(rmse_values$movie_user_release_effect, 5)
genre_effect_val <- round(rmse_values$genre_effect, 5)
regularized_val <- round(rmse_values$regularized, 5)
factorization_val <- round(rmse_values$factorization, 5)
final_validation_val <- round(rmse_values$final_validation, 5)

# Creating a data frame for the RMSE values
rmse_table_combined <- data.frame(
  Model = c("Mean_Based Model", 
            "Movie_Effect Model", 
            "Movie+User_Effect Model", 
            "Movie+User_Release_Year Model", 
            "Movie+User_Release_Year+Genre Model", 
            "Regularized Model", 
            "Matrix Factorization Model", 
            "Final Validation"),
  RMSE = c(mean_based_val, 
           movie_effect_val, 
           movie_user_effect_val, 
           movie_user_release_val, 
           genre_effect_val, 
           regularized_val, 
           factorization_val, 
           final_validation_val)
)

# Generating the table
rmse_table_combined %>%
  kable("html", booktabs = TRUE, row.names = FALSE, align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = FALSE, position = "center", font_size = 12) %>%
  column_spec(1, width = "22em", color = "black") %>%
  column_spec(2, width = "10em", color = "black") %>%
  row_spec(0, bold = TRUE, color = "#FFFFFF", background = "#4472C4") %>%
  row_spec(1:8, extra_css = "padding: 5px;")


