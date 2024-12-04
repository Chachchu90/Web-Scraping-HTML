# Load necessary libraries
library(rvest)
library(tidyverse)
library(lubridate)




# Initialize an empty list to store data from each page
all_reviews <- list()

# Loop through the first 5 pages
for (i in 1:5) {
  # Construct the URL for the current page
  page_url <- paste0("https://www.trustpilot.com/review/ubereats.com?page=", i)
  
  # Read the HTML content of the page
  page <- read_html(page_url)
  # Extract the review titles
  review_titles <- page %>%
    html_elements("h2.typography_heading-s__f7029.typography_appearance-default__AAY17") %>%
    html_text2()
  print(review_titles)
  
  # Extract the review texts
  review_texts <- page %>%
    html_elements("p.typography_body-l__KUYFJ.typography_appearance-default__AAY17.typography_color-black__5LYEn") %>%
    html_text2()
  print(review_texts)
  
  # Extract the reviewers' names
  reviewers <- page %>%
    html_elements("span.typography_heading-xxs__QKBS8.typography_appearance-default__AAY17") %>%
    html_text2()
  print(reviewers)
  
  # Function to help convert date strings to a real date format
  extract_dates <- function(review_date) {
    dates <- sub("Date of experience: ", "", review_date)
    dates <- mdy(dates)
    return(dates)
  }
  
  # Extract the review dates
  review_dates <- page %>%
    html_elements("p.typography_body-m__xgxZ_") %>%
    html_text2()
  
  # Print the extracted dates
  print(review_dates)
  
  # Filter out empty or invalid date strings before applying the extract_dates function
  valid_review_dates <- review_dates[review_dates != "" & grepl("\\d{4}-\\d{2}-\\d{2}", mdy(review_dates), perl = TRUE)]
  
  # Convert the filtered dates to a proper date format
  formatted_dates <- extract_dates(valid_review_dates)
  
  # Convert formatted dates to character to remove quotation marks
  formatted_dates_char <- format(formatted_dates, format = "%Y-%m-%d")
  
  # Print the formatted dates without quotation marks
  cat(formatted_dates_char, sep = "\n")
  
  
  
  
  
  
  #Helper function to isolate the rating number and convert it to an integer
  extract_rating <- function(rating_text) {
    # Use sub to capture the digit from the text
    rating <- sub("Rated (\\d) out of 5 stars", "\\1", rating_text)
    # Convert the captured digit to integer
    return(as.integer(rating))
  }
  
  # Extract the star ratings using the correct CSS selector for the <img> tag
  ratings <- page %>%
    html_elements("img[alt*='Rated']") %>%  # This targets all <img> tags with an alt attribute containing 'Rated'
    html_attr("alt")
  
  # Print the extracted raw rating strings to check
  print(ratings)
  
  # Use the helper function to parse the ratings and convert to integers
  star_ratings <- sapply(ratings, extract_rating, simplify = TRUE)
  
  # Print the extracted and converted ratings
  print(star_ratings)
  
  
  print(length(review_titles))
  print(length(review_texts))
  print(length(reviewers))
  print(length(formatted_dates))
  print(length(star_ratings))
  
  # Only create the tibble if all vectors have the same length
  if (length(review_titles) == length(review_texts) && 
      length(review_texts) == length(reviewers) &&
      length(reviewers) == length(formatted_dates) &&
      length(formatted_dates) == length(star_ratings)) {
    
    
    # Create a data frame for the current page
    reviews_df <- tibble(
      Title = review_titles,
      Review = review_texts,
      Reviewer = reviewers,
      Date = formatted_dates,
      Rating = star_ratings
    )
    
    # Append the data frame to the list
    all_reviews[[i]] <- reviews_df
  } else {
    message("Skipping page ", i, " due to inconsistent data lengths.")
  }
}

# Combine all pages into a single data frame
final_reviews_df <- bind_rows(all_reviews)

# Print the combined data frame
print(final_reviews_df)

# Load necessary library
library(dplyr)

# Ensure the Date column is in Date format
final_reviews_df$Date <- as.Date(final_reviews_df$Date)

# Sort the data frame by the Date column in ascending order
final_reviews_df_sorted <- final_reviews_df %>% arrange(Date)

# Print the sorted data frame
print(final_reviews_df_sorted)



## Extra Expert thing
### use lapply to vectorize the process



# Function to scrape data from a single page
scrape_page <- function(page_number) {
  # Construct the URL for the current page
  page_url <- paste0("https://www.trustpilot.com/review/ubereats.com?page=", page_number)
  
  # Read the HTML content of the page
  page <- read_html(page_url)
  
  # Extract the review titles
  review_titles <- page %>%
    html_elements("h2.typography_heading-s__f7029.typography_appearance-default__AAY17") %>%
    html_text2()
  
  # Extract the review texts
  review_texts <- page %>%
    html_elements("p.typography_body-l__KUYFJ.typography_appearance-default__AAY17.typography_color-black__5LYEn") %>%
    html_text2()
  
  # Extract the reviewers' names
  reviewers <- page %>%
    html_elements("span.typography_heading-xxs__QKBS8.typography_appearance-default__AAY17") %>%
    html_text2()
  
  # Function to help convert date strings to a real date format
  extract_dates <- function(review_date) {
    dates <- sub("Date of experience: ", "", review_date)
    dates <- mdy(dates)
    return(dates)
  }
  
  # Extract the review dates
  review_dates <- page %>%
    html_elements("p.typography_body-m__xgxZ_") %>%
    html_text2()
  
  # Filter out empty or invalid date strings before applying the extract_dates function
  valid_review_dates <- review_dates[review_dates != "" & grepl("\\w+ \\d{2}, \\d{4}", review_dates)]
  
  # Convert the filtered dates to a proper date format
  formatted_dates <- extract_dates(valid_review_dates)
  
  # Helper function to isolate the rating number and convert it to an integer
  extract_rating <- function(rating_text) {
    # Use sub to capture the digit from the text
    rating <- sub("Rated (\\d) out of 5 stars", "\\1", rating_text)
    # Convert the captured digit to integer
    return(as.integer(rating))
  }
  
  # Extract the star ratings using the correct CSS selector for the <img> tag
  ratings <- page %>%
    html_elements("img[alt*='Rated']") %>%  # This targets all <img> tags with an alt attribute containing 'Rated'
    html_attr("alt")
  
  # Use the helper function to parse the ratings and convert to integers
  star_ratings <- sapply(ratings, extract_rating, simplify = TRUE)
  
  # Only create the tibble if all vectors have the same length
  if (length(review_titles) == length(review_texts) && 
      length(review_texts) == length(reviewers) &&
      length(reviewers) == length(formatted_dates) &&
      length(formatted_dates) == length(star_ratings)) {
    
    # Create a data frame for the current page
    reviews_df <- tibble(
      Title = review_titles,
      Review = review_texts,
      Reviewer = reviewers,
      Date = formatted_dates,
      Rating = star_ratings
    )
    
    return(reviews_df)
  } else {
    message("Skipping page ", page_number, " due to inconsistent data lengths.")
    return(NULL)
  }
}

# Use lapply to scrape multiple pages

# Define the page numbers that want to scrape
pages <- 1:5

# Use lapply to scrape each page and store the results in a list of data frames
all_reviews <- lapply(pages, scrape_page)



# Combine the list of data frames into a single data frame
final_reviews_lappy <- bind_rows(all_reviews)

# Print the final combined data frame
print(final_reviews_lappy)

library(dplyr)

# Ensure the Date column is in Date format
final_reviews_lappy$Date <- as.Date(final_reviews_lappy$Date)

# Sort the data frame by the Date column in ascending order
final_reviews_lappy_sorted <- final_reviews_lappy %>% arrange(Date)

# Print the sorted data frame
print(final_reviews_lappy_sorted)

