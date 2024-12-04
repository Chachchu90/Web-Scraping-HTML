# Load necessary libraries
library(rvest)
library(tidyselect)
library(xml2)
library(tidyverse)
library(purrr)
library(readr)

# Define the link to the website
link <- "http://books.toscrape.com/"

# Parse the HTML of the webpage
page <- read_html(link)

# Display the HTML structure of the webpage
print(page)

# Extract the titles of the books
titles <- page %>%
  html_elements("h3")

# Extract the titles of the books and convert them to text
titles <- page %>%
  html_elements("h3") %>% 
  html_text2()

# Print the extracted titles
print(titles)

# Extract the full titles of the books from the 'title' attribute of the <a> tags inside <h3>
titles <- page %>%
  html_elements("h3") %>%  # Select all <h3> tags
  html_elements("a") %>%   # Within each <h3>, select the <a> tag
  html_attr("title")       # Extract the 'title' attribute from each <a> tag

# Print the full titles
print(titles)

# Extract the prices of the books
book_price <- page %>%
  html_elements(".price_color") %>% 
  html_text2()

# Convert titles and prices into tibbles
title_tibble <- tibble(titles = titles)
price_tibble <- tibble(book_price = book_price)

# Combine the title and price tibbles into a single data frame
book_df <- bind_cols(title_tibble, price_tibble)

# Print the resulting data frame
print(book_df)
