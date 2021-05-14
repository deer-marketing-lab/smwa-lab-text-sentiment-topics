library(readr)
library(dplyr)
library(tibble)
library(tidyr)
library(tidytext)
library(textstem)

hotel_reviews <- 
    read_csv('data/reviews.csv') %>%
    rownames_to_column("id")

# provide the output of these text cleaning steps to students
tidy_reviews <- 
    hotel_reviews %>%
    rownames_to_column("id") %>%
    unnest_tokens(word, text) %>%
    mutate(word = lemmatize_words(word))

nums <- tidy_reviews %>% 
    filter(str_detect(word, "^[0-9]")) %>% 
    select(word) %>% 
    unique()

my_stop_words <- tibble(
    word = c(
        "hotel", "stay", "stayed"
    ),
    lexicon = "hotels"
)

tidy_reviews <- 
    tidy_reviews %>% 
    anti_join(stop_words) %>%
    anti_join(my_stop_words) %>%
    anti_join(nums, by = "word") %>%
    select(-hotel, -source)

word_counts <- 
    tidy_reviews %>%
    group_by(word) %>%
    count(sort = TRUE) %>%
    filter(n > 5)

tidy_reviews <- 
    tidy_reviews %>%
    filter(word %in% word_counts$word)

write_csv(tidy_reviews, 'data/tidy_reviews.csv')
