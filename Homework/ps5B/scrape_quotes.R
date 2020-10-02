quotes_html_2 <- read_html("https://www.brainyquote.com/authors/albert-camus-quotes")

quotes2 <- quotes_html_2 %>%
  html_nodes(".oncl_q") %>%
  html_text()

person2 <- quotes_html_2 %>%
  html_nodes(".oncl_a") %>%
  html_text()

# put in data frame with two variables (person and quote)
quotes_dat_2 <- data.frame(person = person2, quote = quotes2
                           , stringsAsFactors = FALSE) %>%
  mutate(together = paste('"', as.character(quote), '" --'
                          , as.character(person), sep=""))

write_csv(quotes_dat_2, "/Users/thainguyen/Desktop/STUDYYYYYYYYYYYYYYYYYYYYYY/College/Fall 2020/Data Science STAT 231/Git/Stat-231-repo/Homework/ps5B/quotes.csv")
