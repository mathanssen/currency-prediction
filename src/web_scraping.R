library(xml2)
library(RSelenium)
library(rvest)
library(lubridate)


# Settings
options(timeout = 100000)

# Parameters (date = Year, month, day) ----------------
first_date <- "2013-01-01"
end_date <- as.character(Sys.Date())

# Create list of dates ----------------
list_date <- seq(as.Date(first_date), as.Date(end_date), by = "days")

# Functions ----------------
scrap_table <- function(webpage) {
  # Scrap table from website
  df <- read_html(webpage) %>%
        html_nodes("table") %>%
        .[1] %>%
        html_table(fill = TRUE) %>%
        .[[1]]
  return(df)
}

create_df <- function(date) {
  # Convert html table to data frame
  year <- substr(date, 1, 4)
  month <- substr(date, 6, 7)
  day <- substr(date, 9, 10)
  url <- paste("https://www.xe.com/currencytables/?from=USD&date=",
               paste(year, month, day, sep = "-"),
               sep = '')
  df <- scrap_table(url)
  df$date_ref <- mdy(paste(month, day, year, sep = '/'))
  return(df)
}

# Create main data frame ----------------
df_main <- create_df(first_date)

# Scrap historical and concatenate data frames ----------------
for (i in 2:(length(list_date) - 1)) {
  c <- 0
  while (c < 5) {
    try(df_new <- create_df(list_date[[i]]))
    c <- ncol(df_new)
  }
  df_main <- rbind(df_main, df_new)
  # Sys.sleep(3)
}

# Rename columns ----------------
colnames(df_main) <- c("currency_code",
                       "currency_name",
                       "units_per_usd",
                       "usd_per_unit",
                       "date")

# Save data frame ----------------
write.csv(df_main, '..\\data\\currency.csv')

