library(data.table)

#Folder of datasets
folder <- "~/Downloads/archive 2"

data_files <- file.path(
  folder,
  paste0("Combined_Flights_", 2018:2022, ".csv")
)

fpreview <- fread(
  data_files[1],
  nrows = 1000
)

str(fpreview)
names(fpreview)
head(fpreview)

#Find missing values
missing_2018 <- sapply(
  Combined_Flights_2018,
  function(x) sum(is.na(x))
)
missing_2019 <- sapply(
  Combined_Flights_2019,
  function(x) sum(is.na(x))
)
missing_2020 <- sapply(
  Combined_Flights_2020,
  function(x) sum(is.na(x))
)
missing_2021 <- sapply(
  Combined_Flights_2021,
  function(x) sum(is.na(x))
)
missing_2022 <- sapply(
  Combined_Flights_2022,
  function(x) sum(is.na(x))
)

missing_2018
table(Combined_Flights_2018$Cancelled)
table(
  Combined_Flights_2018$Cancelled,
  is.na(Combined_Flights_2018$DepTime)
)
