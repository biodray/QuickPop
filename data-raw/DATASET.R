## code to prepare the dataset using usethis::use_data

# strcSummary

# Load raw data from .csv file
strcSummary.data <- readr::read_csv("data-raw/results_summary.csv",
                               skip = 1) |> dplyr::select(-c(`1`))

strcSummary.data |> head()

# Save the cleaned data in the required R package location


usethis::use_data(strcSummary.data, overwrite = T)
