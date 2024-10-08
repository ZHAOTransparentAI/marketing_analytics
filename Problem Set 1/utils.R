# import dependencies
library(dplyr)
library(ggplot2)

# Define project colors
primary <- '#3A6B35'
secondary <- '#79ab74'
tertiary <- '#E3B448'
quaternary <- '#E0A96D'
quinary <- '#B85042'
senary <- '#F1AC88'
septenary <- '#79A7D3'
octonary <- '#6883BC'
nonary <- '#8A307F'
denary <- '#2F3C7E'
eleventh <- '#FF69B4'
dark <- '#28282B'

# function that loads and merges the data from the first problem set
load_problemset1_data <- function() {
  # load the data from the csv file
  df_mat <- read.csv("Material/data/raw/student-mat.csv", sep = ";")
  df_por <- read.csv("Material/data/raw/student-por.csv", sep = ";")
  
  # create a new column is_mat and is_por and merge the two datasets
  df_mat$is_mat <- 1
  df_por$is_por <- 1
  df <- merge(df_mat, df_por, all = TRUE)
  
  # set all null values from is_por and is_mat to 0
  df$is_por[is.na(df$is_por)] <- 0
  df$is_mat[is.na(df$is_mat)] <- 0
  
  # encode all yes and no columns to 1 and 0
  df <- encode_yes_no_columns(df)
  
  return(df)
}


# function that encodes all "yes" and "no" columns to 1 and 0
encode_yes_no_columns <- function(df) {
  # go through the dataframe and find all columns that contain "yes" and "no"
  yes_no_columns <- c()
  for (column in colnames(df)) {
    if (length(grep("yes|no", df[, column])) > 0) {
      yes_no_columns <- c(yes_no_columns, column)
    }
  }
  
  # loop over all columns and encode the values to 1 and 0
  for (column in yes_no_columns) {
    df[, column] <- ifelse(df[, column] == "yes", 1, 0)
  }
  
  return(df)
}


# function that one hot encodes all categorical columns
one_hot_encode <- function(df, columns_cat) {
  # Ensure the specified columns are character or factor type
  df[, columns_cat] <- lapply(df[, columns_cat], as.character)
  
  # One-hot encode the specified categorical columns
  encoded_df <- model.matrix(~.-1 + ., data = df[, columns_cat])
  
  # Combine the one-hot encoded columns with the original data frame
  df <- cbind(df, encoded_df)
  
  # Remove the original categorical columns
  df <- df[, !names(df) %in% columns_cat]
  
  # Return the modified data frame
  return(df)
}