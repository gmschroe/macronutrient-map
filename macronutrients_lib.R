# functions for making macronutrient map -----

library(tidyverse)
library(tricolore)
library(tsne)
library(packcircles)

################################################################################
# Tidy USDA data (column names, numeric variables, which variables and food groups to keep)
#
# usda_data: USDA data from NutrienTrackeR
# rm_group: character vector of food groups to remove
usda_data_tidy <- function(
    usda_data,
    rm_group = c("Baby Foods",
                 "Soups, Sauces, and Gravies",
                 "Fast Foods",
                 "Meals, Entrees, and Side Dishes",
                 "American Indian/Alaska Native Foods",
                 "Restaurant Foods")
) {
  
  # clean column names
  usda_data <- janitor::clean_names(usda_data) 
  
  usda_data <- usda_data |>
    
    # remove unwanted groups
    filter(!food_group %in% rm_group) |>
    
    # remove unwanted variables
    select(contains("food") | c("energy_kcal") | contains("_g")) |>
    
    # change required columns to numeric
    mutate_at(
      c('protein_g', 
        'carbohydrate_by_difference_g', 
        'total_lipid_fat_g',
        'energy_kcal'),
      as.numeric
    ) |>
    
    # name change
    mutate(
      food_name = str_replace(food_name, "Jew's", "Jelly")
    )
  
  return(usda_data)
  
}

################################################################################
# Add number of words and characters in food names.
# Exclude ", without salt ...," and ", without added ...," parts of descriptions 
# from character and word counts.
#
# usda_data: dataframe containing 'food_name'
usda_add_n_words_and_char <- function(usda_data) {
  usda_data <- usda_data |>
    mutate(
      name_simple = gsub("(, without added)\\s[^,]*", "", food_name),
      name_simple = gsub("(, without salt)", "", name_simple)
    ) 
  
  usda_data <- usda_data |>
    mutate(n_char = nchar(name_simple),
           n_words = lengths(gregexpr("\\W+", name_simple)) + 1
    )
  
  return(usda_data)
}

################################################################################
# Remove alcoholic foods/beverages based on estimated calories from 
# non-alcoholic macros.
#
# usda_data: dataframe with 'protein_g', 'carbohydrate_by_difference_g', 
# 'total_lipid_fat_g', and 'energy_kcal' variables
# cal_perc_diff_thresh: calorie difference (between actual and estimated) 
# threshold for removing foods
usda_remove_alcoholic <- function(usda_data, cal_perc_diff_thresh = 0.68) {
  
  usda_data <- usda_data |>
    # estimate calories and calc percentage diff to actual calories
    mutate(
      cal_est = 4 * protein_g + 4 * carbohydrate_by_difference_g + 9 * total_lipid_fat_g,
      cal_perc_diff = (energy_kcal - cal_est) / cal_est
    ) |>
    # only keep foods below calorie percentage diff threshold
    filter(cal_perc_diff < cal_perc_diff_thresh)
  
  return(usda_data)
}

################################################################################
# Compute relative amount of macros
#
# usda_data: dataframe containing 'carb_rel', 'fat_rel', and 'protein_rel' numeric variables
# rm_zero: whether to remove foods with zero total macros (default: TRUE)
usda_relative_macros <- function(usda_data, rm_zero = TRUE) {
  
  usda_data <- usda_data |>
    mutate(
      g_total = protein_g + total_lipid_fat_g + carbohydrate_by_difference_g, # total g of macronutrients
      protein_rel = protein_g / g_total,
      fat_rel = total_lipid_fat_g / g_total,
      carb_rel = carbohydrate_by_difference_g / g_total
    ) 
  
  # remove any foods with no macronutrient content
  if (rm_zero) {
    usda_data <- usda_data |>
      filter(g_total > 0) 
  }
  
  return(usda_data)
}


################################################################################
# Computes radius of points based on calories.
#
# usda_data: dataframe containing 'energy_kcal'
# min_r: smallest point radius (default: 0.5)
# max_r: largest point radius (default: 2)
usda_add_calories_r <- function(
    usda_data, # dataframe of USDA data
    min_r = 0.5, # min radius
    max_r = 2 # max radius
) {
  
  # min and max for normalising calories
  max_cal <- max(usda_data$energy_kcal)
  min_cal <- min(usda_data$energy_kcal)
  
  # radius r
  usda_data <- usda_data |>
    mutate(
      r = (max_r - min_r) * ( (energy_kcal - min_cal) / (max_cal - min_cal) ) + min_r
    )
  
  return(usda_data)
}

################################################################################
# Computes colours from relative macronutrient content.
#
# usda_data: data frame containing 'carb_rel', 'fat_rel', and 'protein_rel' numeric variables
# additional arguments passed to tricolore::Tricolore
usda_add_macronutrient_clrs <- function(usda_data, ...) {
  
  # colourmap
  tri_clrmap <- Tricolore(
    usda_data |> select(contains('rel')), 
    'carb_rel', 'fat_rel', 'protein_rel', ...)
  
  # add colours to data frame
  usda_data <- usda_data |>
    mutate(
      clr = tri_clrmap
    )
  
  return(usda_data)
}

################################################################################
# Embed food using t-SNE of macronutrient and calorie info.
#
# usda_tsne: dataframe containing 'carb_rel', 'fat_rel', 'protein_rel', and 'energy_kcal'
# cal_weight: how much to weight calories relative to macronutrients (< 1 decreases weight, > 1 increases weight)
# seed_tsne: seed for t-SNE algorithm
# additional arguments are passed to t-SNE (e.g., perplexity, number of iterations)
usda_tsne <- function(
    usda_data,
    cal_weight = 1,
    seed_tsne = 0,
    ... 
) {
  
  # extract relative macronutrient content and calories
  usda_data_for_dist <- usda_data |>
    select(contains('rel') | 'energy_kcal') 
  
  # convert to matrix
  usda_data_for_dist <- as.matrix(usda_data_for_dist)
  
  # normalise values to be between zero and 1 so have similar weighting for t-SNE
  usda_data_for_dist <- sweep(usda_data_for_dist, 2, 
                              apply(usda_data_for_dist, 2, max), "/")
  
  # change weight of calories relative to macros
  usda_data_for_dist[,4] <- usda_data_for_dist[,4] * cal_weight
  
  # compute distance matrix
  d <- dist(usda_data_for_dist)
  
  # t-SNE using distances
  set.seed(seed_tsne)
  ydata <- tsne(d, ...)
  
  # add t-SNE coordinates to original dataframe
  usda_data <- usda_data |>
    mutate(
      x_tsne = ydata[,1],
      y_tsne = ydata[,2],
    )
  
  return(usda_data)
}

################################################################################
# Repel nearby points based on t-SNE coordinates (x_tsne, y_tsne) and point size (r).
#
# Uses packcircles package.
# usda_data: dataframe containing 'x_tsne', 'y_tsne', and 'r' numeric variables
# seed_repel: seed for repel algorithm
usda_tsne_repel <- function(
    usda_data,
    seed_repel = 0
) {
  
  # repel nearby points
  set.seed(seed_repel)
  points_repelled <- circleRepelLayout(
    usda_data, 
    xysizecols = c('x_tsne', 'y_tsne', 'r'),
    wrap = FALSE)
  
  # add repelled coordinates to data frame
  usda_data <- usda_data |>
    mutate(
      x = points_repelled$layout$x,
      y = points_repelled$layout$y
    )
  
  return(usda_data)
}