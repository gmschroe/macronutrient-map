# Creative interactive plot of food macronutrient content
# Gabrielle M. Schroeder
# July 2023

# ----
rm(list = ls())
library(tidyverse)
library(NutrienTrackeR)
library(tsne)
library(plotly)
library(processx)
library(orca)
library(tricolore)
library(packcircles)
library(systemfonts)
library(glue)

source('macronutrients_map/macronutrients_lib.R')

# load USDA data ---
data(food_composition_data)
names(food_composition_data) # databases available
usda_data <- as_tibble(food_composition_data$USDA) # keep USDA data
rm(list = c('food_composition_data'))

# tidy USDA data ---
# remove unwanted food groups and variables 
# set numeric variables

# all food groups
unique(usda_data$food_group)

# remove extra groups and only keep macronutrient/calorie info
usda_data <- usda_data_tidy(usda_data)

# Add variables for number of characters and words ---
# Will use to filter out longer, more specific descriptions
usda_data <- usda_add_n_words_and_char(usda_data)

# Plot distributions of word and character counts 
ggplot(usda_data, aes(n_char)) +
  geom_histogram(binwidth = 5, boundary = 0)

ggplot(usda_data, aes(n_words)) +
  geom_histogram(binwidth = 1, boundary = 0.5)  


# select subset of data for tsne ---
word_lim <- 6 # max number of words
usda_data_small <- usda_data |> filter(n_words <= word_lim)
rm(list = c('usda_data'))

# estimate number of calories from macros ---
# if actual calories far exceed, remove (calories are from alcohol)

usda_data_small <- usda_remove_alcoholic(usda_data_small)

# Strings to remove ---
# Will keep alcoholic as a string to help remove some alcoholic edge cases not 
# caught by calorie differences
# case sensitive match 
# Also remove strings with multiple uppercase letters in a row (brand names)
rm_str <- c("Spices", "human", "Emu", "Ostrich", "Turtle", "Game meat", "testes",
            # brands that were not capitalised
            "Reddi Wip", "Oscar Mayer", "Nabisco", "Pillsbury", "George Weston",
            "Heinz", "Weight Watcher", "Interstate Brands Corp", "Glutino",
            "Marie biscuit", "Schar", "Hormel Pillow", "Archway Home", 
            "Artificial Blueberry", "Goya", "Gamesa",
            # alcochol/alcohol related
            "cocktail", "Wine", "Alcoholic", "Whiskey",
            # cooking/processing (space beforehand to exclude words containing - e.g., uncooked)
            " sauteed", " cooked", " roasted", " baked", " home-prepared", " pasteurized",
            " frozen", " sweetened", " simmered", " stewed", " microwaved", " pan-browned",
            " smoked", " stabilized", " reduced calorie", " fried", " boiled", " lite",
            " dehydrated", " canned", " pickled", " dried", " kippered", " toasted",
            "commercially prepared", "Commercially Prepared",
            "-roasted", "-cooked", "freeze-dried", "special dietary"
) 
usda_data_small <- usda_data_small |>
  # remove if contains string in rm_str
  filter(!str_detect(food_name, paste(rm_str, collapse = "|"))) |>
  # remove multiple uppercase in a row
  filter(!grepl('[A-Z]{2,}', food_name))

# Now that we have the final list of foods: compute relative amounts of fat, carbs, and protein ---
usda_data_small <- usda_relative_macros(usda_data_small)

# points colours and sizes ---

# sizes based on calories
usda_data_small <- usda_add_calories_r(usda_data_small, min_r = 0.1, max_r = 1)

# colours based on macros
clr_lightness <- 0.7
clr_contrast <- 0.5
usda_data_small <- usda_add_macronutrient_clrs(
  usda_data_small, breaks = 100, legend = FALSE,
  lightness = clr_lightness, contrast = clr_contrast)

# # tsne scan ----
# (Used to find nice configuration; do not need to run. Slow, especially for large scan size.)
#
# scan_seed <- 5:20
# scan_perplex <- c(75, 100) #seq(60, 100, by = 10)
# scan_weight <- c(0.1, 0.25) #c(0.1, seq(0.25, 1.25, by = 0.25))
# 
# for (i in 1:length(scan_perplex)) {
#   for (j in 1:length(scan_seed)) {
#     for (k in 1:length(scan_weight)) {
#       
#       # tsne
#       scan_usda_data <- usda_tsne(
#         usda_data_small,
#         seed_tsne = scan_seed[j],
#         perplexity = scan_perplex[i],
#         cal_weight = scan_weight[k],
#         max_iter = 250
#         )
#       
#       # repel
#       scan_usda_data <- usda_tsne_repel(scan_usda_data)
#       
#       # file names
#       scan_string <- paste('seed', as.numeric(scan_seed[j]),
#                            'perplex', as.numeric(scan_perplex[i]),
#                            'weight', as.numeric(scan_weight[k]), sep = '_')
#       scan_string <- str_replace(scan_string, fixed('.'), '-')
#       fn_tsne <- file.path('macronutrients_map', 'tsne_scan', 
#                            paste0('tsne_', scan_string, '.png'))
#       fn_repel <- file.path('macronutrients_map', 'tsne_scan', 
#                             paste0('repel_', scan_string, '.png'))
#       
#       # plot and save tsne embedding
#       ggplot(
#         scan_usda_data, 
#         aes(x_tsne, y_tsne, size = r)
#       ) +
#         scale_size_continuous(range = c(0.5, 2)) +
#         geom_point(color = scan_usda_data$clr, alpha = 0.5) +
#         coord_equal() + 
#         theme_void() +
#         theme(panel.background = element_rect(fill = 'white'))
#       
#       ggsave(fn_tsne, width = 10, height = 10, units = 'in', dpi = 300)
#       
#       # plot and save repelled version
#       ggplot(
#         scan_usda_data, 
#         aes(x, y, size = r)
#       ) +
#         scale_size_continuous(range = c(0.5, 2)) +
#         geom_point(color = scan_usda_data$clr, alpha = 0.5) +
#         coord_equal() + 
#         theme_void() +
#       theme(panel.background = element_rect(fill = 'white'))
#       
#       ggsave(fn_repel, width = 10, height = 10, units = 'in', dpi = 300)
#                            
#     }
#   }
# }

# tsne ----
usda_data_small <- usda_tsne(usda_data_small, cal_weight = 0.25, 
                             seed_tsne = 7, perplexity = 100)

# remove "steamed" (missed in first pass; removing now so does not modify configuration)
usda_data_small <- usda_data_small |> filter(!str_detect(food_name, "steamed")) 
usda_data_small <- usda_tsne_repel(usda_data_small)

# plot, static version ---

ggplot(
  usda_data_small, 
  aes(x, y, size = r)
) +
  scale_size_continuous(range = c(0.5, 3)) +
  geom_point(color = usda_data_small$clr, alpha = 0.5) +
  coord_equal() + 
  theme_void()

# plotly vis ----

family_title <- 'Fixel'
family_text <- 'Fixel'

# set up macro annotations
n_macro <- 3
macro_annotations <- tibble(
  x = c(25, -15.5, -29.25),
  y = c(32.5, -36, 8),
  text = c('<b>CARBS</b>', '<b>FATS</b>', '<b>PROTEINS</b>'),
  family = rep(family_title, n_macro),
  size = rep(18, n_macro),
  xanchor = rep('center', n_macro)
)
macro_clr <- usda_add_macronutrient_clrs( # colors based on colourmap
  tibble(
    'carb_rel' = c(1, 0, 0),
    'fat_rel' = c(0, 1, 0),
    'protein_rel' = c(0, 0, 1)
  ), breaks = 100, legend = FALSE,
  lightness = clr_lightness - 0.1, contrast = clr_contrast)
macro_annotations$clr <- macro_clr$clr

# title, subtitle, data source, and encoding notes
# 1) title
# 2) subtitle
# 3) intermediate colours
# 4) larger points
# 5) data source
font_size_text <- 10
font_clr_text <- c('#525252')
font_clr_light <- c('#A3A3A3')
text_annotations <- tibble(
  x = c(-32, -32, -7.5, 18.5, -32),
  y = c(22.2, 18, -2.5, 4.2, -40),
  text = c('<b>Macronutrients Map</b>',
           paste0(
             'Each point in the map is a type of food. ', 
             'Nearby foods<br>have similar calories and proportions of macronutrients: <br>', 
             glue::glue('<b style="color: {macro_annotations$clr[1]};">carbohydrates</b>,',
                        ' <b style="color: {macro_annotations$clr[2]};">fats</b>,', 
                        ' and <b style="color: {macro_annotations$clr[3]};">protein</b>.')),
           'Foods with intermediate<br>colors have a mix of<br>macronutrients',
           'Larger points have more<br>calories per 100 grams',
           'Visualisation by <b>Gabrielle M. Schroeder</b> | Data: USDA food database (R package {NutrienTrackeR})'
  ),
  family = c(family_title, family_text, family_text, family_text, family_text),
  size = c(30, font_size_text + 2, font_size_text, font_size_text, font_size_text - 2),
  xanchor = c('left', 'left', 'left', 'left', 'left'),
  clr = c('black', font_clr_text, font_clr_light, font_clr_light, font_clr_light)
)

# combine annotations
all_annotations <- rbind(macro_annotations, text_annotations)
n_annotations <- nrow(all_annotations)

# create main plot ---
plot_scale <- 1.1 # increase scale (e.g., to 10) to save high resolution png
clr_bg <- c('#FAFCFC')
plot_width <- 700 * plot_scale
plot_height <- 700 * plot_scale
p_map <- plot_ly(
  usda_data_small,
  type = 'scatter',
  x = ~x * -1,
  y = ~y,
  size = ~energy_kcal,
  marker = list(color = ~clr, alpha = 0, line = list(color = ~clr)),
  sizes = c(3,25) * plot_scale * plot_scale,
  mode = 'markers',
  hoverinfo = 'text',
  text = ~paste(food_name, '<br>',
                energy_kcal, 'kcal per 100 grams', '<br>',
                'C:', carbohydrate_by_difference_g, 'g,',
                'F:', total_lipid_fat_g, 'g,',
                'P:', protein_g, 'g',
                sep = ' '),
  width = plot_width,
  height = plot_height,
  opacity = 1
) %>%
  layout(
    xaxis = list(scaleanchor = 'y', scaleratio = 1,
                 showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, title = ''),
    yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, title = ''),
    hoverlabel = list(font = list(family = family_text, size = (font_size_text + 2) * plot_scale )),
    font = list(family = family_text),
    paper_bgcolor = clr_bg,
    plot_bgcolor = clr_bg,
    margin = list(l = 0, r = 0, t = 20 * plot_scale, b = 20 * plot_scale)
  )

# add annotations ---
for (i in 1:n_annotations) {
  p_map <- p_map %>% add_annotations(
    x = all_annotations$x[i],
    y = all_annotations$y[i],
    text = all_annotations$text[i],
    showarrow = FALSE,
    xanchor = all_annotations$xanchor[i],
    align = 'left',
    yanchor = 'top',
    font = list(
      family = all_annotations$family[i],
      size = all_annotations$size[i] * plot_scale,
      color = all_annotations$clr[i]
    )
  )
}

# View
p_map

# save as png
# instructions for downloading packages: 
# https://plotly.com/r/static-image-export/
# https://github.com/plotly/orca#installation
if (plot_scale >= 10) {
  orca(p_map, file.path('macronutrients_map','macro_map.png'), 
       width = plot_width, height = plot_height)
}

# save html ---
if (plot_scale <= 2) {
  htmlwidgets::saveWidget(p_map, file.path('macronutrients_map','map_widget.html'))
}
