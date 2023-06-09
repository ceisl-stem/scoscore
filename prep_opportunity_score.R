library(edbuildmapr)
library(edbuildr)
library(educationdata)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(readr)

df <- get_education_data(level = 'school-districts', 
                         source = 'ccd', 
                         topic = 'directory', 
                         #subtopic = list('agency_type'),
                         filters = list(fips = 18,
                                        year = 2021
                                        ),
                         add_labels = TRUE) |>
  filter(
    agency_type == "Regular local school district"
  ) |>
  select(
    leaid, lea_name, state_leaid, enrollment, urban_centric_locale, enrollment
  )
#View(df)
df2 <- get_education_data(level = 'school-districts', 
                         source = 'ccd', 
                         topic = 'enrollment', 
                         subtopic = list('race'),
                         filters = list(fips = 18,
                                        year = 2021,
                                        grade = 99
                         ),
                         add_labels = TRUE) |>
  select(leaid, race, enrollment) |>
  pivot_wider(id_cols = leaid, names_from = race, values_from = enrollment) |>
  mutate(urm_pct = 1 - (White / Total)) |>
  select(leaid, urm_pct)
#View(df2)
districts_df <- left_join(df, df2, by = "leaid") |>
  filter(leaid != 1800009) |>
  select(leaid, lea_name, enrollment, urban_centric_locale, urm_pct) |>
  arrange(leaid)
#View(districts_df)

total_rows <- nrow(districts_df)
split_rows <- total_rows %/% 3
df1 <- districts_df[1:split_rows, ]
df2 <- districts_df[(split_rows+1):(split_rows*2), ]
df3 <- districts_df[(split_rows*2+1):total_rows, ]
row.names(df1) <- NULL
row.names(df2) <- NULL
row.names(df3) <- NULL
write_csv(df1, file = "data/aj.csv")
write_csv(df2, file = "data/akaash.csv")
write_csv(df3, file = "data/maxim.csv")

#frl_frame <- masterpull(data_year = "2018", data_type = "geo") |>
#  filter(State == "Indiana") |>
#  select("leaid" = "NCESID", "frl_pct" = "FRL_rate")

#districts_df <- left_join(districts_df, frl_frame, by = "leaid")

#scatter_plot <- ggscatter(
#  districts_df,
#  "urm_pct",
#  "frl_pct",
#  size = "enrollment",
#  color = "urban_centric_locale",
#  add = "reg.line",
#  add.params = list(color = "blue", fill = "lightgray"),
#  conf.int = TRUE) + 
#  stat_cor(method = "pearson") +
#  theme_pubr()
#scatter_plot

#scatter_plot2 <- ggboxplot(
#   districts_df,
#   "urban_centric_locale",
#   "frl_pct",
#   size = "enrollment",
#   color = "urban_centric_locale",
#   add = "reg.line",
#   add.params = list(color = "blue", fill = "lightgray"),
#   conf.int = TRUE) + 
#   stat_cor(method = "pearson") +
#   theme_pubr()
# scatter_plot2
# 
# scatter_plot3 <- ggboxplot(
#   districts_df,
#   "urban_centric_locale",
#   "urm_pct",
#   size = "enrollment",
#   color = "urban_centric_locale",
#   add = "reg.line",
#   add.params = list(color = "blue", fill = "lightgray"),
#   conf.int = TRUE) + 
#   stat_cor(method = "pearson") +
#   theme_pubr()
# scatter_plot3
