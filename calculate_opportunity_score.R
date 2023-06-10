library(readr)
library(dplyr)
library(tidyr)
library(here)
library(scales)
library(ggtern)
library(ggplot2)
library(ggpubr)
library(ggrepel)
library(waffle)
library(maps)
library(mapdata)
library(sf)
library(edbuildmapr)
library(educationdata)

ucl_palette <- c(
  "City large" = "#990000",
  "City midsize" = "#F23A3F",
  "City small" = "#FF636A",
  "Suburb large" = "#FFAA00",
  "Suburb midsize" = "#FFD563",
  "Suburb small" = "#FFE694",
  "Town fringe" = "#056E41",
  "Town distant" = "#63B363",
  "Town remote" = "#A7D094",
  "Rural fringe" = "#006298",
  "Rural distant" = "#63B1D3",
  "Rural remote" = "#94D2E7"
)

iu_ramp_palette = colorRampPalette(c("#FFAA00", "#990000", "#59264D"))

state_3rd_proficiency <- .816
state_6th_proficiency <- .341
state_gpc <- .864

school_corp_frame <- read_csv(here("data", "in_sc-data.csv")) |>
  na.omit() |>
  # mutate(ela_3rd = if_else(
  #   is.na(ela_3rd),
  #   state_3rd_proficiency,
  #   ela_3rd
  # )) |>
  # mutate(math_6th = if_else(
  #   is.na(math_6th),
  #   state_6th_proficiency,
  #   math_6th
  # )) |>
  # mutate(grad_comp = if_else(
  #   is.na(grad_comp),
  #   state_gpc,
  #   grad_comp
  # )) |>
  mutate(adj_3rd = if_else(
    ela_3rd < state_3rd_proficiency,
    (state_3rd_proficiency - ela_3rd + 1),
    0
  )) |>
  mutate(adj_6th = if_else(
    math_6th < state_6th_proficiency,
    (state_6th_proficiency - math_6th + 1),
    0
  )) |>
  mutate(adj_gpc = if_else(
    grad_comp < state_gpc,
    (state_gpc - grad_comp + 1),
    0
  )) |>
  mutate(
    adj_academic = (
      (((adj_3rd) + (adj_6th) + (adj_gpc)) / 3)) + 1
  ) |>
  mutate(
    scoScore = (
      ((urm_pct * 1.5) + (frl_pct * 1.5) + (adj_academic)) / 3
    )
  ) |>
  select(
    leaid,
    lea_name,
    enrollment,
    urban_centric_locale,
    urm_pct,
    frl_pct,
    adj_academic,
    scoScore
  ) |>
  mutate(urm_pct = urm_pct * 100) |>
  mutate(frl_pct = frl_pct * 100) |>
  mutate(scoScore = if_else(
    scoScore < 1 & scoScore > 0.99,
    1,
    scoScore
  ))

school_corp_frame$urban_centric_locale <- factor(
  school_corp_frame$urban_centric_locale,
  levels = c(
    "City large",
    "City midsize",
    "City small",
    "Suburb large",
    "Suburb midsize",
    "Suburb small",
    "Town fringe",
    "Town distant",
    "Town remote",
    "Rural fringe",
    "Rural distant",
    "Rural remote"
  )
)

urcl_plot <- ggboxplot(
  school_corp_frame,
 "urban_centric_locale",
 "scoScore",
 outlier.shape = NA,
 fill = "urban_centric_locale",
 palette = ucl_palette) +
 theme_pubr()
urcl_plot <- ggpar(urcl_plot, legend = "none")
ggsave(here("outputs", "urcl_box.png"), urcl_plot, dpi = 300, width = 16, height = 9, units = "in")

opp_plot <- ggscatter(
  school_corp_frame,
  "urm_pct",
  "frl_pct",
  add = "reg.line",
  add.params = list(color = "blue", fill = "lightgray"),
  conf.int = TRUE,
  cor.coef = TRUE,
  size = "enrollment",
  color = "urban_centric_locale",
  palette = ucl_palette) +
  theme_pubr()
ggsave(here("outputs", "opp_plot.png"), opp_plot, dpi = 300, width = 16, height = 9, units = "in")

urcl_frame <- school_corp_frame |>
  group_by(urban_centric_locale) |>
  summarize(num = n(),
            total_enrollment = sum(enrollment))

urcl_enrollment <- ggdonutchart(
  urcl_frame,
  "total_enrollment",
  label = "urban_centric_locale",
  fill = "urban_centric_locale",
  palette = ucl_palette,
  legend = "none")
urcl_enrollment

urcl_n <- ggdonutchart(
  urcl_frame,
  "num",
  label = "urban_centric_locale",
  fill = "urban_centric_locale",
  palette = ucl_palette)
urcl_n

urcl_city <- urcl_frame[1,3] +
  urcl_frame[2,3] +
  urcl_frame[3,3]
urcl_suburb <- urcl_frame[4,3] +
  urcl_frame[5,3] +
  urcl_frame[6,3]
urcl_town <- urcl_frame[7,3] +
  urcl_frame[8,3] +
  urcl_frame[9,3]
urcl_rural <- urcl_frame[10,3] +
  urcl_frame[11,3] +
  urcl_frame[12,3]
urcl_condensed_frame <- data.frame(urcl = c("city", "suburb", "town", "rural"),
                                   enrollment = c(as.numeric(urcl_city),
                                                  as.numeric(urcl_suburb),
                                                  as.numeric(urcl_town),
                                                  as.numeric(urcl_rural)))
urcl_condensed_frame$urcl <- factor(urcl_condensed_frame$urcl,
                                    levels = c("city", "suburb", "town", "rural"))
urcl_c <- ggdonutchart(
  urcl_condensed_frame,
  "enrollment",
  label = "urcl",
  fill = "urcl",
  palette = c("#990000", "#FFAA00", "#056E41", "#006298"))
urcl_c

urcl_condensed_frame$enrollment <- urcl_condensed_frame$enrollment / 1000

enr_waffle <- urcl_frame |>
  select(urban_centric_locale, total_enrollment) |>
  mutate(total_enrollment = total_enrollment / 2000)
enr_waffle_plot <- waffle(enr_waffle, rows = 10,
       colors = ucl_palette, legend_pos = "bottom")
ggsave(here("outputs", "enr_waffle.png"), enr_waffle_plot, dpi = 300, width = 18, height = 6, units = "in")


n_waffle <- urcl_frame |>
  select(urban_centric_locale, num)
n_waffle_plot <- waffle(n_waffle, rows = 8,
       colors = ucl_palette,
       legend_pos = "bottom")
ggsave(here("outputs", "n_waffle.png"), n_waffle_plot, dpi = 300, width = 18, height = 6, units = "in")

tern_plot <- school_corp_frame |>
  #mutate(urm_pct = urm_pct / 100) |>
  #mutate(frl_pct = frl_pct / 100) |>
  mutate(adj_academic = rescale(adj_academic, to = c(1, 100))) |>
  ggtern(aes(urm_pct, frl_pct, adj_academic, color = urban_centric_locale, size = enrollment)) +
  geom_point() +
  scale_color_manual(values = ucl_palette) +
  scale_size_continuous(breaks = c(1000, 5000, 10000, 15000, 20000, 25000)) +
  theme_ggtern() +
  theme_arrowdefault()
ggsave(here("outputs", "tern.png"), tern_plot, dpi = 300, width = 11, height = 8.5, units = "in")

enrollment_plot <- ggscatter(school_corp_frame, x = "enrollment", y = "scoScore", color = "urban_centric_locale",
          palette = ucl_palette,
          add = "reg.line",
          add.params = list(color = "blue", fill = "lightgray"),
          conf.int = TRUE,
          cor.coef = TRUE) +
  theme_pubr()
ggsave(here("outputs", "enr_scoscore.png"), enrollment_plot, dpi = 300, width = 16, height = 9, units = "in")

sc_locations <- get_education_data(level = 'school-districts', 
                         source = 'ccd', 
                         topic = 'directory', 
                         filters = list(fips = 18,
                                        year = 2021
                         ),
                         add_labels = TRUE) |>
  filter(
    agency_type == "Regular local school district"
  ) |>
  select(
    leaid, latitude, longitude
  ) |>
  mutate(
    leaid = as.numeric(leaid)
  )

school_corp_frame <- school_corp_frame |>
  left_join(sc_locations, by = "leaid")

indiana_map <- map_data("state", region = "indiana")
base_map <- ggplot() +
  geom_polygon(data = indiana_map, aes(x = long, y = lat, group = group), fill = "#eeeeee", color = "#243142") +
  coord_map(xlim = range(indiana_map$long), ylim = range(indiana_map$lat))

map_with_points <- base_map +
  geom_point(
    data = school_corp_frame,
    aes(x = longitude, y = latitude, size = enrollment, color = scoScore)) +
  scale_size_binned(breaks = c(1000, 10000, 20000)) +
  scale_color_steps2(
    mid = "#FFAA00",
    high = "#990000",
    low = "#056E41",
    midpoint = 1,
    breaks = c(0, 1, 1.25)
  ) +
  theme_minimal()
map_with_points

in_sc_shapes <- sd_shapepull("2019", with_data = TRUE) |>
  filter(State == "Indiana") |>
  select(leaid = GEOID, geometry) |>
  mutate(leaid = as.numeric(leaid))

school_corp_frame <- school_corp_frame |>
  right_join(in_sc_shapes)

states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))

corp_map <- school_corp_frame |>
  select(leaid, scoScore, geometry) |>
  st_as_sf(map("state", plot = FALSE, fill = TRUE))

sco_map <- ggplot(data = states) +
  geom_sf(data = states, fill = "#eeeeee") +
  geom_sf(data = corp_map, color = "#243142", aes(fill = scoScore)) +
  scale_fill_steps2(
    low = "#FFF4C6",
    high = "#990000",
    mid = "#FFAA00",
    midpoint = 1.1,
    breaks = c(0, 0.5, 0.75, 1, 1.25, 1.5)
  ) +
  coord_sf(xlim = c(-88.5, -84.5), ylim = c(37.5, 42), expand = FALSE) +
  theme_minimal()  +
  theme(panel.grid.major = element_blank(),
        panel.background = element_rect(fill = "#E9F6FC"),
        axis.text = element_blank(),
        axis.title = element_blank())
sco_map
ggsave(here("outputs", "in_sco_map.png"), sco_map, dpi = 300, width = 8.5, height = 11, units = "in")

central_indiana_map <- sco_map +
  geom_label(
    data = school_corp_frame,
    aes(longitude, latitude,
        label = lea_name),
    color = "#191919",
    size = 3
    ) +
  coord_sf(xlim = c(-86.5, -85.75), ylim = c(39.5, 40.1), expand = FALSE)
central_indiana_map
ggsave(here("outputs", "cin_sco_map.png"), central_indiana_map, dpi = 300, width = 11, height = 11, units = "in")

the_region_map <- sco_map +
  geom_label(data = school_corp_frame, aes(longitude, latitude, label = lea_name), color = "#191919", size = 3) +
  coord_sf(xlim = c(-87.6, -86.8), ylim = c(41.2, 41.8), expand = FALSE)
the_region_map
ggsave(here("outputs", "trin_sco_map.png"), the_region_map, dpi = 300, width = 11, height = 11, units = "in")
