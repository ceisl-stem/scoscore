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
library(ggbeeswarm)
library(maps)
library(mapdata)
library(sf)
library(edbuildmapr)
library(educationdata)
library(linearpackcircles)

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


state_3rd_proficiency <- .816
state_6th_proficiency <- .341
state_gpc <- .864

school_corp_frame <- read_csv(here("data", "in_sc-data.csv")) |>
  na.omit() |>
  mutate(urm_pct = round(urm_pct, digits = 4)) |>
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
  )) |>
  mutate(adj_academic = round(adj_academic, digits = 2)) |>
  mutate(scoScore = round(scoScore, digits = 2))

# Conduct a counterfactual analysis to ensure fairness for school corporations.
# If a school corporation has an adjusted academic score **above** mean academic
# score + 1 standard deviation then adjust the non-white and free and
# reduced lunch percents to the state average if these are **lower** than the
# state mean. This gives some school corporations a little "bump" so that it
# cannot be said that any group is given an "unfair" advantage. If the newly
# calculated SCOscore is then >=1, then keep that counterfactual score.

urm_pct_mean <- round(mean(school_corp_frame$urm_pct)/100,2)
frl_pct_mean <- round(mean(school_corp_frame$frl_pct)/100,2)
adj_ac_mean <- round(mean(school_corp_frame$adj_academic),2)
adj_ac_sd <- round(sd(school_corp_frame$adj_academic), 2)
adj_ac_cutoff <- adj_ac_mean + adj_ac_sd

whatif_frame <- school_corp_frame |>
  filter(adj_academic >= adj_ac_cutoff) |>
  filter(scoScore < 1) |>
  select(-scoScore) |>
  mutate(
    urm_pct = urm_pct / 100
  ) |>
  mutate(
    frl_pct = frl_pct / 100
  ) |>
  mutate(urm_pct = ifelse(
    urm_pct < urm_pct_mean,
    urm_pct_mean,
    urm_pct
  )) |>
  mutate(frl_pct = ifelse(
    frl_pct < frl_pct_mean,
    frl_pct_mean,
    frl_pct
  )) |>
  mutate(
    scoScore = (
      ((urm_pct * 1.5) + (frl_pct * 1.5) + (adj_academic)) / 3
    )
  ) |>
  mutate(scoScore = if_else(
    scoScore < 1 & scoScore > 0.99,
    1,
    scoScore
  )) |>
  mutate(
    scoScore = round(scoScore, 2)
  ) |>
  filter(
    scoScore >= 1
  ) |>
  select(leaid, scoScore)

school_corp_frame$cf <- FALSE

school_corp_frame <- school_corp_frame |>
  mutate(scoScore = ifelse(
    leaid %in% whatif_frame$leaid,
    whatif_frame$scoScore,
    scoScore)) |>
  mutate(cf = ifelse(
    leaid %in% whatif_frame$leaid,
    TRUE,
    FALSE))

rural_opp_frame <- school_corp_frame |>
  filter(urban_centric_locale %in% c("Town distant", "Town remote", "Rural fringe",
                                     "Rural distant", "Rural remote")) |>
  filter(scoScore >= 1)# |>
  #filter(urm_pct >=50)
ggscatter(rural_opp_frame, "frl_pct", "urm_pct")

output_frame <- school_corp_frame |>
  select(leaid, lea_name, cf, enrollment, urban_centric_locale, urm_pct, frl_pct, academic = adj_academic, scoScore) |>
  write_csv(file = here("data", "in_scoscores.csv"))

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

urcl_pack = linearpackcircles(school_corp_frame,
                          
                          ID_var = "lea_name",
                          group_var = "urban_centric_locale",
                          area_var = "enrollment",
                          x_var = "scoScore",
                          
                          separation_factor = 200,
                          width_plot = 2000,
                          height_group = 100,
                          
                          label_circles = TRUE,
                          max_overlaps = 8,
                          size_text = 2,
                          area_multiplier = 1000)
urcl_pack

urcl_swarm <- ggplot(
  school_corp_frame,
  aes(
  x = urban_centric_locale,
  y = scoScore,
  color = scoScore
  )
) +
  geom_beeswarm(method = "center", cex = 1.5, corral = "wrap", shape = 18, size = 3) +
  geom_text_repel(aes(x = urban_centric_locale, 
                      y = scoScore, 
                      label = lea_name), size = 2.75, color = "#243142",
                  max.overlaps = 4,
                  point.padding = 0.5) +
  annotate("segment", x = 0, xend = 12.5, y = 1, yend = 1, color = "#A7A9AB",
           linetype = "dotted") +
  #scale_color_manual(values = ucl_palette) +
  scale_color_steps2(
    low = "#FFF4C6",
    high = "#990000",
    mid = "#FFAA00",
    midpoint = 1.1,
    breaks = c(0, 0.5, 0.75, 1, 1.25, 1.5)
  ) +
  ylim(0, 2) +
  ylab("School Corporation Opportunity Score") +
  xlab("NCES Urban-Centric Locale Category") +
  theme_pubr() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    legend.position = "none"
  )
urcl_swarm
ggsave(here("outputs", "urcl_swarm.png"), urcl_swarm, dpi = 300, width = 16, height = 9, units = "in")

urcl_plot <- ggboxplot(
  school_corp_frame,
 "urban_centric_locale",
 "scoScore",
 outlier.shape = NA,
 fill = "urban_centric_locale",
 palette = ucl_palette,
 ylim = c(0, 2)) +
 theme_pubr()
urcl_plot <- ggpar(urcl_plot, legend = "none")
#ggsave(here("outputs", "urcl_box.png"), urcl_plot, dpi = 300, width = 16, height = 9, units = "in")

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
  legend.title = "NCES Urban-Centric Locale Category")
urcl_enrollment
ggsave(here("outputs", "enrollment_donut.png"), urcl_enrollment, dpi = 300, width = 8.5, height = 11, units = "in")

urcl_n <- ggdonutchart(
  urcl_frame,
  "num",
  label = "urban_centric_locale",
  fill = "urban_centric_locale",
  palette = ucl_palette,
  legend.title = "NCES Urban-Centric Locale Category")
urcl_n
ggsave(here("outputs", "scn_donut.png"), urcl_n, dpi = 300, width = 8.5, height = 11, units = "in")

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
urcl_condensed_frame <- data.frame(urcl = c("City", "Suburb", "Town", "Rural"),
                                   enrollment = c(as.numeric(urcl_city),
                                                  as.numeric(urcl_suburb),
                                                  as.numeric(urcl_town),
                                                  as.numeric(urcl_rural)))
urcl_condensed_frame$urcl <- factor(urcl_condensed_frame$urcl,
                                    levels = c("City", "Suburb", "Town", "Rural"))
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
enr_waffle_plot
#ggsave(here("outputs", "enr_waffle.png"), enr_waffle_plot, dpi = 300, width = 18, height = 6, units = "in")


n_waffle <- urcl_frame |>
  select(urban_centric_locale, num)
n_waffle_plot <- waffle(n_waffle, rows = 8,
       colors = ucl_palette,
       legend_pos = "bottom")
n_waffle_plot
#ggsave(here("outputs", "n_waffle.png"), n_waffle_plot, dpi = 300, width = 18, height = 6, units = "in")

tern_plot <- school_corp_frame |>
mutate(urm_pct = urm_pct / 100) |>
mutate(frl_pct = frl_pct / 100) |>
  mutate(adj_academic = rescale(adj_academic, to = c(1, 100))) |>
  ggtern(aes(urm_pct, frl_pct, adj_academic, color = urban_centric_locale, size = enrollment)) +
  geom_point() +
  scale_color_manual(values = ucl_palette) +
  scale_size_continuous(breaks = c(1000, 5000, 10000, 15000, 20000, 25000)) +
  theme_ggtern() +
  theme_arrowdefault()
tern_plot
# ggsave(here("outputs", "tern.png"), tern_plot, dpi = 300, width = 11, height = 8.5, units = "in")

enrollment_plot <- ggscatter(school_corp_frame, x = "enrollment", y = "scoScore", color = "urban_centric_locale",
          palette = ucl_palette,
          add = "reg.line",
          add.params = list(color = "blue", fill = "lightgray"),
          conf.int = TRUE,
          cor.coef = TRUE) +
  geom_text_repel(aes(x = enrollment, 
                      y = scoScore, 
                      label = lea_name), size = 2.75, color = "#243142",
                  max.overlaps = 4,
                  point.padding = 0.5) +
  theme_pubr()
enrollment_plot
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

school_corp_map <- st_as_sf(school_corp_frame)

sco_map <- ggplot(data = states) +
  geom_sf(data = states, fill = "#eeeeee") +
  geom_sf(data = school_corp_map, color = "#243142", aes(fill = scoScore)) +
  scale_fill_steps2(
    low = "#FFF4C6",
    high = "#990000",
    mid = "#FFAA00",
    midpoint = 1.1,
    breaks = c(0, 0.6, 0.8, 1, 1.2, 1.4)
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

the_region_map <- the_region_map +
  theme(legend.position = "none")
close_maps <- ggarrange(the_region_map, central_indiana_map, ncol = 2)
ggsave(here("outputs", "inset_maps.png"), close_maps, dpi = 300, width = 16, height = 9, units = "in")

output_frame <- school_corp_frame |>
  select(leaid, lea_name, enrollment, urban_centric_locale, urm_pct, frl_pct, academic = adj_academic, scoScore, latitude, longitude) |>
  write_csv(file = here("data", "in_scoscores.csv"))
