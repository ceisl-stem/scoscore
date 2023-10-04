library(rstatix)
library(dplyr)
library(tidyr)
library(readr)
library(here)
library(ggplot2)
library(ggbeeswarm)
library(ggrepel)
library(ggpubr)
library(stargazer)
library(sf)
library(edbuildmapr)
library(maps)
library(mapdata)
library(ggmap)
library(ggrepel)
library(showtext)
library(ggtext)

font_add(regular = "BellTopoSans-Bold.otf", "inter")
font_add(regular = "et-book-roman-line-figures.ttf", "newsreader")
font_add(regular = "Rawlinson Bold.otf", "newsreader")
font_add(regular = "NationalPark-VariableVF.ttf", "np")
font_add(regular = "nationalforest.otf", "nf")
font_add(regular = "USGS Regular.ttf", "usgs")
font_add(regular = "CartoGothicStd-Book.otf", "carto")
font_add('fa-reg', 'Font Awesome 6 Free-Regular-400.otf')
font_add('fa-brands', 'Font Awesome 6 Brands-Regular-400.otf')
font_add("jetbrains", "JetBrainsMono-Regular.ttf")
font_add("academicons", "academicons.ttf")
showtext_auto()

state_3rd_proficiency <- .816
state_6th_proficiency <- .341
state_gpc <- .864

school_data <- read_csv(
  here("data", "school_test.csv"),
  show_col_types = FALSE,
  col_types = cols(ncessch = col_character())
) |>
  na.omit() |>
  mutate(ncessch = as.character(ncessch)) |>
  mutate(ac_indicator = case_when(
    school_level == "Primary" ~ if_else(
      ac_indicator < state_3rd_proficiency,
      (state_3rd_proficiency - ac_indicator + 1),
      0
    ),
    school_level == "Middle" ~ if_else(
      ac_indicator < state_6th_proficiency,
      (state_6th_proficiency - ac_indicator + 1),
      0
    ),
    school_level == "High" ~ if_else(
      ac_indicator < state_gpc,
      (state_gpc - ac_indicator + 1),
      0
    )
  )) |>
  mutate(
    scoScore = (
      ((urm_pct * 1.5) + (frl_pct * 1.5) + (ac_indicator)) / 3
    )
  ) |>
  mutate(scoScore = if_else(
    scoScore < 1 & scoScore > 0.98,
    1,
    scoScore
  )) |>
  mutate(scoScore = round(scoScore, digits = 2)) |>
  select(
    school_id,
    leaid,
    lea_name,
    school_name,
    frl_pct,
    urm_pct,
    ac_indicator,
    scoScore,
    latitude,
    longitude
  )

school_data$lea_name <- factor(
  school_data$lea_name,
  levels = c(
    "Avon Community School Corp",
    "School Town of Speedway",
    "MSD Decatur Township",
    "Beech Grove City Schools",
    "Anderson Community School Corp",
    "MSD Washington Township",
    "MSD Lawrence Township",
    "MSD Wayne Township",
    "Perry Township Schools",
    "MSD Pike Township",
    "MSD Warren Township",
    "Indianapolis Public Schools"
  )
)

school_swarm <- ggplot(
  school_data,
  aes(
    x = lea_name,
    y = scoScore,
    color = scoScore
  )
) +
  geom_beeswarm(method = "center", cex = 1.5, corral = "wrap", shape = 18, size = 4) +
  geom_text_repel(
    aes(
      x = lea_name,
      y = scoScore,
      label = school_name
    ),
    size = 5, color = "#243142",
    max.overlaps = 4,
    point.padding = 0.5
  ) +
  annotate("segment",
    x = 0, xend = 12.5, y = 1, yend = 1, color = "#A7A9AB",
    linetype = "dotted"
  ) +
  scale_color_steps2(
    low = "#FFF4C6",
    high = "#990000",
    mid = "#FFAA00",
    midpoint = 1,
    breaks = c(0, 0.5, 0.75, 1, 1.25, 1.5)
  ) +
  ylim(0, 2) +
  ylab("School Opportunity Score") +
  xlab("School Corporation") +
  theme_pubr() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    legend.position = "none"
  )
school_swarm
ggsave(here("outputs", "school_swarm.pdf"), school_swarm, dpi = 300, width = 48, height = 36, units = "in")

avon_data <- school_data |>
  filter(lea_name == "Avon Community School Corp")
speedway_data <- school_data |>
  filter(lea_name == "School Town of Speedway")
decatur_data <- school_data |>
  filter(lea_name == "MSD Decatur Township")
washington_data <- school_data |>
  filter(lea_name == "MSD Washington Township")
lawrence_data <- school_data |>
  filter(lea_name == "MSD Lawrence Township")
warren_data <- school_data |>
  filter(lea_name == "MSD Warren Township")
wayne_data <- school_data |>
  filter(lea_name == "MSD Wayne Township")
ips_data <- school_data |>
  filter(lea_name == "Indianapolis Public Schools")
perry_data <- school_data |>
  filter(lea_name == "Perry Township Schools")
bg_data <- school_data |>
  filter(lea_name == "Beech Grove City Schools")

summary_data <- school_data |>
  select(lea_name, school_name, frl_pct, urm_pct, ac_indicator, scoScore) |>
  group_by(lea_name) |>
  summarize(
    mean = round(mean(scoScore), digits = 2),
    median = round(median(scoScore), digits = 2),
    var = round(var(scoScore), digits = 2),
    sd = round(sd(scoScore), digits = 2)
            ) |>
  ungroup()

msd_avon <- wilcox.test(
  avon_data$scoScore,
  mu = 0.72,
  alternative = "less")
msd_speedway <- wilcox.test(
  speedway_data$scoScore,
  mu = 0.9,
  alternative = "less")
msd_bg <- wilcox.test(
  bg_data$scoScore,
  mu = 1.1,
  alternative = "less")
msd_decatur <- wilcox.test(
  decatur_data$scoScore,
  mu = 1.07,
  alternative = "less")
msd_washington <- wilcox.test(
  washington_data$scoScore,
  mu = 1.13,
  alternative = "less")
msd_lawrence <- wilcox.test(
  lawrence_data$scoScore,
  mu = 1.29,
  alternative = "less")
msd_warren <- wilcox.test(
  warren_data$scoScore,
  mu = 1.42,
  alternative = "less")
msd_wayne <- wilcox.test(
  wayne_data$scoScore,
  mu = 1.39,
  alternative = "less")
msd_ips <- wilcox.test(
  ips_data$scoScore,
  mu = 1.42,
  alternative = "less")
msd_perry <- wilcox.test(
  perry_data$scoScore,
  mu = 1.11,
  alternative = "less")

bg_eff <- bg_data |>
  wilcox_effsize(scoScore ~ 1, mu = 1.10)
perry_eff <- perry_data |>
  wilcox_effsize(scoScore ~ 1, mu = 1.11)
ips_eff <- ips_data |>
  wilcox_effsize(scoScore ~ 1, mu = 1.42)
warren_eff <- warren_data |>
  wilcox_effsize(scoScore ~ 1, mu = 1.42)
wayne_eff <- wayne_data |>
  wilcox_effsize(scoScore ~ 1, mu = 1.39)
lawrence_eff <- lawrence_data |>
  wilcox_effsize(scoScore ~ 1, mu = 1.29)
washington_eff <- washington_data |>
  wilcox_effsize(scoScore ~ 1, mu = 1.13)
decatur_eff <- decatur_data |>
  wilcox_effsize(scoScore ~ 1, mu = 1.07)
speedway_eff <- speedway_data |>
  wilcox_effsize(scoScore ~ 1, mu = 0.9)
avon_eff <- avon_data |>
  wilcox_effsize(scoScore ~ 1, mu = 0.72)

wilcox_frame <- data.frame(
  lea_name = c(
    "Avon Community School Corp",
    "School Town of Speedway",
    "MSD Decatur Township",
    "Beech Grove City Schools",
    "Perry Township Schools",
    "MSD Washington Township",
    "MSD Lawrence Township",
    "MSD Wayne Township",
    "MSD Warren Township",
    "Indianapolis Public Schools"
  ),
  mu = c(
    0.72,
    0.90,
    1.07,
    1.10,
    1.11,
    1.13,
    1.29,
    1.39,
    1.42,
    1.42
  ),
  V = c(
    msd_avon$statistic,
    msd_speedway$statistic,
    msd_decatur$statistic,
    msd_bg$statistic,
    msd_perry$statistic,
    msd_washington$statistic,
    msd_lawrence$statistic,
    msd_wayne$statistic,
    msd_warren$statistic,
    msd_ips$statistic
  ),
  p = c(
    msd_avon$p.value,
    msd_speedway$p.value,
    msd_decatur$p.value,
    msd_bg$p.value,
    msd_perry$p.value,
    msd_washington$p.value,
    msd_lawrence$p.value,
    msd_wayne$p.value,
    msd_warren$p.value,
    msd_ips$p.value
  ),
  eff_size = c(
    avon_eff$effsize,
    speedway_eff$effsize,
    decatur_eff$effsize,
    bg_eff$effsize,
    perry_eff$effsize,
    washington_eff$effsize,
    lawrence_eff$effsize,
    wayne_eff$effsize,
    warren_eff$effsize,
    ips_eff$effsize
  ),
  magnitude = c(
    avon_eff$magnitude,
    speedway_eff$magnitude,
    decatur_eff$magnitude,
    bg_eff$magnitude,
    perry_eff$magnitude,
    washington_eff$magnitude,
    lawrence_eff$magnitude,
    wayne_eff$magnitude,
    warren_eff$magnitude,
    ips_eff$magnitude
  )
) |>
  mutate(p = round(p, digits = 2)) |>
  mutate(eff_size = round(eff_size, digits = 2))

lea_wilcox <- pairwise.wilcox.test(
  school_data$scoScore,
  school_data$lea_name,
  p.adjust.method = "BH"
)
lea_wilcox

summary_data <- left_join(
  summary_data,
  wilcox_frame,
  by = "lea_name"
  ) |>
  ungroup() |>
  as.data.frame()
rownames(summary_data) <- NULL
summary_data
write_csv(summary_data, here("outputs", "sch_vs_corp.csv"))

states <- sf::st_as_sf(map("state", region = "indiana", plot = FALSE, fill = TRUE))

in_sc_shapes <- sd_shapepull("2019", with_data = TRUE) |>
  filter(State == "Indiana") |>
  select(leaid = GEOID, geometry) |>
  mutate(leaid = as.numeric(leaid))

school_map_data <- school_data |>
  select(leaid, school_name, scoScore, latitude, longitude)

just1 <- school_map_data |>
  filter(scoScore >= 1)

the_corps <- in_sc_shapes |>
  filter(leaid %in% just1$leaid)
corps_mid <- sf::st_centroid(the_corps$geometry)



# sample_map <- sc_map +
#   geom_point(
#     data = school_map_data,
#     aes(
#       x = longitude,
#       y = latitude,
#       color = scoScore
#       ),
#     size = 2,
#     shape = 15
#     ) +
#   geom_text_repel(
#     data = school_map_data,
#     aes(
#       x = longitude,
#       y = latitude,
#       label = school_name
#       ),
#     color = "#243142",
#     family = "alpine",
#     size = 2.5) + #,
#     #nudge_x = c(-2, -1, 1, 1.25, 1, -2),
#     #nudge_y = c(0.25, -0.25, 0.25, -0.5, -0.5)) +
#   scale_color_steps2(
#     name = "SCOscore",
#     low = "#FFF4C6",
#     high = "#990000",
#     mid = "#FFAA00",
#     midpoint = 1.1,
#     breaks = c(0, 0.6, 0.8, 1, 1.2, 1.4)
#   )
# 
# sample_map
# ggsave(here("outputs", "school_map.pdf"), sample_map, dpi = 300, width = 16, height = 9, units = "in")

corps_labels <- data.frame(
  lea_name = c(
    "MSD WAYNE\nTOWNSHIP",
    "MSD LAWRENCE TOWNSHIP",
    "MSD PIKE TOWNSHIP",
    "MSD WASHINGTON TOWNSHIP",
    "MSD WARREN\nTOWNSHIP",
    "PERRY TOWNSHIP SCHOOLS",
    "INDIANAPOLIS PUBLIC SCHOOLS"
  ),
  longitude = c(39.76179, 39.897, 39.897, 39.897, 39.77083, 39.675, 39.793),
  latitude = c(-86.295, -86.00662, -86.26749, -86.1415, -86.00938, -86.153, -86.1375)
)

sc_map <- ggplot(data = states) +
  geom_sf(data = states, fill = "#ffffff") +
  geom_sf(data = in_sc_shapes, color = "#D4C8AB", fill = "#FFFEF5") +
  geom_sf(data = the_corps, color = "#613D00", fill = "#FDF7E7") +
  coord_sf(xlim = c(-86.375, -85.9), ylim = c(39.62, 39.94), expand = FALSE) +
  theme_minimal()  +
  theme(panel.grid.major = element_blank(),
        # panel.background = element_rect(fill = "#E9F6FC"),
        axis.text = element_blank(),
        axis.title = element_blank())

other_labels <- data.frame(
  lea_name = c(
    "MSD DECATUR TOWNSHIP",
    "FRANKLIN TOWNSHIP COMMUNITY SCHOOLS",
    "CARMEL CLAY SCHOOLS",
    "HAMILTON SOUTHEASTERN SCHOOL CORP",
    "ZIONSVILLE COMMUNITY SCHOOLS",
    "TOWN OF\nSPEEDWAY\nSCHOOLS",
    "BEECH GROVE\nCITY\nSCHOOLS",
    "AVON COMMUNITY SCHOOL CORP",
    "BROWNSBURG COMMUNITY SCHOOL CORP",
    "PLAINFIELD COMMUNITY SCHOOL CORP",
    "MOUNT VERNON COMMUNITY SCHOOL CORP",
    "SOUTHERN HANCOCK COUNTY\nCOMMUNITY SCHOOL CORP",
    "NORTHWESTERN CONSOLIDATED\nSCHOOL CORP",
    "MOORSEVILLE CONSOLIDATED SCHOOL CORP",
    "CENTER GROVE COMMUNITY SCHOOL CORP",
    "GREENWOOD COMMUNITY\nSCHOOL CORP",
    "CLARK-PLEASANT COMMUNITY SCHOOL CORP"
  ),
  longitude = c(39.675, 39.675, 39.935, 39.935, 39.935, 39.794, 39.715, 39.76179, 39.85, 39.675, 39.85, 39.7375, 39.65,
                39.625, 39.625, 39.625, 39.625),
  latitude = c(-86.275, -86.00938, -86.1415, -86.00662, -86.295, -86.2475, -86.085, -86.36, -86.36, -86.36, -85.925, -85.925,
               -85.925, -86.32, -86.195, -86.11, -86.02),
  direction = c(0, 0, 0, 0, 0, 0, 0, 90, 90, 90, 270, 270, 270, 0, 0, 0, 0)
)

just1$marker <- "s"

just1 <- just1 |>
  filter(leaid != 1800150)

just1_map <- sc_map +
  geom_text(data = corps_labels, aes(x = latitude, y = longitude, label = lea_name),
            family = "inter", size = 14, color = "#D1B063") +
  geom_text(data = other_labels, aes(x = latitude, y = longitude, label = lea_name, angle = direction),
            family = "inter", size = 9, color = "#EBDDB6") +
  geom_text(
    data = just1,
    aes(
      x = longitude,
      y = latitude,
      label = marker,
      color = scoScore,
      family = "usgs"
    ),
    size = 18,
    hjust = 0.5,
    vjust = 0.5
  ) +
  geom_text_repel(
    data = just1,
    aes(
      x = longitude,
      y = latitude,
      label = school_name,
      point.size = 18
    ),
    color = "#613D00",
    size = 9,
    family = "carto") +
  scale_color_steps2(
    name = "SCHOOL CONTEXT OPPORTUNITY SCALE",
    mid = "#b83300",
    high = "#990000",
    low = "#FFAA00",
    midpoint = 1.2,
    breaks = c(1, 1.1, 1.2, 1.3, 1.4)
  ) +
  labs(title = "Central Indiana School Context Opportunity Scores",
          #subtitle = "Indianapolis-Marion County schools with School Context Opportunity Scores greater than 1",
          caption = "Valid for 2023. Data retrieved from Urban Institute's Education Data Explorer via the <span style='font-family:jetbrains'>educationdata</span> R package (Ueyama, 2022) and the Indiana Department of Education's Indiana Graduates Prepared to Succeed (GPS) Dashboard (Indiana Department of Education, 2023). A project of CEISL. <span style='font-family:fa-brands'>&#xf25e;</span> BY-NC-SA 4.0. <span style='font-family:academicons'></span> 10.17605/OSF.IO/ZT9UX.") +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 96, family = "newsreader", hjust = 0.5, color = "#243142",
                              margin = margin(b = 0.25, unit = "in"), face = "bold"),
    plot.subtitle = element_text(size = 24, family = "newsreader", hjust = 0.5),
    plot.caption = element_markdown(size = 14, family = "newsreader", hjust = 1),
    legend.title = element_text(size = 28, family = "carto"),
    legend.text = element_text(size = 14, family = "carto"),
    plot.margin = unit(c(0.5, 1, 0.5, 1), "in"),
    panel.background = element_rect(fill = "#FFFEF5", color = "#FFFEF5"),
    panel.border = element_rect(fill = NA, color = "#243142", linewidth = 2),
    legend.box.margin = margin(t = 0.5, b = 0.5, unit = "in")
  )
ggsave(here("outputs", "just1_school_map.pdf"), just1_map, dpi = 300, width = 44, height = 44, units = "in")
ggsave(here("outputs", "just1_school_map.png"), just1_map, dpi = 300, width = 44, height = 44, units = "in")
