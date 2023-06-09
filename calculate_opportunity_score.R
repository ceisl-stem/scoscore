library(readr)
library(dplyr)
library(tidyr)
library(here)
library(ggplot2)
library(ggpubr)
library(waffle)

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

iu_ramp_palette = colorRampPalette(c("#990000", "#59264D", "#006298"))

state_3rd_proficiency <- .816
state_6th_proficiency <- .341
state_gpc <- .864

school_corp_frame <- read_csv(here("data", "maxim.csv")) |>
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
  mutate(frl_pct = frl_pct * 100)

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
urcl_plot

opp_plot <- ggscatter(
  school_corp_frame,
  "urm_pct",
  "frl_pct",
  size = "enrollment",
  color = "urban_centric_locale",
  palette = ucl_palette) +
  theme_pubr()
opp_plot

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

urcl_city <- urcl_enrollment_frame[1,3] +
  urcl_enrollment_frame[2,3] +
  urcl_enrollment_frame[3,3]
urcl_suburb <- urcl_enrollment_frame[4,3] +
  urcl_enrollment_frame[5,3] +
  urcl_enrollment_frame[6,3]
urcl_town <- urcl_enrollment_frame[7,3] +
  urcl_enrollment_frame[8,3] +
  urcl_enrollment_frame[9,3]
urcl_rural <- urcl_enrollment_frame[10,3] +
  urcl_enrollment_frame[11,3] +
  urcl_enrollment_frame[12,3]
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
  mutate(total_enrollment = total_enrollment / 1000)
enr_waffle_plot <- waffle(enr_waffle, rows = 10,
       colors = ucl_palette, legend_pos = "bottom")
ggsave(here("outputs", "enr_waffle.png"), enr_waffle_plot, dpi = 300, width = 18, height = 6, units = "in")


n_waffle <- urcl_frame |>
  select(urban_centric_locale, num)
n_waffle_plot <- waffle(n_waffle, rows = 5,
       colors = ucl_palette,
       legend_pos = "bottom")
ggsave(here("outputs", "n_waffle.png"), n_waffle_plot, dpi = 300, width = 18, height = 6, units = "in")
