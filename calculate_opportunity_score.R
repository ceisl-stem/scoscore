library(readr)
library(dplyr)
library(here)
library(ggplot2)
library(ggpubr)

ucl_palette <- c(
  "City large" = "#990000",
  "City midsize" = "#DC231E",
  "City small" = "#F23A3F",
  "Suburb large" = "#FFAA00",
  "Suburb midsize" = "#FFC132",
  "Suburb small" = "#FFD563",
  "Town fringe" = "#056E41",
  "Town distant" = "#329345",
  "Town remote" = "#63B363",
  "Rural fringe" = "#006298",
  "Rural distant" = "#328BB8",
  "Rural remote" = "#63B1D3"
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
