library(educationdata)
library(dplyr)
library(tidyr)
library(readr)
library(here)
library(ggpubr)
library(ggbeeswarm)

corporation_list <- c(
  1802640,
  1812720,
  1805670,
  1812360,
  1810920,
  1800270,
  1804770,
  1812810,
  1808820
  )
ccd_data <- get_education_data(level = 'schools', 
                                   source = "ccd",
                                   topic = 'directory', 
                                   filters = list(fips = 18,
                                                  year = 2021
                                   ),
                                   add_labels = TRUE)
crdc_data <- get_education_data(level = 'schools', 
                               source = "crdc",
                               topic = 'enrollment', 
                               filters = list(fips = 18,
                                              year = 2017
                               ),
                               subtopic = c("race", "sex"),
                               add_labels = TRUE)
ccd_data <- ccd_data |>
  #filter(leaid %in% corporation_list) |>
  filter(leaid == 1800150) |>
  filter(highest_grade_offered != "Pre-K") |>
  select(
    ncessch,
    school_id,
         lea_name,
    leaid,
         school_name,
         latitude,
         longitude,
         urban_centric_locale,
         school_level,
         frl = free_or_reduced_price_lunch,
         enrollment
    ) |>
  na.omit() |>
  mutate(frl_pct = frl / enrollment) |>
  arrange(lea_name, frl_pct)
crdc_data <- crdc_data |>
  filter(leaid %in% ccd_data$leaid) |>
  filter(sex == "Total" & disability == "Total") |>
  select(
    ncessch,
    race,
    enrollment_crdc
  ) |>
  pivot_wider(
    names_from = race,
    values_from = enrollment_crdc
  ) |>
  rename(
    white = White,
    black = Black,
    hispanic = Hispanic,
    asian = Asian,
    amerindian = `American Indian or Alaska Native`,
    pacislander = `Native Hawaiian or other Pacific Islander`,
    more = `Two or more races`,
    total = Total
  ) |>
  mutate(urm_total = total - white) |>
  mutate(urm_pct = urm_total / total)

school_data <- left_join(ccd_data, crdc_data, by = "ncessch") |>
  na.omit() |>
  write_csv(here("data", "school_test2.csv"))


##################################


crit_scorps <- c(
  1810650,
  1805670,
  1812360,
  1808820
)


crit_data <- get_education_data(
  level = "schools",
  source = "edfacts",
  topic = "assessments",
  filters = list(
    year = c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2020),
    grade_edfacts = c(3, 6),
    fips = 18
  ),
  subtopic = list("race")
) |>
  filter(leaid %in% crit_scorps) |>
  filter(race != 1 & race != 99) |>
  select(
    leaid = leaid_num,
    ncessch,
    school_name,
    year,
    grade = grade_edfacts,
    race,
    reading_prof = read_test_num_valid,
    reading_low = read_test_pct_prof_low,
    reading = read_test_pct_prof_midpt,
    reading_high = read_test_pct_prof_high,
    read_test_num_valid,
    math_low = math_test_pct_prof_low,
    math = math_test_pct_prof_midpt,
    math_high = math_test_pct_prof_high
  ) |>
  mutate(reading = if_else(
    reading == "-3",
    NA,
    reading
  )) |>
  mutate(math = if_else(
    math == "-3",
    NA,
    math
  )) |>
  na.omit() |>
  mutate(
    sch_level = grade
  ) |>
  mutate(
    race = case_when(
      race == 2 ~ "black",
      race == 3 ~ "latine",
      race == 4 ~ "asian",
      race == 5 ~ "amerindian",
      race == 6 ~ "pacislander",
      race == 7 ~ "more"
    )
  ) |>
  pivot_wider(
    names_from = c(
      "grade"
    ),
    values_from = c(
      "reading_low",
      "reading",
      "reading_high",
      "math_low",
      "math",
      "math_high"
    )
  )



crit_elementary_black <- crit_data |>
  filter(sch_level == 3) |>
  filter(race == "black") |>
  select(
    leaid,
    ncessch,
    school_name,
    year,
    reading_prof,
    reading_low_3,
    reading_3,
    reading_high_3
  ) |>
  na.omit()

hse_frame <- crit_elementary_black |>
  filter(leaid == 1810650)

ggscatter(crit_elementary_black,
          x = "year",
          y = "reading_3")

ggscatter(hse_frame,
          x = "year",
          y = "reading_3")

ggboxplot(crit_elementary_black,
          x = "year",
          y = "reading_3")

ggboxplot(hse_frame,
          x = "year",
          y = "reading_3")

elementary_black_wilcox <- pairwise.wilcox.test(
  crit_elementary_black$reading_3,
  crit_elementary_black$year,
  p.adjust.method = "BH"
)
elementary_black_wilcox

hse_wilcox <- pairwise.wilcox.test(
  hse_frame$reading_3,
  hse_frame$year,
  p.adjust.method = "BH"
)
hse_wilcox

data_09 <- crit_elementary_black |>
  filter(year == "2009")
data_10 <- crit_elementary_black |>
  filter(year == "2010")
data_12 <- crit_elementary_black |>
  filter(year == "2012")
data_14 <- crit_elementary_black |>
  filter(year == "2014")
data_16 <- crit_elementary_black |>
  filter(year == "2016")
data_18 <- crit_elementary_black |>
  filter(year == "2018")
data_20 <- crit_elementary_black |>
  filter(year == "2020")

wilcox_0920 <- wilcox.test(
  data_09$reading_3,
  data_20$reading_3,
  paired = FALSE
)
wilcox_0920

wilcox_0910 <- wilcox.test(
  data_09$reading_3,
  data_10$reading_3,
  paired = FALSE
)
wilcox_0910

wilcox_1012 <- wilcox.test(
  data_10$reading_3,
  data_12$reading_3,
  paired = FALSE
)
wilcox_1012

wilcox_1214 <- wilcox.test(
  data_12$reading_3,
  data_14$reading_3,
  paired = FALSE
)
wilcox_1214


wilcox_1416 <- wilcox.test(
  data_14$reading_3,
  data_16$reading_3,
  paired = FALSE
)
wilcox_1416

wilcox_1618 <- wilcox.test(
  data_16$reading_3,
  data_18$reading_3,
  paired = FALSE
)
wilcox_1618

wilcox_1820 <- wilcox.test(
  data_18$reading_3,
  data_20$reading_3,
  paired = FALSE
)
wilcox_1820

library(effsize)

data_1820 <- bind_rows(data_18, data_20)
eff_1820 <- cohen.d(
  reading_3 ~ year,
  data_1820,
  paired = FALSE,
  hedges.correction = TRUE
)
eff_1820

data_1618 <- bind_rows(data_16, data_18)
eff_1618 <- cohen.d(
  reading_3 ~ year,
  data_1618,
  paired = FALSE,
  hedges.correction = TRUE
)
eff_1618

data_1416 <- bind_rows(data_16, data_18)
eff_1416 <- cohen.d(
  reading_3 ~ year,
  data_1416,
  paired = FALSE,
  hedges.correction = TRUE
)
eff_1416

data_1214 <- bind_rows(data_12, data_14)
eff_1214 <- cohen.d(
  reading_3 ~ year,
  data_1214,
  paired = FALSE,
  hedges.correction = TRUE
)
eff_1214

data_1012 <- bind_rows(data_10, data_12)
eff_1012 <- cohen.d(
  reading_3 ~ year,
  data_1012,
  paired = FALSE,
  hedges.correction = TRUE
)
eff_1012

data_0910 <- bind_rows(data_09, data_10)
eff_0910 <- cohen.d(
  reading_3 ~ year,
  data_0910,
  paired = FALSE,
  hedges.correction = TRUE
)
eff_0910

data_0920 <- bind_rows(data_09, data_20)
eff_0920 <- cohen.d(
  reading_3 ~ year,
  data_0920,
  paired = FALSE,
  hedges.correction = TRUE
)
eff_0920

crit_change_black <- data.frame(
  year = c(
    "2009",
    "2010",
    "2012",
    "2014",
    "2016",
    "2018",
    "2020"
  ),
  reading.mean = c(
    mean(data_09$reading_3),
    mean(data_10$reading_3),
    mean(data_12$reading_3),
    mean(data_14$reading_3),
    mean(data_16$reading_3),
    mean(data_18$reading_3),
    mean(data_20$reading_3)
  ),
  W = c(
    NA,
    wilcox_0910$statistic,
    wilcox_1012$statistic,
    wilcox_1214$statistic,
    wilcox_1416$statistic,
    wilcox_1618$statistic,
    wilcox_1820$statistic
  ),
  p = c(
    NA,
    round(wilcox_0910$p.value, digits = 3),
    round(wilcox_1012$p.value, digits = 3),
    round(wilcox_1214$p.value, digits = 3),
    round(wilcox_1416$p.value, digits = 3),
    round(wilcox_1618$p.value, digits = 3),
    round(wilcox_1820$p.value, digits = 3)
  ),
  g = c(
    NA,
    eff_0910$estimate,
    eff_1012$estimate,
    eff_1214$estimate,
    eff_1416$estimate,
    eff_1618$estimate,
    eff_1820$estimate
  )
)







library(forestplot)

crit_elementary_black_forest_frame <- crit_elementary_black |>
  group_by(year) |>
  rename(
    mean = reading_3,
    lower = reading_low_3,
    upper = reading_high_3,
    prof = reading_prof
  )
crit_elementary_black_forest_frame |>
  forestplot(labeltext = c(
    year,
    prof
  ))

crit_elementary_black_forest <- crit_elementary_black |>
  mutate(
    subgroup = if_else(
      year == "2009",
      "baseline",
      "change"
      )) |>
  rename(
    mean = reading_3,
    lower = reading_low_3,
    upper = reading_high_3,
    prof = reading_prof
  ) |>
  forestplot(
    labeltext = (
      c(
      subgroup
      )
    )
  )

print(crit_elementary_black_forest)


crit_elementary_swarm <- ggplot(
  crit_elementary_black,
  aes(
    x = year,
    y = reading_3#,
    #color = scoScore
  )
) +
  geom_beeswarm(method = "center", cex = 1.5, corral = "wrap", shape = 18, size = 3) +
  geom_text_repel(
    aes(
      x = year,
      y = reading_3,
      label = school_name
    ),
    size = 2.75, color = "#243142",
    max.overlaps = 4,
    point.padding = 0.5
  )
crit_elementary_swarm  
