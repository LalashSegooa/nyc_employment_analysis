# NYC Employment by Industry: Recovery and Recent Momentum 
# Author: Lalash Segooa
# Data: BLS Current Employment Statistics (CES), not seasonally adjusted
#   NYC Total Nonfarm, 6 NYC supersectors, U.S. Total Nonfarm (CEU0000000001)
# Data vintage: Data downloaded from BLS on Sep 29 to 30, 2026: August 2026 preliminary 
# Purpose: YoY growth, recovery vs 2019, indusrty drivers, NYC vs U.S. benchmark
# Note: files in data_raw are unedited BLS downloads; all cleaning is done in this script

library(readxl)
library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)

raw <- read_excel("data_raw/NSA_total_nonfarm.xlsx", skip = 12)
head(raw, 15)
# Reshape: one row per month, build proper dates, drop unreleased months
# Values are in thousands of jobs 
nyc_total <- raw%>%
  pivot_longer(cols = Jan:Dec,
               names_to = "month",
               values_to = "employment") %>%
  mutate(date = ymd(paste(Year, month, "01"))) %>%
  filter(!is.na(employment)) %>%
  select(date, employment) %>%
  arrange(date)

head(nyc_total, 15)
tail(nyc_total, 5)

# YoY growth: this month vs the same month last year (handles NSA seasonality)
nyc_total <- nyc_total %>%
  mutate(yoy_pct = (employment / lag(employment, 12) - 1) * 100)

tail(nyc_total, 13)

# Recovery vs 2019: each month compared to the same month in 2019
base_2019 <- nyc_total %>%
  filter(year(date) == 2019) %>%
  transmute(month_num = month(date), emp_2019 = employment)

nyc_total <- nyc_total %>%
  mutate(month_num = month(date)) %>%
  left_join(base_2019, by = "month_num") %>%
  mutate(vs_2019_pct = (employment / emp_2019 - 1) * 100) %>%
  select(date, employment, yoy_pct, vs_2019_pct)

tail(nyc_total, 5)

# Chart 1: YoY employment growth since 2022
# Starts in 2022 to avoid distorted pandemic base effects in 2021
chart_yoy <- nyc_total %>% 
  filter (date >= as.Date("2022-01-01")) %>%
  ggplot(aes(x = date, y = yoy_pct)) +
  geom_hline(yintercept = 0, color = "grey50") +
  geom_line(color = "#1f4e79", linewidth = 1) +
  labs(title = "NYC Total Nonfarm Employment Growth" , 
       subtitle = "Year over year % change, not seasonally adjusted" , 
       x = NULL,
       y = "YoY change (%)", 
       caption = "Source: BLS CES. Latest month preliminary. Analysis: Lalash Segooa") +
  theme_minimal()

chart_yoy

ggsave("output/nyc_total_yoy.png", chart_yoy,
       width = 8, height = 5, dpi = 300)

#Industries: inspect structure before cleaning 
raw_ind <- read_excel("data_raw/NSA_Industries_NYC.xlsx", skip = 3)
names(raw_ind) [1:4]
raw_ind[, 1:4]

# Map BLS series IDs to industry names 
industry_names <- tibble(
  series_id = c("SMU36935611500000001", "SMU36935615000000001", "SMU36935615500000001", "SMU36935616000000001",
                "SMU36935616500000001", "SMU36935617000000001"),
  industry = c("Mining, Logging, and Construction", "Information","Financial Activities",
               "Professional and Business Services", "Education and Health Services", "Leisure and Hospitality")
)

# Reshape: one row per industry per month, clean date and values 
nyc_ind <- raw_ind %>%
  rename(series_id = `Series ID`) %>%
  pivot_longer(cols = -series_id,
               names_to = "month_year",
               values_to = "employment",
               values_transform = as.character) %>%
  mutate(date = my(gsub("\n", " ", month_year)),
         employment = as.numeric(gsub("[^0-9.]", "", employment))) %>%
  filter(!is.na(employment)) %>%
  left_join(industry_names, by = "series_id") %>%
  select(date, industry, employment) %>%
  arrange(industry, date)

count(nyc_ind, industry)
tail(nyc_ind, 6)

#Step A: build 2019 baseline table by industry and month 
base_ind_2019 <- nyc_ind %>%
  filter(year(date) == 2019) %>%
  transmute(industry, month_num = month(date), emp_2019 = employment)

# Step B: YoY growth and recovery vs 2019, calculated within each industry 
nyc_ind <- nyc_ind %>%
  group_by(industry) %>%
  mutate(yoy_pct = (employment / lag(employment, 12) - 1) * 100) %>%
  ungroup() %>%
  mutate(month_num = month(date)) %>%
  left_join(base_ind_2019, by = c("industry", "month_num")) %>%
  mutate(vs_2019_pct = (employment / emp_2019 - 1) * 100) %>%
  select(date, industry, employment, yoy_pct, vs_2019_pct)

# Latest month snapshot, ranked by YoY growth
latest <- nyc_ind %>%
  filter(date == max(date)) %>%
  arrange(desc(yoy_pct))

latest

# Jobs added or lost since 2019, in thousands
latest <- latest %>%
  mutate(change_vs_2019_k = employment - employment / (1 + vs_2019_pct / 100))

latest %>% select (industry, employment, change_vs_2019_k)

#Chart 2: job change since 2019 by industry (thousands) 
chart_ind <- latest %>%
  mutate(direction = ifelse(change_vs_2019_k >= 0, "Gain", "Loss")) %>%
  ggplot(aes(x = change_vs_2019_k,
             y = reorder(industry, change_vs_2019_k),
             fill = direction)) +
  geom_col() +
  geom_vline(xintercept = 0, color = "grey40") +
  geom_text(aes(label = round(change_vs_2019_k, 1),
                hjust = ifelse(change_vs_2019_k >= 0, -0.15, 1.15)),
            size = 3.5) +
  scale_fill_manual(values = c(Gain = "#1f4e79", Loss = "#c0504d"),
                    guide = "none") +
  scale_x_continuous(expand = expansion(mult = 0.15)) +
  labs(title = "NYC Job Change Since 2019 by Industry", 
       subtitle = "August 2026 vs August 2019, thousands of jobs, not seasonally adjusted", 
       x = "Change (thousands)", 
       y = NULL, 
       caption = "Source: BLS CES. August 2026 preliminary. Analysis: Lalash Segooa") +
  theme_minimal()

chart_ind

ggsave("output/nyc_industry_change_2019.png", chart_ind, 
       width = 8, height = 5, dpi = 300)
# Chart 3: YoY growth by industry since 2023 (small multiples)
chart_ind_yoy <- nyc_ind %>%
  filter(date >= as.Date("2023-01-01")) %>%
  ggplot(aes(x = date, y = yoy_pct)) +
  geom_hline(yintercept = 0, color = "grey50") +
  geom_line(color = "#1f4e79", linewidth = 0.8) +
  facet_wrap(~ industry, ncol = 2) +
  labs(title = "NYC Employment Growth by Industry", 
       subtitle = "Year over year % change, not seasonally adjusted", 
       x = NULL, 
       y = "YoY change (%)", 
       caption = "Source: BLS CES. August 2026 preliminary. Analysis: Lalash Segooa") +
  theme_minimal()

chart_ind_yoy
ggsave("output/nyc_industry_yoy.png", chart_ind_yoy,
       width = 9, height = 7, dpi = 300)

# NSA US Total Nonfarm loaded: as a benchmark 
us_raw <- read_excel("data_raw/NSA_us_total_nonfarm.xlsx", skip = 12)
head(us_raw, 15)

# Reshape U.S. data and calculate YoY growth 
us_total <- us_raw%>%
  pivot_longer(cols = Jan:Dec,
               names_to = "month",
               values_to = "employment") %>%
  mutate(date = ymd(paste(Year, month, "01"))) %>%
  filter(!is.na(employment)) %>%
  select(date, employment) %>%
  arrange(date) %>%
  mutate(yoy_pct = (employment / lag(employment, 12) - 1) * 100)

tail(us_total, 5)

# Combine NYC and U.S. into one table for comparison 
compare <- bind_rows(
  nyc_total %>% select (date, yoy_pct) %>% mutate(area = "New York City"),
  us_total %>% select (date, yoy_pct) %>% mutate(area = "United States"))

# Chart 4: NYC VS U.S. employment growth 
chart_compare <- compare %>%
  filter(date >= as.Date("2022-01-01")) %>%
  ggplot(aes(x = date, y = yoy_pct, color = area)) +
  geom_hline(yintercept = 0, color = "grey50") +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("New York City" = "#1f4e79",
                                "United States" = "#999999")) +
  labs(title = "NYC vs U.S. Employment Growth", 
       subtitle = "Year over year % change in total nonfarm jobs, not seasonally adjusted",
       x = NULL, y = "YoY change (%)", color = NULL, 
       caption = "Source: BLS CES. August 2026 preliminary. Analysis: Lalash Segooa.") +
  theme_minimal() +
  theme(legend.position = "top")

chart_compare

ggsave("output/nyc_vs_us_yoy.png", chart_compare,
       width = 8, height = 5, dpi = 300)

