# 2026 Bigcontest
# Card Data(Shinhan Card)
rm(list = ls())
set.seed(2021016027)

# Data Structure
library(tidyverse)
theme_set(theme_gray(base_family = 'AppleGothic'))
card <- read_delim(
  '/Users/ahnsangjin/Desktop/2026 Bigcontest/Data/Card.txt',
  delim = '\t',
  locale = locale(encoding = 'CP949'),
  col_types = cols(TA_YMD = col_character())
  )
glimpse(card)
colSums(is.na(card))

# Mutate
card <- card %>%
  mutate(AVG_PER_T = TS_AT / USE_CNT,
         REGION = if_else(MCT_SGG_CD == '서울 강남구', '강남', '춘천'))

# Anomaly Detection
summary(card$TS_AT) %>% format(big.mark = ',', scientific = FALSE)
summary(card$USE_CNT)
card %>% arrange(desc(TS_AT)) %>% head(10)
card_n <- card %>%
  filter(!MCT_RY_CD %in% c('세금공과금', 'ZZ_나머지)'))
dim(card_n)
nrow(card) - nrow(card_n)
sum(card_n$TS_AT) / sum(card$TS_AT) * 100
card_n %>% distinct(MCT_RY_CD) %>% arrange(MCT_RY_CD) %>% print(n = 100)

# Distribution
summary(card_n$TS_AT) %>% format(big.mark = ',', scientific = FALSE)
card_n %>% 
  ggplot(aes(TS_AT)) + 
  geom_histogram(bins = 100) +
  labs(title = 'TS_AT 분포(원래 스케일, 세금공과금, ZZ_나머지 제외')
card_n %>% 
  ggplot(aes(TS_AT + 1)) + 
  geom_histogram(bins = 100) +
  scale_x_log10() +
  labs(title = 'TS_AT 분포(로그 스케일, 세금공과금, ZZ_나머지 제외)', x = 'log_10(TS_AT + 1)')

# # Function Estimation
# library(fitdistrplus)
# fit_by_region <- function(region_name, n_sample = 500000)
#   {
#   x <- card_n %>%
#     filter(REGION == region_name, TS_AT > 0) %>%
#     slice_sample(n = n_sample) %>%
#     pull(TS_AT)
#   fit_lnorm <- fitdist(x, 'lnorm')
#   list(region = region_name,
#        fit_lnorm = fit_lnorm
#   )
# }
# result_gangnam <- fit_by_region('강남')
# result_chuncheon <- fit_by_region('춘천')
# summary(result_gangnam$fit_lnorm)
# summary(result_chuncheon$fit_lnorm)
# qqcomp(result_gangnam$fit_lnorm, main = 'Gangnam - QQ plot(lognormal)')
# qqcomp(result_chuncheon$fit_lnorm, main = 'Chuncheon - QQ plot(lognormal)')

# 지역별
card_n %>%
  group_by(REGION) %>%
  summarise(n = n(), mean = mean(TS_AT), median = median(TS_AT), 
            sd = sd(TS_AT), max = max(TS_AT)) %>%
  mutate(across(where(is.numeric) & !n, ~format(., big.mark=',', scientific=FALSE)))
card_n %>%
  ggplot(aes(TS_AT + 1, fill = REGION)) +
  geom_histogram(aes(y = after_stat(density)), bins = 100, alpha = 0.6, position = 'identity') +
  scale_x_log10() +
  labs(title = '지역별 TS_AT 분포(원래 스케일, 세금공과금, ZZ_나머지 제외)',
       x = 'log_10(TS_AT + 1)', y = 'density')

# 시간대별
time_pattern <- card_n %>%
  group_by(REGION, TIME_GB) %>%
  summarise(total_cnt = sum(USE_CNT), total_amt = sum(TS_AT), .groups = 'drop') %>%
  group_by(REGION) %>%
  mutate(pct_cnt = total_cnt / sum(total_cnt) * 100,
         pct_amt = total_amt / sum(total_amt) * 100)
time_pattern
time_pattern %>%
  ggplot(aes(x = TIME_GB, y = pct_cnt, fill = REGION)) +
  geom_col(position = 'dodge') +
  labs(title = '지역별 시간대별 결제 건수 비율', x = 'time', y = 'proportion(%)')
time_pattern %>%
  ggplot(aes(x = TIME_GB, y = pct_amt, fill = REGION)) +
  geom_col(position = 'dodge') +
  labs(title = '지역별 시간대별 결제 금액 비율', x = 'time', y = 'proportion(%)')

# Time series
card_n <- card_n %>%
  mutate(TA_YMD = as.Date(TA_YMD, format = '%Y%m%d'))
time_order <- c('00_05','06_11','12_17','18_23')
ts_data <- card_n %>%
  mutate(TIME_GB = factor(TIME_GB, levels = time_order)) %>%
  group_by(REGION, TA_YMD, TIME_GB) %>%
  summarise(total_amt = sum(TS_AT), .groups = 'drop') %>%
  arrange(REGION, TA_YMD, TIME_GB) %>%
  group_by(REGION) %>%
  mutate(time_idx = row_number()) %>%
  ungroup()
ts_data %>%
  ggplot(aes(x = time_idx, y = total_amt)) +
  geom_line() +
  facet_wrap(~REGION, ncol = 1, scales = 'free_y') +
  labs(title = '', 
       x = '일자+시간대', y = 'Total TS_AT')

# Function(원하는 기간, 지역, 업종의 매출 시각화)
library(lubridate)
bucket_hour <- c('00_05' = 0, '06_11' = 6, '12_17' = 12, '18_23' = 18)
plot_card_ts <- function(input_start, input_end, region_name, category, data = card_n)
  {
  start_t <- as.POSIXct(as.character(input_start), format = '%Y%m%d%H')
  end_t <- as.POSIXct(as.character(input_end), format = '%Y%m%d%H')
  plot_data <- data %>%
    filter(REGION == region_name, MCT_RY_CD == category) %>%
    mutate(bucket_t = as.POSIXct(paste0(format(TA_YMD, '%Y%m%d'),
                                        sprintf('%02d', bucket_hour[TIME_GB])),
                                 format = '%Y%m%d%H')) %>%
    filter(bucket_t >= start_t, bucket_t < end_t) %>%
    group_by(bucket_t) %>%
    summarise(total_amt = sum(TS_AT), .groups = 'drop') %>%
    arrange(bucket_t)
  ggplot(plot_data, aes(x = bucket_t, y = total_amt)) +
    geom_line() +
    geom_point(size = 1) +
    scale_x_datetime(date_labels = '%m/%d(%a)',
                     date_breaks = '1 day') +
    scale_y_continuous(labels = scales::comma) +
    labs(title = paste0(region_name, ' - ', category, ' 결제 금액(합계)'),
         subtitle = paste(start_t, '~', end_t),
         x = '시점(날짜-시간대)', y = '결제 금액(합계)')
}
plot_card_ts(2025081100, 2025081900, '춘천', '편의점')

# Temp Data
temp_gangnam <- read_delim('/Users/ahnsangjin/Desktop/2026 Bigcontest/Data/Weather/Temp_Gangnam.csv',
                      delim = ',', locale = locale(encoding = 'CP949'))
temp_chuncheon <- read_delim('/Users/ahnsangjin/Desktop/2026 Bigcontest/Data/Weather/Temp_Chuncheon.csv',
                             delim = ',', locale = locale(encoding = 'CP949'))
glimpse(temp_gangnam)
glimpse(temp_chuncheon)
colSums(is.na(temp_gangnam))
colSums(is.na(temp_chuncheon))
temp_gangnam <- temp_gangnam %>%
  rename(기온 = `기온(°C)`) %>%
  mutate(REGION = '강남')
temp_chuncheon <- temp_chuncheon %>%
  rename(기온 = `기온(°C)`) %>%
  mutate(REGION = '춘천')
temp <- bind_rows(temp_gangnam, temp_chuncheon)
glimpse(temp)
temp %>% count(REGION)

# 폭염, 한파 시간대 탐색
temp_bucket <- temp %>%
  mutate(hour = hour(일시),
         TIME_GB = case_when(hour < 6  ~ '00_05',
                             hour < 12 ~ '06_11',
                             hour < 18 ~ '12_17',
                             TRUE ~ '18_23'),
         date = as.Date(일시)) %>%
  group_by(REGION, date, TIME_GB) %>%
  summarise(max_temp = max(기온), mean_temp = mean(기온), min_temp = min(기온), .groups = 'drop') %>%
  mutate(is_heatwave = max_temp >= 33,
         is_coldwave = min_temp <= -5)
temp_bucket %>% count(REGION, is_heatwave)
temp_bucket %>% count(REGION, is_coldwave)
heat_bucket <- temp_bucket %>%
  filter(is_heatwave == TRUE)
cold_bucket <- temp_bucket %>%
  filter(is_coldwave == TRUE)

# 폭염, 한파 시간대까지 시각화하는 함수
plot_card_ts_heat <- function(input_from, input_end, region_name, category, 
                              data = card_n, heat_data = heat_bucket, cold_data = cold_bucket)
  {
  start_t <- as.POSIXct(as.character(input_from), format = '%Y%m%d%H')
  end_t <- as.POSIXct(as.character(input_end), format = '%Y%m%d%H')
  plot_data <- data %>%
    filter(REGION == region_name, MCT_RY_CD == category) %>%
    mutate(bucket_t = as.POSIXct(paste0(format(TA_YMD, '%Y%m%d'),
                                        sprintf('%02d', bucket_hour[TIME_GB])),
                                 format = '%Y%m%d%H')) %>%
    filter(bucket_t >= start_t, bucket_t < end_t) %>%
    group_by(bucket_t) %>%
    summarise(total_amt = sum(TS_AT), .groups = 'drop') %>%
    arrange(bucket_t)
  heat_rects <- heat_data %>%
    filter(REGION == region_name) %>%
    mutate(heat_start = as.POSIXct(paste0(format(date, '%Y%m%d'),
                                          sprintf('%02d', bucket_hour[TIME_GB])),
                                   format = '%Y%m%d%H'),
      heat_end = heat_start + hours(6)) %>%
    filter(heat_start >= start_t, heat_start < end_t)
  cold_rects <- cold_data %>%
    filter(REGION == region_name) %>%
    mutate(cold_start = as.POSIXct(paste0(format(date, '%Y%m%d'),
                                          sprintf('%02d', bucket_hour[TIME_GB])),
                                   format = '%Y%m%d%H'),
           cold_end = cold_start + hours(6)) %>%
    filter(cold_start >= start_t, cold_start < end_t)
  ggplot(plot_data, aes(x = bucket_t, y = total_amt)) +
    geom_rect(data = heat_rects, inherit.aes = FALSE,
              aes(xmin = heat_start, xmax = heat_end, ymin = -Inf, ymax = Inf),
              fill = 'red', alpha = 0.25) +
    geom_rect(data = cold_rects, inherit.aes = FALSE,
              aes(xmin = cold_start, xmax = cold_end, ymin = -Inf, ymax = Inf),
              fill = 'blue', alpha = 0.25) +
    geom_line() +
    geom_point(size = 0.7) +
    scale_x_datetime(date_labels = '%m/%d(%a)', date_breaks = '1 day') +
    scale_y_continuous(labels = scales::comma) +
    labs(title = paste0(region_name, ' - ', category, ' 결제 금액(계)'),
         subtitle = paste(start_t, '~', end_t, '/ 빨강: 폭염 구간(33도 이상), 파랑: 한파 구간(-5도 이하)'),
         x = '시점(날짜-시간대)', y = '결제 금액 합계')
}
plot_card_ts_heat(2025071400, 2025072800, '강남', '편의점')
plot_card_ts_heat(2025071400, 2025072800, '강남', '양식')
plot_card_ts_heat(2025120100, 2025120800, '강남', '양식')
