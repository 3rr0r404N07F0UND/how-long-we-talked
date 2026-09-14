source("./total_format_time.r")
source("./format_time.r")

raw_data <- read.csv("phone.csv", fileEncoding = "UTF-8")

raw_data_seconds <- sapply(raw_data$duration, function(raw_time) {
  temp_time <- strsplit(raw_time, ":")
  hour <- as.numeric(temp_time[[1]][1])
  minute <- as.numeric(temp_time[[1]][2])
  second <- as.numeric(temp_time[[1]][3])

  (hour * 60 * 60 + minute * 60 + second)
})

result_list <- list()

result_list$length <- length(raw_data$duration)
result_list$total_seconds <- sum(raw_data_seconds)
result_list$total_minutes <- result_list$total_seconds %/% 60
result_list$total_hours <- result_list$total_minutes %/% 60
result_list$total_days <- result_list$total_hours %/% 24
result_list$total_years <- result_list$total_days %/% 365
result_list$average_seconds <- mean(raw_data_seconds)
result_list$median_seconds <- median(raw_data_seconds)
result_list$max_seconds <- max(raw_data_seconds)
result_list$min_seconds <- min(raw_data_seconds)
result_list$variance <- var(raw_data_seconds)
result_list$standard_deviation <- sd(raw_data_seconds)

dates <- as.Date(raw_data$datetime)
date_counts <- table(dates)
result_list$most_call_date <- names(date_counts)[which.max(date_counts)]
result_list$most_call_count <- max(date_counts)

result_list$first_call_date <- min(dates)
result_list$last_call_date <- max(dates)
result_list$period_days <- as.numeric(result_list$last_call_date - result_list$first_call_date) + 1

result_list$average_calls_per_day <- result_list$length / result_list$period_days

daily_seconds <- tapply(
  raw_data_seconds,
  dates,
  sum
)

result_list$most_call_time_date <- names(daily_seconds)[which.max(daily_seconds)]
result_list$most_call_time <- max(daily_seconds)

cat(sprintf("총 통화 횟수: %d회", result_list$length), end = "\n")
cat(total_format_time(total_seconds = result_list$total_seconds), end = "\n")
cat(sprintf(
  "통화 기록 기간: %s ~ %s, %d일", result_list$first_call_date, result_list$last_call_date, result_list$period_days
), end = "\n")
cat(sprintf("평균 통화 시간: %s (회당)", format_time(as.integer(result_list$average_seconds))), end = "\n")
cat(sprintf("평균 통화 횟수: %.2f회", result_list$average_calls_per_day), end = "\n")
cat(sprintf("중앙값(중위수): %s", format_time(as.integer(result_list$median_seconds))), end = "\n")
cat(sprintf("분산: %s초²", as.integer(result_list$variance)), end = "\n")
cat(sprintf("표준편차: %s", format_time(as.integer(result_list$standard_deviation))), end = "\n")
cat(sprintf("가장 긴 통화: %s", format_time(as.integer(result_list$max_seconds))), end = "\n")
cat(sprintf("가장 짧은 통화: %s", format_time(as.integer(result_list$min_seconds))), end = "\n")
cat(sprintf("가장 전화 많이 한 날: %s (%d회)", result_list$most_call_date, result_list$most_call_count), end = "\n")
cat(sprintf(
  "가장 오래 통화한 날: %s (%s)", result_list$most_call_time_date, format_time(result_list$most_call_time)
), end = "\n")
