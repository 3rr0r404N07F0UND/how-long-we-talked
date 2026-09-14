#TODO: 상관계수 만들기
#TODO: 왜도 만들기
#TODO: 첨도 만들기

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
result_list$average_seconds <- mean(raw_data_seconds)
result_list$median_seconds <- median(raw_data_seconds)
result_list$max_seconds <- max(raw_data_seconds)
result_list$min_seconds <- min(raw_data_seconds)
result_list$variance <- var(raw_data_seconds)
result_list$standard_deviation <- sd(raw_data_seconds)

daily_stats <- local({
  daily_seconds <- tapply(
    raw_data_seconds,
    as.Date(raw_data$datetime),
    sum
  )

  daily_stats <- data.frame(
    date = as.Date(names(daily_seconds)),
    total_second = as.numeric(daily_seconds)
  )

  daily_stats <- merge(
    daily_stats,
    setNames(as.data.frame(table(as.Date(raw_data$datetime))), c("date", "call_count")),
    by = "date"
  )
})

result_list$most_call_date <- daily_stats$date[which.max(daily_stats$call_count)]
result_list$most_call_count <- max(daily_stats$call_count)
result_list$most_call_time_date <- (daily_stats$date)[which.max(daily_stats$total_second)]
result_list$most_call_time <- max(daily_stats$total_second)
result_list$first_call_date <- min(daily_stats$date)
result_list$last_call_date <- max(daily_stats$date)
result_list$period_days <- as.numeric(result_list$last_call_date - result_list$first_call_date) + 1
result_list$average_calls_per_day <- result_list$length / result_list$period_days

cat(sprintf("총 통화 횟수: %d회", result_list$length), end = "\n")
cat(sprintf("총 통화 시간: %s", total_format_time(total_seconds = result_list$total_seconds)), end = "\n")
cat(sprintf(
  "통화 기록 기간: %s ~ %s, %d일", result_list$first_call_date, result_list$last_call_date, result_list$period_days
), end = "\n")
cat(sprintf("평균 통화 시간: %s (회당)", format_time(as.integer(result_list$average_seconds))), end = "\n")
cat(sprintf("중앙값(중위수): %s", format_time(as.integer(result_list$median_seconds))), end = "\n")
cat(sprintf("분산: %s초²", format(as.integer(result_list$variance), big.mark = ",")), end = "\n")
cat(sprintf("표준편차: %s", format_time(as.integer(result_list$standard_deviation))), end = "\n")
cat(sprintf(
  "하루 평균 통화 시간: %s", format_time(as.integer(result_list$total_seconds / result_list$period_days))
), end = "\n")
cat(sprintf("하루 평균 통화 횟수: %.2f회", result_list$average_calls_per_day), end = "\n")

cat(sprintf("가장 긴 통화: %s", format_time(as.integer(result_list$max_seconds))), end = "\n")
cat(sprintf("가장 짧은 통화: %s", format_time(as.integer(result_list$min_seconds))), end = "\n")
cat(sprintf("가장 전화 많이 한 날: %s (%d회)", result_list$most_call_date, result_list$most_call_count), end = "\n")
cat(sprintf(
  "가장 오래 통화한 날: %s (%s)", result_list$most_call_time_date, format_time(result_list$most_call_time)
), end = "\n")
