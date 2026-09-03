source("./total_format_time.r")
source("./format_time.r")

raw_data <- read.csv("phone.txt", fileEncoding = "UTF-8")

raw_data_seconds <- sapply(raw_data$time, function(raw_time) {
  temp_time <- strsplit(raw_time, ":")
  hour <- as.numeric(temp_time[[1]][1])
  minute <- as.numeric(temp_time[[1]][2])
  second <- as.numeric(temp_time[[1]][3])

  (hour * 60 * 60 + minute * 60 + second)
})

result_list <- list()

result_list$length <- length(raw_data$time)
result_list$total_seconds <- sum(raw_data_seconds)
result_list$total_minutes <- result_list$total_seconds %/% 60
result_list$total_hours <- result_list$total_minutes %/% 60
result_list$total_days <- result_list$total_hours %/% 24
result_list$total_years <- result_list$total_days %/% 365
result_list$average_seconds <- mean(raw_data_seconds)
result_list$median_seconds <- median(raw_data_seconds)
result_list$max_seconds <- max(raw_data_seconds)
result_list$min_seconds <- min(raw_data_seconds)

cat(sprintf("총 통화 횟수: %d회", result_list$length), end = "\n")
cat(total_format_time(total_seconds = result_list$total_seconds), end = "\n")
cat(sprintf("평균 통화 시간: %s (회당)", format_time(as.integer(result_list$average_seconds))), end = "\n")
cat(sprintf("중앙값(중위수): %s", format_time(as.integer(result_list$median_seconds))), end = "\n")
cat(sprintf("가장 긴 통화: %s", format_time(as.integer(result_list$max_seconds))), end = "\n")
cat(sprintf("가장 짧은 통화: %s", format_time(as.integer(result_list$min_seconds))), end = "\n")
