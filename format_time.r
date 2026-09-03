format_time <- function(total_seconds) {
  hours <- total_seconds %/% 3600
  minutes <- total_seconds %% 3600 %/% 60
  seconds <- total_seconds %% 60

  if (hours > 0) {
    sprintf("%d시간 %d분 %d초", hours, minutes, seconds)
  } else if (minutes > 0) {
    sprintf("%d분 %d초", minutes, seconds)
  } else {
    sprintf("%d초", seconds)
  }
}
