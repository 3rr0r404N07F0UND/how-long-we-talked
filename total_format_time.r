total_format_time <- function(total_seconds) {
  days <- total_seconds %/% 86400
  hours <- total_seconds %% 86400 %/% 3600
  minutes <- total_seconds %% 3600 %/% 60
  seconds <- total_seconds %% 60

  total_hours <- days * 24 + hours
  total_minutes <- total_hours * 60 + minutes

  paste(
    sprintf("%s시간 %d분 %d초", format(total_hours, big.mark = ","), minutes, seconds),
    sprintf("(= %s분 %d초)", format(total_minutes, big.mark = ","), seconds),
    sprintf("(= %s초)", format(total_seconds, big.mark = ",")),
    sprintf("약 %s일 %d시간 %d분 %d초", format(days, big.mark = ","), hours, minutes, seconds),
    sep = "\n"
  )
}
