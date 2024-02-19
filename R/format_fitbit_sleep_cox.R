#' Format Fitbit sleep data for time varying Cox model
#'
#' @param fitbit data.table with columns: person_id, date, minute_asleep, minute_rem, minute_deep
#' @param dx data.table with columns: person_id, dx_entry_date, dx_status
#' @param last_medical_encounter data.table with columns: person_id, last_medical_encounter_entry_date
#'
#' @return A data.table
#' @import data.table
#' @export
#'
format_sleep_cox <- function(sleep,dx,last_medical_encounter)
{
  sleep[,date := sleep_date]
  sleep[,hour_asleep := minute_asleep / 60]
  sleep[,minute10_deep := minute_deep / 10]
  sleep[,minute10_rem := minute_rem / 10]
  merged_cox <- merge(sleep,dx,by="person_id",all.x=TRUE)
  merged_cox[, had_before := as.numeric(dx_entry_date - min(date)) <= 180, .(person_id)]
  merged_cox <- merged_cox[had_before == FALSE | is.na(had_before)]
  merged_cox <- merge(merged_cox,last_medical_encounter,all.x=TRUE)
  merged_cox[,dx_status := ifelse(is.na(dx_entry_date),FALSE,dx_entry_date)]

  cat("\nInitial cohort:")
  cat("\nN:",length(unique(merged_cox$person_id)))
  cat("\nDays:",nrow(merged_cox))

  merged_cox[,time1 := lubridate::floor_date(date,"month")]
  merged_cox[,time2 := lubridate::ceiling_date(date,"month")-1]

  #censor to last medical encounter
  merged_cox[,max_date := dx_entry_date]
  merged_cox$max_date[is.na(merged_cox$max_date)] <- merged_cox$last_medical_encounter_entry_date[is.na(merged_cox$max_date)]
  merged_cox <- merged_cox[date <= max_date]

  cat("\n")
  cat("\nExclude any that have no data before censor date:")
  cat("\nN:",length(unique(merged_cox$person_id)))
  cat("\nDays:",nrow(merged_cox))

  #Fork baseline dataset from main dataset
  merged_cox[,baseline_marker := (date - min(date)) <= 180,.(person_id)]
  merged_cox_baseline <- merged_cox[baseline_marker == TRUE]
  merged_cox <- merged_cox[baseline_marker == FALSE]

  cat("\n")
  cat("\nExclude any that have no data beyond 180 days:")
  cat("\nN:",length(unique(merged_cox$person_id)))
  cat("\nDays:",nrow(merged_cox))

  merged_cox_baseline_agg <- merged_cox_baseline[,.(baseline_hour_asleep = mean(hour_asleep,na.rm=T),
                                                    baseline_minute10_rem = mean(minute10_rem,na.rm=T),
                                                    baseline_minute10_deep = mean(minute10_deep,na.rm=T),
                                                    count = length(date)),.(person_id)]
  merged_cox_baseline_agg <- merged_cox_baseline_agg[count >= 15]

  cat("\n")
  cat("\nExcluding months with < 15 days in the baseline period.")
  merged_cox <- merge(merged_cox,merged_cox_baseline_agg,by="person_id")
  cat("\nN:",length(unique(merged_cox$person_id)))
  cat("\nDays:",nrow(merged_cox))

  cat("\n")
  cat("\nAnalysis switching to months…")

  init_months <- merged_cox[,.(count = length(hour_asleep)),.(person_id,time1,time2)]
  cat("\nN:",length(unique(merged_cox$person_id)))
  cat("\nMonths: ", nrow(init_months))

  cat("\n")
  cat("\nRemoving months with less than 15 days of observations:")
  cat("\nMonths removed: ",nrow(init_months[count < 15]))
  cat("\nMonths remaining: ",nrow(init_months[count >= 15]))
  cat("\nPercentage removed: ",round(nrow(init_months[count < 15]) / nrow(init_months),3) * 100, "%")

  merged_cox[,count := length(hour_asleep),.(person_id,time1,time2)]
  merged_cox <- merged_cox[count >= 15]

  merged_cox[,count := length(minute_rem),.(person_id,time1,time2)]
  merged_cox <- merged_cox[count >= 15]

  merged_cox[,count := length(minute_deep),.(person_id,time1,time2)]
  merged_cox <- merged_cox[count >= 15]

  #aggregate steps by month
  merged_cox_agg <- merged_cox[,.(mean_hour_asleep = mean(hour_asleep,na.rm=T),
                                  mean_minute10_rem = mean(minute10_rem,na.rm=T),
                                  mean_minute10_deep = mean(minute10_deep,na.rm=T),
                                  baseline_minute10_rem = baseline_minute10_rem[1],
                                  baseline_minute10_deep = baseline_minute10_deep[1],
                                  baseline_hour_asleep = baseline_hour_asleep[1],
                                  count = length(hour_asleep)),.(person_id,time1,time2)]
  merged_cox_agg <- merged_cox_agg[order(merged_cox_agg$time1,decreasing=FALSE)]
  merged_cox_agg <- merged_cox_agg[!is.na(mean_minute10_rem)]
  merged_cox_agg <- merged_cox_agg[!is.na(mean_minute10_deep)]

  cat("\n")
  cat("\nFinal cohort size:")
  cat("\nN:",length(unique(merged_cox_agg$person_id)))
  cat("\nMonths:",nrow(merged_cox_agg))

  #Format data for cox model

  #attach min and max date
  min_max_date <- merged_cox[,.(min_date = min(date),
                                max_date = max_date[1]),.(person_id)]
  merged_cox_agg <- merge(min_max_date,merged_cox_agg,by="person_id")

  #setup intervals
  merged_cox_agg <- merged_cox_agg[,.(time1 = c(min_date[1]-1,time2[1:(length(time2)-1)]),
                                      time2 = c(time2[1:(length(time2)-1)],max_date[1]),
                                      count=count,
                                      min_date = min_date[1],
                                      max_date = max_date[1],
                                      mean_hour_asleep=mean_hour_asleep,
                                      mean_minute10_rem = mean_minute10_rem,
                                      mean_minute10_deep = mean_minute10_deep,
                                      baseline_hour_asleep = baseline_hour_asleep,
                                      baseline_minute10_rem = baseline_minute10_rem,
                                      baseline_minute10_deep = baseline_minute10_deep),
                                   by=.(person_id)]

  # Do a little tidying to keep those with a single month as a single row.
  # The code block above assumes there's at least 2 months of data, so we have to remove that artifact.
  merged_cox_agg[,data_points := length(count),.(person_id)]
  merged_cox_agg[,repeated := data_points == 2 & (mean_hour_asleep[1] == mean_hour_asleep[2]),.(person_id)]
  merged_cox_agg[data_points == 2 & repeated, time2 := time2[length(time2)],.(person_id)]
  merged_cox_agg[data_points == 2 & repeated, time1 := c(time1[1],time2[1]),.(person_id)]
  merged_cox_agg[,seq := 1:length(count),.(person_id)]

  merged_cox_agg[,time_span:= time2-time1]

  merged_cox_agg$max_date <- lubridate::ymd(merged_cox_agg$max_date)
  merged_cox_agg$min_date <- lubridate::ymd(merged_cox_agg$min_date)
  merged_cox_agg[,duration := as.numeric(max_date - min_date)]

  merged_cox_agg <- merge(merged_cox_agg,dx,by="person_id",all.x=T)

  #get status at time2
  merged_cox_agg[,status := time2 >= dx_entry_date]
  merged_cox_agg$status[is.na(merged_cox_agg$status)] <- FALSE

  #cast dates as numeric
  merged_cox_agg <- merged_cox_agg[order(merged_cox_agg$time1,decreasing=FALSE)]
  merged_cox_agg[,time1_num := c(0,cumsum(as.numeric(diff(time1)))),.(person_id)]
  merged_cox_agg[,time2_num := c(time1_num[2:length(time1_num)],
                                 time1_num[length(time1_num)]+time_span[length(time1_num)]),
                 by=.(person_id)]
  merged_cox_agg[,time1_num := time1_num / 365.25]
  merged_cox_agg[,time2_num := time2_num / 365.25]

  #remove data after event (shouldn't ever happen but just in case)
  merged_cox_agg[,event_passed := status == TRUE & time1 >= dx_entry_date]
  merged_cox_agg <- merged_cox_agg[event_passed==FALSE]

  #This is an artifact left over from the tidying above and finally removed here.
  merged_cox_agg <- merged_cox_agg[time_span != 0]

  setorder(merged_cox_agg, cols = "person_id", "time1")

  merged_cox_agg[,data_points :=NULL]
  merged_cox_agg[,repeated :=NULL]
  merged_cox_agg[,seq :=NULL]
  merged_cox_agg[,time_span :=NULL]
  merged_cox_agg[,dx_status :=NULL]
  merged_cox_agg[,dx_entry_date :=NULL]
  merged_cox_agg[,event_passed :=NULL]

  return(merged_cox_agg)
}
