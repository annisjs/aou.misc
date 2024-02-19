#' Format Fitbit activity and sleep data for time varying Cox model
#'
#' @param sleep_pa data.table with columns: person_id, date, minute_asleep, minute_rem, minute_deep, minute_light, minute_in_bed, steps
#' @param dx data.table with columns: person_id, dx_entry_date, dx_status
#' @param last_medical_encounter data.table with columns: person_id, last_medical_encounter_entry_date
#' @param cull_list a character vector containing variables to perform exclusions on with months less than 15 days of observations.
#' @param cull_baseline should there be at least 15 observations in the baseline monitoring period?
#' @return A data.table
#' @import data.table
#' @export
#'
format_fitbit_sleep_activity_cox <- function(sleep_pa,dx,last_medical_encounter,cull_list,cull_baseline=TRUE)
{
  sleep_pa[,hour_asleep := minute_asleep / 60]
  sleep_pa[,hour_light := minute_light / 60]
  sleep_pa[,minute10_deep := minute_deep / 10]
  sleep_pa[,minute10_rem := minute_rem / 10]
  sleep_pa[,efficiency := minute_asleep / minute_in_bed]
  sleep_pa[, pct_awake := minute_awake / minute_in_bed]
  sleep_pa[, pct_restless := minute_restless / minute_in_bed]

  merged_cox <- merge(sleep_pa,dx,by="person_id",all.x=TRUE)
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
                                                    baseline_hour_light = mean(hour_light,na.rm=T),
                                                    baseline_pct_rem = mean((minute_rem / minute_asleep) * 100,na.rm=T),
                                                    baseline_pct_light = mean((minute_light / minute_asleep) * 100,na.rm=T),
                                                    baseline_pct_deep = mean((minute_deep / minute_asleep) * 100,na.rm=T),
                                                    baseline_efficiency = mean(efficiency,na.rm=T),
                                                    baseline_minute_awake = mean(minute_awake, na.rm=T),
                                                    baseline_minute_restless = mean(minute_restless, na.rm=T),
                                                    baseline_pct_awake = mean(pct_awake,na.rm=T),
                                                    baseline_pct_restless = mean(pct_restless,na.rm=T),
                                                    baseline_steps = mean(steps,na.rm=T),
                                                    baseline_sd_minute_asleep = sd(minute_asleep,na.rm=T),
                                                    count = length(date),
                                                    baseline_minute_asleep_count = length(which(!is.na(minute_asleep))),
                                                    baseline_minute_restless_count = length(which(!is.na(minute_restless))),
                                                    baseline_minute_deep_count = length(which(!is.na(minute_deep))),
                                                    baseline_minute_light_count = length(which(!is.na(minute_light))),
                                                    baseline_minute_rem_count = length(which(!is.na(minute_rem))),
                                                    baseline_minute_awake_count = length(which(!is.na(minute_awake)))),
                                                 .(person_id)]

  if (cull_baseline)
  {
    if ("minute_asleep" %in% cull_list)
    {
      merged_cox_baseline_agg <- merged_cox_baseline_agg[baseline_minute_asleep_count >= 15]
    }

    if ("minute_restless" %in% cull_list)
    {
      merged_cox_baseline_agg <- merged_cox_baseline_agg[baseline_minute_restless_count >= 15]
    }

    if ("minute_deep" %in% cull_list)
    {
      merged_cox_baseline_agg <- merged_cox_baseline_agg[baseline_minute_deep_count >= 15]
    }

    if ("minute_light" %in% cull_list)
    {
      merged_cox_baseline_agg <- merged_cox_baseline_agg[baseline_minute_light_count >= 15]
    }

    if ("minute_rem" %in% cull_list)
    {
      merged_cox_baseline_agg <- merged_cox_baseline_agg[baseline_minute_rem_count >= 15]
    }

    if ("minute_awake" %in% cull_list)
    {
      merged_cox_baseline_agg <- merged_cox_baseline_agg[baseline_minute_awake_count >= 15]
    }
  }

  cat("\n")
  cat("\nExcluding months with < 15 days in the baseline period.")
  merged_cox <- merge(merged_cox,merged_cox_baseline_agg,by="person_id")
  cat("\nN:",length(unique(merged_cox$person_id)))
  cat("\nDays:",nrow(merged_cox))

  cat("\n")
  cat("\nAnalysis switching to months…")


  if ("minute_asleep" %in% cull_list)
  {
    cat("\nExcluding months with < 15 days of minute_asleep observations")
    init_months <- merged_cox[,.(count = length(which(!is.na(minute_asleep)))),.(person_id,time1,time2)]
    cat("\nN:",length(unique(merged_cox$person_id)))
    cat("\nMonths: ", nrow(init_months))

    cat("\n")
    cat("\nRemoving months with less than 15 days of observations:")
    cat("\nMonths removed: ",nrow(init_months[count < 15]))
    cat("\nMonths remaining: ",nrow(init_months[count >= 15]))
    cat("\nPercentage removed: ",round(nrow(init_months[count < 15]) / nrow(init_months),3) * 100, "%")

    merged_cox[,count := length(which(!is.na(minute_asleep))),.(person_id,time1,time2)]
    merged_cox <- merged_cox[count >= 15]
  }


  if ("steps" %in% cull_list)
  {
    cat("\nExcluding months with < 15 days of steps observations")
    init_months <- merged_cox[,.(count = length(which(!is.na(steps)))),.(person_id,time1,time2)]
    cat("\nN:",length(unique(merged_cox$person_id)))
    cat("\nMonths: ", nrow(init_months))

    cat("\n")
    cat("\nRemoving months with less than 15 days of observations:")
    cat("\nMonths removed: ",nrow(init_months[count < 15]))
    cat("\nMonths remaining: ",nrow(init_months[count >= 15]))
    cat("\nPercentage removed: ",round(nrow(init_months[count < 15]) / nrow(init_months),3) * 100, "%")

    merged_cox[,count := length(which(!is.na(steps))),.(person_id,time1,time2)]
    merged_cox <- merged_cox[count >= 15]
  }

  if ("minute_rem" %in% cull_list)
  {
    cat("\nExcluding months with < 15 days of minute_rem observations")
    init_months <- merged_cox[,.(count = length(which(!is.na(minute_rem)))),.(person_id,time1,time2)]
    cat("\nN:",length(unique(merged_cox$person_id)))
    cat("\nMonths: ", nrow(init_months))

    cat("\n")
    cat("\nRemoving months with less than 15 days of observations:")
    cat("\nMonths removed: ",nrow(init_months[count < 15]))
    cat("\nMonths remaining: ",nrow(init_months[count >= 15]))
    cat("\nPercentage removed: ",round(nrow(init_months[count < 15]) / nrow(init_months),3) * 100, "%")

    merged_cox[,count := length(which(!is.na(minute_rem))),.(person_id,time1,time2)]
    merged_cox <- merged_cox[count >= 15]
  }

  if ("minute_deep" %in% cull_list)
  {
    cat("\nExcluding months with < 15 days of minute_deep observations")
    init_months <- merged_cox[,.(count = length(which(!is.na(minute_deep)))),.(person_id,time1,time2)]
    cat("\nN:",length(unique(merged_cox$person_id)))
    cat("\nMonths: ", nrow(init_months))

    cat("\n")
    cat("\nRemoving months with less than 15 days of observations:")
    cat("\nMonths removed: ",nrow(init_months[count < 15]))
    cat("\nMonths remaining: ",nrow(init_months[count >= 15]))
    cat("\nPercentage removed: ",round(nrow(init_months[count < 15]) / nrow(init_months),3) * 100, "%")

    merged_cox[,count := length(which(!is.na(minute_deep))),.(person_id,time1,time2)]
    merged_cox <- merged_cox[count >= 15]
  }

  if ("minute_light" %in% cull_list)
  {
    cat("\nExcluding months with < 15 days of minute_light observations")
    init_months <- merged_cox[,.(count = length(which(!is.na(minute_light)))),.(person_id,time1,time2)]
    cat("\nN:",length(unique(merged_cox$person_id)))
    cat("\nMonths: ", nrow(init_months))

    cat("\n")
    cat("\nRemoving months with less than 15 days of observations:")
    cat("\nMonths removed: ",nrow(init_months[count < 15]))
    cat("\nMonths remaining: ",nrow(init_months[count >= 15]))
    cat("\nPercentage removed: ",round(nrow(init_months[count < 15]) / nrow(init_months),3) * 100, "%")

    merged_cox[,count := length(which(!is.na(minute_light))),.(person_id,time1,time2)]
    merged_cox <- merged_cox[count >= 15]
  }

  if ("minute_restless" %in% cull_list)
  {
    cat("\nExcluding months with < 15 days of minute_restless observations")
    init_months <- merged_cox[,.(count = length(which(!is.na(minute_restless)))),.(person_id,time1,time2)]
    cat("\nN:",length(unique(merged_cox$person_id)))
    cat("\nMonths: ", nrow(init_months))

    cat("\n")
    cat("\nRemoving months with less than 15 days of observations:")
    cat("\nMonths removed: ",nrow(init_months[count < 15]))
    cat("\nMonths remaining: ",nrow(init_months[count >= 15]))
    cat("\nPercentage removed: ",round(nrow(init_months[count < 15]) / nrow(init_months),3) * 100, "%")

    merged_cox[,count := length(which(!is.na(minute_restless))),.(person_id,time1,time2)]
    merged_cox <- merged_cox[count >= 15]
  }

  if ("minute_awake" %in% cull_list)
  {

    cat("\nExcluding months with < 15 days of minute_restless observations")
    init_months <- merged_cox[,.(count = length(which(!is.na(minute_awake)))),.(person_id,time1,time2)]
    cat("\nN:",length(unique(merged_cox$person_id)))
    cat("\nMonths: ", nrow(init_months))

    cat("\n")
    cat("\nRemoving months with less than 15 days of observations:")
    cat("\nMonths removed: ",nrow(init_months[count < 15]))
    cat("\nMonths remaining: ",nrow(init_months[count >= 15]))
    cat("\nPercentage removed: ",round(nrow(init_months[count < 15]) / nrow(init_months),3) * 100, "%")

    merged_cox[,count := length(which(!is.na(minute_awake))),.(person_id,time1,time2)]
    merged_cox <- merged_cox[count >= 15]
  }

  #aggregate steps by month
  merged_cox_agg <- merged_cox[,.(mean_hour_asleep = mean(hour_asleep,na.rm=T),
                                  mean_minute10_rem = mean(minute10_rem,na.rm=T),
                                  mean_minute10_deep = mean(minute10_deep,na.rm=T),
                                  mean_hour_light = mean(hour_light,na.rm=T),
                                  mean_pct_rem = mean((minute_rem / minute_asleep) * 100,na.rm=T),
                                  mean_pct_deep = mean((minute_deep / minute_asleep) * 100,na.rm=T),
                                  mean_pct_light = mean((minute_light / minute_asleep) * 100,na.rm=T),
                                  mean_efficiency = mean(efficiency,na.rm=T),
                                  mean_minute_awake = mean(minute_awake,na.rm=T),
                                  mean_minute_restless = mean(minute_restless, na.rm=T),
                                  mean_pct_awake = mean(pct_awake,na.rm=T),
                                  mean_pct_restless = mean(pct_restless,na.rm=T),
                                  mean_steps = mean(steps,na.rm=T),
                                  sd_minute_asleep = sd(minute_asleep,na.rm=T),
                                  baseline_minute10_rem = baseline_minute10_rem[1],
                                  baseline_minute10_deep = baseline_minute10_deep[1],
                                  baseline_hour_light = baseline_hour_light[1],
                                  baseline_hour_asleep = baseline_hour_asleep[1],
                                  baseline_pct_rem = baseline_pct_rem[1],
                                  baseline_pct_light = baseline_pct_light[1],
                                  baseline_pct_deep = baseline_pct_deep[1],
                                  baseline_efficiency = baseline_efficiency[1],
                                  baseline_steps = baseline_steps[1],
                                  baseline_sd_minute_asleep = baseline_sd_minute_asleep[1],
                                  baseline_minute_awake = baseline_minute_awake[1],
                                  baseline_minute_restless = baseline_minute_restless[1],
                                  baseline_pct_awake = baseline_pct_awake[1],
                                  baseline_pct_restless = baseline_pct_restless[1],
                                  count = length(hour_asleep)),.(person_id,time1,time2)]
  merged_cox_agg <- merged_cox_agg[order(merged_cox_agg$time1,decreasing=FALSE)]

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
                                      mean_hour_light = mean_hour_light,
                                      mean_pct_rem = mean_pct_rem,
                                      mean_pct_deep = mean_pct_deep,
                                      mean_pct_light = mean_pct_light,
                                      mean_efficiency = mean_efficiency,
                                      mean_minute_awake = mean_minute_awake,
                                      mean_minute_restless = mean_minute_restless,
                                      mean_pct_awake = mean_pct_awake,
                                      mean_pct_restless = mean_pct_restless,
                                      sd_minute_asleep = sd_minute_asleep,
                                      mean_steps = mean_steps,
                                      baseline_hour_asleep = baseline_hour_asleep,
                                      baseline_minute10_rem = baseline_minute10_rem,
                                      baseline_minute10_deep = baseline_minute10_deep,
                                      baseline_hour_light = baseline_hour_light,
                                      baseline_pct_rem = baseline_pct_rem,
                                      baseline_pct_deep = baseline_pct_deep,
                                      baseline_pct_light = baseline_pct_light,
                                      baseline_efficiency = baseline_efficiency,
                                      baseline_steps = baseline_steps,
                                      baseline_minute_awake = baseline_minute_awake,
                                      baseline_minute_restless = baseline_minute_restless,
                                      baseline_pct_awake = baseline_pct_awake,
                                      baseline_pct_restless = baseline_pct_restless,
                                      baseline_sd_minute_asleep = baseline_sd_minute_asleep),
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
