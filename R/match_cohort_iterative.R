#' Matches cases with control using activity data iteratively
#' @param cohort_dat a data.table representing the activity_summary table for the cohort to match containing at least the following columns person_id, date
#' @param match_rate the number of control to match each case
#' @param control_dat a data.table representing the activity_summary table for the available control candidates to match containing at least the following columns person_id, date
#' @param window_range the window range to require available activity data around the anchor date
#' @param strict_cols vector of column names to match control exactly such as sex or race
#' @param flex_cols vector of column names to match based on similarity
#' @param range_cols named list of column names to match strictly in a range such as age
#' @param baseline_cols vector of column names to match based on similarity of a mean in baseline_range parameter
#' @param fixed_baselines vector of column names to match based on similarity closest to start of baseline_range
#' @param baseline_range baseline_range given in time to event to compute baseline averages and find closest fixed baselines
#' @param anchor_col_name the column name for the anchor date used for baselines and windowing
#' @param min_days_in_baseline the required number of days in baseline applied to both cases and control
#' @param rank ordering column for cases before the iteration
#' 
#' @return a dataframe of matched control with corresponding cases and differences between flexible matched columns and baselines
#' @details
#' This algorithm follows the following steps to match the given cases to a control cohort:
#' First it subsets the cases using window_range and min_days_in_baseline
#' Then orders cases using rank parameter (if given) and iteratively matches each case to match_rate many control cases following the following steps:
#' First it subsets the controls using strict_cols and range_cols
#' Computes baselines and fixed_baselines for controls. The event date for controls is set to event date of case that is being matched at the iterative step
#' Finds the best match_rate many candidates using matchit library  using flex_cols, baseline_cols, and fixed_baselines
#' Returns controls for each cases in one datatable
#' If there is not enough control cases after candidate selection using strict_cols and range_cols then all controls are chosen and if none available then that case won't appear in resulting datatable
#'
#' @examples 
#'\dontrun{
#'control_long_covid_act_cohort = match_cohort_iterative(cases_dat = cases_dat,
#'                                                       controls_dat = controls_act,
#'                                                       match_rate = 5,
#'                                                       window_range = c(180, 180),
#'                                                       strict_cols = c("sex"),
#'                                                       flex_cols = c("age"),
#'                                                       fixed_baselines=c("bmi"),
#'                                                       range_cols = list("age"=5), 
#'                                                       baseline_cols = c("steps"),
#'                                                       baseline_range = c(-180, 0),
#'                                                       anchor_col_name = "date",
#'                                                       min_days_in_baseline = 30,
#'                                                       rank="bmi")

#' }
#' @import data.table
#' @export
match_cohort_iterative = function(cases_dat,
                                  match_rate, 
                                  controls_dat,
                                  window_range = NULL,
                                  strict_cols = NULL, 
                                  flex_cols = NULL, 
                                  range_cols = NULL, 
                                  baseline_cols = NULL,
                                  fixed_baselines = NULL,
                                  baseline_range = NULL, 
                                  anchor_col_name = NULL, 
                                  min_days_in_baseline=NULL,
                                  rank=NULL){
  #window the cases and define person_ids to match
  if(!is.null(anchor_col_name)){
    if(!is.null(window_range)){
      cases_dat = subset_by_date_window(cases_dat, anchor_col_name, before = -1*window_range[[1]], after = window_range[[2]])
    }
    if(!is.null(min_days_in_baseline) && !is.null(baseline_range)){
      cases_dat = subset_by_min_days_in_range(cases_dat, baseline_range[[1]], baseline_range[[2]], min_days_in_baseline, anchor_col_name)
    }
  }
  if(!is.null(rank)){
    cases_dat = cases_dat[order(get(rank))]
  }
  person_ids = cases_dat[!duplicated(person_id)]$person_id
  print(str_glue("There are {length(person_ids)} cases to match and there are {length(controls_dat[!duplicated(person_id)]$person_id)} controls to match!"))
  #create matchlist to hold matched cohort for each case as a named list
  match_list = list()
  
  #iterate over each cases to find matches
  for (pid in person_ids){
    p1_dt = subset(cases_dat, person_id == pid)
    #subset controls datatable to obtain possible candidates using strict matching columns
    if (!is.null(strict_cols)){
      strict_matching_df = subset(p1_dt, select = strict_cols)
      strict_matching_df = unique(strict_matching_df, by = strict_cols)
      candidates = merge(strict_matching_df, controls_dat, by = strict_cols)
    }else{
      candidates = controls_dat
    }
    #subset candidates using range based features
    if (!is.null(range_cols)){
      for (n in names(range_cols)){
        candidates = candidates[abs(get(n) - p1_dt[[n]][[1]]) <= range_cols[[n]]]
      }
    }
    if(!is.null(anchor_col_name)){
      candidates[,(anchor_col_name):= as.IDate(p1_dt[[anchor_col_name]][1])]
      if(!is.null(window_range)){
        candidates = subset_by_date_window(candidates, anchor_col_name, -1*window_range[[1]], window_range[[2]])
      } 
      if(!is.null(min_days_in_baseline)){
        candidates = subset_by_min_days_in_range(candidates, baseline_range[[1]], baseline_range[[2]], min_days_in_baseline, anchor_col_name)
      }
      if(!is.null(fixed_baselines)){
        for (f_baseline in fixed_baselines){
          p1_dt_subset = p1_dt[(as.numeric(date - get(anchor_col_name)) > baseline_range[[1]])]
          p1_dt_subset = p1_dt_subset[rev(order(date))]
          
          p1_dt[,(f_baseline):=p1_dt_subset[1, get(f_baseline)]]
          
          candidates_subset = candidates[(as.numeric(date - get(anchor_col_name)) > baseline_range[[1]])]
          candidates_subset = candidates_subset[rev(order(date))]
          
          candidates[,(f_baseline):=candidates_subset[1, get(f_baseline)]]
        }
        f_baseline_match = paste(baselines, collapse = " + ")
      }else{
        f_baseline_match = NULL
      }
      if(!is.null(baselines) & !is.null(baseline_range)){
        for (baseline in baselines){
          p1_dt = merge(p1_dt, p1_dt[(as.numeric(date - get(anchor_col_name)) > baseline_range[[1]]) &
                                       (as.numeric(date - get(anchor_col_name)) < baseline_range[[2]]), 
                                     .(V1 = mean(get(baseline))), .(person_id)],by="person_id")    
          p1_dt[,(baseline):=NULL]
          setnames(p1_dt, 'V1', paste0(baseline))
          candidates = merge(candidates, candidates[(as.numeric(date - get(anchor_col_name)) > baseline_range[[1]]) &
                                                      (as.numeric(date - get(anchor_col_name)) < baseline_range[[2]]), 
                                                    .(V1 = mean(get(baseline))), .(person_id)],by="person_id")
          
          candidates[,(baseline):=NULL]
          setnames(candidates, 'V1', paste0(baseline))
        }
        baseline_match = paste(baselines, collapse = " + ")
      }else{
        baseline_match = NULL
      }
    }else{
      baseline_match = NULL
    }
    cohort_to_match = rbind(p1_dt, candidates)
    cohort_to_match$condition = cohort_to_match$person_id == pid
    cohort_to_match = cohort_to_match[!duplicated(person_id)]
    if((dim(cohort_to_match)[[1]]-1)<match_rate){
      print(str_glue("The participant {pid} has only {dim(cohort_to_match)[[1]]-1}"))
    }
    f = NULL
    
    if(!is.null(flex_cols)){
      flex_match = paste(flex_cols, collapse = " + ")
      f = "condition ~ {flex_match}"
    }
    if(!is.null(baseline_match)){
      if (!is.null(f)){
        f = paste0(f," + {baseline_match}")
      }else{
        f = "condition ~ {baseline_match}"
      }
    }
    if(!is.null(f_baseline_match)){
      flex_match = paste(flex_cols, collapse = " + ")
      if (!is.null(f)){
        f = paste0(f," + {f_baseline_match}")
      }else{
        f = "condition ~ {baseline_match}"
      }
    }
    if(!is.null(f)){
      match_out <- matchit(as.formula(str_glue(f)),data = cohort_to_match,
                           ratio = match_rate,
                           replace = F,
                           distance = "mahalanobis")
      matched_dat <- match.data(match_out, data = cohort_to_match)
      matched_control = matched_dat[!(condition)]
      matched_control = subset(matched_control, select=unique(c("person_id",baselines,fixed_baselines, flex_cols, strict_cols, names(range_cols))))
      
      matched_cases = matched_dat[(condition)]
      matched_cases = subset(matched_cases, select=unique(c("person_id",baselines, fixed_baselines,flex_cols, names(range_cols))))
    }else{
      matched_control = sample(cohort_to_match[!(condition)], match_rate)
      matched_control = subset(matched_control, select = c(strict_cols, names(range_cols)))
      matched_cases = subset(p1_dt, select=unique(c("person_id", names(range_cols))))
      
      matched_cases = cohort_to_match[(condition)]
      matched_cases = subset(matched_cases, select=unique(c("person_id",baselines, flex_cols, names(range_cols))))
    }
    colnames(matched_cases) = paste0("case_", colnames(matched_cases))
    
    final_match = cbind(matched_control, matched_cases)
    for (relax_matched_col in unique(c(flex_cols, baselines,fixed_baselines, names(range_cols)))){
      final_match[[paste0(relax_matched_col, "_diff")]] = final_match[[relax_matched_col]] - final_match[[paste0("case_", relax_matched_col)]]
    }
    match_list[[as.character(pid)]] = final_match
    controls_dat = controls_dat[!(person_id %in% final_match$person_id)]
  }
  return(rbindlist(match_list))
}