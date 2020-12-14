## Calculating metrics
# requires tidyverse and segmented packages

one_node_metrics <- function(day, bud_length){
  if(length(day) != length(bud_length)){
    stop("day and bud_length have to be of same length")
  }
  if(!is.numeric(day) | !is.numeric(bud_length)){
    stop("day and bud_length have to be numeric")
  }
  
  x <- tibble(day = day, bud_length = bud_length)
  metrics <- x %>%
    mutate(d1_measured = bud_length - lag(bud_length),
           d2_measured = d1_measured - lag(d1_measured),
           # `length_start` is the starting length
           length_start = bud_length[which.min(day)],
           # `length_final` is the bud length on the final day
           length_final = bud_length[which.max(day)],
           length_experiment = max(day),
           # `AGR` is the Average Growth Rate in mm/day, calculated as `length_final` - `length_start` / `length_experiment`
           AGR = (length_final - length_start) / length_experiment) %>%
    summarise(
      length_start = bud_length[which.min(day)],
      length_final = bud_length[which.max(day)],
      AGR = max(AGR),
      # `MGR` is the Maximum Growth Rate in mm/day, calculated by taking the difference between `bud_length` and the `bud_length` (`d1_measured`) of the successive day and then taking the maximum value that occurs within each `sample_id`.
      MGR = max(d1_measured, na.rm = TRUE),
      # `MAcc` is the Maximum Acceleration within each `sample_id`, taken as the difference between successive `d1_measured` and taking the maximum value within each sample.
      MAcc = max(d2_measured, na.rm = TRUE),
      # MGR_day is the day where the MGR occured
      MGR_day = day[which.max(d1_measured)],
      # MGR_length is the bud length on MGR_day
      MGR_length = bud_length[which.max(d1_measured)],
      # MAcc_day is the day where the MAcc occurred
      MAcc_day = day[which.max(d2_measured)],
      # MAcc_length is the bud length on MAcc_day
      MAcc_length = bud_length[which.max(d2_measured)])
  
  ## segmented metrics
  # Check if there's actually some bud growth. If the bud_length on the final day
  # is the same as on the start day, then do not attempt to calculate the segmented
  # variable. This is because a difference in length is required for breakpoint estimation.
  
  # Additional threshold: only calculate a breakpoint if the length on the final day is larger than 5 mm
  
  
  if(x$bud_length[which.min(x$day)] == x$bud_length[which.max(x$day)] || x$bud_length[which.max(x$day)] < 5){
    metrics_segmented <- tibble(pscore = NA,
                                intercept = NA,
                                slope1 = NA,
                                slope2 = NA,
                                breakpoint_day = NA,
                                breakpoint_length = NA,
                                breakpoint_length_avg = NA,
                                breakpoint_length_smoothed = NA)
  } else {
    lm.active <- lm(bud_length ~ day, data = x)
    segfit <- segmented(lm.active, seg.Z = ~ day)
    
    # intercept is the intercept of the first linear regression
    intercept <- segfit$coefficients[1]
    # slope1 is the slope of the first linear regression
    slope1 <- segfit$coefficients[2]
    # slope2 is the slope of the second linear regression
    slope2 <- segfit$coefficients[3]
    
    # `pscore` is a test to check whether the breakpoint should exist
    # if > 0.05 then having no breakpoint is just as good
    pscore <- pscore.test(segfit)
    pscore <- pscore$p.value
    # breakpoint is the breakpoint estimated by `segmented` and
    # is the intersection between the two linear regressions.
    # only breakpoints where pscore > 0.05 are returned.
    if(is.null(segfit$psi) || pscore > 0.05){
      breakpoint_day <- NA
      breakpoint_length <- NA
    } else {
      breakpoint_day <- segfit$psi[2]
      breakpoint_length <- predict(segfit, newdata = tibble(day = breakpoint_day))
    }
    
    # Check if a breakpoint was calculated and the breakpoint exists.
    # If so, calculate the average bud length at the breakpoint.
    # Defined as the average measured bud length before and after breakpoint.
    # Also fit a smoothed curve, using loes-fitted data
    # and determine the bud length from these smoothed data.
    if(is.null(segfit$psi) || pscore > 0.05){
      breakpoint_day <- NA
      breakpoint_length_avg <- NA
      breakpoint_length_smoothed <- NA
    } else {
      breakpoint_day <- segfit$psi[2]
      breakpoint_length_avg <- x %>%
        summarise(breakpoint_length_avg = (bud_length[which(day == round(breakpoint_day) - 1)]
                                           + bud_length[which(day == round(breakpoint_day))]) / 2) %>%
        pull()
      lm.loess <- loess(bud_length ~ day, data = x)
      breakpoint_length_smoothed <- predict(lm.loess, newdata = tibble(day = breakpoint_day))
    }
    
    metrics_segmented <- tibble(pscore,
                                intercept,
                                slope1,
                                slope2,
                                breakpoint_day,
                                breakpoint_length,
                                breakpoint_length_avg,
                                breakpoint_length_smoothed)
  }
  
  # make a summary table
  output <- bind_cols(metrics, metrics_segmented)
  
  return(output)
}