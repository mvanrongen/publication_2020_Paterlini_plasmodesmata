## Calculating growth metrics

#
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
           r1_measured = bud_length/lag(bud_length))

  # make a summary table
  output <- bind_cols(metrics)

  return(output)
}