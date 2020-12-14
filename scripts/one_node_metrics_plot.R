
one_node_metrics_plot <- function(tbl, tbl_summary, id){

  tbl_summary %>%
    filter(sample_id == id) %>%
    select(sample_id, genotype, treatment, MGR_day, MGR_length, MAcc_day, MAcc_length, breakpoint_day, breakpoint_length_smoothed) %>%
    pivot_longer(cols = c(-sample_id, -genotype, -treatment),
                 names_to = c("trait", "day"),
                 names_sep = "_",
                 values_to = "value") %>%
    pivot_wider(names_from = "day",
                values_from = "value") %>%
    # # In order to make the ggtitle work with variables, the ggplot needs to be wrapped in curly brackets
    # # and the data needs to be explicitly called with '.'
    {ggplot(.) +
        geom_line(data = tbl %>% filter(sample_id == id), aes(x = day, y = bud_length, group = sample_id, colour = "raw_data"), size = 1) +
        geom_smooth(data = tbl %>% filter(sample_id == id), aes(x = day, y = bud_length, colour = "smoothed"), se = FALSE, linetype = "dashed") +
        geom_point(aes(x = day, y = length, colour = trait), size = 3) +
        ggtitle(paste0("Growth curves for sample ", .$sample_id, " (", .$genotype, ", ", .$treatment, " treatment)"))}
}
