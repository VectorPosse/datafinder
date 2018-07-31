get_counts <- function(list_vars_data) {
    if (NROW(list_vars_data) != 0) {
        output <- list_vars_data %>%
            group_by_("Dataframe", "Class") %>%
            summarise_(Count = "n()") %>%
            spread_("Class", "Count", fill = 0) %>%
            ungroup()
        # Extract only the class count columns in order to sum them.
        var_classes <- output %>%
            select_("-Dataframe")
        total_vars =  rowSums(var_classes)
        # Attach total_vars to the original output
        output <- data.frame(cbind(output, total_vars))
    } else {
        output <- data.frame()
    }
}