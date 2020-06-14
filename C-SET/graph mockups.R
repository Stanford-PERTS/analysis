source("~/Sites/gymnast/R/util.R")

projects <- c("Gateway math", "O-chem")
constructs <- c("Instructor Growth Mindset", "Class Belonging", "Everything", "Purpose for Learning")
subsets <- c("Struct. Disadv.")#, "Struct. Adv.")

disagg_df <- expand_grid(
    project = projects,
    #construct = constructs,
    subset_value = subsets
) 

# the mean in all_students_df has to be between the adv and disadv groups
all_students_df <- expand_grid(
    project = projects,
    #construct = constructs,
    subset_value = "All Students"
) %>%
    mutate(
        baseline = sample(seq(.15, 1, .01), nrow(.), replace = T),
        change = sample(seq(-.4, .2, .01), nrow(.), replace = T),
        baseline_CI = sample(seq(0, .4, .01), nrow(.), replace = T),
        change_CI = sample(seq(0, .2, .01), nrow(.), replace = T)
    ) %>%
    # put interesting stuff in there
    mutate(
        # have a little more improvement in the improvement projects
        change = round(
            ifelse(
                grepl("Improvement", project),
                change + sample(seq(-.05, .2, .01), sum(grepl("Improvement", project)), replace = T),
                change
                )
            , 2
            )
    ) %>%
    # compute an end value and correct for end values above 1 and below zero
    mutate(
        change = ifelse(baseline + change < 1, change, 1 - baseline),
        change = ifelse(baseline + change > 0, change, baseline),
        end = baseline + change
    )


minus <- function(x) round(x + rnorm(n = length(x), mean = -.1, sd = .05), 2)
plus <- function(x) round(x + rnorm(n = length(x), mean = .1, sd = .05), 2)
disagg_df_w_values <- merge(
        disagg_df,
        all_students_df,
        by = c("project"),# "construct"),
        suffixes = c("_disagg", "_all")
    ) %>%
    mutate(
        baseline_disagg = ifelse(
            subset_value_disagg %in% "Struct. Disadv.",
            minus(baseline),
            plus(baseline)
        ),
        change_disagg = ifelse(
            subset_value_disagg %in% "Struct. Disadv.",
            plus(change),
            minus(change)
        ),
        baseline_CI = plus(baseline_CI),
        change_CI = plus(change_CI)
    ) %>%
    select(-subset_value_all, -baseline, -change) %>%
    rename(
        baseline = baseline_disagg,
        change = change_disagg,
        subset_value = subset_value_disagg
    )  %>%
    # compute an end value and correct for end values above 1 and below zero
    mutate(
        change = ifelse(baseline + change < 1, change, 1 - baseline),
        change = ifelse(baseline + change > 0, change, baseline),
        end = baseline + change
    )
    
my_df <- util.rbind_union(list(all_students_df, disagg_df_w_values)) %>%
    mutate(
        graph_label = project %+% " (" %+% subset_value %+% ")",
        graph_label = gsub(" \\(All Students\\)", "", graph_label),
        graph_label = factor(graph_label, levels = sort(graph_label))
    ) 

comparison_p <- ggplot(
    my_df,
    aes(x = graph_label, y = baseline)
) +
    geom_point() +
    geom_segment(aes(x = graph_label, xend = graph_label, y = baseline, yend = end), arrow = arrow()) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_x_discrete(name = "", limits = sort(unique(my_df$graph_label))) +
    coord_cartesian(ylim = c(0, 1)) +
    xlab("") + ylab("Percent of students having overall positive experiences") +
    coord_flip()
comparison_p

ggsave("~/Sites/analysis/C-SET/comparison_mockup.png", comparison_p, height = 3, width = 7, units = "in")


months <- c("Feb", "Mar", "Apr")
change_df <- expand_grid(
    #project = projects,
    #subset_value = subsets,
    construct = c(setdiff(constructs, "Everything")),
    month = months
) %>%
    mutate(
        month = factor(month, levels = c("Feb", "Mar", "Apr"), labels = c("Feb", "Mar", "Apr")),
        pct_good_base = round(rnorm(n = nrow(.), mean =.4, sd = .1), 2),
        pct_good = ifelse(
            month %in% "Mar",
            round(rnorm(n = nrow(.), mean =.5, sd = .1), 2),
            ifelse(
                month %in% "Apr",
                round(rnorm(n = nrow(.), mean =.5, sd = .1), 2),
                pct_good_base
            )
        )
    )

change_p <- ggplot(change_df, aes(x = as.numeric(month), y = pct_good, linetype = construct)) +
    geom_line() +
    geom_point() +
    scale_x_continuous("", breaks = c(1, 2, 3), labels = c("Feb", "Mar", "Apr")) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    ylab("Percent of students\nhaving positive experiences") +
    #facet_grid(subset_value ~ .) +
    theme(legend.position = "bottom")
change_p  
ggsave("~/Sites/analysis/C-SET/change_mockup.png", change_p, height = 3, width = 5, units = "in")
