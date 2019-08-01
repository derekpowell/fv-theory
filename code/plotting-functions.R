
plot_mean_scores <- function(df, selected_scale){
  plt <- df %>%
    filter(Scale==selected_scale) %>%
    group_by(condition, phase) %>%
      summarize(
        mean_score = mean(score, na.rm = TRUE),
        ul = mean(score, na.rm = TRUE) + sd(score, na.rm = TRUE) / sqrt(n()),
        ll = mean(score, na.rm = TRUE) - sd(score, na.rm = TRUE) / sqrt(n()),
      ) %>%
      ggplot(aes(x = phase, y = mean_score, color = condition, ymin = ll, ymax = ul)) +
      geom_pointrange(position = position_dodge(width = .25)) +
      # scale_color_discrete() +
      labs(fill = "Condition", y = "Score", x = "Phase") +
      theme_bw()
  
  return(plt)
}

plot_change_scores <- function(df, selected_scale){
  plt <- df %>%
    filter(Scale==selected_scale) %>%
    spread(phase, response) %>%
    mutate(
      immediate = posttest-pretest,
      delay = posttest2-pretest
    ) %>%
    gather(phase, value, immediate, delay) %>%
    mutate(phase = ordered(phase, levels=c("immediate","delay"))) %>%
    group_by(condition, Scale, item, phase) %>%
    drop_na() %>%
    summarize(mean_change=mean(value), 
              ul = mean(value) + sd(value)/sqrt(n()),
              ll = mean(value) - sd(value)/sqrt(n())) %>%

    ggplot(aes(x=item, y=mean_change, color=condition, ymin=ll, ymax=ul)) +
    geom_pointrange(position=position_dodge(width=.25)) +
    geom_hline(yintercept = 0, linetype="dashed") +
    facet_grid(~phase, scales="free") +
    theme_bw() +
    labs(x="Item", y="Change Score", color="Condition")
  
  return(plt)
}



