# this script contains some analyses which Sarah needed for a white paper
df_paths <- util.find_crypt_paths(
  list(df= "specific_school_stats_df_2018-10-01.csv"),
  max_depth = 3
)

library(dplyr)
df_all <- read.csv(df_paths$df, stringsAsFactors = FALSE)

df <- df_all %>% filter(n_numer >= 100)

average_n <- df$n %>% as.numeric() %>% mean(.,na.rm = TRUE)
sd_n <- df$n %>% as.numeric() %>% sd(.,na.rm = TRUE)
# 201
pwr::pwr.t.test(n = average_n, sig.level=.05, power = .80, type = 'two.sample')
#pwr::pwr.f2.test(u = 1, v = average_n-2, sig.level=.05, power = .80)

sum(df$b_effect > 0 & df$p_effect < .05)/nrow(df)
z <-  df[(df$b_effect > 0 & df$p_effect < .05),]
z$b_effect %>% min
z$b_effect %>% max


# compute effect sizes from t
# http://www.bwgriffin.com/gsu/courses/edur9131/content/Effect_Sizes_pdf5.pdf

# d = 2t/sqrt(n-2)
df$d = as.numeric(df$t_effect)/sqrt(as.numeric(df$n)-2)
plot(df$d, df$b_effect)

z <-  df[(df$b_effect > 0 & df$p_effect < .05),]
z$d %>% min
z$d %>% max



sum(df$effect_different_from_overall_p >= .05 & df$p_effect >= .05)/nrow(df)
sum(df$effect_different_from_overall_p < .05)/nrow(df)
sum(df$effect_different_from_overall_p < .05 & df$effect_different_from_overall_b < .00)/nrow(df)
sum(df$effect_different_from_overall_p < .05 & df$effect_different_from_overall_b >= .00)/nrow(df)

