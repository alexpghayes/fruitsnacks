library(tidyverse)
library(lme4)
library(rstanarm)
library(emmeans)

ratings <- read_csv(
    here::here("data/ratings.csv"),
    col_types = "iiiiiiiiiiiiic"
)

tasters <- c(
  "kath",
  "kate",
  "gaby",
  "jon",
  "emily",
  "will",
  "rage",
  "mickey",
  "chris",
  "christian",
  "alex",
  "lauren"
)

ratings

ratings_long <- ratings %>% 
  mutate_at(vars(fruit_snack_type), fct_inorder) %>% 
  pivot_longer(
    cols = one_of(tasters),
    names_to = "rater",
    values_to = "rating"
  )

ratings_long %>% 
  ggplot(aes(rater, fct_rev(fruit_snack_type), fill = as.factor(rating))) +
  geom_tile() +
  scale_fill_brewer() +
  theme_classic(20) +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    fill = "rating"
  )


ols <- lm(rating ~ fruit_snack_type + rater, data = ratings_long)
summary(ols)

plot(emmeans(ols, ~ fruit_snack_type))

fit <- lme4::lmer(rating ~ fruit_snack_type + (1|rater), data = ratings_long)
summary(fit)

fit_emm <- emmeans(fit, ~ fruit_snack_type, lmer.df = "satterthwaite")

broom::tidy(fit_emm, conf.int = TRUE) %>% 
  arrange(estimate) %>% 
  mutate_at(vars(fruit_snack_type), fct_inorder) %>% 
  ggplot(aes(x = fruit_snack_type, y = estimate, ymin = conf.low, ymax = conf.high, color = estimate)) +
  geom_pointrange() +
  scale_color_gradient2() +
  coord_flip() +
  theme_classic(18) +
  theme(
    axis.title.y = element_blank()
  ) +
  labs(
    title = "Fruit snack ratings",
    y = "Average rating"
  )

fit_emm

fit_bayes <- rstanarm::stan_lmer(
  rating ~ fruit_snack_type + (1|rater),
  data = ratings_long
)

fit_bayes

plot(emmeans(fit_bayes, ~ fruit_snack_type))

plot(emmeans(fit_bayes, pairwise ~ fruit_snack_type))

ratings_long %>% 
  ggplot(aes(x = consumption_order, y = rating)) +
  geom_jitter() +
  geom_smooth()
