devtools::source_url("https://raw.githubusercontent.com/MatthieuStigler/Misconometrics/master/Gelbach_decompo/dec_covar.R")
t <- grants_def %>% 
  mutate(inst_cap = 
           fct_recode(inst_cap, 
                      WorksWith = "Works with", 
                      Missing = "(Missing)"))
tt <- t %>% 
  model_matrix(score ~ 
                 domain + sex + region + # demographics
                 rints + scopus + hirsh + fake + # quality indicators
                 win_2014 + degree + # institutional memory aka Mathew effect
                 inst_cap + org_prestige) %>%
  mutate(score = t$score)

model_full_1 <- lm(score ~ ., data = tt[-1])
summary(model_full_1)
dec <- dec_covar(object = model_full_1, 
                 var_main = "sexFemale")
dec_long <- 
  dec_covar(object = model_full_1, 
            var_main = "sexFemale", 
            format = "long", add_coefs = TRUE, conf.int = TRUE)
plot_dec(dec_long) +
  ggtitle("Effect of each covariate on the main variable's coef")

beta_orig <- 
  dec_long %>% 
  select(variable, starts_with("beta_var")) %>% 
  distinct() %>% 
  gather(Model, value, starts_with("beta_var")) %>% 
  mutate(Model = case_when(Model == "beta_var_base" ~ "model base",
                           Model == "beta_var_full" ~ "model full"))

n_var <- length(unique(dec_long$variable))
pl <- 
  dec_long %>%
  mutate(covariate = str_replace(covariate, "Астана", "Astana")) %>%
  mutate(covariate = str_replace(covariate, "Шымкент", "Shymkent")) %>%
  mutate(delta_center = beta_var_base - delta) %>%
  ggplot(aes(x = delta_center, y = fct_reorder(covariate, delta_center))) +
  geom_point() +
  geom_segment(aes(x=beta_var_base, xend = delta_center, yend = covariate)) +
  geom_vline(aes(xintercept = value, colour = Model, linetype = Model), data = beta_orig) +
  scale_colour_manual(values = c("model base"= "black", "model full" = "blue"))+
  theme(legend.position = "bottom") +
  xlab("Delta ( = gamma * beta)") + 
  scale_y_discrete(labels = 
                     c("h-index", 
                       "Previous success", 
                       "Degree: Doctor of Science",
                       "Scopus-delisted",
                       "Org Prestige: Research", 
                       "Domain: Natural RM",
                       "Works with Council Member", 
                       "RINTS", 
                       "Domain: Life Science", 
                       "Domain: Energy", 
                       "Domain: Science", 
                       "Region: Shymkent", 
                       "Affiliation: Missing", 
                       "Domain: Security", 
                       "Org Prestige: International", 
                       "Domain: Agriculture", 
                       "Org Prestige: National", 
                       "Region: Other", 
                       "Region: Astana", 
                       "Scopus", 
                       "Research Council Member",
                       "Degree: PhD") %>% rev())


if(n_var > 1) pl <- pl + facet_grid(. ~ variable, scales = "free")
pl_fin <-
  pl + 
  scale_x_continuous(limits = c(-0.75, 0)) +
  labs(y = NULL)
ggsave(filename = "out/pic/fig2_gelbach_decomp.pdf", 
      plot = pl_fin)
ggsave(filename = "out/pic/fig2_gelbach_decomp.tiff", 
       device = "tiff", 
       dpi = 300,
       plot = pl_fin)

plot_gamma_beta(dec_long, add_CI = TRUE) +
  ggtitle("Covariate impact: direct (beta) and indirect (gamma) impact")
plot_gam_bet_del(dec_long)

library(ggbeeswarm)
grants_def %>% 
  ggplot(aes(y = sex, x = log(hirsh + 1))) + 
  geom_jitter() + 
  geom_boxplot(alpha = 0.3)

grants_def %>% 
  group_by(sex) %>%
  summarize(mean = mean(hirsh))


grants_def %>% 
  group_by(sex) %>% 
  summarise(`0` = mean(hirsh == 0), 
            `1` = mean(hirsh == 1), 
            `2` = mean(hirsh == 2), 
            `3` = mean(hirsh == 3), 
            `4` = mean(hirsh == 4), 
            `5+` = mean(hirsh == 5)) %>% 
  pivot_longer(-sex, names_to = "hirsh", values_to = "prop") %>% 
  mutate(hidden = c(0:5, 0:5)) %>%
  ggplot(aes(x = hidden, y = prop, color = sex)) + 
  geom_line() +
  geom_point() + 
  scale_x_continuous(labels = c("0", "1", "2", "3", "4", "5+"))


grants_def %>% 
  group_by(sex) %>% 
  summarise(prev_win = mean(win_2014 == "Yes"))
