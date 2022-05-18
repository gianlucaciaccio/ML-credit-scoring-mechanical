rm(list = ls())
load("split.RData")
rm(list=ls(pattern="train"))
rm(list=ls(pattern="test"))
load("GLM-Mods-BEST.RData")
source("libraries.R")
library(kableExtra)
library(paletteer)
library(santoku)
conflicted::conflict_prefer("chop", "santoku")

#### Calculate 5 class of rating using probabilities of Default ####
strat_lev <- do.call(paste, c(expand_grid(c('no', 'ne', 'ce', 'su', 'is'),
                                          c('Q1', 'Q2', 'Q3', 'Q4')),
                              sep = '-'))

rating_labels <- c("AAA","BBB","CCC","DDD","EEE")
descr_labels <- c("Solid","Good","Speculative","Risky","Default")

data_rating <- data_clean %>%
  mutate(prediction = as.factor(predict(glm_mods_BEST$model,newdata = data_clean)),
         probability = predict(glm_mods_BEST$model,newdata = data_clean, type = "prob")[,1],
         rating = factor(ntile(probability, 5),labels = rating_labels),
         stratification = factor(stratification, levels = strat_lev),
         description = factor(rating, labels = descr_labels)) %>%
  select(id_firm, default, status, province, region, nuts, 
         stratification,  prediction, probability, rating,
         description)

probs_cut <- data_rating %>%
  group_by(rating) %>%
  summarize(Max=max(probability)) %>%
  slice(-5) %>% 
  pull()

score_labels <- levels(chop_quantiles(data_rating$probability, probs_cut))

data_rating$score <- factor(data_rating$rating, labels = score_labels)

confusionMatrix(data_rating$prediction,data_rating$status)


### Contingency tables ####
rating_pallette <- c("green", "limegreen", "gold", "darkorange", "orangered")

rating_tbl <- data_rating %>% 
  select(status, Rating = rating, Description = description,
         Score = score) %>%
  count(Rating, Description, Score, status) %>%
  ungroup() %>%
  pivot_wider(names_from = "status", values_from = "n",
              values_fill = 0)


rating_kbl <- rating_tbl %>%
  mutate(Rating = cell_spec(Rating, color = "white",
                            bold = T, background = rating_pallette),
         Description = cell_spec(Description, color = rating_pallette, bold = T),
         Score = cell_spec(Score, bold = T),
         Default = ifelse(rating_tbl$Rating %in% c("AAA","CCC") & Default > 0,
                          cell_spec(Default, color = "red", bold = T),
                          cell_spec(Default, color = "black")),
         InBonis = ifelse(rating_tbl$Rating %in% c("DDD","EEE") & InBonis > 0,
                          cell_spec(InBonis, color = "red", bold = T),
                          cell_spec(InBonis, color = "black")))

kbl(rating_kbl, escape = F, align = "c") %>%
  kable_styling(c("striped", "hover"))



geom_labs <- data.frame(x = data_rating %>%
                          group_by(rating) %>%
                          summarize(Mean=mean(probability)) %>%
                          pull(),
                        label = descr_labels)


ggplot(data_rating) +
  geom_density(aes(x=probability, color=status, fill=status),
               alpha=0.2)+
  scale_color_manual(values = c("orangered","green")) +
  scale_fill_manual(values = c("orangered","green")) +
  geom_vline(xintercept = probs_cut, 
             linetype = "dashed") +
  geom_text(data = geom_labs,  y = 1.5, size = 3,
            aes(x = x, label=label)) +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "gray98")) +
  labs(title = "Probability of default and rating classes")


ggplot(data_rating, aes(y=probability, x=rating, 
                        color=status, fill=status)) +
  geom_boxplot(alpha=0.2) +
  scale_color_manual(values = c("orangered","green")) +
  scale_fill_manual(values = c("orangered","green")) +
  facet_wrap(~stratification) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "#FDF7E2",
                                        color = "black"))
  

save.image("rating.RData")
