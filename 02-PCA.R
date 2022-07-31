#### load ####
rm(list = ls())
load("split.RData")
source("libraries.R")

#### PCA on train sample ####
pca_train <- prcomp(select(data_train, current.ratio:DPO),
                    center=T, scale=T)
summary(pca_train)


#### Screeplot ####
fviz_screeplot(pca_train, ncp=15)

# Results for Variables
pca_train.var <- get_pca_var(pca_train)
round(pca_train.var$coord[,1:7],2)
round(pca_train.var$contrib[,1:7],2)
round(pca_train.var$cos2[,1:7],2)
round(pca_train.var$cor[,1:7],2)

# pca_trainults for individuals
pca_train.ind <- get_pca_ind(pca_train)
round(pca_train.ind$coord[,1:7],2)
round(pca_train.ind$contrib[,1:7],2)
round(pca_train.ind$cos2[,1:7],2)


#### Plot Correlation Matrix between Variables and PC's ####
data.frame(pca_train.var$cor[,1:7]) %>%
  as_tibble(rownames = "Variable") %>%
  pivot_longer(-Variable, "Dimension",values_to = "Cor") %>%
  mutate(Level = case_when(abs(Cor)<0.5 ~ "L",
                           abs(Cor)>=0.5 & abs(Cor)<0.7 ~ "M",
                           abs(Cor)>=0.7 & abs(Cor)<0.8 ~ "H",
                           TRUE ~ "VH"))  %>%
  mutate_if(is.character,as.factor) %>%
  mutate(Level = fct_relevel(Level, c("L", "M", "H", "VH")),
         Pec_Cor = paste(round(Cor*100,0),"%",sep = "")) %>%
  ggplot(aes(Dimension, fct_rev(Variable),
             fill=Level, label=round(Cor,2))) +
  geom_tile() +
  labs(x = NULL, y = NULL, fill = NULL, 
       title="Pearson's correlation coefficients",
       subtitle="Values between -50% and 50% not showed") +
  scale_fill_manual(values = c("white", "yellow", "orange", "red")) +
  geom_text(aes(label = ifelse(Level == 'L', NA, Pec_Cor))) +
  theme_classic() +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  theme(text=element_text(family="Roboto"),
        legend.position = "none")




#### Variables plot ####
fviz_pca_var(pca_train, col.var = "cos2", repel = T,
             select.var = list(cos2 = 0.25),
             gradient.cols = c("orange", "red")) + theme_bw()


#### Individuals plot ####
fviz_pca_ind(pca_train, geom = "point", repel = TRUE, alpha.ind = 0.4,
             col.ind = fct_rev(Ytrain), palette = c("green", "red"),
             legend.title = "Groups") + theme_bw()

#### Biplot ####
fviz_pca_biplot(pca_train, geom.ind = "point", habillage = Ytrain,
                palette = c("green", "red"), repel = TRUE) + theme_bw()


save.image("PCA.RData")
