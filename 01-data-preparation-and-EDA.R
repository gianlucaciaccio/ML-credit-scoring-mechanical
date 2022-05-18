#### Packages ####
source("libraries.R")

#### Load data ####
dataset <- read_xlsx("dataset-mechanical-companies.xlsx", sheet = "data", 
                     col_types = c("numeric", "numeric", "text", "text", "text",
                                   "text", "date", "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric", "numeric",
                                   "numeric"))

table(dataset$status)


### Calculate variables for analysis ####
dataset2 <- dataset %>%
  filter(sales != 0) %>%
  mutate(status = as.factor(status),
         default = ifelse(status == "Default", 1, 0),
         liquid_assets = current_assets - inventories,
         working_capital = current_assets - current_liabilities,
         others_liabilities = assets - total_debt - net_worth,
         liabilities = total_debt + others_liabilities,
         capital_employed = assets - current_liabilities,
         ebit = ebitda - amortization_depreciation,
         # Liquidity
         current.ratio = current_assets/current_liabilities,
         quick.ratio = liquid_assets/current_liabilities,
         wc.ratio = working_capital/assets,
         # Leverage
         debt.equity = total_debt/net_worth,
         findep = liabilities/assets,
         # Coverage 
         int.cov = ebit/financial_costs,
         asset.cov = (assets - current_liabilities)/liabilities,
         cash.debt = cash_flows/total_debt,
         # Profitability
         roe = net_income/net_worth,
         roce = ebit/capital_employed,
         roa = ebit/assets,
         profit.margin = net_income/sales,
         # Cost of Debt
         cost.debt = financial_costs/liabilities,
         # Efficiency (and DPO and DSO)
         asset.turnover = sales/assets)


#### Summary: there are some extreme values to remove ####
dataset2 %>%
  select(status, current.ratio:asset.turnover,DSO,DPO) %>%
  pivot_longer(cols = -status, names_to = "variable") %>%
  group_by(variable, status) %>%
  summarise_all(list(
    Min = min, Q1 = ~quantile(., 0.25),
    Median = median, Mean = mean,
    Q3 = ~quantile(., 0.75), Max = max,
    SD = sd)) %>%
  ungroup() %>%
  print(n = 32)



#### Create data frame with outliers ####
data_base <- dataset2 %>%
  select(id_firm,status,current.ratio:asset.turnover,DSO,DPO)

data_zscore <- data_base %>%
  mutate(across(current.ratio:DPO, ~(.-mean(.))/sd(.))) %>% 
  filter(if_all(current.ratio:DPO,  ~abs(.)<=3))

table(data_zscore$status)

data_in <- select(data_zscore,id_firm, status) 

#### Delete outliers and create stratification variable ####
data_clean <- dataset2 %>%
  inner_join(data_in,by = c("id_firm","status")) %>%
  select(id_firm:status, assets, default,current.ratio:asset.turnover,DSO,DPO)  %>%
  mutate(quartile = as.factor(ntile(assets,4)),
         quartile = fct_recode(quartile, "Q1" = "1",
                               "Q2" = "2", "Q3" = "3","Q4" = "4"),
         stratification = paste(nuts, quartile, sep = "-"))


#### Plot: bar plot stratification and status ####
data_clean %>%
  ggplot(aes(x = nuts, fill = quartile, color = quartile)) +
  geom_bar(position = position_dodge()) +
  scale_fill_brewer(direction = -1) +
  scale_color_brewer(direction = -1) +
  theme(strip.background = element_rect(fill = "#FDF7E2", color = "black"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())


#### Summary with cleaned data ####
data_clean %>% count(status) %>% mutate(p=n/sum(n))

data_clean %>%
  select(status, current.ratio:DPO) %>%
  pivot_longer(cols = -status, names_to = "variable") %>%
  group_by(variable, status) %>%
  summarise_all(list(
    Min = min, Q1 = ~quantile(., 0.25),
    Median = median, Mean = mean,
    Q3 = ~quantile(., 0.75), Max = max,
    SD = sd)) %>%
  ungroup() %>%
  print(n = 32)

# we decide to drop int.cov

#### Create indices for spliting using outcome and stratification variable ####
indeces <- data_clean %>%
  select(status, stratification) %>%
  group_by(status, stratification) %>%
  group_indices()

#### Split the data ####
set.seed(1)
splitIndex <- createDataPartition(as.factor(indeces), p=0.8, list = FALSE)

data_train <- data_clean[ splitIndex, !(colnames(data_clean) %in% c("int.cov"))]
data_test  <- data_clean[-splitIndex, !(colnames(data_clean) %in% c("int.cov"))]

#### See the proportions ####
table(data_train$status)
round(prop.table(table(data_train$status)),2)
table(data_test$status)
round(prop.table(table(data_test$status)),2)

table(data_train$stratification, data_train$status)
round(apply(table(data_train$stratification, data_train$status), 1, prop.table),2)
table(data_test$stratification, data_test$status)
round(apply(table(data_test$stratification, data_test$status), 1, prop.table),2)



#### Calculate correlations on train data #####
data_cor <- data_train %>%
  select(default:DPO) %>%
  cor() %>% as_tibble(rownames = "Var1") %>%
  pivot_longer(-Var1, "Var2",values_to = "Cor") %>%
  mutate(Level = case_when(abs(Cor)<=0.5 & Var1!="default" & Var2!="default" ~ "Low",
                           abs(Cor)>0.5 & abs(Cor)<=0.7 |
                             abs(Cor)<0.1 & Var1=="default" | 
                             abs(Cor)<0.1 & Var2=="default" ~ "Medium",
                           abs(Cor)>0.7 & abs(Cor)<=0.85 |
                             abs(Cor)>=0.1 & abs(Cor)<=0.2 & Var1=="default" |
                             abs(Cor)>=0.1 & abs(Cor)<=0.2 & Var2=="default" ~ "High",
                           TRUE ~ "VH"),
         Level = fct_relevel(Level, c("Low", "Medium", "High", "VH"))) %>%
  mutate_if(is.character,as.factor) %>%
  mutate(across(c(Var1,Var2), ~ fct_relevel(.,"default", after = 0L)))

#### Plot: correlation matrix ####
data_cor %>%
  ggplot(aes(Var1, fct_rev(Var2), fill=Level, label=round(Cor,2))) +
  geom_tile() +
  labs(x = NULL, y = NULL, fill = "Pearson's\nCorrelation", 
       title="Pearson's correlation coefficients",
       subtitle="Values between -0.50 and 0.50 (except for 'default' variable) not showed") +
  scale_fill_manual(values = c("white", "yellow", "orange", "red")) +
  geom_text(aes(label = ifelse(Level == 'Low', NA, round(Cor,2)))) +
  theme_classic() +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  theme(text=element_text(family="Roboto"), legend.position = "none",
        axis.text.x = element_text(angle = 30, hjust = 1))



#### Plot: histogram ####
data_train %>%
  select(current.ratio:DPO) %>%
  pivot_longer(cols = everything(), names_to = "variable") %>%
  ggplot(aes(x=value)) +
  geom_histogram(color = "white", fill = "cornflowerblue", bins = 120)+
  facet_wrap(~ variable, scales = "free") +
  theme_bw() +
  theme(strip.background = element_rect(fill = "#FDF7E2", color = "black"),
        panel.grid.minor = element_blank())


#### Plot: density by status ####
data_train %>%
  select(status, current.ratio:DPO) %>%
  pivot_longer(cols = -status, names_to = "variable") %>%
  ggplot(aes(x = value, color = status, fill = status)) +
  geom_density(alpha = 0.2)+
  scale_color_manual(values = c("orangered","green")) +
  scale_fill_manual(values = c("orangered","green")) +
  facet_wrap(~ variable, scales = "free") +
  theme_bw() +
  theme(strip.background = element_rect(fill = "#FDF7E2", color = "black"),
        panel.grid.minor = element_blank())

#### Plot: error bar by status ####
data_train %>%
  select(status, current.ratio:DPO) %>%
  pivot_longer(cols = -status, names_to = "variable") %>%
  group_by(variable, status) %>%
  summarize(sd = sd(value), value = mean(value)) %>%
  ggplot(aes(x = status, y = value, color = status)) +
  geom_point()+
  geom_errorbar(aes(x = status, ymin = value-sd, ymax = value+sd)) +
  scale_color_manual(values = c("orangered","green")) +
  facet_wrap(~ variable, scales = "free") +
  theme_bw() +
  theme(strip.background = element_rect(fill = "#FDF7E2", color = "black"),
        panel.grid.minor = element_blank()) +
  labs(x = NULL, y = NULL)


#### Plot: fitted logistic curves ####
data_train %>%
  select(default, current.ratio:DPO) %>%
  pivot_longer(cols = -default, names_to = "variable") %>%
  ggplot(aes(x=value,y=default)) +
  stat_smooth(method = "glm", method.args = list(family=binomial))+
  facet_wrap(~variable, scales = "free") +
  theme_bw() +
  theme(strip.background = element_rect(fill = "#FDF7E2", color = "black"),
        panel.grid.minor = element_blank())




#### save data ####
save.image("data-preparation-and-EDA.RData")
save(list = c("data_clean", "data_train", "data_test", 
              "indeces", "splitIndex"), file = "split.RData")
