{
  library(readxl)
  library(stargazer)
  library(tidyr)
  library(dplyr)
  library(tidyverse)
  library(stringr)
  library(janitor)
  library("plm")
  library("did")
  library("panelView")
  library("interflex")
  library(MatchIt)
  library(fixest)
  library(rdd)
  library('RItools')
  library(tableone)
  library(cobalt)
  library(estimatr)
}


# wine style --------------------------------------------------------------

# 
# ------- Импорт данных winestyle --------
# 


# все данные по winestyle это Россия в наличии 
winestyle_dec_ratings <- read_xlsx('/Users/artempavlov/Desktop/Лучший диплом про вино 10:10/Данные🔢/Разметка/winestyle_dec.xlsx')
winestyle_dec <- read.csv('/Users/artempavlov/Desktop/Лучший диплом про вино 10:10/Данные🔢/WineStyle/ассортимент в наличии/WineStyle_stock_1-92-1-18_2024-12-10.csv')
winestyle_dec25 <- read.csv('/Users/artempavlov/Desktop/Лучший диплом про вино 10:10/Данные🔢/WineStyle/ассортимент в наличии/WineStyle_stock_1-93-1-16_2024-12-25.csv')
winestyle_jan <- read.csv('/Users/artempavlov/Desktop/Лучший диплом про вино 10:10/Данные🔢/WineStyle/ассортимент в наличии/WineStyle_stock_1-90-1-18_2025-01-18.csv')
winestyle_feb <- read.csv('/Users/artempavlov/Desktop/Лучший диплом про вино 10:10/Данные🔢/WineStyle/ассортимент в наличии/WineStyle_stock_1-91-1-18_2025-02-18.csv')
winestyle_mar <- read.csv('/Users/artempavlov/Desktop/Лучший диплом про вино 10:10/Данные🔢/WineStyle/ассортимент в наличии/WineStyle_stock_1-59-1-13_2025-03-22.csv')
winestyle_apr <- read.csv('/Users/artempavlov/Desktop/Лучший диплом про вино 10:10/Данные🔢/WineStyle/ассортимент в наличии/WineStyle_stock_1-59-1-13_2025-04-14.csv')

winestyle_out_of_stock_dec = c(
  'в191192', 'в118331', 'в142256', 'в176688', 'в83734', 'в173360', 'в100980', 'в189222', 'в144844',
  'в194365', 'в223300', 'в169506', 'в118313', 'в233670', 'в158536', 'в228032', 'в203174','в226076',
  'в172787','в234045','в214973','в230249', 'в165193', 'в227541', 'в226517', 'в189109', 'в232770',
  'в200597', 'в244118', 'в236501', 'в238409', 'в245714', 'в227399', 'в226006', 'в247485', 'в243063',
  'в188874', 'в235529', 'в235522', 'в235539', 'в241327', 'в238812', 'в246576', 'в237414', 'в239660',
  'в243148', 'в242245', 'в244637', 'в242270', 'в248173', 'в241904', 'в242473', 'в246406', 'в246979',
  'в165575', 'в228758', 'в58937', 'в138109', 'в87466', 'в182364', 'в114158', 'в164237',
  'в133187', 'в168308', 'в64741', 'в75905', 'в112492', 'в75149', 'в92645', 'в230638',
  'в74713', 'в249910', 'в238775', 'в237195', 'в246368', 'в244676', 'в237829', 'в237512',
  'в243025', 'в249405', 'в249355', 'в249357', 'в228753'
)
winestyle_out_of_stock_feb = c(
  'в236750', 'в195557', 'в220554', 'в166440', 'в165575', 'в139402', 'в228758',
  'в253524', 'в253523', 'в254244', 'в228753', 'в236752'
)

# нижний порог цены, до которого наблюдения исключаются из выборки
# 450 - самое дешевое вино в рейтинге АС
ws_lower_price_boundary = 450

prepare_winestyle_df <- function(df){
  df_mod <- df %>% distinct(product_code, .keep_all = TRUE) %>% 
    mutate(
      price_main = as.numeric(gsub("[^0-9.-]", "", price_main)),
      price_discount = as.numeric(gsub("[^0-9.-]", "", price_discount)),
      price = pmin(price_main, price_discount, na.rm = TRUE)
    ) %>% rename (
      type = 'Вино'
    ) %>% select (
      product_code,
      price
    )
  return(df_mod)
}

winestyle_dec_ratings_prepared <- winestyle_dec_ratings %>% 
  rename(
    region = 'Регион'
  ) %>% select(
    product_code,
    aswg_rating,
    aswg_year,
    aswg_comment, 
    region
  ) %>% mutate (
    # группировка регионов
    region = case_when(!(region %in% c('Крым','Краснодарский край', 'Дагестан')) ~ 'Другие регионы',
                     TRUE ~ region)
  )

winestyle_dec_ratings_prepared$in_aswg_dec_raw <- !is.na(winestyle_dec_ratings_prepared$aswg_rating) & (winestyle_dec_ratings_prepared$aswg_rating >= 87) 
winestyle_dec_ratings_prepared$in_aswg_dec <- !is.na(winestyle_dec_ratings_prepared$aswg_rating) & (winestyle_dec_ratings_prepared$aswg_rating >= 87) & (!(winestyle_dec_ratings_prepared$aswg_comment %in% c('аг', 'аг, до', 'дг', 'дг, до')) | is.na(winestyle_dec_ratings$aswg_comment))
winestyle_dec_ratings_prepared$in_aswg_feb <- !is.na(winestyle_dec_ratings_prepared$aswg_rating) & (!(winestyle_dec_ratings_prepared$aswg_comment %in% c('аг', 'аг, до', 'дг', 'дг, до')) | is.na(winestyle_dec_ratings$aswg_comment))
winestyle_dec_ratings_prepared$aswg_rating <- as.character(winestyle_dec_ratings_prepared$aswg_rating)

winestyle_dec_prepared <- winestyle_dec %>%  mutate(
  price_main = as.numeric(gsub("[^0-9.-]", "", price_main)),
  price_discount = as.numeric(gsub("[^0-9.-]", "", price_discount)),
  price = pmin(price_main, price_discount, na.rm = TRUE)
) %>% rename (
  type_still = 'Вино',
  type_sparkling = 'Игристое.вино.шампанское',
  producer = 'Производитель',
  grape_vars = 'Сорта.винограда',
  vol = 'Объем',
  price_dec=price
) %>% unite (
  type_sugar, type_still,type_sparkling, sep = ''
)  %>% separate (
  type_sugar, into = c('type', 'sugar', 'sugar_sparkling'), sep = ';'
) %>% separate(
  product_code, into = c('pcname', 'pcvalue'), sep = ': '
) %>% filter(
  !(pcvalue %in% winestyle_out_of_stock_dec)
) %>% filter(
  !(pcvalue %in% winestyle_out_of_stock_feb)
)%>% mutate (
  product_code = paste(pcname, pcvalue, sep = ': ')
) %>% mutate(
  price_segment = case_when(price_dec<1000 ~ '1',
                            price_dec<2500 ~ '2',
                            TRUE ~ '3')
) %>% mutate(
  vintage = sapply(str_extract_all(url,"20\\d{1,2}"), toString)
) %>% mutate(
  is_blend = str_count(grape_vars, ";") > 0
) %>% filter (
  # фильтр дешевых вин
  price_dec >= ws_lower_price_boundary,
  #vol %in% c('0.75 л','0.5 л','1.5 л', '375 мл'),
  vol == '0.75 л',
  type %in% c('Красное','Белое', 'Игристое-белое','Розовое','Игристое-розовое', 'Игристое-красное', 'Оранжевое')
) %>% filter(
  !is.na(sugar)
) %>% mutate(
  # группировка категорий
  type = case_when(type %in% c('Игристое-белое','Игристое-розовое', 'Игристое-красное') ~ 'Игристое',
                   type %in% c('Белое','Оранжевое') ~ 'Белое',
                   TRUE ~ type)
) %>% mutate(
  # группировка категорий
  vintage_group = case_when(vintage>=2021 ~ '2-4 года',
                            vintage>=2018 ~ '5-7 лет',
                            vintage>=2002 ~ 'более 7 лет',
                            TRUE ~ 'год не указан')
)

winestyle_dec_ratings_prepared %>% group_by(region) %>% summarise(n = n())

winestyle_dec25_prepared <- prepare_winestyle_df(winestyle_dec25) %>% rename(
  price_dec25=price
)
winestyle_jan_prepared <- prepare_winestyle_df(winestyle_jan) %>% rename(
  price_jan=price
)
winestyle_feb_prepared <- prepare_winestyle_df(winestyle_feb) %>% rename(
  price_feb=price
)
winestyle_mar_prepared <- prepare_winestyle_df(winestyle_mar) %>% rename(
  price_mar=price
)
winestyle_apr_prepared <- prepare_winestyle_df(winestyle_apr) %>% rename(
  price_apr=price
)


winestyle_dec_ratings_prepared %>% group_by(in_aswg_feb) %>% summarise(n = n())
winestyle_dec_prepared %>% group_by(type) %>% summarise(n = n())


winestyle_merged = winestyle_dec_prepared %>% 
  inner_join(winestyle_dec_ratings_prepared, by = 'product_code') %>%
  # inner_join(winestyle_dec25_prepared, by = 'product_code') %>%
  inner_join(winestyle_jan_prepared, by = 'product_code') %>%
  inner_join(winestyle_feb_prepared, by = 'product_code') %>%
  inner_join(winestyle_mar_prepared, by = 'product_code') %>%
  inner_join(winestyle_apr_prepared, by = 'product_code')

winestyle_merged %>% group_by(in_aswg_dec) %>% summarise(n = n())

winestyle_merged <- winestyle_merged %>% mutate(
  price_diff_dec_jan = price_jan - price_dec,
  price_diff_dec_jan_perc = (price_jan - price_dec)/price_dec,
  price_diff_dec_mar = price_mar - price_dec,
  price_diff_dec_mar_perc = price_diff_dec_mar/price_dec,
  price_diff_dec_apr = price_apr - price_dec,
  price_diff_dec_apr_perc = price_diff_dec_apr/price_dec,
  price_diff_feb_apr = price_apr - price_feb,
  price_diff_feb_apr_perc = price_diff_feb_apr/price_dec,
  
  price_match = price_dec
) 
# write.csv(winestyle_merged, file = 'winestyle_merged.csv')







# 
# ------- Модели winestyle --------
# 


# мэтчинг производится по цене до выхода рейтнга
ws_merged_long <- winestyle_merged %>% pivot_longer(
  c('price_dec','price_jan','price_feb','price_mar','price_apr'), names_to = 'month', values_to = 'price'
) %>% mutate (
  treatment_period_dec = if_else(in_aswg_dec, 2, 0),
  treatment_period_feb = if_else(in_aswg_feb, 4, 0),
  # long = if_else(month == 'price_dec', 0, 1),
  lprice = log(price),
  mon = as.numeric(factor(month, levels = c('price_dec', 'price_jan','price_feb','price_mar','price_apr'))),
  after_dec = month != 'price_dec',
  after_feb = !(month %in% c('price_dec', 'price_jan', 'price_feb'))
) %>% select (
  type,
  sugar,
  is_blend,
  price_segment,
  region,
  price, lprice,price_match, 
  X,
  treatment_period_dec, treatment_period_feb, mon, after_dec,
  in_aswg_dec,in_aswg_feb,
  vintage_group,
  month
)



# Simple DiD --------------------------------------------------------------

# тритмент декабрь
att_grouptime_simple_dec <- att_gt(yname = "lprice",
                                  tname = "mon",
                                  idname = "X",
                                  gname = "treatment_period_dec",
                                  alp = 0.05,
                                  bstrap = F,
                                  # biters = 1000,
                                  xformla = ~ type + sugar + is_blend + price_segment + region + vintage_group,
                                  data = ws_merged_long,
                                  est_method = 'reg'
)
summary(att_grouptime_simple_dec)
ggdid(att_grouptime_simple_dec, title = 'Оценка эффекта с контролем на наблюдаемые параметры', legend = FALSE)



# То же самое вручную

# ws_merged_long$mon
# ws_merged_long$in_aswg_dec
# i(ws_merged_long$mon, ws_merged_long$in_aswg_dec, ref = 1) 


# clfe <- feols(lprice ~ i(mon, in_aswg_dec, ref = 1) + type + sugar + is_blend + price_segment + region + vintage_group|
clfe <- feols(lprice ~ i(mon, in_aswg_dec, ref = 1)|
                 X + mon, data = ws_merged_long)

coefplot(clfe, main = 'Эффект выхода рейтинга Артура Саркисяна\n на цену вин в рейтинге', xlab = 'Меясяц наблюдения (янв=2)', value.lab = 'Размер эффекта, %')
summary(clfe)
write_excel_csv(clfe$coeftable, file = 'clfe_coef_table.csv') 
clfe$collin.var


# тут DID для отдельных периодов, годаздо более широкие ДИ
did_1 <- ws_merged_long %>% filter(month %in% c('price_dec', 'price_jan')) 
did_1_lm = lm(lprice ~ in_aswg_dec + after_dec + in_aswg_dec*after_dec + type + sugar + is_blend + price_segment + region + vintage_group, data = did_1)
did_2 <- ws_merged_long %>% filter(month %in% c('price_dec', 'price_feb')) 
did_2_lm = lm(lprice ~ in_aswg_dec + after_dec + in_aswg_dec*after_dec + type + sugar + is_blend + price_segment + region + vintage_group, data = did_2)
did_3 <- ws_merged_long %>% filter(month %in% c('price_dec', 'price_mar')) 
did_3_lm = lm(lprice ~ in_aswg_dec + after_dec + in_aswg_dec*after_dec + type + sugar + is_blend + price_segment + region + vintage_group, data = did_3)
did_4 <- ws_merged_long %>% filter(month %in% c('price_dec', 'price_apr')) 
did_4_lm = lm(lprice ~ in_aswg_dec + after_dec + in_aswg_dec*after_dec + type + sugar + is_blend + price_segment + region + vintage_group, data = did_4)
summary(did_1_lm)
summary(did_2_lm)
summary(did_3_lm)
summary(did_4_lm)

stargazer(did_1_lm, did_2_lm,did_3_lm,did_4_lm, type = 'text')

did_effects_df_dec <- data.frame(per = c('Дек-Янв', 'Дек-Фев', 'Дек-Мар', 'Дек-Апр'), 
                              effect = c(summary(did_1_lm)$coefficients["in_aswg_decTRUE:after_decTRUE", "Estimate"], 
                                         summary(did_2_lm)$coefficients["in_aswg_decTRUE:after_decTRUE", "Estimate"], 
                                         summary(did_3_lm)$coefficients["in_aswg_decTRUE:after_decTRUE", "Estimate"],
                                         summary(did_4_lm)$coefficients["in_aswg_decTRUE:after_decTRUE", "Estimate"]),
                              se = c(summary(did_1_lm)$coefficients["in_aswg_decTRUE:after_decTRUE", "Std. Error"], 
                                     summary(did_2_lm)$coefficients["in_aswg_decTRUE:after_decTRUE", "Std. Error"], 
                                     summary(did_3_lm)$coefficients["in_aswg_decTRUE:after_decTRUE", "Std. Error"],
                                     summary(did_4_lm)$coefficients["in_aswg_decTRUE:after_decTRUE", "Std. Error"])) %>%
  mutate(ci_l = effect-1.96*se,
         ci_h = effect+1.96*se,
         significance = case_when(ci_l <=0 & ci_h <=0 ~ 'Отрицательный',
                                  ci_l >=0 & ci_h >=0 ~ 'Положительный',
                                  ci_l <=0 & ci_h >=0 ~ 'Не значимый'),
         per = factor(per, levels =  c('Дек-Янв', 'Дек-Фев', 'Дек-Мар', 'Дек-Апр')))
#
plot_name <- paste('Графики/', 'Эффект рейтинга', '.png', sep = '')

ggplot(did_effects_df_dec, aes(x = per, y = effect, col = significance)) +
  geom_hline(yintercept = 0, linewidth = 1, linetype = 'dashed') +
  geom_errorbar(aes(ymin = ci_l, ymax = ci_h), width = 0.2, col = 'black') +
  geom_point(size = 5) +
  scale_color_manual(values = c('#e10916', '#fbd62e', '#1d8995'), breaks = c('Отрицательный', 'Не значимый', 'Положительный'))+
  labs(x = "Период",
       y = "Эффект рейтинга, %") + guides(col = guide_legend(title = 'Эффект'))+
  theme_bw()

# для февраля
did_1 <- ws_merged_long %>% filter(month %in% c('price_dec', 'price_jan')) 
did_1_lm = lm(lprice ~ in_aswg_feb + after_dec + in_aswg_feb*after_dec + type + sugar + is_blend + price_segment + region + vintage_group, data = did_1)
did_2 <- ws_merged_long %>% filter(month %in% c('price_dec', 'price_feb')) 
did_2_lm = lm_robust(lprice ~ in_aswg_feb + after_dec + in_aswg_feb*after_dec + type + sugar + is_blend + price_segment + region + vintage_group, data = did_2)
did_3 <- ws_merged_long %>% filter(month %in% c('price_dec', 'price_mar')) 
did_3_lm = lm_robust(lprice ~ in_aswg_feb + after_dec + in_aswg_feb*after_dec + type + sugar + is_blend + price_segment + region + vintage_group, data = did_3)
did_4 <- ws_merged_long %>% filter(month %in% c('price_dec', 'price_apr')) 
did_4_lm = lm_robust(lprice ~ in_aswg_feb + after_dec + in_aswg_feb*after_dec + type + sugar + is_blend + price_segment + region + vintage_group, data = did_4)
summary(did_1_lm)
summary(did_2_lm)
summary(did_3_lm)
summary(did_4_lm)

stargazer(did_1_lm, did_2_lm,did_3_lm,did_4_lm, type = 'text')

did_effects_df_feb <- data.frame(per = c('Дек-Янв', 'Дек-Фев', 'Дек-Мар', 'Дек-Апр'), 
                                 effect = c(summary(did_1_lm)$coefficients["in_aswg_febTRUE:after_decTRUE", "Estimate"], 
                                            summary(did_2_lm)$coefficients["in_aswg_febTRUE:after_decTRUE", "Estimate"], 
                                            summary(did_3_lm)$coefficients["in_aswg_febTRUE:after_decTRUE", "Estimate"],
                                            summary(did_4_lm)$coefficients["in_aswg_febTRUE:after_decTRUE", "Estimate"]),
                                 se = c(summary(did_1_lm)$coefficients["in_aswg_febTRUE:after_decTRUE", "Std. Error"], 
                                        summary(did_2_lm)$coefficients["in_aswg_febTRUE:after_decTRUE", "Std. Error"], 
                                        summary(did_3_lm)$coefficients["in_aswg_febTRUE:after_decTRUE", "Std. Error"],
                                        summary(did_4_lm)$coefficients["in_aswg_febTRUE:after_decTRUE", "Std. Error"])) %>%
  mutate(ci_l = effect-1.96*se,
         ci_h = effect+1.96*se,
         significance = case_when(ci_l <=0 & ci_h <=0 ~ 'Отрицательный',
                                  ci_l >=0 & ci_h >=0 ~ 'Положительный',
                                  ci_l <=0 & ci_h >=0 ~ 'Не значимый'),
         per = factor(per, levels =  c('Дек-Янв', 'Дек-Фев', 'Дек-Мар', 'Дек-Апр')))
#
plot_name <- paste('Графики/', 'Эффект рейтинга', '.png', sep = '')

ggplot(did_effects_df_feb, aes(x = per, y = effect, col = significance)) +
  geom_hline(yintercept = 0, linewidth = 1, linetype = 'dashed') +
  geom_errorbar(aes(ymin = ci_l, ymax = ci_h), width = 0.2, col = 'black') +
  geom_point(size = 5) +
  scale_color_manual(values = c('#e10916', '#fbd62e', '#1d8995'), breaks = c('Отрицательный', 'Не значимый', 'Положительный'))+
  labs(x = "Период",
       y = "Эффект рейтинга, %") + guides(col = guide_legend(title = 'Эффект'))+
  theme_bw()




# 
did_simple <- feols(lprice ~ i(mon, in_aswg_dec, ref = 1) | in_aswg_dec, data = ws_merged_long)
coef(did_simple)

coefplot(did_simple)
# 
# summary(did_ws)
# 
# did_ws <- lm(lprice ~ after_dec + in_aswg_dec*as.character(mon)*after_dec + is_blend + region , data = ws_merged_long, weights = ws_merged_long$weights)
# summary(did_ws)



# DiD + exact/nearest matching ----------------------------------------------------------


# проверка баланса ковариат

xBalance(in_aswg_dec ~  type + sugar + is_blend + price + region+ vintage_group, data=ws_merged_long, report = 'chisquare.test')
balance_table <- bal.tab(in_aswg_dec ~  type + sugar + is_blend + lprice + region + vintage_group, data = ws_merged_long, 
        estimand = "ATE",
        m.threshold = .05) 
as.data.frame(balance_table)
balance_table
write_excel_csv(balance_table$Balance, file = 'balance_table.csv')
# balance <- CreateTableOne(strata = "in_aswg_dec", data = ws_merged_long, test = TRUE)
# summary(balance)
# balance # тесты


match_did_dec <- matchit(in_aswg_dec ~ price_match, data = ws_merged_long,
                     method = 'nearest', exact =c("type", "sugar", 'region', 'is_blend', 'vintage_group'), estimand = "ATT")
match_did_feb <- matchit(in_aswg_feb ~ type + sugar + price_match, data = ws_merged_long,
                     method = 'nearest', exact =c("type", "sugar", 'region','is_blend', 'vintage_group'), estimand = "ATT")
summary(match_did_dec)
summary(match_did_feb)

ws_merged_long$weights_dec <- match_did_dec$weights
ws_merged_long$weights_feb <- match_did_feb$weights

# тритмент декабрь
att_grouptime_match_dec <- att_gt(yname = "lprice",
                        tname = "mon",
                        idname = "X",
                        gname = "treatment_period_dec",
                        alp = 0.05,
                        bstrap = FALSE,
                        # xformla = ~ is_blend + region,
                        data = ws_merged_long,
                        weightsname = 'weights_dec'
)
summary(att_grouptime_match_dec)
ggdid(att_grouptime_match_dec, title = 'Оценка эффекта с точным мэтичнгом на наблюдаемые параметры', legend = FALSE)

# DiD + ps matching ----------------------------------------------------------

### IPTW с помощью пакета WeightIt
ps_model_dec <- weightit(in_aswg_dec ~ type + sugar + price_match + region + vintage_group + is_blend,
                     data = ws_merged_long, estimand = "ATT", method = "ps") # отсюда получаются веса
summary(ps_model_dec)
ws_merged_long$weights_ps_dec <- ps_model_dec$weights

ps_model_feb <- weightit(in_aswg_feb ~ type + sugar + price_match + region + vintage_group + is_blend,
                         data = ws_merged_long, estimand = "ATT", method = "ps") # отсюда получаются веса
summary(ps_model_feb)
ws_merged_long$weights_ps_feb <- ps_model_feb$weights

ws_merged_long$ps_dec <- ps_model_dec$ps # мера склонности
ws_merged_long$ps_feb <- ps_model_feb$ps # мера склонности

# тритмент декабрь
att_grouptime_match_dec <- att_gt(yname = "lprice",
                                  tname = "mon",
                                  idname = "X",
                                  gname = "treatment_period_dec",
                                  alp = 0.05,
                                  bstrap = FALSE,
                                  # xformla = ~ region + vintage_group + is_blend ,
                                  data = ws_merged_long[ws_merged_long$ps_dec > 0.1,],
                                  weightsname = 'weights_ps_dec',
                                  est_method = 'reg'
)
summary(att_grouptime_match_dec)
ggdid(att_grouptime_match_dec, title = 'Оценка эффекта с мэтичнгом по мере склонности\n на наблюдаемые параметры', legend = FALSE)



# 87-88 vs 85-86  ---------------------------------------------------------


ws_rdd <- winestyle_merged %>% 
  filter(in_aswg_feb) %>%
  filter(
    aswg_rating %in% c('87','86','88','85'),
    # aswg_rating %in% c('87','86'),
    # price_dec < 1500,
    # для сладкого нет пары из контроля, иключено 2 наблюдения
    sugar != ' Сладкое'
  )
ggplot(ws_rdd, aes(x=price_dec, color=aswg_rating)) +
  geom_histogram(fill="white", alpha=0.5, position="stack")

ggplot(data = ws_rdd) +
  geom_point(mapping = aes(x = price_dec, y = price_diff_dec_apr_perc, colour = aswg_rating))


ws_rdd %>% group_by(aswg_rating) %>% summarise( price = mean(price_dec), n = n())


ws_rdd_long <- ws_rdd %>% pivot_longer(
  c('price_dec','price_jan','price_feb','price_mar','price_apr'), names_to = 'month', values_to = 'price'
) %>% mutate (
  after = month != 'price_dec',
  treatment_period = if_else(aswg_rating %in% c('87', '88'), 2, 0),
  # long = if_else(month == 'price_dec', 0, 1),
  lprice = log(price),
  mon = as.numeric(factor(month, levels = c('price_dec','price_jan','price_feb','price_mar','price_apr')))
)
att_rdd_grouptime <- att_gt(yname = "lprice",
                            tname = "mon",
                            idname = "X",
                            gname = "treatment_period",
                            alp = 0.05,
                            bstrap = FALSE,
                            xformla = ~ type + sugar + is_blend + price_segment + region,
                            data = ws_rdd_long,
                            est_method = 'reg'
)
summary(att_rdd_grouptime)
ggdid(att_rdd_grouptime, title = 'Естественный эксперимент.\nОценка эффекта с контролем на наблюдаемые параметры', legend = F)

# то же самое, но с ps мэтичнгом

bal.tab(in_aswg_dec ~  type + sugar + lprice + region + vintage_group, data = ws_rdd_long, 
                         estimand = "ATE",
                         m.threshold = .05) 
ps_model_rdd <- weightit(in_aswg_dec ~ type + sugar + price_match + region + vintage_group + is_blend,
                         data = ws_rdd_long, estimand = "ATT", method = "ps") # отсюда получаются веса
summary(ps_model_rdd)
ws_rdd_long$weights_ps_dec <- ps_model_rdd$weights # веса
ws_rdd_long$ps <- ps_model_rdd$ps # мера склонности

att_rdd_grouptime_weight <- att_gt(yname = "lprice",
                            tname = "mon",
                            idname = "X",
                            gname = "treatment_period",
                            alp = 0.05,
                            bstrap = FALSE,
                            # xformla = ~ type + sugar + is_blend + price_segment + region,
                            data = ws_rdd_long[ws_rdd_long$ps > 0.1,],
                            weightsname = 'weights_ps_dec',
                            est_method = 'reg'
)
summary(att_rdd_grouptime_weight)
ggdid(att_rdd_grouptime_weight,title = 'Естественный эксперимент.\nОценка эффекта с мэтичнгом по мере склонности\nна наблюдаемые параметры', legend = F)


winestyle_merged %>% summarise(price = mean(price_dec), n = n())



#  Через rdd не работает

rdd2 <- winestyle_merged %>% filter(in_aswg_feb) %>% mutate(
  aswg_rating = as.numeric(aswg_rating)
)

rdd2 %>% group_by(aswg_rating) %>% summarise( price = mean(price_dec), n = n())
rdd2$centered_rating <- rdd2$aswg_rating - 86.5
modelrdd <- RDestimate(price_diff_dec_apr_perc ~ centered_rating, data=rdd2, 
                     cutpoint=0, kernel="triangular")
summary(modelrdd)




# 
# ------- Анализ данных winestyle --------
# 

# описательные статистики
descr_stat <- winestyle_merged %>% group_by(type, in_aswg_dec) %>% summarise(
  'среднее' = mean(price_dec),
  'ст.откл' = sd(price_dec),
  'медиана' = median(price_dec),
  'мин' = min(price_dec),
  'макс' = max(price_dec),
  'количество'= n(),
)
write_excel_csv(descr_stat, file = 'descr_stat.csv')

ws_rdd
 
# install.packages('descr')
# library(descr)

# как менялись цены по типам и рейтингу в рублях
pd_winestyle <- winestyle_merged %>% 
  group_by( 
    type,
    in_aswg_dec
  ) %>%
  summarise(
    dec = mean(price_dec, na.rm = TRUE),
    # dec25 = mean(price_dec25, na.rm = TRUE),
    jan = mean(price_jan, na.rm = TRUE),
    feb = mean(price_feb, na.rm = TRUE),
    mar = mean(price_mar, na.rm = TRUE),
    apr = mean(price_apr, na.rm = TRUE)
    
    # dec = mean(log(price_dec), na.rm = TRUE),
    # # dec25 = mean(price_dec25, na.rm = TRUE),
    # jan = mean(log(price_jan), na.rm = TRUE),
    # feb = mean(log(price_feb), na.rm = TRUE),
    # mar = mean(log(price_mar), na.rm = TRUE),
    # apr = mean(log(price_apr), na.rm = TRUE)
  ) %>% 
  filter(
    # график для красных, белых и игристых (розовые не включены)
    type %in% c('Красное','Белое', 'Игристое')
  ) %>% 
  mutate(
    in_aswg_dec = as.character(in_aswg_dec)
  ) %>% unite(type_rat,type,in_aswg_dec)

as_tibble(t(pd_winestyle)) %>%
  row_to_names(row_number = 1) %>% 
  mutate(x=row_number()) %>% 
  pivot_longer(-x) %>% 
  mutate (
    value = as.numeric(value)
  ) %>% rename(
    wine_type = name 
  ) %>% 
  ggplot(aes(x=factor(x), y=value, group=wine_type, col=wine_type)) +
  geom_line() +
  xlab("Месяц") +
  ylab("Цена, руб") +
  ggtitle("Динамика цен российских вин по типам") + 
  scale_fill_discrete(name = "New Legend Title") +
  scale_x_discrete(labels=c('Дек', 'Янв', 'Фев', 'Мар', 'Апр') )
  # scale_x_discrete(labels=c('Дек','Дек25', 'Янв', 'Фев', 'Мар', 'Апр') )



# как менялись цены по рейтингу в процентах к предыдыщему месяцу
pd_winestyle3 <- winestyle_merged %>% 
  group_by( 
    in_aswg_dec
  ) %>%
  summarise(
    delta1 = mean((price_jan - price_dec)/price_dec, na.rm = TRUE),
    delta2 = mean((price_feb - price_jan)/price_jan, na.rm = TRUE),
    delta3 = mean((price_mar - price_feb)/price_feb, na.rm = TRUE),
    delta4 = mean((price_apr - price_mar)/price_mar, na.rm = TRUE)
  ) %>% 
  mutate(
    in_aswg_dec = as.character(in_aswg_dec)
  ) 

as_tibble(t(pd_winestyle3)) %>%
  row_to_names(row_number = 1) %>% 
  mutate(x=row_number()) %>% 
  pivot_longer(-x) %>% 
  mutate (
    value = as.numeric(value)
  ) %>% rename(
    in_rating = name 
  ) %>% 
  ggplot(aes(x=factor(x), y=value, group=in_rating, col=in_rating)) +
  geom_line() +
  xlab("Месяц") +
  ylab("Цена, руб") +
  ggtitle("Дельта цен российских вин") + 
  scale_fill_discrete(name = "New Legend Title") +
  scale_x_discrete(labels=c('Дек-Янв', 'Янв-Фев', 'Фев-Мар', 'Мар-Апр') )



# как менялись цены по ценовым сегментам и рейтингу в процентах к предыдыщему месяцу
pd_winestyle4 <- winestyle_merged %>% 
  group_by(
    price_segment,
    in_aswg_dec
  ) %>%
  summarise(
    delta1 = mean((price_jan - price_dec)/price_dec, na.rm = TRUE),
    delta2 = mean((price_feb - price_jan)/price_jan, na.rm = TRUE),
    delta3 = mean((price_mar - price_feb)/price_feb, na.rm = TRUE),
    delta4 = mean((price_apr - price_mar)/price_mar, na.rm = TRUE)
  ) %>% 
  mutate(
    in_aswg_dec = as.character(in_aswg_dec)
  ) %>% unite(rat_seg,in_aswg_dec, price_segment)

as_tibble(t(pd_winestyle4)) %>%
  row_to_names(row_number = 1) %>% 
  mutate(x=row_number()) %>% 
  pivot_longer(-x) %>% 
  mutate (
    value = as.numeric(value)
  ) %>% rename(
    rat_seg = name 
  ) %>% 
  ggplot(aes(x=factor(x), y=value, group=rat_seg, col=rat_seg)) +
  geom_line() +
  xlab("Месяц") +
  ylab("Цена, руб") +
  ggtitle("Дельта цен российских вин") + 
  scale_fill_discrete(name = "New Legend Title") +
  scale_x_discrete(labels=c('Дек-Янв', 'Янв-Фев', 'Фев-Мар', 'Мар-Апр') )

# как менялись цены первого ЦС по типам и рейтингу в процентах к предыдыщему месяцу
pd_winestyle6 <- winestyle_merged %>% 
  filter (
    price_segment == 1
  ) %>% group_by(
    type,
    in_aswg_dec
  ) %>%
  summarise(
    delta1 = mean((price_jan - price_dec)/price_dec, na.rm = TRUE),
    delta2 = mean((price_feb - price_jan)/price_jan, na.rm = TRUE),
    delta3 = mean((price_mar - price_feb)/price_feb, na.rm = TRUE),
    delta4 = mean((price_apr - price_mar)/price_mar, na.rm = TRUE)
  ) %>% 
  mutate(
    in_aswg_dec = as.character(in_aswg_dec)
  ) %>% unite(type_rat, type, in_aswg_dec)

as_tibble(t(pd_winestyle6)) %>%
  row_to_names(row_number = 1) %>% 
  mutate(x=row_number()) %>% 
  pivot_longer(-x) %>% 
  mutate (
    value = as.numeric(value)
  ) %>% rename(
    type_rat = name 
  ) %>% 
  ggplot(aes(x=factor(x), y=value, group=type_rat, col=type_rat)) +
  geom_line() +
  xlab("Месяц") +
  ylab("Цена, руб") +
  ggtitle("Дельта цен российских вин") + 
  scale_fill_discrete(name = "New Legend Title") +
  scale_x_discrete(labels=c('Дек-Янв', 'Янв-Фев', 'Фев-Мар', 'Мар-Апр') )


ggplot(winestyle_merged, aes(x=price_dec, color=in_aswg_dec)) +
  xlim(0,5000) +
  geom_histogram(fill="white", alpha=0.5, position="stack") 



# как менялись цены по ценовым сегментам и рейтингу в процентах к декабрю
pd_winestyle5 <- winestyle_merged %>% 
  group_by(
    price_segment,
    in_aswg_dec
  ) %>%
  summarise(
    delta1 = mean((price_jan - price_dec)/price_dec, na.rm = TRUE),
    delta2 = mean((price_feb - price_dec)/price_dec, na.rm = TRUE),
    delta3 = mean((price_mar - price_dec)/price_dec, na.rm = TRUE),
    delta4 = mean((price_apr - price_dec)/price_dec, na.rm = TRUE)
  ) %>% 
  mutate(
    in_aswg_dec = as.character(in_aswg_dec)
  ) %>% unite(rat_seg,price_segment,in_aswg_dec)

as_tibble(t(pd_winestyle5)) %>%
  row_to_names(row_number = 1) %>% 
  mutate(x=row_number()) %>% 
  pivot_longer(-x) %>% 
  mutate (
    value = as.numeric(value)
  ) %>% rename(
    rat_seg = name 
  ) %>% 
  ggplot(aes(x=factor(x), y=value, group=rat_seg, col=rat_seg)) +
  geom_line() +
  xlab("Месяц") +
  ylab("Цена, руб") +
  ggtitle("Дельта цен российских вин") + 
  scale_fill_discrete(name = "New Legend Title") +
  scale_x_discrete(labels=c('Дек-Янв', 'Янв-Фев', 'Фев-Мар', 'Мар-Апр') )


# гистограммы распределения цен
hist(winestyle_merged$price_dec, 
     xlim = c(0,5000), 
     breaks = 300,
     main="Распределение цен вин до 5000 в декабре",
     xlab="Цена, руб")
hist(winestyle_merged$price_mar, 
     xlim = c(0,5000), 
     breaks = 300,
     main="Распределение цен вин до 5000 в марте",
     xlab="Цена, руб") 
hist(winestyle_merged_treat$price_mar, 
     xlim = c(0,5000), 
     breaks = 100,
     main="Распределение цен вин в рейтинге до 5000 в марте",
     xlab="Цена, руб")
hist(winestyle_merged_cont$price_mar, 
     xlim = c(0,5000), 
     breaks = 300,
     main="Распределение цен вин не в рейтинге до 5000 в марте",
     xlab="Цена, руб")

ggplot(winestyle_merged, aes(x=price_dec, color=in_aswg_dec)) +
  xlim(0,5000) +
  geom_histogram(fill="white", alpha=0.5, position="stack")
  # geom_histogram(aes(y = ..density..), fill="white", alpha=0.5, position="stack") 


hist(winestyle_merged$price_mar, 
     xlim = c(5000,12000), 
     ylim = c(0, 10),
     breaks = 100,
     main="Распределение цен вин дороже 5000 в марте",
     xlab="Цена, руб")


hist(log(winestyle_merged$price_dec), 
     breaks = 100,
     main="Распределение логарифма цен вин в декабре",
     xlab="Логарифм цены")
hist(log(winestyle_merged$price_mar), 
     breaks = 100,
     main="Распределение логарифма цен вин в декабре",
     xlab="Цена, руб")

hist(winestyle_merged$price_diff_dec_jan, 
     xlim = c(-1000,1000), 
     breaks = 100,
     main="Распределение изменения цен с декабря по январь",
     xlab="Цена, руб")
hist(winestyle_merged$price_diff_dec_mar, 
     xlim = c(-1000,1000), 
     breaks = 100,
     main="Распределение изменения цен с декабря по март",
     xlab="Цена, руб")
hist(winestyle_merged$price_diff_dec_apr_perc,
     breaks = 100,
     main="Распределение изменения цен с декабря по апрель",
     xlab="Цена, руб")


# точечная диаграмма
ggplot(data = winestyle_merged) + 
  xlim(0,5000) +
  geom_point(mapping = aes(x = price_dec, y = price_diff_dec_mar, colour = in_aswg_dec))


ggplot(data = winestyle_merged) + 
  xlim(0,10000) +
  geom_point(mapping = aes(x = price_dec, y = price_diff_dec_mar, colour = is_90_plus))

ggplot(data = winestyle_merged) + 
  xlim(5000, 10000) +
  geom_point(mapping = aes(x = price_dec, y = price_diff_dec_mar, colour = aswg_rating))

ggplot(data = winestyle_merged) + 
  geom_point(mapping = aes(x = price_dec, y = price_diff_dec_mar_perc, colour = aswg_rating))

