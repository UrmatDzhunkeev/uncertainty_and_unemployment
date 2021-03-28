

# remove(list = ls())
# dev.off()
library(tidyverse) # %>% 
library(alfred) # get_fred_series()
library(GGally) # ggpairs()
library(vars) # VARselect(); VAR()
library(tseries) # adf.test(); kpss.test()
library(ggbiplot) # ggbiplot()
library(mFilter) # hpfilter()
library(sandwich) # vcovHC()
library(factoextra) # fviz_eig()
# начало рассматриваемого периода
time_start <- '2000-01-01'
# конец рассматриваемого периода
time_end <- '2021-01-01'
# загрузка данных
# production in total manufacturing: https://fred.stlouisfed.org/series/RUSPROMANMISMEI
prod_manuf <- get_fred_series('RUSPROMANMISMEI','product_manuf',
                              observation_start = time_start, observation_end = time_end)
# economic policy uncertainty: https://fred.stlouisfed.org/series/RUSEPUINDXM
epu_index <- get_fred_series('RUSEPUINDXM','epu',
                             observation_start = time_start, observation_end = time_end)
# registered unemployment rate: https://fred.stlouisfed.org/series/LMUNRRTTRUM156S
regist_unempl_rate <- get_fred_series('LMUNRRTTRUM156S','regist_unempl',
                                      observation_start = time_start, observation_end = time_end)

# total share prices for all shares (growth rate): https://fred.stlouisfed.org/series/SPASTT01RUM657N
tot_share_price_grw_rate <- get_fred_series('SPASTT01RUM657N', 'tot_share_price',
                                            observation_start = time_start, observation_end = time_end)
# фильтр Ходрика-Прескотта
tot_share_price_hpfilter <- hpfilter(tot_share_price_grw_rate$tot_share_price, type = 'lambda', freq = 129600)
# выделяем циклическую составляющую
tot_share_price_cycle <-  tot_share_price_hpfilter$cycle %>% data.frame(cycle_tot_share_price = ., 
                                                                        date = tot_share_price_grw_rate$date)

# 3-month or 90-day interbank rate: https://fred.stlouisfed.org/series/IR3TIB01RUM156N
interest_rate_3m <- get_fred_series('IR3TIB01RUM156N', 'int_rate_3m',
                                    observation_start = time_start, observation_end = time_end)
# вместо пропущенных наблюдений заполняем межбанковской ставкой процента срочностью от 8 до 30 дней
# источник: http://www.cbr.ru/hd_base/mkr/mkr_monthes/
interest_rate_3m[241,'int_rate_3m'] <- 6.10
interest_rate_3m[244,'int_rate_3m'] <- 5.88
interest_rate_3m[245,'int_rate_3m'] <- 5.59
interest_rate_3m[251,] <- c('2020-11-01', 4.79)
interest_rate_3m[252,] <- c('2020-12-01', 4.63)

interest_rate_3m$int_rate_3m <- interest_rate_3m$int_rate_3m %>% as.numeric()

# consumer price index: https://fred.stlouisfed.org/series/RUSCPIALLMINMEI
con_price_index <- get_fred_series('RUSCPIALLMINMEI','cpi_level',
                                   observation_start = time_start, observation_end = time_end)
# создаем переменную "темпы инфляции" - то есть процентное изменение индекса потребительских цен
infl_rate <- 100 * diff(log(con_price_index$cpi_level)) %>% data.frame(grw_rate_infl = .)
# добавляем столбец с датой
infl_rate$date <- con_price_index$date[2:nrow(con_price_index)]
# прямой обменный курс доллар / рубль: количество рублей на единицу доллара
usd_to_ruble <- get_fred_series('CCUSMA02RUM618N','usd_ruble_ex_rate', observation_start = time_start, observation_end = time_end)
# фильтрация временного ряда 
usd_to_ruble_hpfilter <- hpfilter(usd_to_ruble$usd_ruble_ex_rate, freq = 129600, type = 'lambda')
# выделяем циклическую составляющую
usd_to_ruble_cycle <- usd_to_ruble_hpfilter$cycle %>% data.frame('usd_ruble_ex_cycle' = ., date = usd_to_ruble$date)


# ---- первый показатель неопределенности: economic policy uncertainty: ----
# порядок переменных в векторной авторегрессии согласно работе [Baker, Bloom, Davis, 2016, p.1628 section IV.D]
# соединяем переменные
df_epu <- merge(epu_index, tot_share_price_grw_rate)
df_epu <- merge(df_epu, interest_rate_3m)
df_epu <- merge(df_epu, infl_rate)
df_epu <- merge(df_epu, regist_unempl_rate)
df_epu <- merge(df_epu, prod_manuf)
# сохраняем базу данных
# write.table(df_epu, 'df_epu.csv', sep = ',')
# преобразовываем во временные ряды
df_epu_ts <- ts(df_epu[,-1], start = c(2000, 1), frequency = 12)
# выбор порядка лага
VARselect(df_epu_ts)
# формирование системы векторных авторегрессий второго лага
var_epu <- VAR(df_epu_ts, 2, 'none')
# выявление причинности по Грейнджеру
causality(var_epu, cause = c("epu"), vcov.=vcovHC(var_epu))
# шок неопределенности экономической политики на уровень безработицы 
irf_epu_unempl_list <- irf(var_epu, impulse = 'epu', response = 'regist_unempl', n.ahead = 24, cumulative = FALSE)
# шок неопределенности экономической политики на капитализацию фондового рынкаы 
irf_epu_tot_share_list <- irf(var_epu, impulse = 'epu', response = 'tot_share_price', n.ahead = 24, cumulative = FALSE)
# шок неопределенности экономической политики на межбанковскую процетную ставку 
irf_epu_int_rate_list <- irf(var_epu, impulse = 'epu', response = 'int_rate_3m', n.ahead = 24, cumulative = FALSE)
# шок неопределенности экономической политики на уровень инфляции
irf_epu_infation_list <- irf(var_epu, impulse = 'epu', response = 'grw_rate_infl', n.ahead = 24, cumulative = FALSE)
# шок неопределенности экономической политики на промышленное производство
irf_epu_prod_manuf_list <- irf(var_epu, impulse = 'epu', response = 'product_manuf', n.ahead = 24, cumulative = FALSE)
# 
# ---- volatility of stock market [Bloom, 2009] ----
# total share price of all share
# объединение переменных
df_tot_share_price <- merge(tot_share_price_cycle, tot_share_price_grw_rate)
df_tot_share_price <- merge(df_tot_share_price, interest_rate_3m)
df_tot_share_price <- merge(df_tot_share_price, infl_rate)
df_tot_share_price <- merge(df_tot_share_price, regist_unempl_rate)
df_tot_share_price <- merge(df_tot_share_price, prod_manuf)
# сохранение базы данных
write.table(df_tot_share_price, 'df_tot_share_price.csv', sep = ',')
# преобразование во временные ряды
df_tot_share_price_ts <- ts(df_tot_share_price[,-1], start = c(2002, 1), frequency = 12)
# определение порядка лага
VARselect(df_tot_share_price_ts)
# формирование 
var_tot_share_price <- VAR(df_tot_share_price_ts, 2, 'none')
# гипотеза причинности
causality(var_tot_share_price, cause = c('cycle_tot_share_price'), vcov. = vcovHC(var_tot_share_price))
# шок капитализации на уровень безработицы
irf_tot_share_unempl_list <- irf(var_tot_share_price, impulse = 'cycle_tot_share_price', response = 'regist_unempl', n.ahead = 24, cumulative = FALSE)
# шок капитализации на межбанковскую процентную ставку 
irf_tot_share_int_rate_list <- irf(var_tot_share_price, impulse = 'cycle_tot_share_price', response = 'int_rate_3m', n.ahead = 24, cumulative = FALSE)
# шок капитализации на уровень безработицы
irf_tot_share_inflation_list <- irf(var_tot_share_price, impulse = 'cycle_tot_share_price', response = 'grw_rate_infl', n.ahead = 24, cumulative = FALSE)
# шок капитализации на уровень безработицы
irf_tot_share_prod_manuf_list <- irf(var_tot_share_price, impulse = 'cycle_tot_share_price', response = 'product_manuf', n.ahead = 24, cumulative = FALSE)
# шок капитализации на уровень капитализации
irf_tot_share_tot_share_list <- irf(var_tot_share_price, impulse = 'cycle_tot_share_price', response = 'tot_share_price', n.ahead = 24, cumulative = FALSE)

# соединение переменных в единую базу данных
df_usd_ruble <- merge(usd_to_ruble_cycle, tot_share_price_grw_rate)
df_usd_ruble <- merge(df_usd_ruble, infl_rate)
df_usd_ruble <- merge(df_usd_ruble, interest_rate_3m)
df_usd_ruble <- merge(df_usd_ruble, regist_unempl_rate)
df_usd_ruble <- merge(df_usd_ruble, prod_manuf)
# сохранение массива данных
write.table(df_usd_ruble, 'df_usd_ruble.csv', sep = ',')
# преобразование во временные ряды
df_usd_ruble_ts <- ts(df_usd_ruble[,-1], start = c(2000, 2), frequency = 12)
# выбор порядка лага
VARselect(df_usd_ruble_ts)
# формирование векторной авторегрессии
var_usd_ruble <- VAR(df_usd_ruble_ts, 2, 'none')
# проверка причинности
causality(var_usd_ruble, cause = c('usd_ruble_ex_cycle'), vcov. = vcovHC(var_usd_ruble))
# шок обменного курса США на уровень безработицы
irf_usd_unempl_list <- irf(var_usd_ruble, impulse = 'usd_ruble_ex_cycle', response = 'regist_unempl', n.ahead = 24, cumulative = FALSE)
# шок обменного курса США на межбанковскую процентную ставку
irf_usd_int_rate_list <- irf(var_usd_ruble, impulse = 'usd_ruble_ex_cycle', response = 'int_rate_3m', n.ahead = 24, cumulative = FALSE)
# шок обменного курса США на капитализацию фондового рынка
irf_usd_tot_share_list <- irf(var_usd_ruble, impulse = 'usd_ruble_ex_cycle', response = 'tot_share_price', n.ahead = 24, cumulative = FALSE)
# шок обменного курса США на промышленное производство
irf_usd_prod_manuf <- irf(var_usd_ruble, impulse = 'usd_ruble_ex_cycle', response = 'product_manuf', n.ahead = 24, cumulative = FALSE)
# шок обменного курса США на уровень инфляции
irf_usd_inflation_list <- irf(var_usd_ruble, impulse = 'usd_ruble_ex_cycle', response = 'grw_rate_infl', n.ahead = 24, cumulative = FALSE)

# ---- макроэкономическая неопределенность ----

# загрузка макроэкономических переменных
# real broad effective exchange rate: https://fred.stlouisfed.org/series/RBRUBIS
rbeer_fred_ts <- get_fred_series('CCUSMA02RUM618N','rbeer_fred')
# production of total industry: https://fred.stlouisfed.org/series/RUSPROINDMISMEI
prod_tot_ind_fred_ts <- get_fred_series('RUSPROINDMISMEI','prod_tot_ind')
# total reserves excluding gold: https://fred.stlouisfed.org/series/TRESEGRUM194N
tot_res_fred_ts <- get_fred_series('TRESEGRUM194N','tot_res', observation_start = time_start)
# m3: https://fred.stlouisfed.org/series/MABMM301RUM189S
m3_fred_ts <- get_fred_series('MABMM301RUM189S','m3')
# exports: value goods: https://fred.stlouisfed.org/series/XTEXVA01RUM667S
export_fred_ts <- get_fred_series('XTEXVA01RUM667S','export')
# domestic producer price index: manufacturing: https://fred.stlouisfed.org/series/RUSPPDMMINMEI
manuf_price_fred_ts <- get_fred_series('RUSPPDMMINMEI','manuf_price')
# leading indicator: gdp normalized: https://fred.stlouisfed.org/series/RUSLORSGPNOSTSAM
norm_gdp_fred_ts <- get_fred_series('RUSLORSGPNOSTSAM','norm_gdp')
# manufacturing confidence indicator: https://fred.stlouisfed.org/series/BSCICP03RUM665S
manuf_conf_fred_ts <- get_fred_series('BSCICP03RUM665S','manuf_conf')
# import: https://fred.stlouisfed.org/series/XTIMVA01RUM667S
import_fred_ts <- get_fred_series('XTIMVA01RUM667S','import')
# price index in total manufacturing production: https://fred.stlouisfed.org/series/PIEATI02RUM661N
prod_price_fred_ts <- get_fred_series('PIEATI02RUM661N','prod_price')
# confidence leading indicator (CLI): https://fred.stlouisfed.org/series/RUSLOLITONOSTSAM
cli_fred_ts <- get_fred_series('RUSLOLITONOSTSAM','cli')
# BTS - production: https://fred.stlouisfed.org/series/RUSLOCOBPORSTM
# monthly earning: https://fred.stlouisfed.org/series/LCEATT03RUM664S
month_earn_fred_ts <- get_fred_series('LCEATT03RUM664S','month_earn')
# employment rate: https://fred.stlouisfed.org/series/RUSLREMTTTTSTSAM
empl_rate_all_fred_ts <- get_fred_series('RUSLREMTTTTSTSAM','empl_rate_all')
# total mining production: https://fred.stlouisfed.org/series/RUSPRMITO01IXOBSAM
tot_mining_prod_fred_ts <- get_fred_series('RUSPRMITO01IXOBSAM','tot_mining_prod')
# producer price index mining: https://fred.stlouisfed.org/series/RUSPIEAMI02GYM
ppi_mining_fred_ts <- get_fred_series('RUSPIEAMI02GYM','ppi_mining')
# consumer confidence indicator: https://fred.stlouisfed.org/series/RUSCSCICP03IXNSAM
# business tendency survey for manufactrugin: selling prices: https://fred.stlouisfed.org/series/BSSPFT02RUM460S
bts_sell_price_fred_ts <- get_fred_series('BSSPFT02RUM460S','bts_sell_price')
# business tendency survey for manufacturing: confidence indicators
buss_conf_index <- get_fred_series('BSCICP03RUM665S','bci',
                                   observation_start = time_start, observation_end = time_end)
# consumer confidence index
cons_conf_index <- read.csv('cons_conf_index.csv')
# создание столбца с датой
cons_conf_index$date <- prod_manuf$date[1:nrow(prod_manuf)-1]

# соединение переменных в единую базу данных
# денежный агрегат М3, ежемесячные доходы
fred_df <- merge(m3_fred_ts, month_earn_fred_ts)
# импорт, руб.
fred_df <- merge(fred_df, import_fred_ts)
# индекс выпуска промышленного производства
fred_df <- merge(fred_df, prod_manuf)
# индекс нормализованного уровня ВВП
fred_df <- merge(fred_df, norm_gdp_fred_ts)
# индекс производства добывающей отрасли
fred_df <- merge(fred_df, tot_mining_prod_fred_ts)
# индекс совокупного производства отраслей
fred_df <- merge(fred_df, prod_tot_ind_fred_ts)
# индекс уверенности домашних хозяйств
fred_df <- merge(fred_df, cons_conf_index)
# индекс уверенности компаний промышленного сектора 
# относительно будущей динамики цен на выпускаемую продукцию
fred_df <- merge(fred_df, bts_sell_price_fred_ts)
# индекс цен производителей добывающей отрасли 
fred_df <- merge(fred_df, ppi_mining_fred_ts)
# индекс цен производителей промышленного сектора
fred_df <- merge(fred_df, manuf_price_fred_ts)
# межбанковская процентная ставка кредита
fred_df <- merge(fred_df, interest_rate_3m)
# преобразование из строкового формата в числовой формат
fred_df$int_rate_3m <- fred_df$int_rate_3m %>% as.numeric()
# общие резервы за исключением запасов золота
fred_df <- merge(fred_df, tot_res_fred_ts)
# опрос частных компаний по производственной деятельности
fred_df <- merge(fred_df, manuf_conf_fred_ts)
# опросный совокупный опережающий индикатор
fred_df <- merge(fred_df, cli_fred_ts)
# прямой обменный курс доллара США, руб.
fred_df <- merge(fred_df, usd_to_ruble)
# реальный широкий обменный курс российского рубля
fred_df <- merge(fred_df, rbeer_fred_ts)
# совокупная стоимость всех акций компаний
fred_df <- merge(fred_df, tot_share_price_grw_rate)
# совокупный индекс уверенности компаний частного сектора
fred_df <- merge(fred_df, buss_conf_index)
# уровень зарегистрированной безработицы, процент
fred_df <- merge(fred_df, regist_unempl_rate)
# экспорт, долл. США
fred_df <- merge(fred_df, export_fred_ts)


# fred_df_pca <-  prcomp(fred_df[,-1], center = TRUE, scale. = TRUE)
# the values of each sample in terms of the principal components `fred_df_pca$x`
# https://purrr.tidyverse.org/reference/map.html
# оцениваем по каждой макроэкономической переменной модель авторегрессии первого порядка
fred_df[,-1] %>% 
  data.frame() %>% 
  # определяем спецификацию модели авторегрессии первого порядка
  map(
    ~lm(. ~ lag(., n = 1))
  ) %>% 
  # сохраняем числовые характеристики оцененных моделей
  map(summary) -> fred_df_ar_models
# выделяем регрессионных остатки
fred_df_ar_models %>% 
  map(resid) -> fred_df_ar_models_resid

# НР-фильтрация и выделение циклической компоненты регрессионных ошибок
fred_df_ar_models_resid %>% 
  map_df(~hpfilter(x = ., freq = 129600, type = 'lambda')$cycle) -> fred_df_ar_models_resid_hp_cycle

fred_df_ar_models_resid_hp_cycle %>% 
  # стандартизируем переменные
  map(scale) %>% 
  # преобразовываем в формат базы данных
  data.frame() %>% 
  # вычисляем среднее значение по строкам каждой переменной
  apply(., 1, IQR) %>%
  # сохраняем в формате базы данных
  data.frame(uncert_feat = .) -> macro_uncert_measure

# создание столбца с датой
macro_uncert_measure$date <- fred_df$date[2:nrow(fred_df)]
# [Chio, Loungani, 2015, Journal of Monetary Economics] and [Jurado, Lurvigson, Ng, 2015, AER]

# соединение переменных в единую базу данных
df_macro_uncert <- merge(macro_uncert_measure, tot_share_price_grw_rate)
df_macro_uncert <- merge(df_macro_uncert, infl_rate)
df_macro_uncert <- merge(df_macro_uncert, interest_rate_3m)
df_macro_uncert <- merge(df_macro_uncert, regist_unempl_rate)
df_macro_uncert <- merge(df_macro_uncert, prod_manuf)

# преобразование во временные ряды
df_macro_uncert_ts <- ts(df_macro_uncert[,-1], start = c(2000, 2), frequency = 12)
# выбор порядка лага
VARselect(df_macro_uncert_ts)
# формирование векторной авторегрессии
var_macro_uncert <- VAR(df_macro_uncert_ts, 2, 'none')
# проверка причинности и одновременного изменения имсульса и отклика
causality(var_macro_uncert, cause = 'uncert_feat', vcov. = vcovHC(var_macro_uncert))
# шок макроэкономической неопределенности на уровень безработицы
irf_macro_uncert_unempl_list <- irf(var_macro_uncert, impulse = 'uncert_feat', response = 'regist_unempl', n.ahead = 24, cumulative = FALSE)
# шок макроэкономической неопределенности на межбанковскую процентную ставку 
irf_macro_uncert_int_rate_list <- irf(var_macro_uncert, impulse = 'uncert_feat', response = 'int_rate_3m', n.ahead = 24, cumulative = FALSE)
# шок макроэкономической неопределенности на уровень инфляции
irf_macro_uncert_inflation_list <- irf(var_macro_uncert, impulse = 'uncert_feat', response = 'grw_rate_infl', n.ahead = 24, cumulative = FALSE)
# шок макроэкономической неопределенности на капитализацию фондового рынка
irf_macro_uncert_tot_share_list <- irf(var_macro_uncert, impulse = 'uncert_feat', response = 'tot_share_price', n.ahead = 24, cumulative = FALSE)
# шок макроэкономической неопределенности на индекс промышленного производства
irf_macro_uncert_prod_manuf_list <- irf(var_macro_uncert, impulse = 'uncert_feat', response = 'product_manuf', n.ahead = 24, cumulative = FALSE)

# ---- метод главных компонент всех показателей неопределенности ----
# соединение всех показателей неопределенности в единую базу данных
uncert_measures_all <- merge(epu_index, usd_to_ruble_cycle)
uncert_measures_all <- merge(uncert_measures_all, tot_share_price_cycle)
uncert_measures_all <- merge(uncert_measures_all, macro_uncert_measure)
# вычисление главных компонент
uncert_measures_all_pca <- prcomp(uncert_measures_all[,-1], center = TRUE, scale. = TRUE)

# график вклада переменной в главную компоненту
ggbiplot(uncert_measures_all_pca)
# отражаем долю дисперсии каждой главной компоненты
fviz_eig(uncert_measures_all_pca)
# числовые характеристики главных компонент
summary(uncert_measures_all_pca)

# сохраняем величины главных компонент в формат база данных
uncert_measures_all_pca_values <- uncert_measures_all_pca$x %>% data.frame(.)
# добавляем столбец с датой
uncert_measures_all_pca_values$date <- uncert_measures_all$date

# соединение переменных к единую базу данных
df_pca_all_uncert_feat <- merge(uncert_measures_all_pca_values %>% dplyr::select(date, PC1), tot_share_price_grw_rate)
df_pca_all_uncert_feat <- merge(df_pca_all_uncert_feat, infl_rate)
df_pca_all_uncert_feat <- merge(df_pca_all_uncert_feat, interest_rate_3m)
df_pca_all_uncert_feat <- merge(df_pca_all_uncert_feat, regist_unempl_rate)
df_pca_all_uncert_feat <- merge(df_pca_all_uncert_feat, prod_manuf)

# преобразуем во временные ряды
df_pca_all_uncert_feat_ts <- ts(df_pca_all_uncert_feat[,-1], start = c(2000, 2), frequency = 12)
# выбор порядка лага
VARselect(df_pca_all_uncert_feat_ts)
# формирование векторной авторегрессии
var_pca_all_uncer_feat <- VAR(df_pca_all_uncert_feat_ts, 2, 'none')
# проверка причинности
causality(var_pca_all_uncer_feat, cause = 'PC1', vcov. = vcovHC(var_pca_all_uncer_feat))
# шок главной компоненты всех показателей неопределенности на уровень безработицы
irf_pca_unempl_list <- irf(var_pca_all_uncer_feat, impulse = 'PC1', response = 'regist_unempl', n.ahead = 24, cumulative = FALSE)
# шок главной компоненты всех показателей неопределенности на капитализацию фондового рынка
irf_pca_tot_share_list <- irf(var_pca_all_uncer_feat, impulse = 'PC1', response = 'tot_share_price', n.ahead = 24, cumulative = FALSE) 
# шок главной компоненты всех показателей неопределенности на уровень безработицы
irf_pca_int_rate_list <- irf(var_pca_all_uncer_feat, impulse = 'PC1', response = 'int_rate_3m', n.ahead = 24, cumulative = FALSE)
# шок главной компоненты всех показателей неопределенности на уровень инфляции
irf_pca_inflation_list <- irf(var_pca_all_uncer_feat, impulse = 'PC1', response = 'grw_rate_infl', n.ahead = 24, cumulative = FALSE)
# шок главной компоненты всех показателей неопределенности на промышленное производство
irf_pca_prod_manuf_list <- irf(var_pca_all_uncer_feat, impulse = 'PC1', response = 'product_manuf', n.ahead = 24, cumulative = FALSE)


# ---- соединяем имсульсные отклики ----
# (а) влияние шоков неопределенности на уровень безработицы
# 
df_irf_unempl <- data.frame(
  time_span = 1:nrow(irf_epu_unempl_list$irf$epu),
  epu_unempl = irf_epu_unempl_list$irf$epu,
  macro_uncert_unempl = irf_macro_uncert_unempl_list$irf$uncert_feat,
  pca_uncert_unempl = irf_pca_unempl_list$irf$PC1,
  tot_share_unempl = irf_tot_share_unempl_list$irf$cycle_tot_share_price,
  usd_ruble_unempl = irf_usd_unempl_list$irf$usd_ruble_ex_cycle
)
# переименовываем метки столбцов
colnames(df_irf_unempl) <- c('time_span','epu_unempl','macro_uncert_unempl',
                             'pca_uncert_unempl','tot_share_unempl','usd_ruble_unempl')
# 
write.table(df_irf_unempl,'df_irf_unempl.csv', sep = ',')

impulse_to_unempl <- df_irf_unempl %>% 
  ggplot(aes(x = time_span)) + 
  geom_line(aes(y = epu_unempl, color = 'epu')) +
  geom_line(aes(y = macro_uncert_unempl, color = 'macro')) +
  geom_line(aes(y = pca_uncert_unempl, color = 'pca')) +
  geom_line(aes(y = tot_share_unempl, color = 'tot_share')) +
  geom_line(aes(y = usd_ruble_unempl, color = 'usd')) +
  geom_hline(yintercept = 0, color  = 'black') +
  scale_color_manual(name = '', values = c('epu' = 'blue', 'macro' = 'black', 'pca' = 'green',
                                           'tot_share' = 'grey','usd' = 'red')) + 
  theme_minimal() +
  theme(legend.position = 'bottom') + 
  labs(y = 'уровень безработицы, %', x = 'месяц')

# (б) влияние шоков неопределенности на капитализацию фондоового рынка
df_irf_tot_share <- data.frame(
  time_span = 1:nrow(irf_epu_tot_share_list$irf$epu),
  epu_tot_share = irf_epu_tot_share_list$irf$epu,
  macro_tot_share = irf_macro_uncert_tot_share_list$irf$uncert_feat,
  pca_tot_share = irf_pca_tot_share_list$irf$PC1,
  tot_tot_share = irf_tot_share_tot_share_list$irf$cycle_tot_share_price,
  usd_tot_share = irf_usd_tot_share_list$irf$usd_ruble_ex_cycle
)
# переименовываем столбцы
colnames(df_irf_tot_share) <- c('time_span','epu_tot_share',
                                'macro_tot_share','pca_tot_share','tot_share_tot_share',
                                'usd_tot_share')
# 
write.table(df_irf_tot_share,'df_irf_tot_share.csv', sep = ',')
# 
impulse_to_tot_share <- df_irf_tot_share %>% 
  ggplot(aes(x = time_span)) + 
  geom_line(aes(y = epu_tot_share, color = 'epu')) +
  geom_line(aes(y = macro_tot_share, color = 'macro')) +
  geom_line(aes(y = pca_tot_share, color = 'pca')) +
  geom_line(aes(y = tot_share_tot_share, color = 'tot_share')) +
  geom_line(aes(y = usd_tot_share, color = 'usd')) +
  geom_hline(yintercept = 0, color  = 'black') +
  scale_color_manual(name = '', values = c('epu' = 'blue', 'macro' = 'black', 'pca' = 'green',
                                           'tot_share' = 'grey','usd' = 'red')) + 
  theme_minimal() +
  theme(legend.position = 'bottom') + 
  labs(y = 'капитализация фондового рынка, темп роста, %', x = 'месяц')  

# (в) межбанковская процентная ставка
df_irf_int_rate <- data.frame(
  time_span = 1:nrow(irf_epu_int_rate_list$irf$epu),
  epu_int_rate = irf_epu_int_rate_list$irf$epu,
  macro_int_rate = irf_macro_uncert_int_rate_list$irf$uncert_feat,
  pca_int_rate = irf_pca_int_rate_list$irf$PC1,
  tot_share_int_rate = irf_tot_share_int_rate_list$irf$cycle_tot_share_price,
  usd_int_rate = irf_usd_int_rate_list$irf$usd_ruble_ex_cycle
)
# 
colnames(df_irf_int_rate) <- c('time_span','epu_int_rate',
                               'macro_int_rate','pca_int_rate','tot_share_int_rate','usd_int_rate')
# 
write.table(df_irf_int_rate,'df_irf_int_rate.csv', sep = ',')
#
impulse_to_int_rate <- df_irf_int_rate %>% 
  ggplot(aes(x = time_span)) +
  geom_line(aes(y = epu_int_rate, color = 'epu')) +
  geom_line(aes(y = macro_int_rate, color = 'macro')) +
  geom_line(aes(y = pca_int_rate, color = 'pca')) +
  geom_line(aes(y = tot_share_int_rate, color = 'tot_share')) +
  geom_line(aes(y = usd_int_rate, color = 'usd')) +
  geom_hline(yintercept = 0, color  = 'black') +
  scale_color_manual(name = '', values = c('epu' = 'blue', 'macro' = 'black', 'pca' = 'green',
                                           'tot_share' = 'grey','usd' = 'red')) + 
  theme_minimal() +
  theme(legend.position = 'bottom') + 
  labs(y = 'межбанковская процентная ставка, %', x = 'месяц')  

# (г) уровень инфляции
df_irf_inflation <- data.frame(
  time_span = 1:nrow(irf_epu_infation_list$irf$epu),
  epu_inflation = irf_epu_infation_list$irf$epu,
  macro_inflation = irf_macro_uncert_inflation_list$irf$uncert_feat,
  pca_inflation = irf_pca_inflation_list$irf$PC1,
  tot_share_inflation = irf_tot_share_inflation_list$irf$cycle_tot_share_price,
  usd_inflation = irf_usd_inflation_list$irf$usd_ruble_ex_cycle
)
# 
colnames(df_irf_inflation) <- c('time_span','epu_inflation',
                                'macro_inflation','pca_inflation','tot_share_inflation','usd_inflation')
# 
write.table(df_irf_inflation,'df_irf_inflation.csv', sep = ',')
# 
impulse_to_inflation <- df_irf_inflation %>% 
  ggplot(aes(x = time_span)) +
  geom_line(aes(y = epu_inflation, color = 'epu')) +
  geom_line(aes(y = macro_inflation, color = 'macro')) +
  geom_line(aes(y = pca_inflation, color = 'pca')) +
  geom_line(aes(y = tot_share_inflation, color = 'tot_share')) +
  geom_line(aes(y = usd_inflation, color = 'usd')) +
  geom_hline(yintercept = 0, color  = 'black') +
  scale_color_manual(name = '', values = c('epu' = 'blue', 'macro' = 'black', 'pca' = 'green',
                                           'tot_share' = 'grey','usd' = 'red')) + 
  theme_minimal() +
  theme(legend.position = 'bottom') + 
  labs(y = 'уровень инфляции, %', x = 'месяц')

# (д) индекс промышленного производства
df_irf_prod_manuf <- data.frame(
  time_span = 1:nrow(irf_epu_prod_manuf_list$irf$epu),
  epu_prod_manuf = irf_epu_prod_manuf_list$irf$epu,
  macro_prod_manuf = irf_macro_uncert_prod_manuf_list$irf$uncert_feat,
  pca_prod_manuf = irf_pca_prod_manuf_list$irf$PC1,
  tot_share_prod_manuf = irf_tot_share_prod_manuf_list$irf$cycle_tot_share_price,
  usd_prod_manuf = irf_usd_prod_manuf$irf$usd_ruble_ex_cycle
)
# 
colnames(df_irf_prod_manuf) <- c('time_span','epu_prod_manuf',
                                 'macro_prod_manuf','pca_prod_manuf','tot_share_prod_manuf','usd_prod_manuf')
# 
write.table(df_irf_prod_manuf,'df_irf_prod_manuf.csv', sep = ',')
# 
impulse_to_prod_manuf <- df_irf_prod_manuf %>% 
  ggplot(aes(x = time_span)) +
  geom_line(aes(y = epu_prod_manuf, color = 'epu')) +
  geom_line(aes(y = macro_prod_manuf, color = 'macro')) +
  geom_line(aes(y = pca_prod_manuf, color = 'pca')) +
  geom_line(aes(y = tot_share_prod_manuf, color = 'tot_share')) +
  geom_line(aes(y = usd_prod_manuf, color = 'usd')) +
  geom_hline(yintercept = 0, color  = 'black') +
  scale_color_manual(name = '', values = c('epu' = 'blue', 'macro' = 'black', 'pca' = 'green',
                                           'tot_share' = 'grey','usd' = 'red')) + 
  theme_minimal() +
  theme(legend.position = 'bottom') + 
  labs(y = 'индекс промышленного производства, %', x = 'месяц')

# отображаем инмульсные отлики
impulse_to_unempl
impulse_to_inflation
impulse_to_int_rate
impulse_to_prod_manuf
impulse_to_tot_share
# сохраняем долю объъсняемой дисперсии
fevd_epu <- fevd(var_epu, n.ahead = 24)
fevd_tot_share <- fevd(var_tot_share_price, n.ahead = 24)
fevd_macro <- fevd(var_macro_uncert, n.ahead = 24)
fevd_pca <- fevd(var_pca_all_uncer_feat, n.ahead = 24)
fevd_usd <- fevd(var_usd_ruble, n.ahead = 24)

# ---- проверка устойчивости: шок межбанковской процентной ставки на уровень безработицы ----
# фильтр Ходрика-Прескотта
interest_rate_3m_hp_filter <- hpfilter(interest_rate_3m$int_rate_3m, freq = 129600, type = 'lambda')
# выделяем циклическую компоненту
interest_rate_3m_cycle <- interest_rate_3m_hp_filter$cycle %>% data.frame(int_rate_uncert = ., date = interest_rate_3m$date)

# создаем базу данных
df_int_uncert <- merge(interest_rate_3m_cycle, tot_share_price_grw_rate)
df_int_uncert <- merge(df_int_uncert, infl_rate)
df_int_uncert <- merge(df_int_uncert, prod_manuf)
df_int_uncert <- merge(df_int_uncert, regist_unempl_rate)
# преобразуем во временные ряды
df_int_uncert_ts <- ts(df_int_uncert[,-1], start = c(2000, 2), frequency = 12)
# выбор порядка лага
VARselect(df_int_uncert_ts)
# формирование векторной авторегрессии
var_int_rate <- VAR(df_int_uncert_ts, 2, type = 'none')
# шок циклической компоненты процентной ставки на уровень безработицы
irf_int_rate_unempl <- irf(var_int_rate, impulse = 'int_rate_uncert', response = 'regist_unempl', n.ahead = 24, cumulative = FALSE)
# создание базы данных импульсных откликов уровня безработицы на шок межбанковской процентной ставки
df_irf_int_rate_uncert <- data.frame(
  time_span = 1:nrow(irf_int_rate_unempl$irf$int_rate_uncert),
  int_rate_cycle = irf_int_rate_unempl$irf$int_rate_uncert,
  lower_bound = irf_int_rate_unempl$Lower$int_rate_uncert,
  upper_bound = irf_int_rate_unempl$Upper$int_rate_uncert
)
colnames(df_irf_int_rate_uncert) <- c('time_span','main_bound','lower_bound','upper_bound')
# сохранение результатов
write.table(df_irf_int_rate_uncert, 'df_irf_int_rate_uncert.csv', sep = ',')

fevd_epu$regist_unempl
fevd_macro$regist_unempl
fevd_tot_share$regist_unempl
fevd_usd$regist_unempl
fevd_pca$regist_unempl
df_whole <- merge(df_epu, df_usd_ruble)
df_whole <- merge(df_whole, df_tot_share_price)
df_whole <- merge(df_whole, df_macro_uncert)
df_whole <- merge(df_whole, df_pca_all_uncert_feat)
write.table(df_whole, 'df_whole.csv', sep = ',')


