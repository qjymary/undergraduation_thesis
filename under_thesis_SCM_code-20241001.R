# 加载所需库
library(MSCMT)
library(dplyr)
library(ggplot2)
library(tidysynth)

# 读取新能源汽车数据
nev <- read.csv("E:\\qjy\\ecnu\\毕业论文\\新能源汽车数据\\中国城市 SCM-年度-全\\中国城市 SCM-合并数据.csv", header = TRUE)

# 对GDP和收入列进行对数变换
nev$logGDP_per <- log10(nev$GDP_per)
nev$logincome_per <- log10(nev$income_per)

# 计算其他城市的新能源汽车累计推广数量的年均值
other_cities_mean <- nev %>%
  filter(city %in% c("chongqing", "changchun", "wuhan", "nantong", "shenyang", "zhengzhou", "hefei", "chengdu", "dalian", "xiangyang")) %>%
  filter(year >= 2010 & year <= 2018) %>%
  group_by(year) %>%
  summarise(mean_nev_cum = mean(nev_cum, na.rm = TRUE))

# 创建深圳和天津的数据框
xiangou_data <- nev %>%
  filter(city %in% c("shenzhen", "tianjin")) %>%
  filter(year >= 2010 & year <= 2018) %>%
  group_by(year) %>%
  summarise(mean_nev_cum = mean(nev_cum, na.rm = TRUE))

# 合并限购城市和其他城市的年均数据，准备绘图
xiangou_data$group <- "限购城市"
other_cities_mean$group <- "其他城市"
combined_data <- rbind(xiangou_data, other_cities_mean)

# 绘制限购城市和其他城市的新能源汽车累计推广数量变化
ggplot(combined_data, aes(x = year, y = mean_nev_cum, group = group, color = group)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 2015, linetype = "dashed", color = "red") + # 添加2015年竖线
  annotate("text", x = 2015, y = max(combined_data$mean_nev_cum, na.rm = TRUE), label = "车牌限购", hjust = 1.1, vjust = 1.1) +
  theme_minimal() +
  labs(title = "限购城市与其他城市 NEV 累计推广数量随年份的变化",
       x = "年份",
       y = "NEV 累计推广数量",
       color = "城市类别") +
  scale_x_continuous(breaks = seq(2010, 2018, 1))

# 深圳车牌限购政策，筛选试验组和控制组
nev_shenzhen <- nev %>%
  filter(city %in% c("chongqing", "changchun", "wuhan", "nantong", "shenyang", "zhengzhou", "hefei", "chengdu", "dalian", "xiangyang", "shenzhen")) %>%
  filter(year >= 2010 & year <= 2018)

nev_out <- nev_shenzhen %>%
  synthetic_control(outcome = nev_cum, 
                    unit = city, 
                    time = year, 
                    i_unit = "shenzhen", 
                    i_time = 2014, 
                    generate_placebos = TRUE) %>%
  generate_predictor(time_window = 2010:2014, 
                     logGDP_per = mean(logGDP_per, na.rm = TRUE), 
                     logincome_per = mean(logincome_per, na.rm = TRUE), 
                     population = mean(population, na.rm = TRUE)) %>%
  generate_predictor(time_window = 2011:2014, 
                     temp_low = mean(temp_low, na.rm = TRUE), 
                     temp_high = mean(temp_high, na.rm = TRUE)) %>%
  generate_predictor(time_window = 2013:2014, 
                     oil_price = mean(oil_price92, na.rm = TRUE)) %>%
  generate_predictor(time_window = 2014, 
                     nev_cum_2014 = nev_cum) %>%
  generate_predictor(time_window = 2013, 
                     nev_cum_2013 = nev_cum) %>%
  generate_predictor(time_window = 2012, 
                     nev_cum_2012 = nev_cum) %>%
  generate_weights(optimization_window = 2010:2014, 
                   margin_ipop = 0.02, sigf_ipop = 7, bound_ipop = 6) %>%
  generate_control()

# 绘制深圳车牌限购政策效果
nev_out %>% plot_trends()
nev_out %>% plot_differences()
nev_out %>% plot_weights()
nev_out %>% grab_unit_weights()
nev_out %>% grab_synthetic_control()
nev_out %>% grab_balance_table()
nev_out %>% plot_mspe_ratio()
nev_out %>% plot_placebos()

# 天津车牌限购政策
nev_tianjin <- nev %>%
  filter(city %in% c("chongqing", "changchun", "wuhan", "nantong", "shenyang", "zhengzhou", "hefei", "chengdu", "dalian", "xiangyang", "tianjin")) %>%
  filter(year >= 2010 & year <= 2018)

nev_out <- nev_tianjin %>%
  synthetic_control(outcome = nev_cum, 
                    unit = city, 
                    time = year, 
                    i_unit = "tianjin", 
                    i_time = 2014, 
                    generate_placebos = TRUE) %>%
  generate_predictor(time_window = 2010:2014, 
                     logGDP_per = mean(logGDP_per, na.rm = TRUE), 
                     logincome_per = mean(logincome_per, na.rm = TRUE), 
                     population = mean(population, na.rm = TRUE)) %>%
  generate_predictor(time_window = 2011:2014, 
                     temp_low = mean(temp_low, na.rm = TRUE), 
                     temp_high = mean(temp_high, na.rm = TRUE)) %>%
  generate_predictor(time_window = 2013:2014, 
                     oil_price = mean(oil_price92, na.rm = TRUE)) %>%
  generate_predictor(time_window = 2014, 
                     nev_cum_2014 = nev_cum) %>%
  generate_predictor(time_window = 2013, 
                     nev_cum_2013 = nev_cum) %>%
  generate_predictor(time_window = 2012, 
                     nev_cum_2012 = nev_cum) %>%
  generate_weights(optimization_window = 2010:2014, 
                   margin_ipop = 0.02, sigf_ipop = 7, bound_ipop = 6) %>%
  generate_control()

# 绘制天津车牌限购政策效果
nev_out %>% plot_trends()
nev_out %>% plot_differences()
nev_out %>% plot_weights()
nev_out %>% grab_unit_weights()
nev_out %>% grab_synthetic_control()
nev_out %>% grab_balance_table()
nev_out %>% plot_mspe_ratio()
nev_out %>% plot_placebos()

# 成都停车优惠政策
nev_chengdu <- nev %>%
  filter(city %in% c("chongqing", "changchun", "wuhan", "shenyang", "zhengzhou", "dalian", "xiangyang", "chengdu")) %>%
  filter(year >= 2010 & year <= 2020)

nev_out <- nev_chengdu %>%
  synthetic_control(outcome = nev_cum, 
                    unit = city, 
                    time = year, 
                    i_unit = "chengdu", 
                    i_time = 2017, 
                    generate_placebos = TRUE) %>%
  generate_predictor(time_window = 2010:2017, 
                     logGDP_per = mean(logGDP_per, na.rm = TRUE), 
                     logincome_per = mean(logincome_per, na.rm = TRUE), 
                     population = mean(population, na.rm = TRUE)) %>%
  generate_predictor(time_window = 2013:2017, 
                     oil_price = mean(oil_price92, na.rm = TRUE)) %>%
  generate_predictor(time_window = 2017, 
                     nev_cum_2017 = nev_cum) %>%
  generate_predictor(time_window = 2016, 
                     nev_cum_2016 = nev_cum) %>%
  generate_predictor(time_window = 2015, 
                     nev_cum_2015 = nev_cum) %>%
  generate_weights(optimization_window = 2010:2017, 
                   margin_ipop = 0.02, sigf_ipop = 7, bound_ipop = 6) %>%
  generate_control()

# 绘制成都停车优惠政策效果
nev_out %>% plot_trends()
nev_out %>% plot_differences()
nev_out %>% plot_weights()
nev_out %>% grab_unit_weights()
nev_out %>% grab_synthetic_control()
nev_out %>% grab_balance_table()
nev_out %>% plot_mspe_ratio()
nev_out %>% plot_placebos()
