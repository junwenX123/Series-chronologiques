# ============================================================================
# ============================================================================
# ==                    Analyse de Séries Temporelles                       ==
# ============================================================================
# ============================================================================

# 加载并处理必要的包
packages <- c("ggplot2",       ## 用于画图（时间序列曲线，ACF/PACF，预测vs真实）
              "dplyr",         ## 数据清洗与变换
              "tidyr",         ## 数据结构整理
              "lubridate",     ## 日期时间处理器（提取年月日，计算时间差）
              "forecast",      ## ARMA/ARIMA/SARIMA模型预测 + 置信区间
              "tseries",       ## 统计检验（ADF，KPSS检验）
              "gridExtra",     ## 多图排版（一个页面放多个图）
              "scales",        ## 坐标轴与刻度格式化
              "zoo",           ## 不规则时间序列类（移动平均，插值、填补缺失值）
              "xts",           ## 基于zoo的金融时间序列扩展（高频金融数据，带时间索引的矩阵）
              "ggthemes",      ## 画图主题风格扩展
              "reshape2",      ## 老版本数据reshape(现在多用tidyr,为了兼容加载)
              "corrplot",      ## 相关系数矩阵可视化
              "GGally")        ## 相关系数矩阵可视化

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    ## require()尝试加载一个包，成功返回TRUE，失败返回FALSE，比library()更适合循环
    ## character.only表示pkg是字符串，quietly表示不显示加载提示信息，保持控制台干净
    
    install.packages(pkg, repos = "https://cloud.r-project.org/", quiet = TRUE)
    library(pkg, character.only = TRUE, quietly = TRUE)
    ## 如果加载失败，就进行下载以及加载
    ## repos指定CRAN镜像，避免请选择一个镜像的弹窗，在不同电脑跑不出来
  }
}

# ============================================================================
# ==             1. CHARGEMENT ET PRÉPARATION DES DONNÉES                   ==
# ============================================================================

# ----------------------------------------------------------------------------
# 加载数据集
# ----------------------------------------------------------------------------
data <- read.csv("energydata_complete.csv", stringsAsFactors = FALSE)
## stringsAsFactors = FALSE 这里保证加载时所有文本列的格式为character而不是factor
## 如果错误地转换成factor，时间转换as.Date容易错，ggplot时间轴会混乱，建模时会被当作类别变量而不是时间变量


# ----------------------------------------------------------------------------
# 把数据集中date列转换成标准的datetime格式
# ----------------------------------------------------------------------------
data$date <- as.POSIXct(data$date, format = "%Y-%m-%d %H:%M:%S")
## as语法：属于R自带的类型转换函数族，不需加载包可以直接使用
## as.POSIXct将对象转换成时间格式，后面对应四位年份-月-日 小时：分钟：秒
## 只有转换成POSIXct以后R才能把数据当成时间数据而不是类别数据，可以进行排序、画时间轴、做时间差或滞后


# ----------------------------------------------------------------------------
# 删除时间列为NA的行
# ----------------------------------------------------------------------------
data <- data[!is.na(data$date), ]
## 注意这里的逗号不能省略，中括号中的参数表示对行和列进行筛选
## data[行索引，列索引]中索引可以是：1.由True和False组成的列表，选取True的部分保留 2.数值，对应某一行某一列 3.行名或列名
## 这里!is.na(data$date)是第一种情况，表示不为NA的行；逗号后面为空表示所有列


# ----------------------------------------------------------------------------
# 选取出变量为数值类型变量的列
# ----------------------------------------------------------------------------
numeric_cols <- c("Appliances",    ## 家用电器总耗电量（Wh），时间序列目标变量
                  "lights",        ## 灯光耗电量（Wh），室内照明功率
                  "T1",            ## 室内温度传感器1号（°C）
                  "RH_1",          ## 室内相对湿度传感器1号（%）
                  "T2",            ## 室内温度传感器2号（°C）
                  "RH_2",          ## 室内相对湿度传感器2号（%）
                  "T3",            ## 室内温度传感器3号（°C）
                  "RH_3",          ## 室内相对湿度传感器3号（%）
                  "T4",            ## 室内温度传感器4号（°C）
                  "RH_4",          ## 室内相对湿度传感器4号（%）
                  "T5",            ## 室内温度传感器5号（°C）
                  "RH_5",          ## 室内相对湿度传感器5号（%）
                  "T6",            ## 室内温度传感器6号（°C）
                  "RH_6",          ## 室内相对湿度传感器6号（%）
                  "T7",            ## 室内温度传感器7号（°C）
                  "RH_7",          ## 室内相对湿度传感器7号（%）
                  "T8",            ## 室内温度传感器8号（°C）
                  "RH_8",          ## 室内相对湿度传感器8号（%）
                  "T9",            ## 室内温度传感器9号（°C）
                  "RH_9",          ## 室内相对湿度传感器9号（%）
                  "T_out",         ## 室外温度（°C）
                  "Press_mm_hg",   ## 大气压力（mmHg）
                  "RH_out",        ## 室外相对湿度（%）
                  "Windspeed",     ## 风速（m/s）
                  "Visibility",    ## 能见度（m）
                  "Tdewpoint")     ## 露点温度（°C），空气湿度指标

for (col in numeric_cols) {
  if (col %in% names(data)) {                                       ## %in% 返回 TRUE / FALSE，防止出现列名拼写错误或缺失列导致报错
    data[[col]] <- as.numeric(trimws(as.character(data[[col]])))    ## data[[]]是访问列的标准方法，与data$列名类似，但$不适合用变量名访问
  }
}
## as.character(data[[col]]将对象先转换成字符串确保数值正确
## trimws()是去掉字符串首尾的空格，防止as.numeric()出错或者返回NA
## 这里是进行数据清洗的一步，确保所有的数值变量都为numeric，保证能够进行时间序列建模


# ----------------------------------------------------------------------------
# 创建时间序列变量
# ----------------------------------------------------------------------------
data$Hour <- hour(data$date)                                                  ## hour()：提取时间中的小时（0~23），记录每条观测的小时，便于分析一天内的耗电模式
data$Day <- day(data$date)                                                    ## day()：提取日期中的日（1~31），可用于按天聚合或计算日均耗电
data$Month <- month(data$date)                                                ## month()：提取月份（1~12），可以分析季节性模式（冬天和夏天的耗电差异）
data$DayOfWeek <- wday(data$date)                                             ## wday()：提取星期几（默认 1 = 星期日，2 = 星期一 … 7 = 星期六），可分析工作日与周末耗电差异
data$NSM <- hour(data$date) * 3600 + minute(data$date) * 60                   ## NSM = Number of Seconds from Midnight（从午夜开始的秒数），用于建模一天内的连续时间特征
data$WeekStatus <- ifelse(data$DayOfWeek %in% c(1, 7), "Weekend", "Weekday")  ## 星期日或者星期六特征命名为weekend，其他星期特征命名为weekday，方便分析周末跟周日不同耗电模式
## 能够进行这样操作的前提是已经将date列的变量变为POSIXct类型的日期数据


cat("Dimensions des données:", dim(data), "\n")                     ## 输出数据集的维度（行数，列数）
cat("Période:", as.character(min(data$date, na.rm = TRUE)), "à", 
    as.character(max(data$date, na.rm = TRUE)), "\n")               ## 输出数据集的时间跨度
## as.character将对象转变为字符串，na.rm = True表示忽略缺失值





# ============================================================================
# ==                     2. ANALYSE EXPLORATOIRE                            ==
# ============================================================================

# ----------------------------------------------------------------------------
# 自定义ggplot2的绘图主题（创建一个新的变量来储存自定义主题）
# ----------------------------------------------------------------------------
theme_article <- theme_minimal() +                           ## ggplot2内置的简约主题
  theme(
    plot.title = element_text(size = 12, face = "bold"),     ## 设置图表标题文字大小=12，加粗
    axis.title = element_text(size = 10),                    ## 坐标轴文字大小=10
    axis.text = element_text(size = 9),                      ## 坐标轴刻度文字大小=9
    legend.position = "bottom",                              ## 图例位置放在底部
    panel.grid.minor = element_blank()                       ## 去掉次级网格线
  )
## ggplot2使用分层的逻辑，+代表图形组件对象的叠加，加号前后的顺序有意义，后面的设置会覆盖前面的设置


# ----------------------------------------------------------------------------
# 绘制整个时间序列的电器耗电量趋势图（Figure 7a）
# ----------------------------------------------------------------------------
fig7a <- ggplot(data, aes(x = date, y = Appliances)) +                             ## 基础图层，指定x轴为date，y轴为Appliances
  geom_line(color = "black", linewidth = 0.3) +                                    ## 绘制折线图，颜色为黑色，线宽0.3
  labs(x = "Time", y = "Appliances Wh") +                                          ## 坐标轴标签
  scale_x_datetime(date_labels = "%b", date_breaks = "1 month") +                  ## scale_x_datetime() 专门处理时间类型的 x 轴；date_labels = "%b" → 显示月份缩写（Jan, Feb, Mar…）；date_breaks = "1 month" → 每个月显示一个刻度
  scale_y_continuous(limits = c(0, 1100), breaks = seq(0, 1000, 200)) +            ## limits = c(0, 1100) → y 轴范围 0~1100 Wh；breaks = seq(0, 1000, 200) → 刻度为 0、200、400、600、800、1000
  theme_article
ggsave("fig7a_consumption_profile.png", fig7a, width = 12, height = 4, dpi = 300)  ## ggsave()保存图像到本地
cat("Figure 7A sauvegardée\n")                                                     ## 控制台输出提示信息：图像已保存
print(fig7a)


# ----------------------------------------------------------------------------
# 绘制第一周时间序列的电器耗电量趋势图（Figure 7b）
# ----------------------------------------------------------------------------
# 计算第一周的最后一天日期
min_date <- min(data$date, na.rm = TRUE)
end_first_week <- min_date + 7 * 24 * 60 * 60                                      ## 以秒为单位加上一周的时间

first_week <- data[data$date >= min_date & data$date < end_first_week, ]           ## 取出data中的第一周的数据
cat("Nombre d'observations première semaine:", nrow(first_week), "\n")             ## 打印第一周包含的观测点数量，用来确认数据是否连续

fig7b <- ggplot(first_week, aes(x = date, y = Appliances)) +
  geom_line(color = "black", linewidth = 0.3) +
  labs(x = "Time (1 week)", y = "Appliances Wh") +
  scale_y_continuous(limits = c(0, 1100), breaks = seq(0, 1000, 200)) +
  theme_article
ggsave("fig7b_first_week.png", fig7b, width = 12, height = 4, dpi = 300)
cat("✓ Figure 7B sauvegardée\n")
print(fig7b)


# ----------------------------------------------------------------------------
# 绘制耗电量的直方图（fig8_hist）
# ----------------------------------------------------------------------------
fig8_hist <- ggplot(data, aes(x = Appliances)) +                            ## X轴是能耗（单位Wh），Y轴默认是频数
  geom_histogram(binwidth = 50, fill = "gray70", color = "black") +         ## 箱形图每根柱子代表50Wh的区间
  labs(x = "Appliances Wh", y = "Frequency") +
  scale_x_continuous(limits = c(0, 1200), breaks = seq(0, 1200, 200)) +     ## 限制X轴的范围在0-1200Wh，刻度每200Wh一个
  theme_article
print(fig8_hist)

# ----------------------------------------------------------------------------
# 绘制耗电量的箱线图（fig8_box）来描述能耗分布的集中趋势、离散程度和异常值
# ----------------------------------------------------------------------------
fig8_box <- ggplot(data, aes(y = Appliances)) +                             ## 只指定用来描述箱子长度的变量y
  geom_boxplot(fill = "steelblue", width = 0.5, outlier.size = 1) +
  labs(x = "", y = "Appliances Wh") +
  scale_y_continuous(limits = c(0, 1200), breaks = seq(0, 1200, 200)) +
  coord_flip() +                                                            ## 将箱线图的方向由竖直翻转到水平
  theme_article +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
print(fig8_box)

# ----------------------------------------------------------------------------
# 将直方图与箱线图上下拼接成一张图并保存(fig8)
# ----------------------------------------------------------------------------
fig8 <- grid.arrange(fig8_hist, fig8_box, ncol = 1, heights = c(3, 1))
ggsave("fig8_histogram_boxplot.png", fig8, width = 10, height = 6, dpi = 300)
cat("✓ Figure 8 sauvegardée\n")


# ----------------------------------------------------------------------------
# 绘制相关性矩阵(fig9)
# ----------------------------------------------------------------------------
# 只展示了最有代表性的变量
cor_vars <- c("Appliances", "lights", "T1", "RH_1", "T2", "RH_2", "T3", "RH_3")
cor_data <- data[, cor_vars]
cor_data <- cor_data[complete.cases(cor_data), ]
cor_matrix <- cor(cor_data)  # 计算相关系数矩阵## 删除缺失行
fig9 <- ggpairs(cor_data,
                upper = list(continuous = wrap("cor", size = 4)),                                                 ## 上三角写出相关系数
                lower = list(continuous = wrap("smooth", method = "lm", se = FALSE, size = 0.3, color = "red")),  ## 下三角画散点图与线性趋势
                diag = list(continuous = wrap("densityDiag", fill = "turquoise", color = "black")))               ## 对角线蓝绿色直方图

ggsave("fig9_pairs_plot.png", fig9, width = 10, height = 10, dpi = 300)
cat("✓ Figure 9 (pairs plot) sauvegardée\n")
print(fig9)
png("fig9_correlation.png", width = 800, height = 800, res = 100)
corrplot(cor_matrix, 
         method = "color", 
         type = "upper", 
         addCoef.col = "black", 
         tl.col = "black",
         title = "Matrice de Corrélation",
         mar = c(0,0,1,0))
dev.off() # 关闭设备，确保文件成功写入磁盘

cat("✓ Figure 9 (correlation matrix) sauvegardée\n")
# 提取所有与能耗Appliances有关变量的相关系数并将它们从大到小排序，最正相关在前，最负相关在后
cat("\n--- Corrélations avec Appliances ---\n")
print(sort(cor_matrix[, "Appliances"], decreasing = TRUE))


# ----------------------------------------------------------------------------
# 画出前四周不同星期 × 不同小时的热力图
# ----------------------------------------------------------------------------
# 通过图像判断：有没有日内规律（按小时分布的规律）；有没有周内规律（工作日与周末分布）；规律在不同周是否稳定
## 提取前四周的数据
end_4weeks <- min_date + 28 * 24 * 60 * 60
four_weeks <- data[data$date >= min_date & data$date < end_4weeks, ]
four_weeks$Week <- ceiling(as.numeric(difftime(four_weeks$date, min_date, units = "days")) / 7)
four_weeks$Week[four_weeks$Week == 0] <- 1
four_weeks$DayName <- weekdays(four_weeks$date, abbreviate = TRUE)

## 创建热力图
heatmap_data <- four_weeks %>%
  group_by(Week, DayName, Hour) %>%
  summarise(Appliances = sum(Appliances, na.rm = TRUE), .groups = 'drop')

fig10 <- ggplot(heatmap_data, aes(x = DayName, y = factor(Hour), fill = Appliances)) +
  geom_tile(color = "white") +
  scale_fill_gradientn(colors = c("yellow", "orange", "orangered", "red"),
                       limits = c(0, 4000), breaks = c(1000, 2000, 3000), name = "Appliances Wh") +
  facet_wrap(~Week, ncol = 4, labeller = labeller(Week = function(x) paste("Semaine", x))) +
  labs(x = "Jour de la semaine", y = "Heure") + coord_fixed(ratio = 1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8), axis.text.y = element_text(size = 7))
ggsave("fig10_heatmap.png", fig10, width = 10, height = 14, dpi = 300)
print(fig10)
cat("✓ Figure 10 sauvegardée\n")






# ============================================================================
# 3. ÉTUDE DE LA STATIONNARITÉ
# ============================================================================

cat("\n================================================================\n")
cat("3. ÉTUDE DE LA STATIONNARITÉ\n")
cat("================================================================\n")

# 把原始数据按照小时对齐，然后对把每个小时内的耗电量求和得到该时间段内的总耗电量
hourly_data <- data %>%
  mutate(date_hour = floor_date(date, "hour")) %>%
  group_by(date_hour) %>%
  summarise(Appliances = sum(Appliances, na.rm = TRUE), .groups = 'drop')

# 删除缺失值
hourly_data <- hourly_data[!is.na(hourly_data$Appliances), ]
# 把向量类型的对象hourly_data转换成时间序列对象,frequency = 24因为一天有24个小时（周期）
ts_appliances <- ts(hourly_data$Appliances, frequency = 24)

# 计算时间序列的统计量
cat("\n--- Statistiques de la série temporelle ---\n")
cat("Moyenne:", mean(ts_appliances, na.rm = TRUE), "\n")      ## 计算平均值
cat("Écart-type:", sd(ts_appliances, na.rm = TRUE), "\n")     ## 计算标准差
cat("Médiane:", median(ts_appliances, na.rm = TRUE), "\n")    ## 计算中位数


# ----------------------------------------------------------------------------
# Test ADF（Dicky-Fuller Augmenté)
# ----------------------------------------------------------------------------
cat("\n--- Test ADF (Dickey-Fuller Augmenté) ---\n")
adf_test <- adf.test(ts_appliances, alternative = "stationary")  ## alternative = "stationary"表示备择假设是序列平稳
print(adf_test)
## 检验的结果会给出一个p值，如果p < 0.05则拒绝原假设接受备择假设，即序列平稳
## 如果p >= 0.05则接受原假设，即序列不平稳


# ----------------------------------------------------------------------------
# Test KPSS（Kwiatkowski-Phillips-Schimidt-Shin）
# ----------------------------------------------------------------------------
cat("\n--- Test KPSS ---\n")
kpss_test <- kpss.test(ts_appliances)    ## 默认情况下KPSS原假设是序列平稳，备择假设是序列不平稳
print(kpss_test)
## KPSS检验输出结果：KPSS统计量、p值、滞后参数
## 如果p < 0.05则拒绝原假设，即序列不平稳；如果p >= 0.05则接受原假设，即序列平稳 


# ----------------------------------------------------------------------------
# 画自相关函数（ACF）和偏自相关函数（PACF）
# ----------------------------------------------------------------------------
max_lag <- 72  # 最大滞后阶数（72小时 = 3天）

## 计算ACF和PACF（plot = FALSE只返回数值不画图）
acf_result <- acf(as.numeric(ts_appliances), lag.max = max_lag, plot = FALSE)
pacf_result <- pacf(as.numeric(ts_appliances), lag.max = max_lag, plot = FALSE)

## 计算95%置信区间
n <- length(ts_appliances)
ci <- 1.96 / sqrt(n)

cat("\n--- Analyse ACF/PACF ---\n")
cat("nombre d'observasion:", n, "\n")
cat("Intervalle de confiance à 95%: ±", round(ci, 4), "\n")

## 创建数据框用于ggplot
acf_df <- data.frame(
  lag = as.numeric(acf_result$lag),
  acf = as.numeric(acf_result$acf)
)

pacf_df <- data.frame(
  lag = as.numeric(pacf_result$lag),
  pacf = as.numeric(pacf_result$acf)
)

## 绘制ACF图
fig_acf <- ggplot(acf_df, aes(x = lag, y = acf)) +
  ### 零线
  geom_hline(yintercept = 0, color = "gray50", linewidth = 0.5) +
  ### 置信区间
  geom_hline(yintercept = ci, color = "red", linetype = "dashed", linewidth = 0.7) +
  geom_hline(yintercept = -ci, color = "red", linetype = "dashed", linewidth = 0.7) +
  #### 柱状图
  geom_segment(aes(x = lag, xend = lag, y = 0, yend = acf), 
               color = "steelblue", linewidth = 0.8) +
  geom_point(color = "steelblue", size = 1.5) +
  ### 季节性标记线（24h, 48h, 72h）
  geom_vline(xintercept = 24, color = "green4", linetype = "dotted", linewidth = 0.7, alpha = 0.8) +
  geom_vline(xintercept = 48, color = "green4", linetype = "dotted", linewidth = 0.7, alpha = 0.8) +
  geom_vline(xintercept = 72, color = "green4", linetype = "dotted", linewidth = 0.7, alpha = 0.8) +
  ### 季节性标签
  annotate("text", x = 24, y = max(acf_df$acf) * 0.95, label = "24h", 
           color = "green4", size = 3.5, fontface = "bold") +
  annotate("text", x = 48, y = max(acf_df$acf) * 0.95, label = "48h", 
           color = "green4", size = 3.5, fontface = "bold") +
  annotate("text", x = 72, y = max(acf_df$acf) * 0.95, label = "72h", 
           color = "green4", size = 3.5, fontface = "bold") +
  ### 图例标注（置信区间）
  annotate("text", x = max_lag * 0.75, y = ci + 0.05, 
           label = paste0("95% CI (±", round(ci, 4), ")"), 
           color = "red", size = 3) +
  ### 坐标轴和标题
  scale_x_continuous(breaks = seq(0, max_lag, by = 12)) +
  labs(
    title = "ACF - Fonction d'Autocorrélation",
    x = "Lag (heures)",
    y = "ACF"
  ) +
  theme_article +
  coord_cartesian(xlim = c(-1, max_lag + 1))

# 绘制PACF图
fig_pacf <- ggplot(pacf_df, aes(x = lag, y = pacf)) +
  # 零线
  geom_hline(yintercept = 0, color = "gray50", linewidth = 0.5) +
  # 置信区间
  geom_hline(yintercept = ci, color = "red", linetype = "dashed", linewidth = 0.7) +
  geom_hline(yintercept = -ci, color = "red", linetype = "dashed", linewidth = 0.7) +
  # 柱状图
  geom_segment(aes(x = lag, xend = lag, y = 0, yend = pacf), 
               color = "steelblue", linewidth = 0.8) +
  geom_point(color = "steelblue", size = 1.5) +
  # 季节性标记线
  geom_vline(xintercept = 24, color = "green4", linetype = "dotted", linewidth = 0.7, alpha = 0.8) +
  geom_vline(xintercept = 48, color = "green4", linetype = "dotted", linewidth = 0.7, alpha = 0.8) +
  geom_vline(xintercept = 72, color = "green4", linetype = "dotted", linewidth = 0.7, alpha = 0.8) +
  # 季节性标签
  annotate("text", x = 24, y = max(pacf_df$pacf) * 0.95, label = "24h", 
           color = "green4", size = 3.5, fontface = "bold") +
  annotate("text", x = 48, y = max(pacf_df$pacf) * 0.95, label = "48h", 
           color = "green4", size = 3.5, fontface = "bold") +
  annotate("text", x = 72, y = max(pacf_df$pacf) * 0.95, label = "72h", 
           color = "green4", size = 3.5, fontface = "bold") +
  ### 图例标注
  annotate("text", x = max_lag * 0.75, y = ci + 0.03, 
           label = paste0("95% CI (±", round(ci, 4), ")"), 
           color = "red", size = 3) +
  ### 坐标轴和标题
  scale_x_continuous(breaks = seq(0, max_lag, by = 12)) +
  labs(
    title = "PACF - Fonction d'Autocorrélation Partielle",
    x = "Lag (heures)",
    y = "PACF"
  ) +
  theme_article +
  coord_cartesian(xlim = c(0, max_lag + 1))

## 合并ACF和PACF图并保存
fig_acf_pacf <- grid.arrange(fig_acf, fig_pacf, ncol = 2)
ggsave("fig_acf_pacf.png", fig_acf_pacf, width = 16, height = 5, dpi = 300, bg = "white")
print(fig_acf_pacf)
cat("✓ Figure ACF/PACF sauvegardée\n")

## 打印关键ACF/PACF值
cat("\n--- ACF key value ---\n")
key_lags <- c(1, 2, 3, 12, 24, 48, 72)
for (lag in key_lags) {
  acf_val <- acf_df$acf[acf_df$lag == lag]
  sig <- ifelse(abs(acf_val) > ci, "*", "")
  cat(sprintf("Lag %2d: %+.4f %s\n", lag, acf_val, sig))
}

cat("\n--- PACF key value ---\n")
for (lag in key_lags) {
  pacf_val <- pacf_df$pacf[pacf_df$lag == lag]
  sig <- ifelse(abs(pacf_val) > ci, "*", "")
  cat(sprintf("Lag %2d: %+.4f %s\n", lag, pacf_val, sig))
}


# -------------------------------------------------------------------------------
# 将时间序列分解为三个部分（Tend（趋势），Seasonal（季节性），Random（残差波动））
# -------------------------------------------------------------------------------
decomp <- decompose(ts_appliances, type = "additive")
png("fig_decomposition.png", width = 1200, height = 800, res = 100)
plot(decomp)
dev.off()
print(plot(decomp))
cat("✓ Figure de décomposition sauvegardée\n")




# ============================================================================
# 4. DIFFÉRENCIATION
# ============================================================================

cat("\n================================================================\n")
cat("4. DIFFÉRENCIATION\n")
cat("================================================================\n")

# ============================================================================
# 差分后的ACF/PACF图
# ============================================================================

# 先对时间序列进行一阶差分
diff_ts <- diff(ts_appliances, differences = 1)

# 对一阶差分后的时间序列进行ADF检验（判断差分后是否稳定）
cat("\n--- Test ADF sur la série différenciée (d=1) ---\n")
adf_diff <- adf.test(diff_ts, alternative = "stationary")
print(adf_diff)

# 计算差分后序列的ACF和PACF
# 注意：使用as.numeric()避免frequency导致横坐标问题
acf_diff_result <- acf(as.numeric(diff_ts), lag.max = max_lag, plot = FALSE)
pacf_diff_result <- pacf(as.numeric(diff_ts), lag.max = max_lag, plot = FALSE)

# 计算差分后序列的95%置信区间
n_diff <- length(diff_ts)
ci_diff <- 1.96 / sqrt(n_diff)

cat("差分后观测点数量:", n_diff, "\n")
cat("95%置信区间: ±", round(ci_diff, 4), "\n")

# 创建数据框用于ggplot
acf_diff_df <- data.frame(
  lag = as.numeric(acf_diff_result$lag),
  acf = as.numeric(acf_diff_result$acf)
)

pacf_diff_df <- data.frame(
  lag = as.numeric(pacf_diff_result$lag),
  pacf = as.numeric(pacf_diff_result$acf)
)

# 绘制差分后的ACF图
fig_acf_diff <- ggplot(acf_diff_df, aes(x = lag, y = acf)) +
  # 零线
  geom_hline(yintercept = 0, color = "gray50", linewidth = 0.5) +
  # 置信区间
  geom_hline(yintercept = ci_diff, color = "red", linetype = "dashed", linewidth = 0.7) +
  geom_hline(yintercept = -ci_diff, color = "red", linetype = "dashed", linewidth = 0.7) +
  # 柱状图
  geom_segment(aes(x = lag, xend = lag, y = 0, yend = acf), 
               color = "steelblue", linewidth = 0.8) +
  geom_point(color = "steelblue", size = 1.5) +
  # 季节性标记线（24h, 48h, 72h）
  geom_vline(xintercept = 24, color = "green4", linetype = "dotted", linewidth = 0.7, alpha = 0.8) +
  geom_vline(xintercept = 48, color = "green4", linetype = "dotted", linewidth = 0.7, alpha = 0.8) +
  geom_vline(xintercept = 72, color = "green4", linetype = "dotted", linewidth = 0.7, alpha = 0.8) +
  # 季节性标签
  annotate("text", x = 24, y = max(acf_diff_df$acf) * 0.95, label = "24h", 
           color = "green4", size = 3.5, fontface = "bold") +
  annotate("text", x = 48, y = max(acf_diff_df$acf) * 0.95, label = "48h", 
           color = "green4", size = 3.5, fontface = "bold") +
  annotate("text", x = 72, y = max(acf_diff_df$acf) * 0.95, label = "72h", 
           color = "green4", size = 3.5, fontface = "bold") +
  # 图例标注（置信区间）
  annotate("text", x = max_lag * 0.75, y = ci_diff + 0.05, 
           label = paste0("95% CI (±", round(ci_diff, 4), ")"), 
           color = "red", size = 3) +
  # 坐标轴和标题
  scale_x_continuous(breaks = seq(0, max_lag, by = 12)) +
  labs(
    title = "ACF - Série Différenciée (d=1)",
    x = "Lag (heures)",
    y = "ACF"
  ) +
  theme_article +
  coord_cartesian(xlim = c(-1, max_lag + 1))

# 绘制差分后的PACF图
fig_pacf_diff <- ggplot(pacf_diff_df, aes(x = lag, y = pacf)) +
  # 零线
  geom_hline(yintercept = 0, color = "gray50", linewidth = 0.5) +
  # 置信区间
  geom_hline(yintercept = ci_diff, color = "red", linetype = "dashed", linewidth = 0.7) +
  geom_hline(yintercept = -ci_diff, color = "red", linetype = "dashed", linewidth = 0.7) +
  # 柱状图
  geom_segment(aes(x = lag, xend = lag, y = 0, yend = pacf), 
               color = "steelblue", linewidth = 0.8) +
  geom_point(color = "steelblue", size = 1.5) +
  # 季节性标记线
  geom_vline(xintercept = 24, color = "green4", linetype = "dotted", linewidth = 0.7, alpha = 0.8) +
  geom_vline(xintercept = 48, color = "green4", linetype = "dotted", linewidth = 0.7, alpha = 0.8) +
  geom_vline(xintercept = 72, color = "green4", linetype = "dotted", linewidth = 0.7, alpha = 0.8) +
  # 季节性标签
  annotate("text", x = 24, y = max(pacf_diff_df$pacf) * 0.95, label = "24h", 
           color = "green4", size = 3.5, fontface = "bold") +
  annotate("text", x = 48, y = max(pacf_diff_df$pacf) * 0.95, label = "48h", 
           color = "green4", size = 3.5, fontface = "bold") +
  annotate("text", x = 72, y = max(pacf_diff_df$pacf) * 0.95, label = "72h", 
           color = "green4", size = 3.5, fontface = "bold") +
  # 图例标注
  annotate("text", x = max_lag * 0.75, y = ci_diff + 0.03, 
           label = paste0("95% CI (±", round(ci_diff, 4), ")"), 
           color = "red", size = 3) +
  # 坐标轴和标题
  scale_x_continuous(breaks = seq(0, max_lag, by = 12)) +
  labs(
    title = "PACF - Série Différenciée (d=1)",
    x = "Lag (heures)",
    y = "PACF"
  ) +
  theme_article +
  coord_cartesian(xlim = c(0, max_lag + 1))

# 合并差分后的ACF和PACF图并保存
fig_acf_pacf_diff <- grid.arrange(fig_acf_diff, fig_pacf_diff, ncol = 2)
ggsave("fig_acf_pacf_diff.png", fig_acf_pacf_diff, width = 16, height = 5, dpi = 300, bg = "white")
print(fig_acf_pacf_diff)
cat("✓ Figure ACF/PACF différenciée sauvegardée\n")

# 打印差分后关键ACF/PACF值
cat("\n--- ACF différenciée key value ---\n")
key_lags <- c(1, 2, 3, 12, 24, 48, 72)
for (lag in key_lags) {
  acf_val <- acf_diff_df$acf[acf_diff_df$lag == lag]
  sig <- ifelse(abs(acf_val) > ci_diff, "*", "")
  cat(sprintf("Lag %2d: %+.4f %s\n", lag, acf_val, sig))
}

cat("\n--- PACF différenciée key value ---\n")
for (lag in key_lags) {
  pacf_val <- pacf_diff_df$pacf[pacf_diff_df$lag == lag]
  sig <- ifelse(abs(pacf_val) > ci_diff, "*", "")
  cat(sprintf("Lag %2d: %+.4f %s\n", lag, pacf_val, sig))
}

# 比较原始序列和差分后序列的方差
cat("\n--- Comparaison des variances ---\n")
cat("Variance série originale:", round(var(ts_appliances), 2), "\n")
cat("Variance série différenciée (d=1):", round(var(diff_ts), 2), "\n")

if (var(diff_ts) > var(ts_appliances)) {
  cat("\n⚠️  ATTENTION: La variance AUGMENTE après différenciation!\n")
  cat("   Cela suggère une sur-différenciation → d=0 est préférable.\n")
} else {
  cat("\n✓ La variance diminue après différenciation.\n")
  cat("   La différenciation peut être justifiée.\n")
}



# ============================================================================
# 5. MODÉLISATION ARIMA
# ============================================================================

cat("\n================================================================\n")
cat("5. MODÉLISATION ARIMA\n")
cat("================================================================\n")

# ============================================================================
# 5.1 Méthode originale（原方法）
# ============================================================================

cat("\n------------------------------------------------------------\n")
cat("5.1 Méthode originale（原方法）\n")
cat("------------------------------------------------------------\n")

## 划分训练集和测试集
n <- length(ts_appliances)
train_size <- round(0.75 * n)
train_ts <- ts(ts_appliances[1:train_size], frequency = 24)
test_ts <- ts(ts_appliances[(train_size + 1):n], frequency = 24)
## 打印训练集和测试集大小，方便确认划分
cat("\nTaille ensemble d'entraînement:", train_size, "\n")
cat("Taille ensemble de test:", n - train_size, "\n")

# 自动选择ARIMA（p,d,q）（P,D,Q）[s]模型中的参数
cat("\n--- Sélection automatique du modèle ARIMA ---\n")
auto_model <- auto.arima(train_ts, seasonal = TRUE, stepwise = TRUE, 
                         trace = FALSE, approximation = FALSE)

cat("\n--- MODÈLE ARIMA SÉLECTIONNÉ ---\n")
print(summary(auto_model))                            ## 打印模型详细信息

# 残差诊断
png("fig_diagnostics.png", width = 1000, height = 800, res = 100)
checkresiduals(auto_model)                            ## 生成残差时序图，残差ACF图，Ljung-Box检验p值，残差直方图
dev.off()
print(checkresiduals(auto_model))
cat("✓ Figure de diagnostic sauvegardée\n")
## 如果残差像白噪声则模型拟合好；如果仍有自相关，则说明模型可能欠拟合

# Test Ljung-Box
cat("\n--- Test de Ljung-Box sur les résidus ---\n")
ljung_box <- Box.test(residuals(auto_model), lag = 20, type = "Ljung-Box")
print(ljung_box)


# ============================================================================
# 5.2 Méthode améliorée（改进方法）- 滚动预测模型
# ============================================================================

cat("\n------------------------------------------------------------\n")
cat("5.2 Méthode améliorée（改进方法）\n")
cat("------------------------------------------------------------\n")

## 改进说明：
## 原方法的问题：一次性预测整个测试集（约1700小时），ARIMA长期预测会收敛到均值，导致预测接近水平线
## 改进思路：使用滚动预测（Rolling Forecast），每次只预测短期（24小时），然后用真实值更新模型

## 使用与原方法相同的训练集/测试集划分
## train_ts 和 test_ts 已在5.1中定义

## 改进方法使用更全面的模型搜索（stepwise = FALSE）以更好地捕捉季节性
cat("\n--- Sélection du modèle ARIMA amélioré (recherche complète) ---\n")
auto_model_improved <- auto.arima(train_ts, 
                                  seasonal = TRUE, 
                                  stepwise = FALSE,    ## 更全面的搜索，可能找到更好的季节性模型
                                  approximation = FALSE,
                                  trace = FALSE)

cat("\n--- MODÈLE ARIMA AMÉLIORÉ ---\n")
print(summary(auto_model_improved))

# 残差诊断（改进模型）
png("fig_diagnostics_improved.png", width = 1000, height = 800, res = 100)
checkresiduals(auto_model_improved)
dev.off()
print(checkresiduals(auto_model_improved))
cat("✓ Figure de diagnostic (amélioré) sauvegardée\n")

# Test Ljung-Box（改进模型）
cat("\n--- Test de Ljung-Box sur les résidus (modèle amélioré) ---\n")
ljung_box_improved <- Box.test(residuals(auto_model_improved), lag = 20, type = "Ljung-Box")
print(ljung_box_improved)

## 比较两个模型的AIC
cat("\n--- Comparaison des modèles ---\n")
cat("AIC modèle original:", AIC(auto_model), "\n")
cat("AIC modèle amélioré:", AIC(auto_model_improved), "\n")
if (AIC(auto_model_improved) < AIC(auto_model)) {
  cat("→ Le modèle amélioré a un AIC plus faible (meilleur)\n")
} else {
  cat("→ Les deux modèles ont des performances similaires\n")
}



# ============================================================================
# 6. PRÉDICTION
# ============================================================================

cat("\n================================================================\n")
cat("6. PRÉDICTION\n")
cat("================================================================\n")


# ============================================================================
# 6.1 Prédiction - Méthode originale（原方法预测）
# ============================================================================

cat("\n------------------------------------------------------------\n")
cat("6.1 Prédiction - Méthode originale（原方法预测）\n")
cat("------------------------------------------------------------\n")

## 利用训练好的ARIMA模型对测试集进行预测
h <- length(test_ts)                                  ## h是预测步数，将h设置为测试集长度，意味着预测的小时数与测试集中的数据量相同
forecast_result <- forecast(auto_model, h = h)        ## 应用forecast方法使用模型进行预测

## 提取真实值和预测值（修复原代码中未定义的变量）
actual <- as.numeric(test_ts)
predicted <- as.numeric(forecast_result$mean)

## 计算预测性能指标RMSE（Root Mean Squared Error）和MAE（Mean Absolute Error）
rmse <- sqrt(mean((actual - predicted)^2, na.rm = TRUE))          ## 对大误差更敏感，单位与原序列一致，常用来衡量预测整体精度
mae <- mean(abs(actual - predicted), na.rm = TRUE)                ## 平均每小时预测差多少

## 计算MAPE (Mean Absolute Percentage Error) 平均相对误差（预测值平均偏离真实值多少百分比）
valid_idx <- which(actual != 0)
mape <- mean(abs((actual[valid_idx] - predicted[valid_idx]) / actual[valid_idx])) * 100
## 当数据可能接近0，波动很大，惩罚低值过重的时候，不适合用MAPE

## 计算R²
ss_res <- sum((actual - predicted)^2, na.rm = TRUE)
ss_tot <- sum((actual - mean(actual, na.rm = TRUE))^2, na.rm = TRUE)
r_squared <- 1 - (ss_res / ss_tot)
## R² = 1说明模型完美预测；R² = 0说明与用均值预测一样；R² < 0说明比用均值预测效果还差
## R²不是时间序列核心指标，主要还是看上面两个
## R²不考虑自相关结构与滞后依赖，对非平稳序列可能产生误导

cat("\n=== MÉTRIQUES DE PERFORMANCE (Méthode originale) ===\n")
cat("RMSE:", round(rmse, 2), "Wh\n")
cat("MAE:", round(mae, 2), "Wh\n")
cat("MAPE:", round(mape, 2), "%\n")
cat("R²:", round(r_squared, 4), "\n")

## 解释R² < 0的原因
if (r_squared < 0) {
  cat("\n⚠️  R² < 0 说明预测效果比用均值预测还差\n")
  cat("   原因：ARIMA长期预测（", h, "步）会收敛到序列均值，形成水平线\n")
  cat("   而真实值有大幅波动，导致预测误差很大\n")
}

# 画出预测值与真实值的对比图像
n_plot <- min(500, length(test_ts))
forecast_df <- data.frame(
  Time = 1:n_plot,
  Actual = actual[1:n_plot],
  Predicted = predicted[1:n_plot],
  Lower95 = as.numeric(forecast_result$lower[1:n_plot, 2]),
  Upper95 = as.numeric(forecast_result$upper[1:n_plot, 2])
)

fig_forecast <- ggplot(forecast_df, aes(x = Time)) +
  geom_ribbon(aes(ymin = Lower95, ymax = Upper95), fill = "lightblue", alpha = 0.5) +
  geom_line(aes(y = Actual, color = "Valeurs Réelles"), linewidth = 0.5) +
  geom_line(aes(y = Predicted, color = "Prédictions"), linewidth = 0.5, linetype = "dashed") +
  scale_color_manual(values = c("Valeurs Réelles" = "black", "Prédictions" = "red")) +
  labs(title = paste0("Prédiction ARIMA vs Valeurs Réelles (Méthode originale, R² = ", round(r_squared, 3), ")"),
       x = "Temps (heures)", y = "Appliances Wh", color = "") +
  theme_article
ggsave("fig_forecast.png", fig_forecast, width = 14, height = 5, dpi = 300)
print(fig_forecast)
cat("✓ Figure de prédiction sauvegardée\n")

# Graphique scatter: Prédictions vs Réelles
fig_scatter <- ggplot(forecast_df, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Prédictions vs Valeurs Réelles (Méthode originale)",
       x = "Valeurs Réelles (Wh)", y = "Prédictions (Wh)") +
  theme_article
ggsave("fig_scatter.png", fig_scatter, width = 8, height = 8, dpi = 300)
print(fig_scatter)
cat("✓ Figure scatter sauvegardée\n")


# ============================================================================
# 6.2 Prédiction - Méthode améliorée（改进方法预测）- 滚动预测
# ============================================================================

cat("\n------------------------------------------------------------\n")
cat("6.2 Prédiction - Méthode améliorée（改进方法预测）\n")
cat("------------------------------------------------------------\n")

## 滚动预测（Rolling Forecast）核心思想：
## 不是一次性预测整个测试集，而是每次只预测短期（如24小时）
## 然后用真实值更新训练集，重新拟合模型，再预测下一个短期
## 这样可以避免长期预测趋向均值的问题

## 预测步长（每次预测多少步）
forecast_horizon <- 24  ## 每次预测24小时（1天），这是ARIMA合理的预测范围
test_size_improved <- length(test_ts)

cat("\n滚动预测设置:\n")
cat("- 测试集大小:", test_size_improved, "小时\n")
cat("- 每次预测步长:", forecast_horizon, "小时\n")
cat("- 预测轮数:", ceiling(test_size_improved / forecast_horizon), "\n")

## 存储滚动预测结果
predicted_improved <- numeric(test_size_improved)
actual_improved <- as.numeric(test_ts)

## 执行滚动预测
current_train_end <- train_size

cat("\n开始滚动预测...\n")
for (i in seq(1, test_size_improved, by = forecast_horizon)) {
  ## 更新训练集（包含之前的真实值）
  current_train <- ts(ts_appliances[1:current_train_end], frequency = 24)
  
  ## 使用改进模型的阶数快速拟合（避免每次都重新搜索参数）
  current_model <- Arima(current_train, model = auto_model_improved)
  
  ## 预测下一个时间段
  steps_to_forecast <- min(forecast_horizon, test_size_improved - i + 1)
  current_forecast <- forecast(current_model, h = steps_to_forecast)
  
  ## 存储预测值
  end_idx <- min(i + forecast_horizon - 1, test_size_improved)
  predicted_improved[i:end_idx] <- as.numeric(current_forecast$mean)[1:(end_idx - i + 1)]
  
  ## 更新训练集终点（加入真实值）
  current_train_end <- current_train_end + steps_to_forecast
  
  ## 打印进度
  if (i %% 336 == 1) {  ## 每两周打印一次
    cat("已完成预测至第", end_idx, "小时 (", round(end_idx/test_size_improved*100, 1), "%)\n")
  }
}
cat("滚动预测完成!\n")

## 计算改进方法的预测性能指标
rmse_improved <- sqrt(mean((actual_improved - predicted_improved)^2, na.rm = TRUE))
mae_improved <- mean(abs(actual_improved - predicted_improved), na.rm = TRUE)

valid_idx_improved <- which(actual_improved != 0)
mape_improved <- mean(abs((actual_improved[valid_idx_improved] - predicted_improved[valid_idx_improved]) / 
                            actual_improved[valid_idx_improved])) * 100

ss_res_improved <- sum((actual_improved - predicted_improved)^2, na.rm = TRUE)
ss_tot_improved <- sum((actual_improved - mean(actual_improved, na.rm = TRUE))^2, na.rm = TRUE)
r_squared_improved <- 1 - (ss_res_improved / ss_tot_improved)

cat("\n=== MÉTRIQUES DE PERFORMANCE (Méthode améliorée - Rolling) ===\n")
cat("RMSE:", round(rmse_improved, 2), "Wh\n")
cat("MAE:", round(mae_improved, 2), "Wh\n")
cat("MAPE:", round(mape_improved, 2), "%\n")
cat("R²:", round(r_squared_improved, 4), "\n")

## 画出改进方法的预测图
forecast_df_improved <- data.frame(
  Time = 1:n_plot,
  Actual = actual_improved[1:n_plot],
  Predicted = predicted_improved[1:n_plot]
)

fig_forecast_improved <- ggplot(forecast_df_improved, aes(x = Time)) +
  geom_line(aes(y = Actual, color = "Valeurs Réelles"), linewidth = 0.5) +
  geom_line(aes(y = Predicted, color = "Prédictions (Rolling)"), linewidth = 0.5, alpha = 0.8) +
  scale_color_manual(values = c("Valeurs Réelles" = "black", "Prédictions (Rolling)" = "red")) +
  labs(title = paste0("Prédiction Rolling ARIMA vs Valeurs Réelles (R² = ", round(r_squared_improved, 3), ")"),
       x = "Temps (heures)", y = "Appliances Wh", color = "") +
  theme_article
ggsave("fig_forecast_improved.png", fig_forecast_improved, width = 14, height = 5, dpi = 300)
print(fig_forecast_improved)
cat("✓ Figure de prédiction (améliorée) sauvegardée\n")

## Scatter plot（改进方法）
fig_scatter_improved <- ggplot(forecast_df_improved, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.5, size = 1, color = "steelblue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = paste0("Prédictions vs Réelles (Méthode améliorée, R² = ", round(r_squared_improved, 3), ")"),
       x = "Valeurs Réelles (Wh)", y = "Prédictions (Wh)") +
  theme_article
ggsave("fig_scatter_improved.png", fig_scatter_improved, width = 8, height = 8, dpi = 300)
print(fig_scatter_improved)
cat("✓ Figure scatter (améliorée) sauvegardée\n")


# ============================================================================
# 6.3 Comparaison des deux méthodes（两种方法对比）
# ============================================================================

cat("\n------------------------------------------------------------\n")
cat("6.3 Comparaison des deux méthodes（两种方法对比）\n")
cat("------------------------------------------------------------\n")

## 性能指标对比表
cat("\n╔══════════════════════════════════════════════════════════════╗\n")
cat("║           COMPARAISON DES MÉTRIQUES DE PERFORMANCE           ║\n")
cat("╠══════════════════════════════════════════════════════════════╣\n")
cat("║  Métrique    │  Méthode originale  │  Méthode améliorée      ║\n")
cat("╠══════════════════════════════════════════════════════════════╣\n")
cat(sprintf("║  RMSE        │  %8.2f Wh        │  %8.2f Wh            ║\n", rmse, rmse_improved))
cat(sprintf("║  MAE         │  %8.2f Wh        │  %8.2f Wh            ║\n", mae, mae_improved))
cat(sprintf("║  MAPE        │  %8.2f %%         │  %8.2f %%             ║\n", mape, mape_improved))
cat(sprintf("║  R²          │  %8.4f          │  %8.4f              ║\n", r_squared, r_squared_improved))
cat("╚══════════════════════════════════════════════════════════════╝\n")

## 计算改进幅度
cat("\n--- Amélioration ---\n")
cat("RMSE减少:", round((rmse - rmse_improved) / rmse * 100, 1), "%\n")
cat("MAE减少:", round((mae - mae_improved) / mae * 100, 1), "%\n")
cat("R²提升:", round(r_squared_improved - r_squared, 4), "\n")

## 两种方法的预测对比图（最重要的可视化）
forecast_df_compare <- data.frame(
  Time = 1:n_plot,
  Actual = actual[1:n_plot],
  Original = predicted[1:n_plot],
  Improved = predicted_improved[1:n_plot]
)

fig_compare <- ggplot(forecast_df_compare, aes(x = Time)) +
  geom_line(aes(y = Actual, color = "Valeurs Réelles"), linewidth = 0.5) +
  geom_line(aes(y = Original, color = "Méthode originale"), linewidth = 0.5, linetype = "dashed") +
  geom_line(aes(y = Improved, color = "Méthode améliorée (Rolling)"), linewidth = 0.5, alpha = 0.8) +
  scale_color_manual(values = c("Valeurs Réelles" = "black", 
                                "Méthode originale" = "blue",
                                "Méthode améliorée (Rolling)" = "red")) +
  labs(title = "Comparaison: Méthode originale vs Méthode améliorée (Rolling Forecast)",
       subtitle = paste0("R² originale = ", round(r_squared, 3), " | R² améliorée = ", round(r_squared_improved, 3)),
       x = "Temps (heures)", y = "Appliances Wh", color = "") +
  theme_article +
  theme(legend.position = "bottom")
ggsave("fig_forecast_compare.png", fig_compare, width = 14, height = 6, dpi = 300)
print(fig_compare)
cat("✓ Figure de comparaison sauvegardée\n")

## 两种方法的散点图对比（并排显示）
scatter_original <- ggplot(forecast_df, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.3, size = 0.8, color = "blue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = paste0("Méthode originale (R² = ", round(r_squared, 3), ")"),
       x = "Valeurs Réelles (Wh)", y = "Prédictions (Wh)") +
  theme_article +
  coord_fixed(ratio = 1)

scatter_improved <- ggplot(forecast_df_improved, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.3, size = 0.8, color = "red") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = paste0("Méthode améliorée (R² = ", round(r_squared_improved, 3), ")"),
       x = "Valeurs Réelles (Wh)", y = "Prédictions (Wh)") +
  theme_article +
  coord_fixed(ratio = 1)

fig_scatter_compare <- grid.arrange(scatter_original, scatter_improved, ncol = 2)
ggsave("fig_scatter_compare.png", fig_scatter_compare, width = 14, height = 7, dpi = 300)
print(fig_scatter_compare)
cat("✓ Figure scatter comparaison sauvegardée\n")

