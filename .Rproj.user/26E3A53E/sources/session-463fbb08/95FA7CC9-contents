# 题目：
# 使用mtcars数据集，检验汽车的平均油耗(mpg)是否显著不同于20英里/加仑。

# 考点：
  # 单样本t检验的应用
  # 假设的设定（H₀与H₁）
  # p值的解读
  # 置信区间的计算与解释

# 加载必要的包
library(ggplot2)
library(showtext)
# pwr包用于功效分析
library(pwr)

# 设置中文显示
# 使用Mac系统自带的中文字体
font_add("Heiti SC", "/System/Library/Fonts/STHeiti Light.ttc") # 黑体-简
# 或者尝试
# font_add("PingFang SC", "/System/Library/Fonts/PingFang.ttc") # 苹方字体
showtext_auto()

# 加载数据
data(mtcars)

# 查看数据结构
str(mtcars)
summary(mtcars$mpg)

# 3.检查前提条件
# 样本是否来自正态分布或样本量是否足够大（中心极限定理）
# 样本是否独立

# 检查正态性
shapiro_test <- shapiro.test(mtcars$mpg)
print(shapiro_test)

# 创建QQ图检查正态性
qqnorm(mtcars$mpg)
qqline(mtcars$mpg, col = "red")

# 检查样本量是否足够大
# 1. 经验法则: 通常样本量n≥30被认为足够大，可以应用中心极限定理
n <- length(mtcars$mpg)
cat("样本量:", n, "\n")
if(n >= 30) {
  cat("根据经验法则(n≥30)，样本量足够大，可以应用中心极限定理\n")
} else {
  cat("样本量小于30，需要更严格地检验正态性假设\n")
}

# 2. 计算标准误，评估估计精度
sd_mpg <- sd(mtcars$mpg)
se_mpg <- sd_mpg/sqrt(n)
cat("标准误(SE):", se_mpg, "\n")
cat("相对标准误(SE/平均值):", se_mpg/mean(mtcars$mpg), "\n")

# 3. 功效分析 - 计算样本量对检测给定效应量的能力
# 假设我们想要检测的效应量d=0.5(中等效应)，显著性水平α=0.05，功效(1-β)=0.8
effect_size <- 0.5  # 中等效应量
power_val <- 0.8    # 常用功效值
alpha <- 0.05       # 显著性水平
required_n <- pwr.t.test(d = effect_size, sig.level = alpha, power = power_val, type = "one.sample")$n
cat("为检测效应量d=", effect_size, "，需要的样本量约为:", ceiling(required_n), "\n")

# 4. 当前样本的功效分析
actual_effect <- abs(mean(mtcars$mpg) - 20) / sd_mpg  # 实际效应量
actual_power <- pwr.t.test(d = actual_effect, n = n, sig.level = alpha, type = "one.sample")$power
cat("对于当前效应量d=", round(actual_effect, 4), "，实际功效为:", round(actual_power, 4), "\n")

# 检查样本独立性
# 样本独立性是基于数据收集方法，不能通过统计测试确定
# 假设mtcars数据集中的样本是独立的

# 5. 执行检验步骤

# 计算样本量
n <- length(mtcars$mpg)
cat("样本量:", n, "\n")

#  计算样本平均值
mean_mpg <- mean(mtcars$mpg)
cat("样本平均值:", mean_mpg, "\n")

#  计算样本标准差
sd_mpg <- sd(mtcars$mpg)
cat("样本标准差:", sd_mpg, "\n")

#  计算标准误(SE = s/√n)
se_mpg <- sd_mpg / sqrt(n)
cat("标准误:", se_mpg, "\n")

#  计算t统计量: t = (x̄ - μ₀)/SE
mu0 <- 20 # 假设的总体均值
t_stat <- (mean_mpg - mu0) / se_mpg
cat("t统计量:", t_stat, "\n")

#  确定自由度: df = n - 1
df <- n - 1
cat("自由度:", df, "\n")

#  计算p值
p_value <- 2 * pt(abs(t_stat), df, lower.tail = FALSE)
cat("p值:", p_value, "\n")

# 进行单样本t检验
t_test_result <- t.test(mtcars$mpg, mu = 20)
print(t_test_result)

# 	One Sample t-test

# data:  mtcars$mpg
# t = 0.08506, df = 31, p-value = 0.9328
# alternative hypothesis: true mean is not equal to 20
# 95 percent confidence interval:
#  17.91768 22.26357
# sample estimates:
# mean of x 
#  20.09062 
# 计算平均值用于绘图
mean_mpg <- mean(mtcars$mpg)
mu_value <- 20

# 绘制油耗分布直方图，使用ggplot2
p <- ggplot(mtcars, aes(x = mpg)) +
  geom_histogram(bins = 10, fill = "#ff8c00", color = "black") +
  geom_vline(xintercept = mu_value, color = "red", linetype = "dashed", size = 1) +
  geom_vline(xintercept = mean_mpg, color = "blue", linetype = "solid", size = 1) +
  annotate("text", x = mu_value + 1, y = 7, label = "Hypo Mean = 20", color = "red") +
  annotate("text", x = mean_mpg + 1, y = 6, label = paste("Sample Mean =", round(mean_mpg, 2)), color = "blue") +
  labs(title = "Car Mileage Distribution", 
       subtitle = paste("One-sample t-test: p-value =", round(t_test_result$p.value, 4)),
       x = "Mileage (mpg)", 
       y = "Count",
       caption = "Data Source: mtcars dataset") +
  theme_minimal()

# 显示图形
print(p)

# 保存图形
ggsave("car_mileage_distribution.png", p, width = 8, height = 6, dpi = 300)

# 如果仍有中文显示问题，尝试以下方案：
# 1. 使用英文避免中文编码问题
# 2. 尝试使用不同的图形设备保存
# 3. 使用Cairo包增强图形支持:
# library(Cairo)
# CairoPNG("car_mileage_distribution.png", width = 800, height = 600)
# print(p)
# dev.off()

# 基础绘图方法备选
par(family = "sans") # 使用无衬线字体
hist(mtcars$mpg, main = "Car Mileage Distribution", xlab = "Mileage (mpg)", col = "orange", border = "black")
abline(v = 20, col = "red", lwd = 2, lty = 2)
abline(v = mean_mpg, col = "blue", lwd = 2)