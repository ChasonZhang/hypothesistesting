# 题目：
# 使用mtcars数据集，比较手动挡(am=1)和自动挡(am=0)汽车的平均油耗(mpg)是否有显著差异。

# 考点：
# 独立样本t检验的应用
# 方差齐性检验的重要性
# 分组比较的基本方法
# 效应量计算

### 1. 明确研究问题和假设

# - **研究问题**：手动挡(am=1)和自动挡(am=0)汽车的平均油耗(mpg)是否有显著差异
# - **原假设(H₀)**：手动挡和自动挡汽车的平均油耗相等 (μ₁ = μ₂)
# - **备择假设(H₁)**：手动挡和自动挡汽车的平均油耗不相等 (μ₁ ≠ μ₂)

### 2. 数据准备

# 加载数据集
data(mtcars)

# 查看数据结构
str(mtcars)

# 将am转换为因子变量，便于后续分析
mtcars$am <- as.factor(mtcars$am)
levels(mtcars$am) <- c("自动挡", "手动挡")

# 查看分组的基本描述统计量
tapply(mtcars$mpg, mtcars$am, summary)
tapply(mtcars$mpg, mtcars$am, sd)       # 查看每组的SD，SD的意思是标准差，SD越小，样本越集中

### 3. 检验前提条件

#### 3.1 正态性检验

# 分组进行正态性检验
shapiro.test(mtcars$mpg[mtcars$am == "自动挡"]) 
shapiro.test(mtcars$mpg[mtcars$am == "手动挡"])

# 绘制Q-Q图检查正态性
par(mfrow = c(1, 2))
qqnorm(mtcars$mpg[mtcars$am == "自动挡"], main = "自动挡Q-Q图")
qqline(mtcars$mpg[mtcars$am == "自动挡"], col = "red")
qqnorm(mtcars$mpg[mtcars$am == "手动挡"], main = "手动挡Q-Q图")
qqline(mtcars$mpg[mtcars$am == "手动挡"], col = "red")

#### 3.2 方差齐性检验（Levene检验/F检验）

# 使用leveneTest进行方差齐性检验
library(car)
leveneTest(mpg ~ am, data = mtcars)

# 或使用var.test进行F检验
var.test(mpg ~ am, data = mtcars)

### 4. 执行独立样本t检验

# 如果方差齐性假设成立，使用等方差t检验
t.test(mpg ~ am, data = mtcars, var.equal = TRUE)

# 如果方差齐性假设不成立，使用Welch校正的t检验（不等方差）
t.test(mpg ~ am, data = mtcars, var.equal = FALSE)

### 6. 数据可视化

# 箱线图比较两组
boxplot(mpg ~ am, data = mtcars, 
        xlab = "变速器类型", ylab = "平均油耗(mpg)",
        main = "手动挡与自动挡汽车的平均油耗比较")

# 添加均值点和95%置信区间
library(gplots)
plotmeans(mpg ~ am, data = mtcars, 
          xlab = "变速器类型", ylab = "平均油耗(mpg)",
          main = "手动挡与自动挡汽车的平均油耗均值比较(95%置信区间)")

# 密度图
library(ggplot2)
ggplot(mtcars, aes(x = mpg, fill = am)) +
  geom_density(alpha = 0.5) +
  labs(title = "手动挡与自动挡汽车的油耗分布对比",
       x = "平均油耗(mpg)", y = "密度") +
  theme_classic()