# 题目：
# 使用iris数据集，检验三种鸢尾花种类的花瓣长度(Petal.Length)是否有显著差异。

# 考点：

# 单因素方差分析的应用
# 多组比较的统计方法
# 方差分析的假设条件
# 事后多重比较的必要性

library(showtext)

# 设置中文显示
# 使用Mac系统自带的中文字体
# font_add("Heiti SC", "/System/Library/Fonts/STHeiti Light.ttc") # 黑体-简
# 或者尝试
font_add("PingFang SC", "/System/Library/Fonts/PingFang.ttc") # 苹方字体
# showtext_auto()

### 1. 数据准备
# 加载数据
data(iris)
# 查看数据结构
str(iris)
# 检查缺失值
sum(is.na(iris$Petal.Length))
# 查看因素变量的各个水平
levels(iris$Species)
# 确认样本量
table(iris$Species)

### 2. 探索性分析
# 按组计算描述性统计量
aggregate(Petal.Length ~ Species, data = iris, 
          FUN = function(x) c(n = length(x), 
                             mean = mean(x), 
                             sd = sd(x),
                             min = min(x),
                             max = max(x)))

# 使用tapply函数进行汇总
tapply(iris$Petal.Length, iris$Species, summary)
tapply(iris$Petal.Length, iris$Species, sd)

# 箱线图
boxplot(Petal.Length ~ Species, data = iris, 
        main = "三种鸢尾花的花瓣长度比较",
        xlab = "鸢尾花种类",
        ylab = "花瓣长度(cm)",
        col = c("lightblue", "lightgreen", "lightpink"))


### 3. 检验ANOVA假设
# 正态性检验 - Shapiro-Wilk检验
shapiro.test(iris$Petal.Length[iris$Species == "setosa"])
shapiro.test(iris$Petal.Length[iris$Species == "versicolor"])
shapiro.test(iris$Petal.Length[iris$Species == "virginica"])

# 方差齐性检验
bartlett.test(Petal.Length ~ Species, data = iris)