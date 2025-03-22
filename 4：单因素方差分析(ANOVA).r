# 题目：
# 使用iris数据集，检验三种鸢尾花种类的花瓣长度(Petal.Length)是否有显著差异。

# 考点：

# 单因素方差分析的应用
# 多组比较的统计方法
# 方差分析的假设条件
# 事后多重比较的必要性

# 加载数据
data(iris)

# 查看数据结构
str(iris)
tapply(iris$Petal.Length, iris$Species, summary)

# 进行单因素方差分析
anova_result <- aov(Petal.Length ~ Species, data = iris)
summary(anova_result)

# 进行事后多重比较
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)

# 可视化
boxplot(Petal.Length ~ Species, data = iris, 
        main = "三种鸢尾花的花瓣长度比较",
        ylab = "花瓣长度(cm)")