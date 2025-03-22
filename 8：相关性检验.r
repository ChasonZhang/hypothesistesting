# 题目：
# 使用mtcars数据集，检验汽车重量(wt)与油耗(mpg)之间是否存在显著的线性相关关系。

# 考点：

# 相关系数的计算与解释
# 相关性显著性检验的原理
# 散点图的构建与解读
# 线性关系与非线性关系的区分


# 加载数据
data(mtcars)

# 计算Pearson相关系数
cor_result <- cor.test(mtcars$wt, mtcars$mpg, method = "pearson")
print(cor_result)

# 计算Spearman等级相关系数
spearman_result <- cor.test(mtcars$wt, mtcars$mpg, method = "spearman")
print(spearman_result)

# 可视化散点图
plot(mtcars$wt, mtcars$mpg, 
     main = "汽车重量与油耗的关系",
     xlab = "重量(1000 lbs)", ylab = "油耗(mpg)",
     pch = 19, col = "blue")
abline(lm(mpg ~ wt, data = mtcars), col = "red")