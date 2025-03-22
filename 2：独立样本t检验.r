# 题目：
# 使用mtcars数据集，比较手动挡(am=1)和自动挡(am=0)汽车的平均油耗(mpg)是否有显著差异。

# 考点：
# 独立样本t检验的应用
# 方差齐性检验的重要性
# 分组比较的基本方法
# 效应量计算

# 将am变量转换为分类变量
mtcars$am <- as.factor(mtcars$am)
levels(mtcars$am) <- c("自动挡", "手动挡")

# 分组查看描述性统计
tapply(mtcars$mpg, mtcars$am, summary)

# 进行独立样本t检验
t_test_result <- t.test(mpg ~ am, data = mtcars, var.equal = FALSE)
print(t_test_result)

# 检查方差齐性
var_test <- var.test(mpg ~ am, data = mtcars)
print(var_test)

# 可视化比较
boxplot(mpg ~ am, data = mtcars, 
        main = "手动挡与自动挡汽车油耗比较",
        ylab = "油耗(mpg)")