# 题目：
# 使用airquality数据集，比较5月和8月的臭氧水平(Ozone)是否有显著差异，不假设数据呈正态分布。

# 考点：
# 非参数检验的应用场景
# Wilcoxon秩和检验的原理
# 处理非正态分布数据
# 处理含有缺失值的数据集

# 加载数据
data(airquality)

# 查看数据结构
str(airquality)
summary(airquality)

# 提取5月和8月的数据
may_ozone <- airquality$Ozone[airquality$Month == 5]
aug_ozone <- airquality$Ozone[airquality$Month == 8]

# 移除NA值
may_ozone <- may_ozone[!is.na(may_ozone)]
aug_ozone <- aug_ozone[!is.na(aug_ozone)]

# 进行Wilcoxon秩和检验
wilcox_result <- wilcox.test(may_ozone, aug_ozone)
print(wilcox_result)

# 可视化
boxplot(may_ozone, aug_ozone, 
        names = c("5月", "8月"),
        main = "5月与8月臭氧水平比较",
        ylab = "臭氧水平(ppb)")