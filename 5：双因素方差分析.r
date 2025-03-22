# 题目：
# 使用ToothGrowth数据集，检验维生素C的剂量(dose)和补充方式(supp)对豚鼠牙齿生长(len)的交互作用。

# 考点：

# 双因素方差分析的应用
# 交互效应的理解与解释
# 主效应与交互效应的区分
# 双因素实验设计的数据分析

# 加载数据
data(ToothGrowth)

# 将dose转换为因子变量
ToothGrowth$dose <- as.factor(ToothGrowth$dose)

# 查看数据结构
str(ToothGrowth)
head(ToothGrowth)

# 进行双因素方差分析
anova_result <- aov(len ~ supp * dose, data = ToothGrowth)
summary(anova_result)

# 可视化交互效应
interaction.plot(ToothGrowth$dose, ToothGrowth$supp, ToothGrowth$len,
                main = "剂量与补充方式对牙齿生长的交互作用",
                xlab = "剂量", ylab = "牙齿长度",
                trace.label = "补充方式")

# 分组绘制箱线图
boxplot(len ~ supp:dose, data = ToothGrowth,
        main = "不同剂量和补充方式下的牙齿生长",
        xlab = "补充方式:剂量", ylab = "牙齿长度")