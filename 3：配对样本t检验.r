# 题目：
# 使用sleep数据集，比较两种药物对睡眠时间的影响是否有显著差异。

# 考点：

# 配对样本t检验的应用场景
# 配对设计与独立样本设计的区别
# 数据重塑与处理
# 配对前后差异的计算

# 加载数据
data(sleep)

# 查看数据结构
str(sleep)
head(sleep)

# 进行配对样本t检验
t_test_result <- t.test(extra ~ group, data = sleep, paired = TRUE)
print(t_test_result)

# 计算配对差异
sleep_wide <- reshape(sleep, idvar = "ID", timevar = "group", direction = "wide")
sleep_wide$diff <- sleep_wide$extra.2 - sleep_wide$extra.1
summary(sleep_wide$diff)

# 可视化
boxplot(extra ~ group, data = sleep, 
        main = "两种药物对睡眠的影响", 
        names = c("药物1", "药物2"),
        ylab = "额外睡眠时间(小时)")