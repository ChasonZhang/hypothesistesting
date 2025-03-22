# 题目：
# 使用HairEyeColor数据集，检验头发颜色和眼睛颜色是否相互独立。

# 考点：

# 卡方独立性检验的应用
# 列联表的构建与分析
# 期望频数与观察频数的比较
# 残差分析

# 加载数据
data(HairEyeColor)

# 将三维表转为二维表（合并性别维度）
hair_eye_table <- margin.table(HairEyeColor, c(1, 2))
print(hair_eye_table)

# 进行卡方独立性检验
chi_sq_result <- chisq.test(hair_eye_table)
print(chi_sq_result)

# 查看期望频数
print(chi_sq_result$expected)

# 查看残差
print(chi_sq_result$residuals)

# 可视化列联表
mosaicplot(hair_eye_table, 
           main = "头发颜色与眼睛颜色的关系",
           color = TRUE)