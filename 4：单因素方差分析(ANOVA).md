# 题目：
# 使用iris数据集，检验三种鸢尾花种类的花瓣长度(Petal.Length)是否有显著差异。

# 考点：

# 单因素方差分析的应用
# 多组比较的统计方法
# 方差分析的假设条件
# 事后多重比较的必要性

# 单因素方差分析(ANOVA)完整分析思路与代码实现

## 一、基本概念

单因素方差分析(One-way ANOVA)是用于比较三个或更多独立组均值差异的统计方法。与t检验相比，ANOVA能够在一次分析中比较多个组，有效控制总体的第一类错误率。

ANOVA的核心思想是将总变异分解为组间变异和组内变异，通过比较这两种变异来判断组间差异是否显著。如果组间变异显著大于组内变异，则可以拒绝"所有组均值相等"的零假设。

## 二、应用场景

单因素方差分析适用于以下场景：
1. **比较多个组的均值**：当需要比较3个或更多组的均值差异时
2. **单一分类变量的影响**：研究一个分类变量（因素）对连续型结果变量的影响
3. **实验设计**：在完全随机设计中评估不同处理的效果
4. **产品比较**：比较多个产品、方法或处理的效果

## 三、统计假设

- **零假设(H₀)**：所有组的总体均值相等（μ₁ = μ₂ = ... = μₖ）
- **备择假设(H₁)**：至少有一组的总体均值与其他组不同

## 四、ANOVA的基本假设

单因素方差分析的有效性基于以下假设：
1. **独立性**：样本是相互独立的（特别是组间独立）
2. **正态性**：每组内的数据近似服从正态分布
3. **方差齐性**：各组的总体方差相等（同方差性）

## 五、逐步分析与代码实现

### 1. 数据准备

```r
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
```

### 2. 探索性分析

```r
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
```

### 3. 检验ANOVA假设

```r
# 正态性检验 - Shapiro-Wilk检验
shapiro.test(iris$Petal.Length[iris$Species == "setosa"])
shapiro.test(iris$Petal.Length[iris$Species == "versicolor"])
shapiro.test(iris$Petal.Length[iris$Species == "virginica"])

# 方差齐性检验
bartlett.test(Petal.Length ~ Species, data = iris)
```

### 4. 执行方差分析

```r
# 进行单因素方差分析
anova_result <- aov(Petal.Length ~ Species, data = iris)
summary(anova_result)

# 获取更详细的分析结果
anova_details <- summary(anova_result)[[1]]
print(anova_details)

# 手动计算效应量 - Eta平方(η²)
eta_squared <- anova_details[1, "Sum Sq"] / sum(anova_details[, "Sum Sq"])
print(paste("Eta平方(η²) =", round(eta_squared, 4)))
```

### 5. 事后多重比较

```r
# Tukey's HSD检验
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)

# 可视化Tukey检验结果
plot(tukey_result, las = 1)
```

### 6. 结果报告要点

```r
# 格式化ANOVA结果信息
anova_summary <- function(model) {
  result <- summary(model)[[1]]
  cat("ANOVA结果：\n")
  cat("F(", result[1, "Df"], ",", result[2, "Df"], ") = ", 
      round(result[1, "F value"], 2), 
      ", p ", ifelse(result[1, "Pr(>F)"] < 0.001, "< 0.001", 
                    paste("=", round(result[1, "Pr(>F)"], 3))), 
      ", η² = ", round(result[1, "Sum Sq"]/sum(result[, "Sum Sq"]), 3), 
      "\n", sep="")
}

# 输出分析结果
anova_summary(anova_result)

# 输出事后检验结果摘要
tukey_summary <- function(tukey_result) {
  cat("\nTukey HSD事后检验结果：\n")
  for(i in 1:nrow(tukey_result$Species)) {
    cat(rownames(tukey_result$Species)[i], ": 差值 = ", 
        round(tukey_result$Species[i, "diff"], 2),
        ", 95% CI [", round(tukey_result$Species[i, "lwr"], 2), ", ", 
        round(tukey_result$Species[i, "upr"], 2), "], p ",
        ifelse(tukey_result$Species[i, "p adj"] < 0.001, "< 0.001", 
               paste("=", round(tukey_result$Species[i, "p adj"], 3))),
        "\n", sep="")
  }
}

tukey_summary(tukey_result)
```

## 七、完整代码示例：鸢尾花花瓣长度分析

```r
# 加载数据
data(iris)
str(iris)
cat("缺失值检查:", sum(is.na(iris)), "个缺失值\n")
cat("样本量:", table(iris$Species), "\n")

# 描述性统计
stats <- aggregate(Petal.Length ~ Species, data = iris, 
                  FUN = function(x) c(mean = mean(x), sd = sd(x)))
print(stats)

# 箱线图
boxplot(Petal.Length ~ Species, data = iris, 
        main = "三种鸢尾花的花瓣长度比较",
        xlab = "鸢尾花种类", ylab = "花瓣长度(cm)",
        col = rainbow(3))

# 正态性检验
shapiro_results <- lapply(levels(iris$Species), function(sp) {
  test <- shapiro.test(iris$Petal.Length[iris$Species == sp])
  c(species = sp, W = test$statistic, p.value = test$p.value)
})
shapiro_results

# 方差齐性检验
bartlett.test(Petal.Length ~ Species, data = iris)

# 单因素方差分析
anova_result <- aov(Petal.Length ~ Species, data = iris)
summary(anova_result)

# 计算效应量
anova_table <- summary(anova_result)[[1]]
eta_squared <- anova_table[1, "Sum Sq"] / sum(anova_table[, "Sum Sq"])
cat("效应量 η² =", round(eta_squared, 3), "\n")

# Tukey HSD事后检验
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)

plot(tukey_result, las = 1, col = "blue", 
     main = "Tukey HSD: 鸢尾花种类间花瓣长度差异")

# 输出分析结果
cat("\n分析结论：\n")
cat("1. 单因素方差分析表明，不同鸢尾花种类的花瓣长度存在极其显著差异")
cat("   (F(", anova_table[1, "Df"], ",", anova_table[2, "Df"], ") = ", 
    round(anova_table[1, "F value"], 2), ", p < 0.001)。\n", sep="")
cat("2. 效应量η² =", round(eta_squared, 3), 
    "表明鸢尾花种类对花瓣长度的影响非常大。\n")
cat("3. Tukey HSD事后检验显示，所有三种鸢尾花种类之间的花瓣长度均存在显著差异(p < 0.001)。\n")

## 八、与其他方法的比较

| 特征 | 单因素ANOVA | t检验 | Kruskal-Wallis检验 |
|------|------------|------|------------------|
| 适用组数 | 三个或更多组 | 两组 | 三个或更多组 |
| 数据类型 | 连续型 | 连续型 | 连续型或有序型 |
| 假设要求 | 正态分布、方差齐性 | 正态分布、方差齐性(独立样本t检验) | 无正态性要求 |
| 统计检验力 | 较高(满足假设时) | 较高(满足假设时) | 略低于ANOVA |
| 适用场景 | 参数满足时的多组比较 | 两组比较 | 假设不满足时的多组比较 |

**方法选择代码示例**：
```r
# 根据数据特性选择适当的分析方法
choose_method <- function(data, group_var, response_var) {
  # 检查组数
  n_groups <- length(unique(data[[group_var]]))
  
  if(n_groups == 2) {
    cat("只有两组，推荐使用t检验\n")
    return(t.test(data[[response_var]] ~ data[[group_var]]))
  } else {
    # 检查正态性
    normal <- TRUE
    for(g in unique(data[[group_var]])) {
      test <- shapiro.test(data[[response_var]][data[[group_var]] == g])
      if(test$p.value < 0.05) {
        normal <- FALSE
        break
      }
    }
    
    # 检查方差齐性
    var_equal <- bartlett.test(data[[response_var]] ~ data[[group_var]])$p.value >= 0.05
    
    if(normal && var_equal) {
      cat("满足ANOVA假设，使用单因素方差分析\n")
      return(summary(aov(data[[response_var]] ~ data[[group_var]])))
    } else {
      cat("不满足ANOVA假设，使用Kruskal-Wallis检验\n")
      return(kruskal.test(data[[response_var]] ~ data[[group_var]]))
    }
  }
}

# 应用到iris数据集
choose_method(iris, "Species", "Petal.Length")
```

## 九、注意事项与局限性

1. **方差分析只是"全局检验"**：仅告诉我们是否存在组间差异，不指明具体哪些组不同
2. **样本量平衡**：各组样本量相近时，ANOVA对方差齐性假设的违反更为稳健
3. **事后检验的选择**：不同事后检验方法有不同的统计特性，应根据研究目的选择
4. **效应量的重要性**：统计显著性不等同于实际重要性，应结合效应量解释结果
5. **局限性**：
   - 只能检测均值差异，无法检测分布形状的差异
   - 对极端离群值敏感
   - 无法处理重复测量或配对设计（需要用重复测量ANOVA）

## 十、进阶分析选项

**处理方差不齐的代码示例**：
```r
# 当方差不齐时，使用Welch's ANOVA
oneway.test(Petal.Length ~ Species, data = iris, var.equal = FALSE)

# 非参数替代 - Kruskal-Wallis检验
kruskal.test(Petal.Length ~ Species, data = iris)

# 事后检验 - 当不假设方差齐性时
pairwise.t.test(iris$Petal.Length, iris$Species, 
                p.adjust.method = "bonferroni", 
                pool.sd = FALSE)
```

通过以上系统分析和完整的代码实现，您可以全面掌握单因素方差分析的理论基础、实施步骤和结果解释，为多组比较提供科学可靠的统计依据。