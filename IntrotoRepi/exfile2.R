

## @knitr or_df
# 2. Perform multiple logistic regression
glm1 <- glm(diab1 ~ chol + hdl + age + weight + height, data = diabetes,
            family = "binomial")
# 3. Create data frame
# a. get odds ratios
MLR_OR <- exp(glm1$coef)[-1]
# b. get p-values
sum_glm <- summary(glm1)$coef[-1, ]
MLR_pval <- sum_glm[, 4]
# c. get 95% confidence interval
confint1 <- exp(confint(glm1))[-1, ]
# d. get names of variables
Variable <- rownames(sum_glm)
# Create output data frame
OR_df <- data.frame(Variable, MLR_OR, MLR_pval, confint1)
# Name columns, not rows
colnames(OR_df)[-1] <- c("OR", "p-value", "LB", "UB")
rownames(OR_df) <- NULL


#Create output file
kable(OR_df,digits = 2, caption = "Table 2. Odds ratios", format = "pandoc")



## @knitr figure

# create figure
ggplot(OR_df, aes(x = Variable, y = OR, color = Variable)) +
  geom_point(size = 3, shape = 20) +
  geom_errorbar(aes(ymin = LB, ymax = UB), width = 0.3) +
  ylab("Odds ratio") + xlab("Covariates")