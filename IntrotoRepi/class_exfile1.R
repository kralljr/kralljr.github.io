## @knitr g1
g1 <- ggplot(data = airquality, aes(x = Temp, y = Ozone, color = factor(Month))) + 
    geom_point() + scale_color_brewer(palette = "Set1")
print(g1)

## @knitr tab1
gb <- group_by(airquality, Month)
tab1 <- summarise_each(gb, funs(mean(., na.rm = T)), 1 : 4)
kable(tab1, format = "pandoc", caption = "Table 1. Means of airquality variables by month")



## @knitr myfun
# x is numeric vector 
# y is numeric vector
myfun <- function(x, y) {
    sum1 <- x + y
    return(sum1)
}




