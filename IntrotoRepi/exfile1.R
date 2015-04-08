## @knitr tabdiab
load("tab_diab.RData")
tab_diab2 <- tab_diab[, 1 : 5]
kable(tab_diab2, digits = 2, caption = "Table 1. Summary statistics", 
  format = "pandoc")


## @knitr cat
# Courtesy of Scott Chamberlain \url{http://rforcats.net/}
getcutecat <- function(){
    writeLines(
        sprintf("![](http://placekitten.com/g/%s/%s)", 300, 300)
    )
}
cat1 <- getcutecat()
