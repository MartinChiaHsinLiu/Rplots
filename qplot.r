##include library
library(ggplot2)
diamonds.subset <- diamonds[sample(nrow(diamonds), 100), ]

#plots with one more dimension
qplot(carat, price, data = diamonds.subset, color = color) # different color
qplot(carat, price, data = diamonds.subset, shape = cut) #different shape
qplot(carat, price, data = diamonds, alpha = I(1/100)) #different transparence 
qplot(carat, price, data = diamonds.subset, geom = c("point", "smooth")) #add lowess line
qplot(color, price / carat, data = diamonds, geom = "boxplot") 
qplot(color, price / carat, data = diamonds, geom = "jitter")
qplot(color, price / carat, data = diamonds, geom = "jitter", alpha = I(1 / 50))
qplot(color, price / carat, data = diamonds, geom = "jitter", alpha = I(1 / 5), shape = cut)# 以資料點的形狀區分 cut 變數
qplot(color, price / carat, data = diamonds, geom = "jitter", alpha = I(1 / 5), color = color)# 以資料點的顏色區分 color 變數
qplot(color, price / carat, data = diamonds, geom = "jitter", alpha = I(1 / 5), color = cut)# 以資料點的顏色區分 cut 變數
qplot(color, price / carat, data = diamonds, geom = "boxplot", color = color)# 以箱形圖的外框顏色區分 color 變數
qplot(color, price / carat, data = diamonds, geom = "boxplot", fill = color)# 以箱形圖的內部顏色區分 color 變數
qplot(color, price / carat, data = diamonds, geom = "boxplot", size = I(2))# 調整箱形圖的外框粗細




qplot(carat, data = diamonds, geom = "histogram")
qplot(carat, data = diamonds, geom = "histogram",  binwidth = 0.5, xlim = c(0, 3))
qplot(carat, data = diamonds, geom = "histogram",  binwidth = 0.01, xlim = c(0, 3))
qplot(carat, data = diamonds, geom = "histogram",  fill = color)
qplot(carat, data = diamonds, geom = "density")
qplot(carat, data = diamonds, geom = "density", adjust = 3)
qplot(carat, data = diamonds, geom = "density", color = color)
qplot(carat, ..density.., data = diamonds, geom = c("histogram", "density")) #plot histogram and density plot together



qplot(color, data = diamonds, geom = "bar")
qplot(color, data = diamonds, geom = "bar", weight = carat) + ylab("carat")

#line 圖形的 x 軸是時間的資訊，用來呈現某個變數隨著時間的變化
qplot(date, uempmed, data = economics, geom = "line")
qplot(date, unemploy / pop, data = economics, geom = "line") 
#path 則是用在比較兩個變數隨的時間變化的關係。
qplot(unemploy / pop, uempmed, data = economics,  geom = c("point", "path"))
year <- function(x) as.POSIXlt(x)$year + 1900
qplot(unemploy / pop, uempmed, data = economics, geom = "path", colour = year(date))

#facets 參數配合公式的方式指定，公式的左邊是列（row）、右邊是行（column）
qplot(carat, data = diamonds, facets = color ~ cut,  geom = "histogram", binwidth = 0.1, xlim = c(0, 3))
qplot(carat, data = diamonds, facets = color ~ ., geom = "histogram", binwidth = 0.1, xlim = c(0, 3))


# 產生繪圖用的因子變數
mtcars$gear <- factor(mtcars$gear,levels=c(3, 4, 5), labels=c("3gears", "4gears", "5gears"))
mtcars$am <- factor(mtcars$am,levels=c(0, 1), labels=c("Automatic", "Manual"))
mtcars$cyl <- factor(mtcars$cyl,levels=c(4, 6, 8), labels=c("4cyl", "6cyl", "8cyl"))

# gear 分組畫出 mpg 密度函數圖, 以 fill 做grouping 
qplot(mpg, data = mtcars, geom = "density", fill = gear, alpha = I(.5), main="Distribution of Gas Milage", xlab="Miles Per Gallon", ylab="Density")
#將資料以 gear 與 cylinder 分組，畫出 mpg 與 hp 的散佈圖，並且以資料點的顏色與形狀標示 am
qplot(hp, mpg, data = mtcars, shape = am, color = am,  facets = gear~cyl, size = I(3),  xlab = "Horsepower", ylab = "Miles per Gallon")
# 畫出箱形圖，並且在上面用 jitter 資料點畫出實際資料的位置
qplot(gear, mpg, data = mtcars, geom = c("boxplot", "jitter"), fill = gear, main = "Mileage by Gear Number", xlab = "", ylab = "Miles per Gallon")






















