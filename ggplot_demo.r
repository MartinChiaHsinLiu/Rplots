#include library
library(ggplot2)

#Build a graph with qplot() or ggplot()
      #   aesthetic mappings			 data		geom
      #############################    ########	  ###########
qplot(x = cty, y = hwy, color = cyl, data = mpg, geom = "point");

ggplot(data = mpg, aes(x = cty, y = hwy)) +
geom_point(aes(color = cyl)) +
geom_smooth(method ="lm") +
coord_cartesian() +
scale_color_gradient() +
theme_bw()
#Saves last plot as 5’ x 5’ file named "plot.png"
ggsave("plot.png", width = 5, height = 5);



## data 指定預設資料集
#  x  指定 x 軸
#  y  指定 y 軸
#  colour 依照指定的變數，按照不同分類給予不同顏色
## preparing data and save to justdoit
justdoit<-ggplot(data=CO2)
## 用 geom 、 aes ()繪製圖形
#  geom 的功能是在圖中產生點、線、三角等圖形
#  aes 的作用則是管理座標軸、顏色變化 aesthetic means "something you can see"
#  指定boxplot繪圖的資料為CO2，x軸為conc變數、y軸為uptake變數，依Plant變數的分類來上色
justdoit + geom_boxplot(data=CO2, aes(x=conc, y=uptake, colour=Plant))
#增加一個圖層係由 geom_point() 函數所繪製的散佈圖
justdoit + geom_boxplot(data=CO2, aes(x=conc, y=uptake, colour=Plant)) + geom_point(data=CO2, aes(x=conc, y=uptake, colour=Plant))
justdoit + geom_boxplot(data=CO2, aes(x=conc, y=uptake, colour=Plant)) + geom_jitter(data=CO2, aes(x=conc, y=uptake, colour=Plant))
#用 stat 增加統計資訊
#method="lm, glm, gam, loess, rlm". For datasets with n < 1000 default is loess.
justdoit + geom_boxplot(data=CO2, aes(x=conc, y=uptake, colour=Plant)) + geom_point(data=CO2, aes(x=conc, y=uptake, colour=Plant)) + stat_smooth(data=CO2, aes(x=conc, y=uptake))

#Radar plot
#theta 參數指定圖形的視角(從 X 座標或 Y 座標)
#start 參數(預設是 0 )代表圖形的旋轉角度( 12 點鐘方式)
#direction 以 1 或 -1 指定順時鐘或逆時鐘
#coord_polar(theta = "x", start = 0, direction = 1) 
W1<-ggplot(data=ChickWeight)
W1 + geom_point(aes(x=Time, y=weight, col=Diet))#naive X-Y plot
W1 + geom_point(aes(x=Time, y=weight, col=Diet)) + coord_polar()
W1 + geom_point(aes(x=Time, y=weight, col=Diet)) + coord_polar(theta="y", start=5, direction=-1)

#theme
W1 + geom_point(aes(x=Time, y=weight, col=Diet)) + coord_polar(theta="y", start=5, direction=-1) + theme(panel.background = element_rect(fill = "transparent", color = "green", size=5, linetype=3))

#以 facet 保留圖形多重輸出
#wrap and grid.
W1 +geom_point(aes(x=Time, y=weight, col=Diet)) + facet_wrap(~Diet)
W1 +geom_point(aes(x=Time, y=weight, col=Diet)) + facet_grid(~Diet)


#http://docs.ggplot2.org/current/geom_abline.html








#########################################################################################################################################
#Geoms - Use a geom to represent data points, use the geom’s aesthetic properties to represent variables. Each function returns a layer.#
#########################################################################################################################################
##############
##One Variable (1D plot)
a <- ggplot(mpg, aes(hwy));
a + geom_area(stat = "bin",binwidth=5); #count plot
a + geom_density(kernel = "gaussian"); #density plot
a + geom_dotplot(); #dotplot
a + geom_freqpoly() #dotplot in line type
a + geom_histogram(binwidth=2); #histogram

#discrete variable
b <- ggplot(mpg, aes(fl));
b + geom_bar(); #barplot

#Graphical Primitives
C <- ggplot(map, aes(long, lat));
C + geom_polygon(aes(group = group)); #plot map

d <- ggplot(economics, aes(date, unemploy));
d + geom_path(lineend="butt", linejoin="round",linemitre=1); # curve
d + geom_ribbon(aes(ymin=unemploy - 900, ymax=unemploy + 900)); #curve with min, max

e <- ggplot(seals, aes(x = long, y = lat));
e + geom_segment(aes(xend = long + delta_long, yend = lat + delta_lat)); #segment plot
e + geom_rect(aes(xmin = long, ymin = lat, xmax= long + delta_long, ymax = lat + delta_lat)); #segment plot with min, max

##Two Variables (2D plot)
f <- ggplot(mpg, aes(cty, hwy));
f + geom_blank(); #plot nothing, just see the framework
f + geom_point(); #x-y plot
f + geom_jitter(); #x-y plot with perturbation
f + geom_quantile(); #quantile lines
f + geom_rug(sides = "bl"); #plot density on the axis
f + geom_smooth(model = lm); #regression line with sd
f + geom_text(aes(label = cty)); #with lable

g <- ggplot(mpg, aes(class, hwy))
g + geom_bar(stat = "identity") #barplot
g + geom_boxplot() #boxplot
g + geom_dotplot(binaxis = "y", stackdir = "center") # with dots
g + geom_violin(scale = "area") #violine plot

h <- ggplot(diamonds, aes(cut, color))
h + geom_jitter()

#Continuous Bivariate Distribution
i <- ggplot(mpg, aes(year, hwy))
i + geom_bin2d(binwidth = c(5, 0.5))
i + geom_density2d()
i + geom_hex()

j <- ggplot(economics, aes(date, unemploy))
j + geom_area() # line plot with filled area
j + geom_line() # line plot
j + geom_step(direction = "hv")

#Visualizing error bar
DF <- data.frame(grp = c("A", "B"), fit = 4:5, se = 1:2);
k <- ggplot(DF, aes(grp, fit, ymin = fit-se, ymax = fit+se));
k + geom_crossbar(fatten = 2);
k + geom_errorbar();
k + geom_linerange();
k + geom_pointrange();

#map
USdata <- data.frame(murder = USArrests$Murder, state = tolower(rownames(USArrests))) 
map <- map_data("state") 
l <- ggplot(USdata, aes(fill = murder))
l + geom_map(aes(map_id = state), map = map) + expand_limits(x = map$long, y = map$lat)

##Three Variables (3D)
seals$z <- with(seals, sqrt(delta_long^2 + delta_lat^2)) 
m <- ggplot(seals, aes(long, lat))
m + geom_contour(aes(z = z))
m + geom_raster(aes(fill = z), hjust=0.5, vjust=0.5, interpolate=FALSE)
m + geom_tile(aes(fill = z))



###################################################
####Stats - An alternative way to build a layer####
###################################################
#Some plots visualize a transformation of the original data set.
#Use a stat to choose a common transformation to visualize.

#   stat function  layer transformation     geom           parameter           
	#############  ###################   ###########       #########
i + stat_density2d(aes(fill = ..level..),geom = "polygon", n = 100)

a + stat_bin(binwidth = 1, origin = 10)
a + stat_bindot(binwidth = 1, binaxis = "x")
a + stat_density(adjust = 1, kernel = "gaussian")

f + stat_bin2d(bins = 30, drop = TRUE)
f + stat_binhex(bins = 30)
f + stat_density2d(contour = TRUE, n = 100)

m + stat_contour(aes(z = z))
m + stat_spoke(aes(radius= z, angle = z))
m + stat_summary_hex(aes(z = z), bins = 30, fun = mean)
m + stat_summary2d(aes(z = z), bins = 30, fun = mean)

g + stat_boxplot(coef = 1.5)
g + stat_ydensity(adjust = 1, kernel = "gaussian", scale = "area")

f + stat_ecdf(n = 40) #empirical CDF
f + stat_quantile(quantiles = c(0.25, 0.5, 0.75), formula = y ~ log(x),method = "rq")
f + stat_smooth(method = "auto", formula = y ~ x, se = TRUE, n = 80,fullrange = FALSE, level = 0.95)

ggplot() + stat_function(aes(x = -3:3),fun = dnorm, n = 101, args = list(sd=0.5))#plot normal dortribution
f + stat_identity()
ggplot() + stat_qq(aes(sample=1:100), distribution = qt, dparams = list(df=5))
f + stat_sum() #focus plot
f + stat_summary(fun.data = "mean_cl_boot")
f + stat_unique() 

############################################################################################################################
#Scales control how a plot maps data values to the visual values of an aesthetic. To change the mapping, add a custom scale#
############################################################################################################################
n <- b + geom_bar(aes(fill = fl)) 
n
n + scale_fill_manual(
values = c("skyblue", "royalblue", "blue", "navy"),
limits = c("d", "e", "p", "r"), breaks =c("d", "e", "p", "r"),
name = "fuel", labels = c("D", "E", "P", "R"))

o <- a + geom_dotplot(aes(fill = ..x..))
n + scale_fill_brewer(palette = "Blues")

library(RcolorBrewer)
display.brewer.all()
n + scale_fill_grey(start = 0.2, end = 0.8,na.value = "red") 

o + scale_fill_gradient2(low = "red", hight = "blue", mid = "white", midpoint = 25)
#colours parameter
#rainbow(), heat.colors(),topo.colors(), cm.colors(),RColorBrewer::brewer.pal()
o + scale_fill_gradientn(colours = terrain.colors(6))

p <- f + geom_point(aes(shape = fl))
p + scale_shape(solid = FALSE)
p + scale_shape_manual(values = c(3:7)) 

Q <- f + geom_point(aes(size = cyl))
Q + scale_size_area(max = 6) 


##Coordinate Systems
r <- b + geom_bar() #barplot
r + coord_cartesian(xlim = c(0, 5)) #same
r + coord_fixed(ratio = 1/2)
r + coord_flip() #trans x-y
r + coord_polar(theta = "x", direction=1 ) #radar plot
r + coord_trans(ytrans = "sqrt") #square root presentation in y-axis, similar with log view


##Position Adjustments
#Position adjustments determine how to arrange geoms that would otherwise occupy the same space
s <- ggplot(mpg, aes(fl, fill = drv))
s + geom_bar(position = "dodge") #besided bar plot
s + geom_bar(position = "fill")
s + geom_bar(position = "stack")
f + geom_point(position = "jitter")
s + geom_bar(position = position_dodge(width = 1))#besided bar plot seperated

##Faceting
#Facets divide a plot into subplots based on the values of one or more discrete variables
t <- ggplot(mpg, aes(cty, hwy)) + geom_point()
t + facet_grid(. ~ fl) #facet into columns based on fl
t + facet_grid(year ~ .) #facet into rows based on year
t + facet_grid(year ~ fl) #facet into both rows and columns
t + facet_wrap(~ fl) #wrap facets into a rectangular layout
#x and y axis limits adjust to individual facets
#• "free_x" - x axis limits adjust
#• "free_y" - y axis limits adjust
t + facet_grid(y ~ x, scales = "free") #Set scales to let axis limits vary across facets

#Set labeller to adjust facet labels
t + facet_grid(. ~ fl, labeller = label_both)
t + facet_grid(. ~ fl, labeller = label_bquote(alpha ^ .(x)))
t + facet_grid(. ~ fl, labeller = label_parsed)

t + ggtitle("New Plot Title") #Add a main title above the plot
t + xlab("New X label") #Change the label on the X axis
t + ylab("New Y label") #Change the label on the Y axis
t + labs(title =" New title", x = "New x", y = "New y") #All of the above

t + theme(legend.position = "bottom") #Place legend at "bottom", "top", "lef", or "right"
t + guides(color = "none") #Set legend type for each aesthetic: colorbar, legend, or none (no legend) 
t + scale_fill_discrete(name = "Title",labels = c("A", "B", "C")) #Set legend title and labels with a scale function.

##Themes
r + theme_grey() #Grey background (default theme)
r + theme_bw() #White background with grid lines
r + theme_classic() #White background no gridlines
r + theme_minimal() # Minimal theme

t + coord_cartesian(xlim = c(0, 100), ylim = c(10, 20))
t + xlim(0, 100) + ylim(10, 20)
t + scale_x_continuous(limits = c(0, 100)) + scale_y_continuous(limits = c(0, 100))


#multiple histogram
#http://www.sthda.com/english/wiki/ggplot2-histogram-plot-quick-start-guide-r-software-and-data-visualization


















