link = "https://github.com/MartinChiaHsinLiu/Rplots/raw/main/wmpeople1.TTF";
download.file(link, "wmpeople1.TTF", mode = "wb");

library(showtext);
#font.add("wmpeople1", "wmpeople1.TTF"); older version than 4.0.5
font_add("wmpeople1", "wmpeople1.TTF");

library(ggplot2);
library(plyr);
#library(Cairo);

dat = read.csv(textConnection('
edu,educode,gender,population
未上學,1,男,17464
未上學,1,女,41268
小學,2,男,139378
小學,2,女,154854
國中,3,男,236369
國中,3,女,205537
高中,4,男,94528
高中,4,女,70521
大學以上,5,男,57013
大學以上,5,女,50334
'));

dat$int = round(dat$population / 10000);
gdat = ddply(dat, "educode", function(d) {
    male = d$int[d$gender == "男"];
    female = d$int[d$gender == "女"];
    data.frame(gender = c(rep("男", male), rep("女", female)),
               x = 1:(male + female));
});
#gdat$char = ifelse(gdat$gender == "m", "p", "u");
gdat$char = ifelse(gdat$gender == "男", "p", "u")
#showtext.begin();#older version
png("人口分布barplot(用小人).png", 900, 600, pointsize = 15)
showtext_begin();
theme_set(theme_grey(base_size = 15));
ggplot(gdat, aes(x = x, y = educode)) +
    geom_text(aes(label = char, colour = gender),
              family = "wmpeople1", size = 8) +
    scale_x_continuous("人數(千萬人)") +
    scale_y_discrete("教育程度",
        labels = unique(dat$edu[order(dat$educode)])) +
    #scale_colour_hue(guide = FALSE) +
	scale_colour_manual("性別", values = c("#00BFC4", "#F8766D")) +
    ggtitle("人口教育程度統計") ;
#showtext.end();#older version
showtext_end();
dev.off()
