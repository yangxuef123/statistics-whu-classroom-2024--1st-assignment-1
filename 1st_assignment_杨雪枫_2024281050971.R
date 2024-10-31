library(readxl)
library(tidyverse)
library(kableExtra)
library(lubridate)
library(scales)
library(plotly)
library(patchwork)
library(ggrepel)

getwd()
setwd("F:/project/statis-work")

# 导入数据
wh <- read.csv("./2023-09-12_cleaned.csv",sep = ",")


# 做一些数据预处理，比如把字符型变成factor。
wh$bedrooms <- as.factor(wh$bedrooms)
wh$livingrooms <- as.factor(wh$livingrooms)


wh %>% 
  head(10) %>%
  kable(caption = "武汉链家二手房") %>% 
  kable_styling(latex_options = c("striped", "scale_down"),font_size = 8)

str(wh)
names(wh)
glimpse(wh)
summary(wh)


ggplot(wh,aes(decoration,price_sqm,colour = property_height))+
  geom_point()


wh2 <- group_by(wh,property_region) %>% 
  summarise(mean(price_sqm))  


# 集中趋势
summary(wh[,c("price_ttl","price_sqm","building_area","property_t_height")]) %>% 
  kable(caption = "武汉二手房数据") %>% 
  kable_styling(latex_options = c("striped", "scale_down"))

#-----------------
# 单价离散趋势
range(wh$price_sqm)
IQR(wh$price_sqm) 
var(wh$price_sqm)
sd(wh$price_sqm)
mad(wh$price_sqm)

# 单价形状趋势
wh %>% 
  ggplot(aes(price_sqm)) +
  geom_histogram()

wh %>% 
  ggplot(aes(price_sqm)) +
  geom_density()

e1071::skewness(wh$price_sqm)
e1071::kurtosis(wh$price_sqm)

#-----------------
# 总价离散趋势
range(wh$price_ttl)
IQR(wh$price_ttl) 
var(wh$price_ttl)
sd(wh$price_ttl)
mad(wh$price_ttl)

# 总价形状趋势
wh %>% 
  ggplot(aes(price_ttl)) +
  geom_histogram()+
  xlim(0,750)

wh %>% 
  ggplot(aes(price_ttl)) +
  geom_density()

e1071::skewness(wh$price_ttl)
e1071::kurtosis(wh$price_ttl)


#-----------------
# 房屋面积离散趋势
range(wh$building_area)
IQR(wh$building_area) 
var(wh$building_area)
sd(wh$building_area)
mad(wh$building_area)

# 房屋面积形状趋势
wh %>% 
  ggplot(aes(building_area)) +
  geom_histogram()

wh %>% 
  ggplot(aes(building_area)) +
  geom_density()

e1071::skewness(wh$building_area)
e1071::kurtosis(wh$building_area)

# 装修类型分布趋势
wh %>% 
  ggplot(aes(as.factor(decoration))) +
  geom_bar() 

# 关注人数
summary(wh$followers)

# 关注人数形状趋势
wh %>% 
  ggplot(aes(followers)) +
  geom_histogram()+
  xlim(0,50)

#-----------------
# 分析装修与单价关系
wh %>% 
  ggplot(aes(price_sqm,colour = decoration)) +
  geom_boxplot() 

# 分析楼层位置与单价关系
wh %>% 
  ggplot(aes(price_sqm,colour = property_height)) +
  geom_boxplot() 

# 房屋主要朝向与单价关系
wh %>% 
  ggplot(aes(price_sqm,colour = directions1)) +
  geom_boxplot()+
  geom_vline(xintercept = median(wh$price_sqm)
             ,color = "red",size =1)+
  geom_vline(xintercept = quantile(wh$price_sqm,c(0.25,0.75))
             ,color = "blue",size =1)


# 房屋主要朝向分布
temp <- wh %>% 
  group_by(directions1) %>% 
  count()
temp

temp%>% 
  ggplot(aes(directions1)) +
  geom_col(aes(y=n)) +
  scale_x_discrete(limits=as.character(1:8))

#先count计数，后对频数逆序（顺序）reorder
ggplot(temp, aes(x = reorder(directions1, -n), y = n)) + geom_col() 



wh %>% 
  ggplot(aes(price_sqm,colour = decoration)) +
  geom_histogram()


#---------
# 建筑形式与单价关系
wh %>% 
  ggplot(aes(price_sqm,colour = property_style)) +
  geom_boxplot() 

# 卧室数量与单价关系
wh %>% 
  ggplot(aes(price_sqm,colour = as.factor(bedrooms))) +
  geom_boxplot()

wh %>% 
  ggplot(aes(bedrooms)) +
  geom_bar() 


# 分析建筑形式、是否近地铁与单价关系
# 处理部分异常数据
wh_sub <- filter(wh,near_subway == "近地铁"|is.na(near_subway))

wh_sub %>% 
  ggplot(aes(price_sqm,colour = property_style)) +
  geom_boxplot()+
  geom_vline(xintercept = median(wh$price_sqm)
           ,color = "red",size =1)+
  facet_wrap(~near_subway,ncol = 1)


# 户型与单价关系
# 去除极值点
up_bound <- quantile(wh$building_area,0.75) + 1.5*IQR(wh$building_area)
low_bound <- quantile(wh$building_area,0.25) - 1.5*IQR(wh$building_area)
wh_area_filter <- filter(wh, building_area >= low_bound & building_area <= up_bound)

# 定义户型，[下界，25%]为小，[25%，75%]为中，[75%，上界]为大
wh_area_size <- wh_area_filter %>%  
  mutate(  
    size = case_when(  
      building_area <= quantile(wh$building_area,0.25) ~ "小户型",  
      building_area > quantile(wh$building_area,0.25) & building_area <= quantile(wh$building_area,0.75) ~ "中户型",  
      building_area > quantile(wh$building_area,0.75) ~ "大户型"  
    )  
  ) 

# 分析户型占比
wh_area_size %>% 
  ggplot(aes(size)) +
  geom_bar() 

# 分析户型与单价关系
wh_area_size %>% 
  ggplot(aes(price_sqm,colour = size)) +
  geom_boxplot()


# 进一步分析装修、是否近地铁与价格关系
wh %>% 
  ggplot(aes(building_area,price_ttl,colour = near_subway)) +
  geom_point()+
  geom_smooth(method = "lm") +
  facet_wrap(~decoration)+
  xlim(50,300)+
  ylim(0,800)


# # 关注人数与单价
# wh %>% 
#   ggplot(aes(followers,price_sqm)) +
#   geom_point()
# 
# # 分析是否2年与关注人数关系
# wh %>% 
#   ggplot(aes(followers,colour = if_2y)) +
#   geom_boxplot()+
#   xlim(0,50)


# 分析区域词云
library(wordcloud2)
wordcloud2(freq(segment(wh$property_region,worker())))


##  尝试建模寻找显著性
library(modelr)
wh_mod <- lm(price_sqm ~ directions1 + decoration + property_style + near_subway + size,
             data = wh_area_size)
summary(wh_mod)


wh_mod2 <- lm(price_sqm ~ decoration  + property_style  + size,
             data = wh_area_size)
summary(wh_mod2)


