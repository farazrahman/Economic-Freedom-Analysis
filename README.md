library('tidyverse') 
library('leaflet')
library('ggmap')
library('GGally')
library('viridis')
library('plotly')
library('IRdisplay')
library('ggrepel')
library('cowplot')
library('jtools')
library('car')
library('MASS')

options(warn = -1)

data <- read_csv("../input/efw_cc.csv")
summary(data)
glimpse(data)


##CHECKING FOR THE MISSING DATA:
missing_data <- data %>% summarise_all(funs(sum(is.na(.))/n()))
missing_data <- gather(missing_data, key = "variables", value = "percent_missing") 
ggplot(missing_data, aes(x = reorder(variables, percent_missing), y = percent_missing)) +xlab('variables')+
  geom_bar(stat = "identity", fill = "red", aes(color = I('white')), size = 0.3)+coord_flip()+ theme_bw()
  
 ##DATA PREPARATION:
 
 data$year <- factor(data$year)
data$countries <- factor(data$countries)
colnames(data)[4] <- "Economic_Freedom"


##ECONOMIC FREEDOM OF THE WORLD(1970-2016):

data %>% filter(!is.na(Economic_Freedom) | !is.na(year)) %>% group_by(year) %>%
ggplot(aes(year,Economic_Freedom, fill = Economic_Freedom, group = 1))+
geom_line(aes(color = Economic_Freedom))+
scale_color_viridis(option = "plasma",direction = -1, discrete=FALSE) +
facet_wrap(~countries)+theme_bw()+
theme(legend.position = "none", axis.text.x = element_blank(), 
      strip.text.x = element_text(size = 6))+
xlab(" ") + ylab("")+ ggtitle("ECONOMIC FREEDOM OF THE WORLD 1970-2016")


##ANAYSIS FOR THE YEAR 2016:

data1 <- data %>% filter(year == 2016)
head(data1)


##COUNTRIES HAVING HIGHEST AND LEAST ECONOMIC FREEDOM INDEX:

a1 <- ggplotly(ggplot(data1, aes(quartile,Economic_Freedom , size = -rank)) + 
       geom_jitter(aes(color=countries, alpha=0.5)) +
        theme_bw()+ theme(legend.position= "none")+
        xlab("Quartile") + 
        ggtitle("Economic Freedom Index 2016"), tooltip = c("countries"))

htmlwidgets::saveWidget(a1, "a1.html")
display_html('<iframe src="a1.html" width=100% height=450></iframe>')


##ECONOMIC FREEDOM INDEX 2016:

l <- list(color = toRGB("black"), width = 0.5)

g <- list(showframe = FALSE,
  showcoastlines = TRUE,
  projection = list(type = 'Mercator'))

p1 <- plot_geo(data1) %>%
  add_trace(z = ~Economic_Freedom, color = ~Economic_Freedom, colors = 'RdYlBu',
    text = ~data1$countries, locations = ~data1$ISO_code, marker = list(line = l)) %>%
  colorbar(title = 'Countries' , tickprefix = 'EF') %>%
  layout(title = 'Economic Freedom 2016')

htmlwidgets::saveWidget(p1, "p1.html")
display_html('<iframe src="p1.html" width=100% height=450></iframe>')
