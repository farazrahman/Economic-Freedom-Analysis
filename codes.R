#LOADING THE LIBRARIES:

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

#READING THE FILE:

data <- read_csv("../input/efw_cc.csv")

#CHECKING THE SUMMARY AND STRUCTURE OF THE DATA:

summary(data)
glimpse(data)
#Observations: 3,726
#Variables: 36

head(data)

#CHECKING FOR THE MISSING DATA:


missing_data <- data %>% summarise_all(funs(sum(is.na(.))/n()))
missing_data <- gather(missing_data, key = "variables", value = "percent_missing") 
ggplot(missing_data, aes(x = reorder(variables, percent_missing), y = percent_missing)) +xlab('variables')+
geom_bar(stat = "identity", fill = "red", aes(color = I('white')), size = 0.3)+coord_flip()+ theme_bw()

#DATA PREPARATION:

#Changing the data types
#Renaming the columns
#Removing the numerics from column names
#data <- data[complete.cases(data),]

#options(repr.plot.width=8, repr.plot.height=6)
data$year <- factor(data$year)
data$countries <- factor(data$countries)
colnames(data)[4] <- "Economic_Freedom"
#ggplot(data, aes(year)) + geom_density() + theme_bw()+theme(legend.position= 'none')

#ECONOMIC FREEDOM OF THE WORLD(1970-2016):

#The plot below tries to visualize the Economic Freedom and it's trend from 1970 to 2016. The trend has been promising for countries like New Zealand, USA, UK, Canada, Australia, SIngapore, Hongkong etc. Almost every country be it India, china, Spain or Thailand have shown either an increasing or a steady global trend. However, the trend for Venezuela is showing a declining trend over the years. Syria is also one country where the Economic Freedom is declining from the past 10 years.

options(repr.plot.width=8, repr.plot.height=10)

data %>% filter(!is.na(Economic_Freedom) | !is.na(year)) %>% group_by(year) %>%
  ggplot(aes(year,Economic_Freedom, fill = Economic_Freedom, group = 1))+
  geom_line(aes(color = Economic_Freedom))+
  scale_color_viridis(option = "plasma",direction = -1, discrete=FALSE) +
  facet_wrap(~countries)+theme_bw()+
  theme(legend.position = "none", axis.text.x = element_blank(), 
        strip.text.x = element_text(size = 6))+
  xlab(" ") + ylab("")+ ggtitle("ECONOMIC FREEDOM OF THE WORLD 1970-2016")

#ANAYSIS FOR THE YEAR 2016:
  
  #Checking the top few rows of our dataset and filtering the data for the year 2016.

data1 <- data %>% filter(year == 2016)
head(data1)

#COUNTRIES HAVING HIGHEST AND LEAST ECONOMIC FREEDOM INDEX:
  
 # Hongkong, Singapore, New Zealand, Switzerland and Ireland are the countries where people have maximum Economic Freedom.

#Venezuela, Libya, Argentina, Algeria and Syria have the least Economic Freedom amongst 162 countries.

a1 <- ggplotly(ggplot(data1, aes(quartile,Economic_Freedom , size = -rank)) + 
                 geom_jitter(aes(color=countries, alpha=0.5)) +
                 theme_bw()+ theme(legend.position= "none")+
                 xlab("Quartile") + 
                 ggtitle("Economic Freedom Index 2016"), tooltip = c("countries"))

htmlwidgets::saveWidget(a1, "a1.html")
display_html('<iframe src="a1.html" width=100% height=450></iframe>')

#ECONOMIC FREEDOM INDEX 2016:
  
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

#ECONOMIC FREEDOM INDEX- RANK OF COUNTRIES IN 2016:
  
  
  #ECONOMIC FREEDOM INDEX QUARTILES 2016:
  
  #1st QUARTILE- Most Free Countries
#4th QUARTILE- Least Free Countries

#AREA 1: SIZE OF GOVERNMENT


MODELLING:
  
  While from the above plots and EDAs we can see that, there are multiple parameters/attributes that affect the Economic Freedom Index of a Country. To be precise, there are 30 explanatory variables or indicators that affect the Economic Freedom Index. I am sure most of the parameters are multicollinear as well. In this scenario wouldn't it be nice to know what are the most important predictors of Economic Freedom of a country among these 30 predictors. Before going through any complex algorithm, I would like to try the basic Multiple Linear regression technique to predict the Economic Freedom Index, using the explanatory variables and try to figure out what are those variables that truly contribute towards the Economic Freedom Index.

MULTIPLE LINEAR REGRESSION:

Now, I will use the data for the years 2016, and prepare the data for modelling by removing the not so required columns such as year, countries, ISO codes, quartile and rank. Our target variable is Economic_Freedom and 30 independent variables.

library(caTools)
#Data preparation for modelling
head(data2)


efw_final <- data2[, -c(1,2,3,5,6)]
efw_final <- na.omit(efw_final)
str(efw_final)

Splitting the data 70:30 into train and test.

# splitting the data between train and test
set.seed(100)

indices = sample.split(efw_final$Economic_Freedom, SplitRatio = 0.7)

train = efw_final[indices,]

test = efw_final[!(indices),]
Creating the very first model: I found that the summary of the first model shows Multiple R-squared: 1 and Adjusted R-squared: 1 . So, I am sure there are a lot of multicollinear variables are present in the data.

model_1 <-lm(Economic_Freedom~.,data=train)
summary(model_1)
Call:
lm(formula = Economic_Freedom ~ ., data = train)

Residuals:
Min        1Q    Median        3Q       Max 
-0.004982 -0.001641  0.000025  0.001646  0.004888 

Coefficients: (4 not defined because of singularities)
Estimate Std. Error t value Pr(>|t|)
(Intercept)                            -8.877e-03  1.081e-02  -0.821    0.415
Sub_1a_government_consumption           5.016e-02  2.551e-04 196.682   <2e-16
Sub_1b_transfers                        5.024e-02  2.943e-04 170.721   <2e-16
Sub_1c_gov_enterprises                  4.994e-02  1.369e-04 364.734   <2e-16
Sub_1d_top_marg_tax_rate                4.988e-02  2.224e-04 224.278   <2e-16
Sub_1_size_government                          NA         NA      NA       NA
Sub_2a_judicial_independence           -2.001e-04  6.007e-04  -0.333    0.740
Sub_2b_impartial_courts                 4.374e-04  7.838e-04   0.558    0.579
Sub_2c_protection_property_rights      -6.832e-04  7.654e-04  -0.893    0.376
Sub_2d_military_interference            1.141e-04  4.622e-04   0.247    0.806
Sub_2e_integrity_legal_system           3.713e-04  6.317e-04   0.588    0.559
Sub_2f_legal_enforcement_contracts      4.707e-04  5.151e-04   0.914    0.365
Sub_2g_restrictions_sale_real_property  8.893e-05  4.454e-04   0.200    0.842
Sub_2h_reliability_police               2.654e-04  6.067e-04   0.438    0.663
Sub_2i_business_costs_crime             3.778e-04  5.809e-04   0.650    0.518
Sub_2j_gender_adjustment                6.523e-03  1.177e-02   0.554    0.581
Sub_2_property_rights                   1.990e-01  3.803e-03  52.328   <2e-16
Sub_3a_money_growth                     4.952e-02  3.691e-04 134.170   <2e-16
Sub_3b_std_inflation                    4.991e-02  3.948e-04 126.407   <2e-16
Sub_3c_inflation                        5.003e-02  3.172e-04 157.733   <2e-16
Sub_3d_freedom_own_foreign_currency     4.987e-02  1.323e-04 377.002   <2e-16
Sub_3_sound_money                              NA         NA      NA       NA
Sub_4a_tariffs                          5.006e-02  3.451e-04 145.074   <2e-16
Sub_4b_regulatory_trade_barriers        4.986e-02  4.517e-04 110.388   <2e-16
Sub_4c_black_market                     5.043e-02  3.501e-04 144.073   <2e-16
Sub_4d_control_movement_capital_ppl     5.077e-02  2.806e-04 180.903   <2e-16
Sub_4_trade                                    NA         NA      NA       NA
Sub_5a_credit_market_reg                6.630e-02  3.234e-04 205.025   <2e-16
Sub_5b_labor_market_reg                 6.651e-02  3.095e-04 214.858   <2e-16
Sub_5c_business_reg                     6.705e-02  7.166e-04  93.566   <2e-16
Sub_5_regulation                               NA         NA      NA       NA

(Intercept)                               
Sub_1a_government_consumption          ***
Sub_1b_transfers                       ***
Sub_1c_gov_enterprises                 ***
Sub_1d_top_marg_tax_rate               ***
Sub_1_size_government                     
Sub_2a_judicial_independence              
Sub_2b_impartial_courts                   
Sub_2c_protection_property_rights         
Sub_2d_military_interference              
Sub_2e_integrity_legal_system             
Sub_2f_legal_enforcement_contracts        
Sub_2g_restrictions_sale_real_property    
Sub_2h_reliability_police                 
Sub_2i_business_costs_crime               
Sub_2j_gender_adjustment                  
Sub_2_property_rights                  ***
Sub_3a_money_growth                    ***
Sub_3b_std_inflation                   ***
Sub_3c_inflation                       ***
Sub_3d_freedom_own_foreign_currency    ***
Sub_3_sound_money                         
Sub_4a_tariffs                         ***
Sub_4b_regulatory_trade_barriers       ***
Sub_4c_black_market                    ***
Sub_4d_control_movement_capital_ppl    ***
Sub_4_trade                               
Sub_5a_credit_market_reg               ***
Sub_5b_labor_market_reg                ***
Sub_5c_business_reg                    ***
Sub_5_regulation                          
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 0.002703 on 57 degrees of freedom
Multiple R-squared:      1,	Adjusted R-squared:      1 
F-statistic: 3.128e+05 on 26 and 57 DF,  p-value: < 2.2e-16
Next, I am using the stepAIC function to remove variables based on Akaike Information Criteria.

step <- stepAIC(model_1, direction="both")
step
Start:  AIC=-972
Economic_Freedom ~ Sub_1a_government_consumption + Sub_1b_transfers + 
Sub_1c_gov_enterprises + Sub_1d_top_marg_tax_rate + Sub_1_size_government + 
Sub_2a_judicial_independence + Sub_2b_impartial_courts + 
Sub_2c_protection_property_rights + Sub_2d_military_interference + 
Sub_2e_integrity_legal_system + Sub_2f_legal_enforcement_contracts + 
Sub_2g_restrictions_sale_real_property + Sub_2h_reliability_police + 
Sub_2i_business_costs_crime + Sub_2j_gender_adjustment + 
Sub_2_property_rights + Sub_3a_money_growth + Sub_3b_std_inflation + 
Sub_3c_inflation + Sub_3d_freedom_own_foreign_currency + 
Sub_3_sound_money + Sub_4a_tariffs + Sub_4b_regulatory_trade_barriers + 
Sub_4c_black_market + Sub_4d_control_movement_capital_ppl + 
Sub_4_trade + Sub_5a_credit_market_reg + Sub_5b_labor_market_reg + 
Sub_5c_business_reg + Sub_5_regulation


Step:  AIC=-972
Economic_Freedom ~ Sub_1a_government_consumption + Sub_1b_transfers + 
Sub_1c_gov_enterprises + Sub_1d_top_marg_tax_rate + Sub_1_size_government + 
Sub_2a_judicial_independence + Sub_2b_impartial_courts + 
Sub_2c_protection_property_rights + Sub_2d_military_interference + 
Sub_2e_integrity_legal_system + Sub_2f_legal_enforcement_contracts + 
Sub_2g_restrictions_sale_real_property + Sub_2h_reliability_police + 
Sub_2i_business_costs_crime + Sub_2j_gender_adjustment + 
Sub_2_property_rights + Sub_3a_money_growth + Sub_3b_std_inflation + 
Sub_3c_inflation + Sub_3d_freedom_own_foreign_currency + 
Sub_3_sound_money + Sub_4a_tariffs + Sub_4b_regulatory_trade_barriers + 
Sub_4c_black_market + Sub_4d_control_movement_capital_ppl + 
Sub_4_trade + Sub_5a_credit_market_reg + Sub_5b_labor_market_reg + 
Sub_5c_business_reg


Step:  AIC=-972
Economic_Freedom ~ Sub_1a_government_consumption + Sub_1b_transfers + 
Sub_1c_gov_enterprises + Sub_1d_top_marg_tax_rate + Sub_1_size_government + 
Sub_2a_judicial_independence + Sub_2b_impartial_courts + 
Sub_2c_protection_property_rights + Sub_2d_military_interference + 
Sub_2e_integrity_legal_system + Sub_2f_legal_enforcement_contracts + 
Sub_2g_restrictions_sale_real_property + Sub_2h_reliability_police + 
Sub_2i_business_costs_crime + Sub_2j_gender_adjustment + 
Sub_2_property_rights + Sub_3a_money_growth + Sub_3b_std_inflation + 
Sub_3c_inflation + Sub_3d_freedom_own_foreign_currency + 
Sub_3_sound_money + Sub_4a_tariffs + Sub_4b_regulatory_trade_barriers + 
Sub_4c_black_market + Sub_4d_control_movement_capital_ppl + 
Sub_5a_credit_market_reg + Sub_5b_labor_market_reg + Sub_5c_business_reg


Step:  AIC=-972
Economic_Freedom ~ Sub_1a_government_consumption + Sub_1b_transfers + 
Sub_1c_gov_enterprises + Sub_1d_top_marg_tax_rate + Sub_1_size_government + 
Sub_2a_judicial_independence + Sub_2b_impartial_courts + 
Sub_2c_protection_property_rights + Sub_2d_military_interference + 
Sub_2e_integrity_legal_system + Sub_2f_legal_enforcement_contracts + 
Sub_2g_restrictions_sale_real_property + Sub_2h_reliability_police + 
Sub_2i_business_costs_crime + Sub_2j_gender_adjustment + 
Sub_2_property_rights + Sub_3a_money_growth + Sub_3b_std_inflation + 
Sub_3c_inflation + Sub_3d_freedom_own_foreign_currency + 
Sub_4a_tariffs + Sub_4b_regulatory_trade_barriers + Sub_4c_black_market + 
Sub_4d_control_movement_capital_ppl + Sub_5a_credit_market_reg + 
Sub_5b_labor_market_reg + Sub_5c_business_reg


Step:  AIC=-972
Economic_Freedom ~ Sub_1a_government_consumption + Sub_1b_transfers + 
Sub_1c_gov_enterprises + Sub_1d_top_marg_tax_rate + Sub_2a_judicial_independence + 
Sub_2b_impartial_courts + Sub_2c_protection_property_rights + 
Sub_2d_military_interference + Sub_2e_integrity_legal_system + 
Sub_2f_legal_enforcement_contracts + Sub_2g_restrictions_sale_real_property + 
Sub_2h_reliability_police + Sub_2i_business_costs_crime + 
Sub_2j_gender_adjustment + Sub_2_property_rights + Sub_3a_money_growth + 
Sub_3b_std_inflation + Sub_3c_inflation + Sub_3d_freedom_own_foreign_currency + 
Sub_4a_tariffs + Sub_4b_regulatory_trade_barriers + Sub_4c_black_market + 
Sub_4d_control_movement_capital_ppl + Sub_5a_credit_market_reg + 
Sub_5b_labor_market_reg + Sub_5c_business_reg

Df Sum of Sq     RSS     AIC
- Sub_2g_restrictions_sale_real_property  1   0.00000 0.00042 -973.94
- Sub_2d_military_interference            1   0.00000 0.00042 -973.91
- Sub_2a_judicial_independence            1   0.00000 0.00042 -973.83
- Sub_2h_reliability_police               1   0.00000 0.00042 -973.72
- Sub_2j_gender_adjustment                1   0.00000 0.00042 -973.54
- Sub_2b_impartial_courts                 1   0.00000 0.00042 -973.54
- Sub_2e_integrity_legal_system           1   0.00000 0.00042 -973.49
- Sub_2i_business_costs_crime             1   0.00000 0.00042 -973.38
- Sub_2c_protection_property_rights       1   0.00001 0.00042 -972.83
- Sub_2f_legal_enforcement_contracts      1   0.00001 0.00042 -972.78
<none>                                                0.00042 -972.00
- Sub_2_property_rights                   1   0.02001 0.02043 -647.02
- Sub_5c_business_reg                     1   0.06398 0.06440 -550.57
- Sub_4b_regulatory_trade_barriers        1   0.08906 0.08947 -522.95
- Sub_3b_std_inflation                    1   0.11678 0.11720 -500.28
- Sub_3a_money_growth                     1   0.13156 0.13198 -490.30
- Sub_4c_black_market                     1   0.15170 0.15212 -478.37
- Sub_4a_tariffs                          1   0.15382 0.15423 -477.21
- Sub_3c_inflation                        1   0.18183 0.18225 -463.19
- Sub_1b_transfers                        1   0.21301 0.21342 -449.92
- Sub_4d_control_movement_capital_ppl     1   0.23918 0.23959 -440.21
- Sub_1a_government_consumption           1   0.28272 0.28313 -426.18
- Sub_5a_credit_market_reg                1   0.30721 0.30763 -419.21
- Sub_5b_labor_market_reg                 1   0.33739 0.33780 -411.35
- Sub_1d_top_marg_tax_rate                1   0.36762 0.36803 -404.15
- Sub_1c_gov_enterprises                  1   0.97225 0.97266 -322.52
- Sub_3d_freedom_own_foreign_currency     1   1.03875 1.03917 -316.96

Step:  AIC=-973.94
Economic_Freedom ~ Sub_1a_government_consumption + Sub_1b_transfers + 
Sub_1c_gov_enterprises + Sub_1d_top_marg_tax_rate + Sub_2a_judicial_independence + 
Sub_2b_impartial_courts + Sub_2c_protection_property_rights + 
Sub_2d_military_interference + Sub_2e_integrity_legal_system + 
Sub_2f_legal_enforcement_contracts + Sub_2h_reliability_police + 
Sub_2i_business_costs_crime + Sub_2j_gender_adjustment + 
Sub_2_property_rights + Sub_3a_money_growth + Sub_3b_std_inflation + 
Sub_3c_inflation + Sub_3d_freedom_own_foreign_currency + 
Sub_4a_tariffs + Sub_4b_regulatory_trade_barriers + Sub_4c_black_market + 
Sub_4d_control_movement_capital_ppl + Sub_5a_credit_market_reg + 
Sub_5b_labor_market_reg + Sub_5c_business_reg

Df Sum of Sq     RSS     AIC
- Sub_2d_military_interference            1   0.00000 0.00042 -975.90
- Sub_2h_reliability_police               1   0.00000 0.00042 -975.71
- Sub_2a_judicial_independence            1   0.00000 0.00042 -975.60
- Sub_2b_impartial_courts                 1   0.00000 0.00042 -975.53
- Sub_2i_business_costs_crime             1   0.00000 0.00042 -975.35
- Sub_2e_integrity_legal_system           1   0.00000 0.00042 -975.34
- Sub_2j_gender_adjustment                1   0.00000 0.00042 -975.30
- Sub_2f_legal_enforcement_contracts      1   0.00001 0.00042 -974.59
- Sub_2c_protection_property_rights       1   0.00001 0.00043 -974.00
<none>                                                0.00042 -973.94
+ Sub_2g_restrictions_sale_real_property  1   0.00000 0.00042 -972.00
- Sub_2_property_rights                   1   0.05818 0.05859 -560.51
- Sub_5c_business_reg                     1   0.06620 0.06661 -549.73
- Sub_4b_regulatory_trade_barriers        1   0.09434 0.09475 -520.13
- Sub_3b_std_inflation                    1   0.11720 0.11761 -501.98
- Sub_3a_money_growth                     1   0.13232 0.13273 -491.82
- Sub_4c_black_market                     1   0.15286 0.15328 -479.73
- Sub_4a_tariffs                          1   0.15521 0.15563 -478.45
- Sub_3c_inflation                        1   0.18461 0.18503 -463.92
- Sub_1b_transfers                        1   0.21433 0.21475 -451.41
- Sub_4d_control_movement_capital_ppl     1   0.23918 0.23960 -442.21
- Sub_5a_credit_market_reg                1   0.31496 0.31538 -419.12
- Sub_1a_government_consumption           1   0.31547 0.31589 -418.99
- Sub_5b_labor_market_reg                 1   0.33832 0.33874 -413.12
- Sub_1d_top_marg_tax_rate                1   0.38655 0.38696 -401.94
- Sub_1c_gov_enterprises                  1   0.98089 0.98130 -323.77
- Sub_3d_freedom_own_foreign_currency     1   1.03876 1.03918 -318.96

Step:  AIC=-975.9
Economic_Freedom ~ Sub_1a_government_consumption + Sub_1b_transfers + 
Sub_1c_gov_enterprises + Sub_1d_top_marg_tax_rate + Sub_2a_judicial_independence + 
Sub_2b_impartial_courts + Sub_2c_protection_property_rights + 
Sub_2e_integrity_legal_system + Sub_2f_legal_enforcement_contracts + 
Sub_2h_reliability_police + Sub_2i_business_costs_crime + 
Sub_2j_gender_adjustment + Sub_2_property_rights + Sub_3a_money_growth + 
Sub_3b_std_inflation + Sub_3c_inflation + Sub_3d_freedom_own_foreign_currency + 
Sub_4a_tariffs + Sub_4b_regulatory_trade_barriers + Sub_4c_black_market + 
Sub_4d_control_movement_capital_ppl + Sub_5a_credit_market_reg + 
Sub_5b_labor_market_reg + Sub_5c_business_reg

Df Sum of Sq     RSS     AIC
- Sub_2h_reliability_police               1   0.00000 0.00042 -977.71
- Sub_2b_impartial_courts                 1   0.00000 0.00042 -977.52
- Sub_2a_judicial_independence            1   0.00000 0.00042 -977.51
- Sub_2i_business_costs_crime             1   0.00000 0.00042 -977.34
- Sub_2e_integrity_legal_system           1   0.00000 0.00042 -977.28
- Sub_2j_gender_adjustment                1   0.00000 0.00042 -977.12
- Sub_2f_legal_enforcement_contracts      1   0.00001 0.00042 -976.42
<none>                                                0.00042 -975.90
- Sub_2c_protection_property_rights       1   0.00001 0.00043 -975.80
+ Sub_2d_military_interference            1   0.00000 0.00042 -973.94
+ Sub_2g_restrictions_sale_real_property  1   0.00000 0.00042 -973.91
- Sub_5c_business_reg                     1   0.06640 0.06682 -551.47
- Sub_4b_regulatory_trade_barriers        1   0.09682 0.09724 -519.96
- Sub_3b_std_inflation                    1   0.12135 0.12177 -501.06
- Sub_2_property_rights                   1   0.12335 0.12376 -499.70
- Sub_3a_money_growth                     1   0.13294 0.13336 -493.42
- Sub_4c_black_market                     1   0.15311 0.15353 -481.59
- Sub_4a_tariffs                          1   0.15621 0.15663 -479.91
- Sub_3c_inflation                        1   0.18551 0.18592 -465.51
- Sub_1b_transfers                        1   0.22126 0.22167 -450.74
- Sub_4d_control_movement_capital_ppl     1   0.24090 0.24131 -443.61
- Sub_5a_credit_market_reg                1   0.31937 0.31979 -419.96
- Sub_1a_government_consumption           1   0.33769 0.33810 -415.28
- Sub_1d_top_marg_tax_rate                1   0.39622 0.39664 -401.87
- Sub_5b_labor_market_reg                 1   0.39661 0.39703 -401.78
- Sub_1c_gov_enterprises                  1   0.98109 0.98151 -325.76
- Sub_3d_freedom_own_foreign_currency     1   1.09211 1.09252 -316.76

Step:  AIC=-977.71
Economic_Freedom ~ Sub_1a_government_consumption + Sub_1b_transfers + 
Sub_1c_gov_enterprises + Sub_1d_top_marg_tax_rate + Sub_2a_judicial_independence + 
Sub_2b_impartial_courts + Sub_2c_protection_property_rights + 
Sub_2e_integrity_legal_system + Sub_2f_legal_enforcement_contracts + 
Sub_2i_business_costs_crime + Sub_2j_gender_adjustment + 
Sub_2_property_rights + Sub_3a_money_growth + Sub_3b_std_inflation + 
Sub_3c_inflation + Sub_3d_freedom_own_foreign_currency + 
Sub_4a_tariffs + Sub_4b_regulatory_trade_barriers + Sub_4c_black_market + 
Sub_4d_control_movement_capital_ppl + Sub_5a_credit_market_reg + 
Sub_5b_labor_market_reg + Sub_5c_business_reg

Df Sum of Sq     RSS     AIC
- Sub_2b_impartial_courts                 1   0.00000 0.00042 -979.27
- Sub_2a_judicial_independence            1   0.00000 0.00042 -979.21
- Sub_2e_integrity_legal_system           1   0.00000 0.00042 -979.18
- Sub_2j_gender_adjustment                1   0.00000 0.00042 -979.10
- Sub_2i_business_costs_crime             1   0.00001 0.00042 -978.45
- Sub_2f_legal_enforcement_contracts      1   0.00001 0.00042 -978.38
- Sub_2c_protection_property_rights       1   0.00001 0.00043 -977.79
<none>                                                0.00042 -977.71
+ Sub_2h_reliability_police               1   0.00000 0.00042 -975.90
+ Sub_2g_restrictions_sale_real_property  1   0.00000 0.00042 -975.71
+ Sub_2d_military_interference            1   0.00000 0.00042 -975.71
- Sub_5c_business_reg                     1   0.06651 0.06692 -553.34
- Sub_4b_regulatory_trade_barriers        1   0.09975 0.10017 -519.47
- Sub_3b_std_inflation                    1   0.12285 0.12326 -502.04
- Sub_3a_money_growth                     1   0.13313 0.13354 -495.31
- Sub_2_property_rights                   1   0.14192 0.14233 -489.95
- Sub_4a_tariffs                          1   0.15661 0.15702 -481.70
- Sub_4c_black_market                     1   0.16045 0.16087 -479.67
- Sub_3c_inflation                        1   0.19204 0.19245 -464.61
- Sub_1b_transfers                        1   0.22250 0.22292 -452.27
- Sub_4d_control_movement_capital_ppl     1   0.24663 0.24705 -443.64
- Sub_5a_credit_market_reg                1   0.32420 0.32462 -420.70
- Sub_1a_government_consumption           1   0.34100 0.34141 -416.46
- Sub_5b_labor_market_reg                 1   0.39719 0.39761 -403.66
- Sub_1d_top_marg_tax_rate                1   0.41594 0.41636 -399.79
- Sub_1c_gov_enterprises                  1   0.98109 0.98151 -327.76
- Sub_3d_freedom_own_foreign_currency     1   1.09282 1.09324 -318.70

Step:  AIC=-979.27
Economic_Freedom ~ Sub_1a_government_consumption + Sub_1b_transfers + 
Sub_1c_gov_enterprises + Sub_1d_top_marg_tax_rate + Sub_2a_judicial_independence + 
Sub_2c_protection_property_rights + Sub_2e_integrity_legal_system + 
Sub_2f_legal_enforcement_contracts + Sub_2i_business_costs_crime + 
Sub_2j_gender_adjustment + Sub_2_property_rights + Sub_3a_money_growth + 
Sub_3b_std_inflation + Sub_3c_inflation + Sub_3d_freedom_own_foreign_currency + 
Sub_4a_tariffs + Sub_4b_regulatory_trade_barriers + Sub_4c_black_market + 
Sub_4d_control_movement_capital_ppl + Sub_5a_credit_market_reg + 
Sub_5b_labor_market_reg + Sub_5c_business_reg

Df Sum of Sq     RSS     AIC
- Sub_2e_integrity_legal_system           1   0.00000 0.00042 -980.99
- Sub_2a_judicial_independence            1   0.00000 0.00042 -980.98
- Sub_2j_gender_adjustment                1   0.00000 0.00042 -980.87
- Sub_2f_legal_enforcement_contracts      1   0.00001 0.00043 -979.81
- Sub_2i_business_costs_crime             1   0.00001 0.00043 -979.80
- Sub_2c_protection_property_rights       1   0.00001 0.00043 -979.76
<none>                                                0.00042 -979.27
+ Sub_2b_impartial_courts                 1   0.00000 0.00042 -977.71
+ Sub_2h_reliability_police               1   0.00000 0.00042 -977.52
+ Sub_2d_military_interference            1   0.00000 0.00042 -977.32
+ Sub_2g_restrictions_sale_real_property  1   0.00000 0.00042 -977.30
- Sub_5c_business_reg                     1   0.06796 0.06838 -553.53
- Sub_4b_regulatory_trade_barriers        1   0.10049 0.10091 -520.84
- Sub_3b_std_inflation                    1   0.12486 0.12528 -502.67
- Sub_3a_money_growth                     1   0.14336 0.14378 -491.10
- Sub_2_property_rights                   1   0.14793 0.14835 -488.48
- Sub_4a_tariffs                          1   0.15901 0.15943 -482.43
- Sub_4c_black_market                     1   0.16051 0.16093 -481.64
- Sub_3c_inflation                        1   0.20428 0.20470 -461.43
- Sub_1b_transfers                        1   0.23495 0.23537 -449.70
- Sub_4d_control_movement_capital_ppl     1   0.24666 0.24708 -445.62
- Sub_5a_credit_market_reg                1   0.32583 0.32625 -422.28
- Sub_1a_government_consumption           1   0.35444 0.35486 -415.22
- Sub_5b_labor_market_reg                 1   0.40771 0.40813 -403.47
- Sub_1d_top_marg_tax_rate                1   0.41816 0.41858 -401.34
- Sub_1c_gov_enterprises                  1   0.98331 0.98373 -329.57
- Sub_3d_freedom_own_foreign_currency     1   1.25788 1.25830 -308.89

Step:  AIC=-980.99
Economic_Freedom ~ Sub_1a_government_consumption + Sub_1b_transfers + 
Sub_1c_gov_enterprises + Sub_1d_top_marg_tax_rate + Sub_2a_judicial_independence + 
Sub_2c_protection_property_rights + Sub_2f_legal_enforcement_contracts + 
Sub_2i_business_costs_crime + Sub_2j_gender_adjustment + 
Sub_2_property_rights + Sub_3a_money_growth + Sub_3b_std_inflation + 
Sub_3c_inflation + Sub_3d_freedom_own_foreign_currency + 
Sub_4a_tariffs + Sub_4b_regulatory_trade_barriers + Sub_4c_black_market + 
Sub_4d_control_movement_capital_ppl + Sub_5a_credit_market_reg + 
Sub_5b_labor_market_reg + Sub_5c_business_reg

Df Sum of Sq     RSS     AIC
- Sub_2j_gender_adjustment                1   0.00000 0.00042 -982.77
- Sub_2a_judicial_independence            1   0.00000 0.00042 -982.47
- Sub_2f_legal_enforcement_contracts      1   0.00001 0.00043 -981.77
- Sub_2c_protection_property_rights       1   0.00001 0.00043 -981.54
- Sub_2i_business_costs_crime             1   0.00001 0.00043 -981.31
<none>                                                0.00042 -980.99
+ Sub_2e_integrity_legal_system           1   0.00000 0.00042 -979.27
+ Sub_2b_impartial_courts                 1   0.00000 0.00042 -979.18
+ Sub_2d_military_interference            1   0.00000 0.00042 -979.14
+ Sub_2h_reliability_police               1   0.00000 0.00042 -979.13
+ Sub_2g_restrictions_sale_real_property  1   0.00000 0.00042 -979.12
- Sub_5c_business_reg                     1   0.06810 0.06852 -555.36
- Sub_4b_regulatory_trade_barriers        1   0.10082 0.10125 -522.57
- Sub_3b_std_inflation                    1   0.12772 0.12814 -502.77
- Sub_3a_money_growth                     1   0.14348 0.14390 -493.03
- Sub_4a_tariffs                          1   0.15937 0.15979 -484.23
- Sub_4c_black_market                     1   0.16298 0.16340 -482.36
- Sub_2_property_rights                   1   0.18401 0.18443 -472.19
- Sub_3c_inflation                        1   0.20550 0.20592 -462.93
- Sub_1b_transfers                        1   0.23981 0.24023 -449.99
- Sub_4d_control_movement_capital_ppl     1   0.24782 0.24825 -447.23
- Sub_5a_credit_market_reg                1   0.32616 0.32658 -424.19
- Sub_1a_government_consumption           1   0.35444 0.35486 -417.21
- Sub_5b_labor_market_reg                 1   0.40814 0.40856 -405.38
- Sub_1d_top_marg_tax_rate                1   0.43298 0.43340 -400.42
- Sub_1c_gov_enterprises                  1   0.98334 0.98376 -331.56
- Sub_3d_freedom_own_foreign_currency     1   1.27018 1.27060 -310.07

Step:  AIC=-982.77
Economic_Freedom ~ Sub_1a_government_consumption + Sub_1b_transfers + 
Sub_1c_gov_enterprises + Sub_1d_top_marg_tax_rate + Sub_2a_judicial_independence + 
Sub_2c_protection_property_rights + Sub_2f_legal_enforcement_contracts + 
Sub_2i_business_costs_crime + Sub_2_property_rights + Sub_3a_money_growth + 
Sub_3b_std_inflation + Sub_3c_inflation + Sub_3d_freedom_own_foreign_currency + 
Sub_4a_tariffs + Sub_4b_regulatory_trade_barriers + Sub_4c_black_market + 
Sub_4d_control_movement_capital_ppl + Sub_5a_credit_market_reg + 
Sub_5b_labor_market_reg + Sub_5c_business_reg

Df Sum of Sq     RSS     AIC
- Sub_2f_legal_enforcement_contracts      1   0.00001 0.00043 -983.76
- Sub_2a_judicial_independence            1   0.00001 0.00043 -983.60
- Sub_2i_business_costs_crime             1   0.00001 0.00043 -983.11
- Sub_2c_protection_property_rights       1   0.00001 0.00043 -983.04
<none>                                                0.00042 -982.77
+ Sub_2d_military_interference            1   0.00000 0.00042 -981.08
+ Sub_2g_restrictions_sale_real_property  1   0.00000 0.00042 -981.04
+ Sub_2j_gender_adjustment                1   0.00000 0.00042 -980.99
+ Sub_2b_impartial_courts                 1   0.00000 0.00042 -980.91
+ Sub_2e_integrity_legal_system           1   0.00000 0.00042 -980.87
+ Sub_2h_reliability_police               1   0.00000 0.00042 -980.80
- Sub_5c_business_reg                     1   0.06960 0.07002 -555.54
- Sub_4b_regulatory_trade_barriers        1   0.10126 0.10168 -524.20
- Sub_3a_money_growth                     1   0.14382 0.14424 -494.83
- Sub_3b_std_inflation                    1   0.14875 0.14917 -492.01
- Sub_4a_tariffs                          1   0.16054 0.16096 -485.62
- Sub_4c_black_market                     1   0.16350 0.16392 -484.09
- Sub_3c_inflation                        1   0.22691 0.22734 -456.62
- Sub_1b_transfers                        1   0.24570 0.24612 -449.95
- Sub_4d_control_movement_capital_ppl     1   0.24991 0.25034 -448.52
- Sub_5a_credit_market_reg                1   0.32845 0.32888 -425.60
- Sub_1a_government_consumption           1   0.35453 0.35495 -419.19
- Sub_5b_labor_market_reg                 1   0.40847 0.40889 -407.31
- Sub_1d_top_marg_tax_rate                1   0.43373 0.43415 -402.28
- Sub_2_property_rights                   1   0.44728 0.44770 -399.69
- Sub_1c_gov_enterprises                  1   1.00268 1.00310 -331.93
- Sub_3d_freedom_own_foreign_currency     1   1.28341 1.28383 -311.20

Step:  AIC=-983.76
Economic_Freedom ~ Sub_1a_government_consumption + Sub_1b_transfers + 
Sub_1c_gov_enterprises + Sub_1d_top_marg_tax_rate + Sub_2a_judicial_independence + 
Sub_2c_protection_property_rights + Sub_2i_business_costs_crime + 
Sub_2_property_rights + Sub_3a_money_growth + Sub_3b_std_inflation + 
Sub_3c_inflation + Sub_3d_freedom_own_foreign_currency + 
Sub_4a_tariffs + Sub_4b_regulatory_trade_barriers + Sub_4c_black_market + 
Sub_4d_control_movement_capital_ppl + Sub_5a_credit_market_reg + 
Sub_5b_labor_market_reg + Sub_5c_business_reg

Df Sum of Sq     RSS     AIC
- Sub_2c_protection_property_rights       1   0.00001 0.00043 -984.39
- Sub_2i_business_costs_crime             1   0.00001 0.00044 -983.94
<none>                                                0.00043 -983.76
- Sub_2a_judicial_independence            1   0.00001 0.00044 -983.76
+ Sub_2f_legal_enforcement_contracts      1   0.00001 0.00042 -982.77
+ Sub_2d_military_interference            1   0.00000 0.00043 -982.29
+ Sub_2b_impartial_courts                 1   0.00000 0.00043 -982.12
+ Sub_2g_restrictions_sale_real_property  1   0.00000 0.00043 -981.89
+ Sub_2e_integrity_legal_system           1   0.00000 0.00043 -981.80
+ Sub_2h_reliability_police               1   0.00000 0.00043 -981.79
+ Sub_2j_gender_adjustment                1   0.00000 0.00043 -981.77
- Sub_5c_business_reg                     1   0.07042 0.07085 -556.55
- Sub_4b_regulatory_trade_barriers        1   0.10901 0.10944 -520.03
- Sub_3a_money_growth                     1   0.14390 0.14433 -496.79
- Sub_3b_std_inflation                    1   0.14878 0.14920 -493.99
- Sub_4a_tariffs                          1   0.16102 0.16145 -487.37
- Sub_4c_black_market                     1   0.16466 0.16508 -485.50
- Sub_3c_inflation                        1   0.23019 0.23062 -457.41
- Sub_4d_control_movement_capital_ppl     1   0.25382 0.25425 -449.22
- Sub_1b_transfers                        1   0.26352 0.26395 -446.08
- Sub_5a_credit_market_reg                1   0.33707 0.33750 -425.43
- Sub_1a_government_consumption           1   0.37116 0.37159 -417.35
- Sub_5b_labor_market_reg                 1   0.42318 0.42361 -406.34
- Sub_1d_top_marg_tax_rate                1   0.45315 0.45357 -400.60
- Sub_2_property_rights                   1   0.53103 0.53146 -387.29
- Sub_1c_gov_enterprises                  1   1.10251 1.10293 -325.96
- Sub_3d_freedom_own_foreign_currency     1   1.39191 1.39234 -306.39

Step:  AIC=-984.39
Economic_Freedom ~ Sub_1a_government_consumption + Sub_1b_transfers + 
Sub_1c_gov_enterprises + Sub_1d_top_marg_tax_rate + Sub_2a_judicial_independence + 
Sub_2i_business_costs_crime + Sub_2_property_rights + Sub_3a_money_growth + 
Sub_3b_std_inflation + Sub_3c_inflation + Sub_3d_freedom_own_foreign_currency + 
Sub_4a_tariffs + Sub_4b_regulatory_trade_barriers + Sub_4c_black_market + 
Sub_4d_control_movement_capital_ppl + Sub_5a_credit_market_reg + 
Sub_5b_labor_market_reg + Sub_5c_business_reg

Df Sum of Sq     RSS     AIC
- Sub_2i_business_costs_crime             1   0.00001 0.00044 -984.54
<none>                                                0.00043 -984.39
+ Sub_2c_protection_property_rights       1   0.00001 0.00043 -983.76
+ Sub_2f_legal_enforcement_contracts      1   0.00000 0.00043 -983.04
+ Sub_2d_military_interference            1   0.00000 0.00043 -982.51
+ Sub_2j_gender_adjustment                1   0.00000 0.00043 -982.50
+ Sub_2h_reliability_police               1   0.00000 0.00043 -982.43
+ Sub_2b_impartial_courts                 1   0.00000 0.00043 -982.40
+ Sub_2e_integrity_legal_system           1   0.00000 0.00043 -982.39
+ Sub_2g_restrictions_sale_real_property  1   0.00000 0.00043 -982.39
- Sub_2a_judicial_independence            1   0.00004 0.00047 -979.12
- Sub_5c_business_reg                     1   0.07263 0.07306 -555.97
- Sub_4b_regulatory_trade_barriers        1   0.11402 0.11445 -518.27
- Sub_3a_money_growth                     1   0.14758 0.14802 -496.67
- Sub_3b_std_inflation                    1   0.15629 0.15673 -491.86
- Sub_4a_tariffs                          1   0.16375 0.16419 -487.96
- Sub_4c_black_market                     1   0.16498 0.16541 -487.33
- Sub_3c_inflation                        1   0.23158 0.23202 -458.91
- Sub_4d_control_movement_capital_ppl     1   0.25405 0.25449 -451.14
- Sub_1b_transfers                        1   0.28795 0.28839 -440.64
- Sub_5a_credit_market_reg                1   0.33917 0.33961 -426.91
- Sub_1a_government_consumption           1   0.37192 0.37236 -419.17
- Sub_5b_labor_market_reg                 1   0.42436 0.42480 -408.10
- Sub_1d_top_marg_tax_rate                1   0.45421 0.45464 -402.40
- Sub_2_property_rights                   1   0.55051 0.55094 -386.26
- Sub_1c_gov_enterprises                  1   1.13810 1.13853 -325.29
- Sub_3d_freedom_own_foreign_currency     1   1.40967 1.41011 -307.32

Step:  AIC=-984.54
Economic_Freedom ~ Sub_1a_government_consumption + Sub_1b_transfers + 
Sub_1c_gov_enterprises + Sub_1d_top_marg_tax_rate + Sub_2a_judicial_independence + 
Sub_2_property_rights + Sub_3a_money_growth + Sub_3b_std_inflation + 
Sub_3c_inflation + Sub_3d_freedom_own_foreign_currency + 
Sub_4a_tariffs + Sub_4b_regulatory_trade_barriers + Sub_4c_black_market + 
Sub_4d_control_movement_capital_ppl + Sub_5a_credit_market_reg + 
Sub_5b_labor_market_reg + Sub_5c_business_reg

Df Sum of Sq     RSS     AIC
<none>                                                0.00044 -984.54
+ Sub_2i_business_costs_crime             1   0.00001 0.00043 -984.39
+ Sub_2c_protection_property_rights       1   0.00001 0.00044 -983.94
+ Sub_2f_legal_enforcement_contracts      1   0.00000 0.00044 -983.32
+ Sub_2h_reliability_police               1   0.00000 0.00044 -983.11
+ Sub_2d_military_interference            1   0.00000 0.00044 -983.07
+ Sub_2e_integrity_legal_system           1   0.00000 0.00044 -982.92
+ Sub_2j_gender_adjustment                1   0.00000 0.00044 -982.85
+ Sub_2b_impartial_courts                 1   0.00000 0.00044 -982.66
+ Sub_2g_restrictions_sale_real_property  1   0.00000 0.00044 -982.58
- Sub_2a_judicial_independence            1   0.00005 0.00050 -976.76
- Sub_5c_business_reg                     1   0.07733 0.07777 -552.72
- Sub_4b_regulatory_trade_barriers        1   0.11414 0.11459 -520.17
- Sub_3a_money_growth                     1   0.14797 0.14842 -498.44
- Sub_3b_std_inflation                    1   0.15730 0.15775 -493.32
- Sub_4a_tariffs                          1   0.16375 0.16419 -489.95
- Sub_4c_black_market                     1   0.16596 0.16641 -488.83
- Sub_3c_inflation                        1   0.23163 0.23208 -460.89
- Sub_4d_control_movement_capital_ppl     1   0.26094 0.26138 -450.90
- Sub_1b_transfers                        1   0.28933 0.28977 -442.24
- Sub_5a_credit_market_reg                1   0.34165 0.34210 -428.29
- Sub_1a_government_consumption           1   0.38364 0.38409 -418.57
- Sub_5b_labor_market_reg                 1   0.42435 0.42480 -410.10
- Sub_1d_top_marg_tax_rate                1   0.45450 0.45494 -404.35
- Sub_2_property_rights                   1   0.62050 0.62094 -378.22
- Sub_1c_gov_enterprises                  1   1.16282 1.16326 -325.49
- Sub_3d_freedom_own_foreign_currency     1   1.46754 1.46798 -305.94
Call:
lm(formula = Economic_Freedom ~ Sub_1a_government_consumption + 
Sub_1b_transfers + Sub_1c_gov_enterprises + Sub_1d_top_marg_tax_rate + 
Sub_2a_judicial_independence + Sub_2_property_rights + Sub_3a_money_growth + 
Sub_3b_std_inflation + Sub_3c_inflation + Sub_3d_freedom_own_foreign_currency + 
Sub_4a_tariffs + Sub_4b_regulatory_trade_barriers + Sub_4c_black_market + 
Sub_4d_control_movement_capital_ppl + Sub_5a_credit_market_reg + 
Sub_5b_labor_market_reg + Sub_5c_business_reg, data = train)

Coefficients:
(Intercept)        Sub_1a_government_consumption  
-0.0046972                            0.0501254  
Sub_1b_transfers               Sub_1c_gov_enterprises  
0.0501557                            0.0498513  
Sub_1d_top_marg_tax_rate         Sub_2a_judicial_independence  
0.0499166                           -0.0008145  
Sub_2_property_rights                  Sub_3a_money_growth  
0.2012723                            0.0495250  
Sub_3b_std_inflation                     Sub_3c_inflation  
0.0500013                            0.0499551  
Sub_3d_freedom_own_foreign_currency                       Sub_4a_tariffs  
0.0499013                            0.0500865  
Sub_4b_regulatory_trade_barriers                  Sub_4c_black_market  
0.0496454                            0.0505071  
Sub_4d_control_movement_capital_ppl             Sub_5a_credit_market_reg  
0.0507076                            0.0663383  
Sub_5b_labor_market_reg                  Sub_5c_business_reg  
0.0664735                            0.0672248  
Using the final step from stepAIC function and assigning it as model_2. I found from the summary of this model that, although the number of insignificant variables have reduced considerably, the multiple Rsquare and adjusted Rsquare is still 1, meaning multicollinear variables are still present in our data and they will definitely reduce the accuracy on the test data. So we need to find them and remove. For that I am using VIF(variance inflation factor) to get rid of redundant predictors or the variables that have high multicollinearity between them.

Multicollinearity exists when two or more predictor variables are highly related to each other and then it becomes difficult to understand the impact of an independent variable on the dependent variable. A predictor having a VIF of 2 or less is generally considered safe and it can be assumed that it is not correlated with other predictor variables. Higher the VIF, greater is the correlation of the predictor variable w.r.t other predictor variables.

model_2 <- lm(formula = Economic_Freedom ~ Sub_1a_government_consumption + 
Sub_1b_transfers + Sub_1c_gov_enterprises + Sub_1d_top_marg_tax_rate + 
Sub_2a_judicial_independence + Sub_2_property_rights + Sub_3a_money_growth + 
Sub_3b_std_inflation + Sub_3c_inflation + Sub_3d_freedom_own_foreign_currency + 
Sub_4a_tariffs + Sub_4b_regulatory_trade_barriers + Sub_4c_black_market + 
Sub_4d_control_movement_capital_ppl + Sub_5a_credit_market_reg + 
Sub_5b_labor_market_reg + Sub_5c_business_reg, data = train)

summary(model_2)

sort(vif(model_2))
Call:
lm(formula = Economic_Freedom ~ Sub_1a_government_consumption + 
Sub_1b_transfers + Sub_1c_gov_enterprises + Sub_1d_top_marg_tax_rate + 
Sub_2a_judicial_independence + Sub_2_property_rights + Sub_3a_money_growth + 
Sub_3b_std_inflation + Sub_3c_inflation + Sub_3d_freedom_own_foreign_currency + 
Sub_4a_tariffs + Sub_4b_regulatory_trade_barriers + Sub_4c_black_market + 
Sub_4d_control_movement_capital_ppl + Sub_5a_credit_market_reg + 
Sub_5b_labor_market_reg + Sub_5c_business_reg, data = train)

Residuals:
Min         1Q     Median         3Q        Max 
-0.0049510 -0.0016784  0.0001251  0.0016886  0.0050041 

Coefficients:
Estimate Std. Error t value Pr(>|t|)    
(Intercept)                         -0.0046972  0.0038480  -1.221  0.22655    
Sub_1a_government_consumption        0.0501254  0.0002100 238.664  < 2e-16 ***
Sub_1b_transfers                     0.0501557  0.0002420 207.262  < 2e-16 ***
Sub_1c_gov_enterprises               0.0498513  0.0001200 415.508  < 2e-16 ***
Sub_1d_top_marg_tax_rate             0.0499166  0.0001922 259.770  < 2e-16 ***
Sub_2a_judicial_independence        -0.0008145  0.0002852  -2.856  0.00574 ** 
Sub_2_property_rights                0.2012723  0.0006631 303.524  < 2e-16 ***
Sub_3a_money_growth                  0.0495250  0.0003341 148.223  < 2e-16 ***
Sub_3b_std_inflation                 0.0500013  0.0003272 152.824  < 2e-16 ***
Sub_3c_inflation                     0.0499551  0.0002694 185.449  < 2e-16 ***
Sub_3d_freedom_own_foreign_currency  0.0499013  0.0001069 466.786  < 2e-16 ***
Sub_4a_tariffs                       0.0500865  0.0003212 155.922  < 2e-16 ***
Sub_4b_regulatory_trade_barriers     0.0496454  0.0003814 130.180  < 2e-16 ***
Sub_4c_black_market                  0.0505071  0.0003218 156.974  < 2e-16 ***
Sub_4d_control_movement_capital_ppl  0.0507076  0.0002576 196.830  < 2e-16 ***
Sub_5a_credit_market_reg             0.0663383  0.0002945 225.225  < 2e-16 ***
Sub_5b_labor_market_reg              0.0664735  0.0002648 251.008  < 2e-16 ***
Sub_5c_business_reg                  0.0672248  0.0006274 107.151  < 2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 0.002595 on 66 degrees of freedom
Multiple R-squared:      1,	Adjusted R-squared:      1 
F-statistic: 5.191e+05 on 17 and 66 DF,  p-value: < 2.2e-16
Sub_5b_labor_market_reg
1.42386946456915
Sub_1c_gov_enterprises
1.61761813774769
Sub_3a_money_growth
1.62740780957483
Sub_4a_tariffs
1.70506380799146
Sub_3d_freedom_own_foreign_currency
1.99324818780368
Sub_4c_black_market
2.00824481356788
Sub_1d_top_marg_tax_rate
2.21775784478939
Sub_1a_government_consumption
2.22572235904933
Sub_4d_control_movement_capital_ppl
2.31600162169714
Sub_5a_credit_market_reg
2.36450186641012
Sub_1b_transfers
2.57196467980691
Sub_3c_inflation
3.18946020235768
Sub_3b_std_inflation
3.57705132608818
Sub_2a_judicial_independence
4.25746886715685
Sub_4b_regulatory_trade_barriers
5.5396937645821
Sub_5c_business_reg
6.24491142294768
Sub_2_property_rights
11.2217120694939
After running model_2 and analysing the summary and vif, I found that property_rights has high vif, so in the next model I will remove property_rights and check the summary and vif of all predictors again. I will repeat these steps until I get the VIF of all predictors within 2.

#Removing Sub_2_property_rights
model_3 <- lm(formula = Economic_Freedom ~ Sub_1a_government_consumption + 
Sub_1b_transfers + Sub_1c_gov_enterprises + Sub_1d_top_marg_tax_rate + 
Sub_2a_judicial_independence + Sub_3a_money_growth + 
Sub_3b_std_inflation + Sub_3c_inflation + Sub_3d_freedom_own_foreign_currency + 
Sub_4a_tariffs + Sub_4b_regulatory_trade_barriers + Sub_4c_black_market + 
Sub_4d_control_movement_capital_ppl + Sub_5a_credit_market_reg + 
Sub_5b_labor_market_reg + Sub_5c_business_reg, data = train)

summary(model_3)

sort(vif(model_3))
Call:
lm(formula = Economic_Freedom ~ Sub_1a_government_consumption + 
Sub_1b_transfers + Sub_1c_gov_enterprises + Sub_1d_top_marg_tax_rate + 
Sub_2a_judicial_independence + Sub_3a_money_growth + Sub_3b_std_inflation + 
Sub_3c_inflation + Sub_3d_freedom_own_foreign_currency + 
Sub_4a_tariffs + Sub_4b_regulatory_trade_barriers + Sub_4c_black_market + 
Sub_4d_control_movement_capital_ppl + Sub_5a_credit_market_reg + 
Sub_5b_labor_market_reg + Sub_5c_business_reg, data = train)

Residuals:
Min       1Q   Median       3Q      Max 
-0.25487 -0.05437  0.00272  0.04908  0.19836 

Coefficients:
Estimate Std. Error t value Pr(>|t|)    
(Intercept)                         -0.027308   0.142714  -0.191 0.848830    
Sub_1a_government_consumption        0.027429   0.007280   3.768 0.000350 ***
Sub_1b_transfers                     0.033123   0.008732   3.793 0.000322 ***
Sub_1c_gov_enterprises               0.050140   0.004450  11.266  < 2e-16 ***
Sub_1d_top_marg_tax_rate             0.051245   0.007126   7.191 6.88e-10 ***
Sub_2a_judicial_independence         0.045938   0.008905   5.158 2.40e-06 ***
Sub_3a_money_growth                  0.069842   0.012143   5.752 2.38e-07 ***
Sub_3b_std_inflation                 0.071880   0.011838   6.072 6.65e-08 ***
Sub_3c_inflation                     0.034707   0.009817   3.535 0.000744 ***
Sub_3d_freedom_own_foreign_currency  0.056875   0.003873  14.685  < 2e-16 ***
Sub_4a_tariffs                       0.036492   0.011799   3.093 0.002891 ** 
Sub_4b_regulatory_trade_barriers     0.092904   0.013121   7.080 1.09e-09 ***
Sub_4c_black_market                  0.052555   0.011933   4.404 3.92e-05 ***
Sub_4d_control_movement_capital_ppl  0.037097   0.009410   3.942 0.000196 ***
Sub_5a_credit_market_reg             0.078101   0.010831   7.211 6.34e-10 ***
Sub_5b_labor_market_reg              0.082055   0.009637   8.514 2.85e-12 ***
Sub_5c_business_reg                  0.146773   0.021145   6.941 1.93e-09 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 0.09627 on 67 degrees of freedom
Multiple R-squared:  0.9896,	Adjusted R-squared:  0.9871 
F-statistic: 396.6 on 16 and 67 DF,  p-value: < 2.2e-16
Sub_5b_labor_market_reg
1.37036977530913
Sub_3a_money_growth
1.5620917464262
Sub_1c_gov_enterprises
1.61751678023668
Sub_4a_tariffs
1.67191727715887
Sub_3d_freedom_own_foreign_currency
1.90118743596658
Sub_1a_government_consumption
1.94357346782453
Sub_4c_black_market
2.00736207019696
Sub_1d_top_marg_tax_rate
2.21660754341332
Sub_4d_control_movement_capital_ppl
2.2458352610146
Sub_5a_credit_market_reg
2.32356558453878
Sub_1b_transfers
2.43365200619307
Sub_2a_judicial_independence
3.01592696376653
Sub_3c_inflation
3.07853680358688
Sub_3b_std_inflation
3.40342322897958
Sub_4b_regulatory_trade_barriers
4.76597961449525
Sub_5c_business_reg
5.1551516461053
#Removing Sub_5c_business_reg due to high VIF
model_4 <- lm(formula = Economic_Freedom ~ Sub_1a_government_consumption + 
Sub_1b_transfers + Sub_1c_gov_enterprises + Sub_1d_top_marg_tax_rate + 
Sub_2a_judicial_independence + Sub_3a_money_growth + 
Sub_3b_std_inflation + Sub_3c_inflation + Sub_3d_freedom_own_foreign_currency + 
Sub_4a_tariffs + Sub_4b_regulatory_trade_barriers + Sub_4c_black_market + 
Sub_4d_control_movement_capital_ppl + Sub_5a_credit_market_reg + 
Sub_5b_labor_market_reg, data = train)

summary(model_4)

sort(vif(model_4))
Call:
lm(formula = Economic_Freedom ~ Sub_1a_government_consumption + 
Sub_1b_transfers + Sub_1c_gov_enterprises + Sub_1d_top_marg_tax_rate + 
Sub_2a_judicial_independence + Sub_3a_money_growth + Sub_3b_std_inflation + 
Sub_3c_inflation + Sub_3d_freedom_own_foreign_currency + 
Sub_4a_tariffs + Sub_4b_regulatory_trade_barriers + Sub_4c_black_market + 
Sub_4d_control_movement_capital_ppl + Sub_5a_credit_market_reg + 
Sub_5b_labor_market_reg, data = train)

Residuals:
Min       1Q   Median       3Q      Max 
-0.30426 -0.07899 -0.00748  0.08172  0.32114 

Coefficients:
Estimate Std. Error t value Pr(>|t|)    
(Intercept)                         0.223870   0.179670   1.246 0.217036    
Sub_1a_government_consumption       0.026973   0.009475   2.847 0.005834 ** 
Sub_1b_transfers                    0.041849   0.011246   3.721 0.000404 ***
Sub_1c_gov_enterprises              0.052975   0.005768   9.185 1.57e-13 ***
Sub_1d_top_marg_tax_rate            0.067792   0.008740   7.756 6.07e-11 ***
Sub_2a_judicial_independence        0.085654   0.008881   9.644 2.35e-14 ***
Sub_3a_money_growth                 0.064576   0.015773   4.094 0.000115 ***
Sub_3b_std_inflation                0.063869   0.015334   4.165 8.98e-05 ***
Sub_3c_inflation                    0.048601   0.012508   3.886 0.000234 ***
Sub_3d_freedom_own_foreign_currency 0.054342   0.005018  10.829  < 2e-16 ***
Sub_4a_tariffs                      0.048820   0.015182   3.216 0.001993 ** 
Sub_4b_regulatory_trade_barriers    0.139405   0.014684   9.494 4.37e-14 ***
Sub_4c_black_market                 0.030442   0.014967   2.034 0.045856 *  
Sub_4d_control_movement_capital_ppl 0.026396   0.012082   2.185 0.032358 *  
Sub_5a_credit_market_reg            0.106398   0.013060   8.147 1.18e-11 ***
Sub_5b_labor_market_reg             0.079441   0.012533   6.338 2.16e-08 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 0.1253 on 68 degrees of freedom
Multiple R-squared:  0.982,	Adjusted R-squared:  0.9781 
F-statistic: 247.9 on 15 and 68 DF,  p-value: < 2.2e-16
Sub_5b_labor_market_reg
1.36827774282104
Sub_3a_money_growth
1.55599424806306
Sub_1c_gov_enterprises
1.60388574657426
Sub_4a_tariffs
1.63403932911938
Sub_2a_judicial_independence
1.77095509120911
Sub_4c_black_market
1.86429605681789
Sub_3d_freedom_own_foreign_currency
1.88431745935354
Sub_1a_government_consumption
1.94341524230429
Sub_1d_top_marg_tax_rate
1.96855109820965
Sub_5a_credit_market_reg
1.9944111569992
Sub_4d_control_movement_capital_ppl
2.18556394162135
Sub_1b_transfers
2.38320483530425
Sub_3c_inflation
2.9505585453731
Sub_3b_std_inflation
3.37107122398518
Sub_4b_regulatory_trade_barriers
3.52367746676447
#Removing  Sub_4b_regulatory_trade_barriers due to high VIF
model_5 <- lm(formula = Economic_Freedom ~ Sub_1a_government_consumption + 
Sub_1b_transfers + Sub_1c_gov_enterprises + Sub_1d_top_marg_tax_rate + 
Sub_2a_judicial_independence + Sub_3a_money_growth + 
Sub_3b_std_inflation + Sub_3c_inflation + Sub_3d_freedom_own_foreign_currency + 
Sub_4a_tariffs + Sub_4c_black_market + 
Sub_4d_control_movement_capital_ppl + Sub_5a_credit_market_reg + 
Sub_5b_labor_market_reg, data = train)

summary(model_5)

sort(vif(model_5))
Call:
lm(formula = Economic_Freedom ~ Sub_1a_government_consumption + 
Sub_1b_transfers + Sub_1c_gov_enterprises + Sub_1d_top_marg_tax_rate + 
Sub_2a_judicial_independence + Sub_3a_money_growth + Sub_3b_std_inflation + 
Sub_3c_inflation + Sub_3d_freedom_own_foreign_currency + 
Sub_4a_tariffs + Sub_4c_black_market + Sub_4d_control_movement_capital_ppl + 
Sub_5a_credit_market_reg + Sub_5b_labor_market_reg, data = train)

Residuals:
Min       1Q   Median       3Q      Max 
-0.57295 -0.11913  0.02406  0.11038  0.38059 

Coefficients:
Estimate Std. Error t value Pr(>|t|)    
(Intercept)                         0.225713   0.271994   0.830 0.409489    
Sub_1a_government_consumption       0.028863   0.014340   2.013 0.048045 *  
Sub_1b_transfers                    0.005496   0.016008   0.343 0.732386    
Sub_1c_gov_enterprises              0.044231   0.008619   5.132 2.52e-06 ***
Sub_1d_top_marg_tax_rate            0.061264   0.013190   4.645 1.58e-05 ***
Sub_2a_judicial_independence        0.106738   0.013018   8.199 8.64e-12 ***
Sub_3a_money_growth                 0.049436   0.023756   2.081 0.041145 *  
Sub_3b_std_inflation                0.078202   0.023101   3.385 0.001177 ** 
Sub_3c_inflation                    0.041639   0.018903   2.203 0.030955 *  
Sub_3d_freedom_own_foreign_currency 0.066004   0.007366   8.961 3.51e-13 ***
Sub_4a_tariffs                      0.106178   0.021085   5.036 3.64e-06 ***
Sub_4c_black_market                 0.055497   0.022302   2.488 0.015250 *  
Sub_4d_control_movement_capital_ppl 0.062325   0.017370   3.588 0.000618 ***
Sub_5a_credit_market_reg            0.127449   0.019484   6.541 8.92e-09 ***
Sub_5b_labor_market_reg             0.086351   0.018941   4.559 2.16e-05 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 0.1897 on 69 degrees of freedom
Multiple R-squared:  0.9582,	Adjusted R-squared:  0.9498 
F-statistic: 113.1 on 14 and 69 DF,  p-value: < 2.2e-16
Sub_5b_labor_market_reg
1.36366280225974
Sub_4a_tariffs
1.37525987056602
Sub_3a_money_growth
1.54008907761059
Sub_1c_gov_enterprises
1.56297956066213
Sub_2a_judicial_independence
1.66021984266488
Sub_3d_freedom_own_foreign_currency
1.77140151956667
Sub_4c_black_market
1.80632897205263
Sub_5a_credit_market_reg
1.93691787224818
Sub_1a_government_consumption
1.94255654903238
Sub_1d_top_marg_tax_rate
1.9563671357535
Sub_4d_control_movement_capital_ppl
1.97113254526475
Sub_1b_transfers
2.10691265292863
Sub_3c_inflation
2.94041698621175
Sub_3b_std_inflation
3.3383902022863
#Removing  Sub_3b_std_inflation due to high VIF
model_6 <- lm(formula = Economic_Freedom ~ Sub_1a_government_consumption + 
Sub_1b_transfers + Sub_1c_gov_enterprises + Sub_1d_top_marg_tax_rate + 
Sub_2a_judicial_independence + Sub_3a_money_growth + 
Sub_3c_inflation + Sub_3d_freedom_own_foreign_currency + 
Sub_4a_tariffs + Sub_4c_black_market + 
Sub_4d_control_movement_capital_ppl + Sub_5a_credit_market_reg + 
Sub_5b_labor_market_reg, data = train)

summary(model_6)

sort(vif(model_6))
Call:
lm(formula = Economic_Freedom ~ Sub_1a_government_consumption + 
Sub_1b_transfers + Sub_1c_gov_enterprises + Sub_1d_top_marg_tax_rate + 
Sub_2a_judicial_independence + Sub_3a_money_growth + Sub_3c_inflation + 
Sub_3d_freedom_own_foreign_currency + Sub_4a_tariffs + Sub_4c_black_market + 
Sub_4d_control_movement_capital_ppl + Sub_5a_credit_market_reg + 
Sub_5b_labor_market_reg, data = train)

Residuals:
Min       1Q   Median       3Q      Max 
-0.72863 -0.11048  0.00592  0.11071  0.40159 

Coefficients:
Estimate Std. Error t value Pr(>|t|)    
(Intercept)                         0.467640   0.281363   1.662 0.100975    
Sub_1a_government_consumption       0.048957   0.013995   3.498 0.000819 ***
Sub_1b_transfers                    0.012801   0.017005   0.753 0.454132    
Sub_1c_gov_enterprises              0.049543   0.009086   5.452 7.03e-07 ***
Sub_1d_top_marg_tax_rate            0.040698   0.012552   3.242 0.001818 ** 
Sub_2a_judicial_independence        0.113448   0.013794   8.224 7.05e-12 ***
Sub_3a_money_growth                 0.033828   0.024985   1.354 0.180101    
Sub_3c_inflation                    0.084232   0.015125   5.569 4.43e-07 ***
Sub_3d_freedom_own_foreign_currency 0.063112   0.007843   8.047 1.50e-11 ***
Sub_4a_tariffs                      0.116062   0.022388   5.184 2.01e-06 ***
Sub_4c_black_market                 0.054429   0.023908   2.277 0.025871 *  
Sub_4d_control_movement_capital_ppl 0.076297   0.018089   4.218 7.26e-05 ***
Sub_5a_credit_market_reg            0.128073   0.020888   6.132 4.58e-08 ***
Sub_5b_labor_market_reg             0.081786   0.020256   4.038 0.000136 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 0.2034 on 70 degrees of freedom
Multiple R-squared:  0.9513,	Adjusted R-squared:  0.9423 
F-statistic: 105.2 on 13 and 70 DF,  p-value: < 2.2e-16
Sub_4a_tariffs
1.34889005634407
Sub_5b_labor_market_reg
1.35675219181632
Sub_3a_money_growth
1.48207607825918
Sub_1c_gov_enterprises
1.51116838702178
Sub_1d_top_marg_tax_rate
1.54136829965736
Sub_1a_government_consumption
1.60973436871063
Sub_2a_judicial_independence
1.62172893227723
Sub_3c_inflation
1.63773861924844
Sub_3d_freedom_own_foreign_currency
1.74756753312764
Sub_4c_black_market
1.80596760933549
Sub_4d_control_movement_capital_ppl
1.85984775956134
Sub_5a_credit_market_reg
1.93674455331594
Sub_1b_transfers
2.06863058885069
Upto model_6 we removed variables that are multicollinear, and in the model_6 we have all variables that have vif within 2. The summary of model_6 also shows that the Adjusted Rsquare has come down to 0.94 after all the above steps. Next we will use the p-value to remove some more insignificant variables.

#Removing  Sub_1b_transfers due to high p-value 
model_7 <- lm(formula = Economic_Freedom ~ Sub_1a_government_consumption + 
Sub_1c_gov_enterprises + Sub_1d_top_marg_tax_rate + 
Sub_2a_judicial_independence + Sub_3a_money_growth + 
Sub_3c_inflation + Sub_3d_freedom_own_foreign_currency + 
Sub_4a_tariffs + Sub_4c_black_market + 
Sub_4d_control_movement_capital_ppl + Sub_5a_credit_market_reg + 
Sub_5b_labor_market_reg, data = train)

summary(model_7)
Call:
lm(formula = Economic_Freedom ~ Sub_1a_government_consumption + 
Sub_1c_gov_enterprises + Sub_1d_top_marg_tax_rate + Sub_2a_judicial_independence + 
Sub_3a_money_growth + Sub_3c_inflation + Sub_3d_freedom_own_foreign_currency + 
Sub_4a_tariffs + Sub_4c_black_market + Sub_4d_control_movement_capital_ppl + 
Sub_5a_credit_market_reg + Sub_5b_labor_market_reg, data = train)

Residuals:
Min       1Q   Median       3Q      Max 
-0.72884 -0.10349  0.01504  0.11102  0.40181 

Coefficients:
Estimate Std. Error t value Pr(>|t|)    
(Intercept)                         0.526685   0.269382   1.955 0.054501 .  
Sub_1a_government_consumption       0.052164   0.013290   3.925 0.000198 ***
Sub_1c_gov_enterprises              0.048204   0.008883   5.426 7.57e-07 ***
Sub_1d_top_marg_tax_rate            0.044327   0.011554   3.836 0.000268 ***
Sub_2a_judicial_independence        0.113742   0.013746   8.274 5.18e-12 ***
Sub_3a_money_growth                 0.040084   0.023490   1.706 0.092290 .  
Sub_3c_inflation                    0.083358   0.015034   5.545 4.73e-07 ***
Sub_3d_freedom_own_foreign_currency 0.061352   0.007464   8.220 6.54e-12 ***
Sub_4a_tariffs                      0.112528   0.021823   5.156 2.18e-06 ***
Sub_4c_black_market                 0.052760   0.023732   2.223 0.029393 *  
Sub_4d_control_movement_capital_ppl 0.077405   0.017974   4.306 5.23e-05 ***
Sub_5a_credit_market_reg            0.128468   0.020817   6.171 3.73e-08 ***
Sub_5b_labor_market_reg             0.082367   0.020179   4.082 0.000116 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 0.2027 on 71 degrees of freedom
Multiple R-squared:  0.9509,	Adjusted R-squared:  0.9426 
F-statistic: 114.6 on 12 and 71 DF,  p-value: < 2.2e-16
#Removing  Sub_3a_money_growth due to high p-value 
model_8 <- lm(formula = Economic_Freedom ~ Sub_1a_government_consumption + 
Sub_1c_gov_enterprises + Sub_1d_top_marg_tax_rate + 
Sub_2a_judicial_independence + Sub_3c_inflation + Sub_3d_freedom_own_foreign_currency + 
Sub_4a_tariffs + Sub_4c_black_market + 
Sub_4d_control_movement_capital_ppl + Sub_5a_credit_market_reg + 
Sub_5b_labor_market_reg, data = train)

summary(model_8)
Call:
lm(formula = Economic_Freedom ~ Sub_1a_government_consumption + 
Sub_1c_gov_enterprises + Sub_1d_top_marg_tax_rate + Sub_2a_judicial_independence + 
Sub_3c_inflation + Sub_3d_freedom_own_foreign_currency + 
Sub_4a_tariffs + Sub_4c_black_market + Sub_4d_control_movement_capital_ppl + 
Sub_5a_credit_market_reg + Sub_5b_labor_market_reg, data = train)

Residuals:
Min       1Q   Median       3Q      Max 
-0.70903 -0.09362  0.00125  0.13093  0.42709 

Coefficients:
Estimate Std. Error t value Pr(>|t|)    
(Intercept)                         0.717587   0.248287   2.890 0.005086 ** 
Sub_1a_government_consumption       0.052525   0.013464   3.901 0.000213 ***
Sub_1c_gov_enterprises              0.047221   0.008981   5.258 1.43e-06 ***
Sub_1d_top_marg_tax_rate            0.044869   0.011702   3.834 0.000267 ***
Sub_2a_judicial_independence        0.111207   0.013846   8.032 1.34e-11 ***
Sub_3c_inflation                    0.083633   0.015232   5.491 5.69e-07 ***
Sub_3d_freedom_own_foreign_currency 0.063261   0.007477   8.461 2.11e-12 ***
Sub_4a_tariffs                      0.117592   0.021905   5.368 9.26e-07 ***
Sub_4c_black_market                 0.068939   0.022043   3.127 0.002544 ** 
Sub_4d_control_movement_capital_ppl 0.075003   0.018156   4.131 9.61e-05 ***
Sub_5a_credit_market_reg            0.123721   0.020903   5.919 1.01e-07 ***
Sub_5b_labor_market_reg             0.083921   0.020425   4.109 0.000104 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 0.2054 on 72 degrees of freedom
Multiple R-squared:  0.9489,	Adjusted R-squared:  0.9411 
F-statistic: 121.5 on 11 and 72 DF,  p-value: < 2.2e-16
Now, in the model_8 we have all significant variables with us. All the variables have low p-values and now we have only 11 predictors against 30 predictors when we started out with modelling at the beginning. The Adjusted R-squared of model_8 is 0.9411, which is very good. Now, we have to go through a litmus test, and our model building is nothing without predicting the Economic Freedom on the test data.

# predicting the results on test dataset
Predict <- predict(model_8,test[,-1])
test$test_efw <- Predict

# Now, we need to test the r square between actual and predicted efw 
r <- cor(test$Economic_Freedom,test$test_efw)
rsquared <- cor(test$Economic_Freedom,test$test_efw)^2
rsquared
0.952008818575079
Great!!! the predicted R-squared from model_8 gives an R-squared value of 0.95 and tells us how well the model predicts new observations.

Next we will analyze some diagonostic plots and get to know them better.

Residual Vs. Fitted - This plot helps to determine if the the Residuals have any patterns in them. From the plot below we can se that the Residuals are equally spread around the horizontal line and does not have a distinct patterns.

Normal Q-Q Plot- This plot tells us if the Residuals are normally distributed or not. We can see that the Residuals follow a straight line mostly and deviation is not much as well.

Scale Location- This plot shows if residuals are spread equally along the ranges of predictors. It is a good if there is a horizontal line with randomly points.

Residuals vs Leverage- This plot helps us to find influential cases. Some outliers do not affect the regression line. There wouldn't be much difference to our analysis, if we include or exclude these influential cases from analysis. They follow the trend in the majority of cases are not influential. On the other hand, some cases could be very influential even if they look to be within a reasonable range of the values. They can alter the results if we exclude them from analysis. The influential cases have high cook's distance and are generally beyond the dashed red lines. In our data we don't have any.

options(repr.plot.width=6, repr.plot.height=6)
par(mfrow = c(2, 2))
plot(model_8)

VARIABLE IMPORTANCE:
  
  While starting out with this analysis, my aim was to determine, the most important explanatory variables that affect the Economic Freedom and reduce the number of variables and get a decent model that explains our target. We found that our models is pretty decent and with only 11 out of 30 variables we are able to explain 95% of variance in our data. So, my next step is to analyze these 11 important predictors and come up with variable importance.

In Linear Regression, we know that the coefficient values and p-values are important for determining which variable is important. We know that coefficient value represents the mean change in the response given a one-unit increase in the predictor. But not always, predictors with higher coefficient values are the most important variables, because if the data is not standardised and there are different units available for the predictors, the comparing them as it is will not yield good results.

Same is the case with low p-value, a statistically low p-value of a predictor doesn't mean that it is important in the real scenario. So solely judging using p-value is not appropriate either.

What we can use to determine the variable importance is by using standardized regression coefficients. Standardized coefficients represent the mean change in the target given a one standard deviation change in the predictor. All the predictors are compared using one scale which is a fair game. I have used plot_summs function from library jtools to compare the standardized coeeficients of predictors. We have to look for predictors that have highest estimate of standardized coefficient in the plot below.

plot_summs(model_8, scale = TRUE, plot.distributions = TRUE)

While there are several variables that affect the Economic Freedom of a country, the ones that seems more important after our modelling exercise are as follows,

Freedom_own_foreign_currency(Freedom to own foreign currency)
Judicial Independence
Credit Market Regulations
Inflation
Government Enterprises
Control Movement of Capital and People
Tariffs
Labor Market Regulations
Marginalised Tax Rate
Government Consumption
Black Market
If you find this kernel useful or fork it, then please give it a thumbs up.

REFERENCES:

https://www.fraserinstitute.org/sites/default/files/economic-freedom-of-the-world-2018.pdf

https://data.library.virginia.edu/diagnostic-plots/

https://cran.r-project.org/web/packages/jtools/vignettes/summ.html