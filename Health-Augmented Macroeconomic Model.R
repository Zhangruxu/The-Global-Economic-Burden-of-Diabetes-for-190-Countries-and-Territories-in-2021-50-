library(dplyr)
library(Hmisc)
library(tidyr)

setwd("D:/S/基于健康增益宏观效应模型的疾病经济成本评估/Diabetes/人力资本")
####疾病数据处理------
###数据导入及合并
pop <- read.csv("Population GBD.csv")
colnames(pop) <- c(paste(colnames(pop), "pop", sep = "."))
pre <- read.csv("Prevalence.csv")
colnames(pre) <- c(paste(colnames(pre), "pre", sep = "."))
inc <- read.csv("Incidence.csv")
colnames(inc) <- c(paste(colnames(inc), "inc", sep = "."))
dea <- read.csv("Deaths.csv")
colnames(dea) <- c(paste(colnames(dea), "dea", sep = "."))
yl <- read.csv("YL.csv")
yld <- subset(yl, measure_id == 3)
yll <- subset(yl, measure_id == 4)
remove(yl)
colnames(yld) <- c(paste(colnames(yld), "yld", sep = "."))
colnames(yll) <- c(paste(colnames(yll), "yll", sep = "."))
data <- merge(pop, pre, by.x = c("location_name.pop", "sex_name.pop", "age_name.pop", "year.pop"), by.y = c("location_name.pre", "sex_name.pre", "age_name.pre", "year.pre"))
data <- data[, c(7, 1, 4, 9, 3, 8, 2, 5, 6, 10:16, 20:26)]
colnames(data) <- c(gsub(".pop", "", colnames(data[, c(1:7)])), colnames(data[, c(8:23)]))
data <- merge(data, inc, by.x = c("location_name", "sex_name", "age_name", "year"), by.y = c("location_name.inc", "sex_name.inc", "age_name.inc", "year.inc"))
data <- data[, c(5, 1, 4, 6, 3, 7, 2, 8:25, 29:35)]
data <- merge(data, dea, by.x = c("location_name", "sex_name", "age_name", "year"), by.y = c("location_name.dea", "sex_name.dea", "age_name.dea", "year.dea"))
data <- data[, c(5, 1, 4, 6, 3, 7, 2, 8:34, 38:44)]
data <- merge(data, yld, by.x = c("location_name", "sex_name", "age_name", "year"), by.y = c("location_name.yld", "sex_name.yld", "age_name.yld", "year.yld"))
data <- data[, c(5, 1, 4, 6, 3, 7, 2, 8:43, 47:53)]
data <- merge(data, yll, by.x = c("location_name", "sex_name", "age_name", "year"), by.y = c("location_name.yll", "sex_name.yll", "age_name.yll", "year.yll"))
data <- data[, c(5, 1, 4, 6, 3, 7, 2, 8:52, 56:62)]
remove(pop, pre, inc, dea, yld, yll)
###region
data$location_name[which(data$location_name == "Czechia")] <- "Czech Republic"
data$location_name[which(data$location_name == "United States Virgin Islands")] <- "Virgin Islands"
data$location_name[which(data$location_name == "North America")] <- "North America - WB"
data <- mutate(data, region = ifelse(location_name == "American Samoa" | location_name ==  "Australia" | location_name ==  "Brunei Darussalam" | location_name ==  "Cambodia" | location_name ==  "China" | location_name ==  "Fiji" | location_name ==  "Guam" | location_name ==  "Indonesia" | location_name == "Japan" | location_name == "Kiribati" | location_name == "Democratic People's Republic of Korea" | location_name == "Republic of Korea" | location_name == "Lao People's Democratic Republic" | location_name == "Malaysia" | location_name == "Marshall Islands" | location_name == "Micronesia (Federated States of)" | location_name == "Mongolia" | location_name == "Myanmar" | location_name == "Nauru" | location_name == "New Zealand" | location_name == "Northern Mariana Islands" | location_name == "Palau" | location_name == "Papua New Guinea" | location_name == "Philippines" | location_name == "Samoa" | location_name == "Singapore" | location_name == "Solomon Islands" | location_name == "Taiwan (Province of China)" | location_name == "Thailand" | location_name == "Timor-Leste" | location_name == "Tonga" | location_name == "Tuvalu" | location_name == "Vanuatu" | location_name == "Viet Nam", "East Asia and Pacific", 
                                     ifelse(location_name == "Albania" | location_name ==  "Andorra" | location_name ==  "Armenia" | location_name ==  "Austria" | location_name ==  "Azerbaijan" | location_name ==  "Belarus" | location_name ==  "Belgium" | location_name ==  "Bosnia and Herzegovina" | location_name ==  "Bulgaria" | location_name ==  "Croatia" | location_name ==  "Cyprus" | location_name ==  "Czech Republic" | location_name ==  "Denmark" | location_name ==  "Estonia" | location_name ==  "Finland" | location_name ==  "France" | location_name ==  "Georgia" | location_name ==  "Germany" | location_name ==  "Greece" | location_name ==  "Greenland" | location_name ==  "Hungary" | location_name ==  "Iceland" | location_name ==  "Ireland" | location_name ==  "Italy" | location_name ==  "Kazakhstan" | location_name ==  "Kyrgyzstan" | location_name ==  "Latvia" | location_name ==  "Lithuania" | location_name ==  "Luxembourg" | location_name ==  "Republic of Moldova" | location_name ==  "Monaco" | location_name ==  "Montenegro" | location_name ==  "Netherlands" | location_name ==  "North Macedonia" | location_name ==  "Norway" | location_name ==  "Poland" | location_name ==  "Portugal" | location_name ==  "Romania" | location_name ==  "Russian Federation" | location_name ==  "San Marino" | location_name ==  "Serbia" | location_name ==  "Slovakia" | location_name ==  "Slovenia" | location_name ==  "Spain" | location_name ==  "Sweden" | location_name ==  "Switzerland" | location_name ==  "Tajikistan" | location_name ==  "Türkiye" | location_name ==  "Turkmenistan" | location_name ==  "Ukraine" | location_name ==  "United Kingdom" | location_name ==  "Uzbekistan", "Europe and central Asia", 
                                            ifelse(location_name == "Antigua and Barbuda" | location_name ==  "Argentina" | location_name ==  "Bahamas" | location_name ==  "Barbados" | location_name ==  "Belize" | location_name ==  "Bolivia (Plurinational State of)" | location_name ==  "Brazil" | location_name ==  "Chile" | location_name ==  "Colombia" | location_name ==  "Costa Rica" | location_name ==  "Cuba" | location_name ==  "Dominica" | location_name ==  "Dominican Republic" | location_name ==  "Ecuador" | location_name ==  "El Salvador" | location_name ==  "Grenada" | location_name ==  "Guatemala" | location_name ==  "Guyana" | location_name ==  "Haiti" | location_name ==  "Honduras" | location_name ==  "Jamaica" | location_name ==  "Mexico" | location_name ==  "Nicaragua" | location_name ==  "Panama" | location_name ==  "Paraguay" | location_name ==  "Peru" | location_name ==  "Puerto Rico" | location_name ==  "Saint Kitts and Nevis" | location_name ==  "Saint Lucia" | location_name ==  "Saint Vincent and the Grenadines" | location_name ==  "Suriname" | location_name ==  "Trinidad and Tobago" | location_name ==  "Uruguay" | location_name ==  "Venezuela (Bolivarian Republic of)" | location_name ==  "Virgin Islands", "Latin America and Caribbean", 
                                                   ifelse(location_name == "Algeria" | location_name ==  "Bahrain" | location_name ==  "Djibouti" | location_name ==  "Egypt" | location_name ==  "Iran (Islamic Republic of)" | location_name ==  "Iraq" | location_name ==  "Israel" | location_name ==  "Jordan" | location_name ==  "Kuwait" | location_name ==  "Lebanon" | location_name ==  "Libya" | location_name ==  "Malta" | location_name ==  "Morocco" | location_name ==  "Oman" | location_name ==  "Qatar" | location_name ==  "Saudi Arabia" | location_name ==  "Syrian Arab Republic" | location_name ==  "Tunisia" | location_name ==  "United Arab Emirates" | location_name ==  "Yemen", "Middle East and north Africa", 
                                                          ifelse(location_name == "Bermuda" | location_name ==  "Canada" | location_name ==  "United States of America", "North America", 
                                                                 ifelse(location_name == "Afghanistan" | location_name ==  "Bangladesh" | location_name ==  "Bhutan" | location_name ==  "India" | location_name ==  "Maldives" | location_name ==  "Nepal" | location_name ==  "Pakistan" | location_name ==  "Sri Lanka", "South Asia", 
                                                                        ifelse(location_name == "Angola" | location_name ==  "Benin" | location_name ==  "Botswana" | location_name ==  "Burkina Faso" | location_name ==  "Burundi" | location_name ==  "Cabo Verde" | location_name ==  "Cameroon" | location_name ==  "Central African Republic" | location_name ==  "Chad" | location_name ==  "Comoros" | location_name ==  "Democratic Republic of the Congo" | location_name ==  "Congo" | location_name ==  "Côte d'Ivoire" | location_name ==  "Equatorial Guinea" | location_name ==  "Eritrea" | location_name ==  "Eswatini" | location_name ==  "Ethiopia" | location_name ==  "Gabon" | location_name ==  "Gambia" | location_name ==  "Ghana" | location_name ==  "Guinea" | location_name ==  "Guinea-Bissau" | location_name ==  "Kenya" | location_name ==  "Lesotho" | location_name ==  "Liberia" | location_name ==  "Madagascar" | location_name ==  "Malawi" | location_name ==  "Mali" | location_name ==  "Mauritania" | location_name ==  "Mauritius" | location_name ==  "Mozambique" | location_name ==  "Namibia" | location_name ==  "Niger" | location_name ==  "Nigeria" | location_name ==  "Rwanda" | location_name ==  "Sao Tome and Principe" | location_name ==  "Senegal" | location_name ==  "Seychelles" | location_name ==  "Sierra Leone" | location_name ==  "Somalia" | location_name ==  "South Africa" | location_name ==  "South Sudan" | location_name ==  "Sudan" | location_name ==  "United Republic of Tanzania" | location_name ==  "Togo" | location_name ==  "Uganda" | location_name ==  "Zambia" | location_name ==  "Zimbabwe", "Sub-Saharan Africa", 
                                                                               ifelse(location_name == "Cook Islands" | location_name ==  "Niue" | location_name ==  "Palestine" | location_name ==  "Tokelau", "Others", NA)))))))))
data <- subset(data, !is.na(region))
###65+
data$age_name <- gsub(" years", "", data$age_name)
data$age_name[which(data$age_name == "<5")] <- "0-4"
age_id.100 <- subset(data, age_id == 18 | age_id == 26)
age_id.100 <- aggregate(cbind(age_id.100$val.pop, age_id.100$lower.pop, age_id.100$upper.pop, age_id.100$val.pre, age_id.100$lower.pre, age_id.100$upper.pre, age_id.100$val.inc, age_id.100$lower.inc, age_id.100$upper.inc, age_id.100$val.dea, age_id.100$lower.dea, age_id.100$upper.dea, age_id.100$val.yld, age_id.100$lower.yld, age_id.100$upper.yld, age_id.100$val.yll, age_id.100$lower.yll, age_id.100$upper.yll), list(age_id.100$location_id, age_id.100$year, age_id.100$sex_id), sum)
colnames(age_id.100) <- c("location_id", "year", "sex_id", "val.pop.100", "lower.pop.100", "upper.pop.100", "val.pre.100", "lower.pre.100", "upper.pre.100", "val.inc.100", "lower.inc.100", "upper.inc.100", "val.dea.100", "lower.dea.100", "upper.dea.100", "val.yld.100", "lower.yld.100", "upper.yld.100", "val.yll.100", "lower.yll.100", "upper.yll.100")
age_id.100$age_id <- 100
data <- subset(data, age_id != 26) 
data$age_id[which(data$age_id == 18)] <- 100
data$age_name[which(data$age_name == "65-69")] <- "65+"
data <- merge(data, age_id.100, by = c("location_id", "year", "sex_id", "age_id"), all.x = T)
remove(age_id.100)
data$val.pop <- ifelse(data$age_id == 100, data$val.pop.100, data$val.pop)
data$lower.pop <- ifelse(data$age_id == 100, data$lower.pop.100, data$lower.pop)
data$upper.pop <- ifelse(data$age_id == 100, data$upper.pop.100, data$upper.pop)
data$val.pre <- ifelse(data$age_id == 100, data$val.pre.100, data$val.pre)
data$lower.pre <- ifelse(data$age_id == 100, data$lower.pre.100, data$lower.pre)
data$upper.pre <- ifelse(data$age_id == 100, data$upper.pre.100, data$upper.pre)
data$val.inc <- ifelse(data$age_id == 100, data$val.inc.100, data$val.inc)
data$lower.inc <- ifelse(data$age_id == 100, data$lower.inc.100, data$lower.inc)
data$upper.inc <- ifelse(data$age_id == 100, data$upper.inc.100, data$upper.inc)
data$val.dea <- ifelse(data$age_id == 100, data$val.dea.100, data$val.dea)
data$lower.dea <- ifelse(data$age_id == 100, data$lower.dea.100, data$lower.dea)
data$upper.dea <- ifelse(data$age_id == 100, data$upper.dea.100, data$upper.dea)
data$val.yld <- ifelse(data$age_id == 100, data$val.yld.100, data$val.yld)
data$lower.yld <- ifelse(data$age_id == 100, data$lower.yld.100, data$lower.yld)
data$upper.yld <- ifelse(data$age_id == 100, data$upper.yld.100, data$upper.yld)
data$val.yll <- ifelse(data$age_id == 100, data$val.yll.100, data$val.yll)
data$lower.yll <- ifelse(data$age_id == 100, data$lower.yll.100, data$lower.yll)
data$upper.yll <- ifelse(data$age_id == 100, data$upper.yll.100, data$upper.yll)
data <- select(data, -c(61:78))
data <- data[, c(60, 1, 5, 2, 4, 6, 3, 7:59)]
data <- subset(data, year >= 2011 & year <= 2020)
###率
data$rate.pre <- data$val.pre/data$val.pop
data$rate.inc <- data$val.inc/data$val.pop
data$rate.lower.inc <- data$lower.inc/data$val.pop
data$rate.upper.inc <- data$upper.inc/data$val.pop
data$rate.dea <- data$val.dea/data$val.pop
data$rate.lower.dea <- data$lower.dea/data$val.pop
data$rate.upper.dea <- data$upper.dea/data$val.pop
data$yl.ratio <- data$val.yld/data$val.yll
data <- rbind(data, data, data, data)
data$year[((nrow(data)/4)+1):(nrow(data)/2)] <- data$year[1:(nrow(data)/4)] + 10
data$year[((nrow(data)/2)+1):(nrow(data)*3/4)] <- data$year[1:(nrow(data)/4)] + 20
data$year[((nrow(data)*3/4)+1):nrow(data)] <- data$year[1:(nrow(data)/4)] + 30
data[(nrow(data)/4+1):nrow(data), c(9:60)] <- NA
predict_for_rate <- function(data) {
  
  bind <- NULL
  
  for (x in 1:length(unique(data$location_id))) {
    
    for (y in 1:length(unique(data$age_id))) {
      
      for (z in 1:length(unique(data$sex_id))) {
        
        sub <- subset(data, location_id == unique(data$location_id)[x] & age_id == unique(data$age_id)[y] & sex_id == unique(data$sex_id)[z])
        

        
        gr_pre <- (sub$rate.pre[sub$year == 2020]/sub$rate.pre[sub$year == 2011])^(1/9)-1
        gr_pre <- ifelse(gr_pre > 0.02, 0.02, gr_pre)
        sub$rate.pre <- ifelse(sub$year >= 2021, sub$rate.pre[sub$year == 2020]*((1+gr_pre)^(sub$year-2020)), sub$rate.pre)
        
        gr_inc <- (sub$rate.inc[sub$year == 2020]/sub$rate.inc[sub$year == 2011])^(1/9)-1
        gr_inc <- ifelse(gr_inc > 0.02, 0.02, gr_inc)
        sub$rate.inc <- ifelse(sub$year >= 2021, sub$rate.inc[sub$year == 2020]*((1+gr_inc)^(sub$year-2020)), sub$rate.inc)
        sub$rate.lower.inc <- ifelse(sub$year >= 2021, sub$rate.lower.inc[sub$year == 2020]*((1+gr_inc)^(sub$year-2020)), sub$rate.lower.inc)
        sub$rate.upper.inc <- ifelse(sub$year >= 2021, sub$rate.upper.inc[sub$year == 2020]*((1+gr_inc)^(sub$year-2020)), sub$rate.upper.inc)
        
        gr_dea <- (sub$rate.dea[sub$year == 2020]/sub$rate.dea[sub$year == 2011])^(1/9)-1
        gr_dea <- ifelse(gr_dea > 0.02, 0.02, gr_dea)
        sub$rate.dea <- ifelse(sub$year >= 2021, sub$rate.dea[sub$year == 2020]*((1+gr_dea)^(sub$year-2020)), sub$rate.dea)
        sub$rate.lower.dea <- ifelse(sub$year >= 2021, sub$rate.lower.dea[sub$year == 2020]*((1+gr_dea)^(sub$year-2020)), sub$rate.lower.dea)
        sub$rate.upper.dea <- ifelse(sub$year >= 2021, sub$rate.upper.dea[sub$year == 2020]*((1+gr_dea)^(sub$year-2020)), sub$rate.upper.dea)
        
        gr_ylr <- (sub$yl.ratio[sub$year == 2020]/sub$yl.ratio[sub$year == 2011])^(1/9)-1
        gr_ylr <- ifelse(gr_ylr > 0.02, 0.02, gr_ylr)
        sub$yl.ratio <- ifelse(sub$year >= 2021, sub$yl.ratio[sub$year == 2020]*((1+gr_ylr)^(sub$year-2020)), sub$yl.ratio)
        
        bind <- rbind(bind, sub)
        
      }
    }
  }
  
  bind$rate.pre <- ifelse(bind$rate.pre >= 1, 0.99, bind$rate.pre)
  
  bind
  
}
data <- predict_for_rate(data)
remove(predict_for_rate)
all_location <- unique(data$location_name)
write.csv(data, "data_dp.csv")
data0 <-read.csv("data_dp.csv")
all_location <- unique(data0$location_name)
####人口------
###预处理
pop_DESA <- read.csv("Population DESA.csv")
pop_DESA <- pop_DESA[, c(8, 10, 13, 15:19)]
pop_DESA <- subset(pop_DESA, Time >= 2021 & Time <= 2050)
pop_DESA <- subset(pop_DESA, LocTypeName == "Country/Area" | LocTypeName == "World")
pop_DESA$AgeGrp <- ifelse(pop_DESA$AgeGrpStart== 5, "5-9", 
                          ifelse(pop_DESA$AgeGrpStart == 10, "10-14", pop_DESA$AgeGrp))
pop_DESA <- pop_DESA[, c(2:5, 7, 8)]
colnames(pop_DESA) <- c("location_name", "year", "age_name", "age_grp_start", "PopMale", "PopFemale")
pop_DESA <- gather(pop_DESA, key = "sex_name", value = "pop_DESA", -"location_name", -"year", -"age_name", -"age_grp_start") #宽数据转换为长数据
pop_DESA$location_name[which(pop_DESA$location_name == "State of Palestine")] <- "Palestine"
pop_DESA$location_name[which(pop_DESA$location_name == "Micronesia (Fed. States of)")] <- "Micronesia (Federated States of)"
pop_DESA$location_name[which(pop_DESA$location_name == "United States Virgin Islands")] <- "Virgin Islands"
pop_DESA$location_name[which(pop_DESA$location_name == "Czechia")] <- "Czech Republic"
pop_DESA$location_name[which(pop_DESA$location_name == "Dem. People's Republic of Korea")] <- "Democratic People's Republic of Korea"
pop_DESA$location_name[which(pop_DESA$location_name == "China, Taiwan Province of China")] <- "Taiwan (Province of China)"
pop_DESA$pop_DESA <- ifelse(pop_DESA$location_name == "China", pop_DESA$pop_DESA[pop_DESA$location_name == "China"] + pop_DESA$pop_DESA[pop_DESA$location_name == "China, Hong Kong SAR"] + pop_DESA$pop_DESA[pop_DESA$location_name == "China, Macao SAR"], pop_DESA$pop_DESA)
pop_DESA$sex_name <- gsub("Pop", "", pop_DESA$sex_name)
###生成65+
age_id.100 <- subset(pop_DESA, age_grp_start >= 65)
age_id.100 <- aggregate(age_id.100$pop_DESA, list(age_id.100$location_name, age_id.100$year, age_id.100$sex_name), sum)
colnames(age_id.100) <- c("location_name", "year", "sex_name", "pop_DESA.100")
age_id.100$age_name <- "65+"
pop_DESA <- subset(pop_DESA, age_grp_start <= 65)
pop_DESA$age_name[which(pop_DESA$age_name == "65-69")] <- "65+"
pop_DESA <- merge(pop_DESA, age_id.100, by = c("location_name", "year", "sex_name", "age_name"), all.x = T)
remove(age_id.100)
pop_DESA$pop_DESA <- ifelse(pop_DESA$age_name == "65+", pop_DESA$pop_DESA.100, pop_DESA$pop_DESA)
pop_DESA <- pop_DESA[, c(1:4, 6)]
pop_DESA$pop_DESA <- pop_DESA$pop_DESA*1000 
###合并疾病数据和DESA人口数据
data <- read.csv("data_dp.csv")
data <- data[, c(2:9, 62:69)]
data <- subset(data, year >= 2021 & year <= 2050)
data <- merge(data, pop_DESA, by = c("location_name", "year", "age_name", "sex_name"), all.x = T)
remove(pop_DESA)
data <- data[, c(5, 1, 2, 3, 4, 9:17)]
colnames(data) <- c("region", "location", "year", "age", "sex", "rate.pre", "rate.inc", "rate.lower.inc", "rate.upper.inc", "rate.dea", "rate.lower.dea", "rate.upper.dea", "yl.ratio", "pop_sq")
###反事实人口
pop_cf <- function(data) {
  
  data$pop_cf <- data$pop_sq
  data$pop_cf_lower <- data$pop_sq
  data$pop_cf_upper <- data$pop_sq
  
  data <- mutate(data, age_id = ifelse(age == "0-4", 1, 
                                       ifelse(age == "5-9", 2, 
                                              ifelse(age == "10-14", 3,
                                                     ifelse(age == "15-19", 4, 
                                                            ifelse(age == "20-24", 5, 
                                                                   ifelse(age == "25-29", 6, 
                                                                          ifelse(age == "30-34", 7, 
                                                                                 ifelse(age == "35-39", 8, 
                                                                                        ifelse(age == "40-44", 9, 
                                                                                               ifelse(age == "45-49", 10, 
                                                                                                      ifelse(age == "50-54", 11, 
                                                                                                             ifelse(age == "55-59", 12, 
                                                                                                                    ifelse(age == "60-64", 13, 14))))))))))))))
  
  bind <- NULL
  
  for (x in 1:length(unique(data$location))) {
    
    for (y in 1:length(unique(data$sex))) {
      
      sub <- subset(data, location == unique(data$location)[x] & sex == unique(data$sex)[y])
      
      for (z in 2:length(unique(data$age))) {
        
        for (i in 2022:2050) {
          
          if (z <= (i-2021)) {
            
            for (j in 1:(z-1)) {
              
              sub$pop_cf[sub$year == i & sub$age_id == z] <- sub$pop_cf[sub$year == i & sub$age_id == z]/(1-sub$rate.dea[sub$year == (i-j) & sub$age_id == (z-j)])
              sub$pop_cf_lower[sub$year == i & sub$age_id == z] <- sub$pop_cf_lower[sub$year == i & sub$age_id == z]/(1-sub$rate.lower.dea[sub$year == (i-j) & sub$age_id == (z-j)])
              sub$pop_cf_upper[sub$year == i & sub$age_id == z] <- sub$pop_cf_upper[sub$year == i & sub$age_id == z]/(1-sub$rate.upper.dea[sub$year == (i-j) & sub$age_id == (z-j)])
              
            }
            
          } else {
            
            for (j in 1:(i-2021)) {
              
              sub$pop_cf[sub$year == i & sub$age_id == z] <- sub$pop_cf[sub$year == i & sub$age_id == z]/(1-sub$rate.dea[sub$year == (i-j) & sub$age_id == (z-j)])
              sub$pop_cf_lower[sub$year == i & sub$age_id == z] <- sub$pop_cf_lower[sub$year == i & sub$age_id == z]/(1-sub$rate.lower.dea[sub$year == (i-j) & sub$age_id == (z-j)])
              sub$pop_cf_upper[sub$year == i & sub$age_id == z] <- sub$pop_cf_upper[sub$year == i & sub$age_id == z]/(1-sub$rate.upper.dea[sub$year == (i-j) & sub$age_id == (z-j)])
              
            }
            
          }
          
        }
        
      }
      
      bind <- rbind(bind, sub)
      
    }
    
  }
  
  bind <- bind[, -18]
  
}
data <- pop_cf(data)
remove(pop_cf)
write.csv(data, "data_dp_n.csv")

####劳动------
###预处理
data <- read.csv("data_dp_n.csv")
data <- data[, -1]
lfp <- read.csv("Labour force participation rate by sex and age.csv")
lfp <- lfp[, c(1, 6, 5, 4, 7)]
colnames(lfp) <- c("location", "year", "age", "sex", "lfp_sq")
lfp <- subset(lfp, year >= 2001 & year <= 2020)
lfp <- subset(lfp, sex == "Sex: Male" | sex == "Sex: Female")
lfp <- subset(lfp, age == "Age (5-year bands): 15-19" | age == "Age (5-year bands): 20-24" | age == "Age (5-year bands): 25-29" | age == "Age (5-year bands): 30-34" | age == "Age (5-year bands): 35-39" | age == "Age (5-year bands): 40-44" | age == "Age (5-year bands): 45-49" | age == "Age (5-year bands): 50-54" | age == "Age (5-year bands): 55-59" | age == "Age (5-year bands): 60-64" | age == "Age (5-year bands): 65+")
agegroup <- as.data.frame(strsplit(lfp$age, ": "))
lfp$age <- as.character(t(agegroup[2, ]))
remove(agegroup)
lfp$sex <- gsub("Sex: ", "", lfp$sex)
setdiff(unique(lfp$location), intersect(unique(data$location), unique(lfp$location)))
setdiff(unique(data$location), intersect(unique(data$location), unique(lfp$location)))
lfp$location[which(lfp$location == "Côte d’Ivoire")] <- "Côte d'Ivoire"
lfp$location[which(lfp$location == "Congo, Democratic Republic of the")] <- "Democratic Republic of the Congo"
lfp$location[which(lfp$location == "Czechia")] <- "Czech Republic"
lfp$location[which(lfp$location == "Taiwan, China")] <- "Taiwan (Province of China)"
lfp$location[which(lfp$location == "Tanzania, United Republic of")] <- "United Republic of Tanzania"
lfp$location[which(lfp$location == "United Kingdom of Great Britain and Northern Ireland")] <- "United Kingdom"
lfp$location[which(lfp$location == "Occupied Palestinian Territory")] <- "Palestine"
###回归
lfp_logis <- function(data) {
  
  bind <- NULL
  
  for (x in 1:length(unique(data$location))) {
    
    for (y in 1:length(unique(data$age))) {
      
      for (z in 1:length(unique(data$sex))) {
        
        sub <- subset(data, location == unique(data$location)[x] & age == unique(data$age)[y] & sex == unique(data$sex)[z])
        
        if (nrow(sub) == 0) {
          
          for (i in 1:30) {
            
            sub[nrow(sub)+1, ] <- c(unique(data$location)[x], i+2020, unique(data$age)[y], unique(data$sex)[z], NA)
            
          }
          
        } else if (nrow(sub) == 1) {
          
          for (i in 1:30) {
            
            sub[nrow(sub)+1, ] <- sub[nrow(sub), ]
            sub[nrow(sub), 2] <- i+2020
            sub[nrow(sub), 5] <- sub[1, 5]
            
          }
          
        } else {
          
          model <- lm(formula = lfp_sq ~ year, data = sub)
          
          cof <- as.numeric(model$coefficients[2])
          
          intercept <- as.numeric(model$coefficients[1])
          
          for (i in 1:30) {
            
            sub[nrow(sub)+1, ] <- sub[nrow(sub), ]
            sub[nrow(sub), 2] <- i+2020
            sub[nrow(sub), 5] <- cof*sub[nrow(sub), 2]+intercept
            
          }
          
          sub$lfp_sq[which(sub$lfp_sq >= 100)] <- 99.9
          sub$lfp_sq[which(sub$lfp_sq <= 0)] <- 0.1
          
        }
        
        sub <- subset(sub, year >= 2021)
        
        bind <- rbind(bind, sub)
        
      }
    }
  }
  
  bind
  
}
lfp <- lfp_logis(lfp)
remove(lfp_logis)
lfp$year <- as.numeric(lfp$year)
lfp$lfp_sq <- as.numeric(lfp$lfp_sq)
###补少量不该缺的缺失
na_lfp <- subset(lfp, is.na(lfp_sq))
setdiff(unique(na_lfp$location), intersect(unique(data$location), unique(na_lfp$location)))
remove(na_lfp)
lfp <- subset(lfp, location != "Anguilla" & location != "Guernsey")
lfp_mi <- function(data) {
  
  bind <- NULL
  
  for (x in 1:length(unique(data$year))) {
    
    for (y in 1:length(unique(data$age))) {
      
      for (z in 1:length(unique(data$sex))) {
        
        sub <- subset(data, year == unique(data$year)[x] & age == unique(data$age)[y] & sex == unique(data$sex)[z])
        
        sub$lfp_sq[which(is.na(sub$lfp_sq))] <- mean(sub$lfp_sq, na.rm = T)
        
        bind <- rbind(bind, sub)
        
      }
    }
  }
  
  bind
  
}
lfp <- lfp_mi(lfp)
remove(lfp_mi)
na_lfp <- setdiff(all_location, intersect(all_location, unique(lfp$location)))
sd <- setdiff(unique(data$location), intersect(unique(data$location), unique(lfp$location)))
for (i in 1:length(sd)) {
  
  data <- subset(data, location != sd[i])
  
}
remove(sd, i)
data <- merge(data, lfp, by = c("location", "year", "age", "sex"), all.x = T)
remove(lfp)
###反事实劳动参与率 
lfp_cf <- function(data) {
  
  data$lfp_cf <- data$lfp_sq
  data$lfp_cf_lower <- data$lfp_sq
  data$lfp_cf_upper <- data$lfp_sq
  
  data <- mutate(data, age_id = ifelse(age == "0-4", 1, 
                                       ifelse(age == "5-9", 2, 
                                              ifelse(age == "10-14", 3,
                                                     ifelse(age == "15-19", 4, 
                                                            ifelse(age == "20-24", 5, 
                                                                   ifelse(age == "25-29", 6, 
                                                                          ifelse(age == "30-34", 7, 
                                                                                 ifelse(age == "35-39", 8, 
                                                                                        ifelse(age == "40-44", 9, 
                                                                                               ifelse(age == "45-49", 10, 
                                                                                                      ifelse(age == "50-54", 11, 
                                                                                                             ifelse(age == "55-59", 12, 
                                                                                                                    ifelse(age == "60-64", 13, 14))))))))))))))
  
  data$p <- 1-1/(data$rate.pre/data$rate.inc)
  data$p_lower <- 1-1/(data$rate.pre/data$rate.lower.inc)
  data$p_upper <- 1-1/(data$rate.pre/data$rate.upper.inc)
  
  bind <- NULL
  
  for (x in 1:length(unique(data$location))) {
    
    for (y in 1:length(unique(data$sex))) {
      
      sub <- subset(data, location == unique(data$location)[x] & sex == unique(data$sex)[y])
      
      for (z in 4:length(unique(data$age))) {
        
        for (i in 2022:2050) {
          
          if (z <= (i-2021)) {
            
            for (j in 1:(z-1)) {
              
              sub$lfp_cf[sub$year == i & sub$age_id == z] <- sub$lfp_cf[sub$year == i & sub$age_id == z]/(1-sub$rate.dea[sub$year == (i-j) & sub$age_id == (z-j)]*sub$yl.ratio[sub$year == i & sub$age_id == (z-j)]*sub$p[sub$year == i & sub$age_id == (z-j)]^(j-1))
              sub$lfp_cf_lower[sub$year == i & sub$age_id == z] <- sub$lfp_cf_lower[sub$year == i & sub$age_id == z]/(1-sub$rate.lower.dea[sub$year == (i-j) & sub$age_id == (z-j)]*sub$yl.ratio[sub$year == i & sub$age_id == (z-j)]*sub$p_lower[sub$year == i & sub$age_id == (z-j)]^(j-1))
              sub$lfp_cf_upper[sub$year == i & sub$age_id == z] <- sub$lfp_cf_upper[sub$year == i & sub$age_id == z]/(1-sub$rate.upper.dea[sub$year == (i-j) & sub$age_id == (z-j)]*sub$yl.ratio[sub$year == i & sub$age_id == (z-j)]*sub$p_upper[sub$year == i & sub$age_id == (z-j)]^(j-1))
              
            }
            
          } else {
            
            for (j in 1:(i-2021)) {
              
              sub$lfp_cf[sub$year == i & sub$age_id == z] <- sub$lfp_cf[sub$year == i & sub$age_id == z]/(1-sub$rate.dea[sub$year == (i-j) & sub$age_id == (z-j)]*sub$yl.ratio[sub$year == i & sub$age_id == (z-j)]*sub$p[sub$year == i & sub$age_id == (z-j)]^(j-1))
              sub$lfp_cf_lower[sub$year == i & sub$age_id == z] <- sub$lfp_cf_lower[sub$year == i & sub$age_id == z]/(1-sub$rate.lower.dea[sub$year == (i-j) & sub$age_id == (z-j)]*sub$yl.ratio[sub$year == i & sub$age_id == (z-j)]*sub$p_lower[sub$year == i & sub$age_id == (z-j)]^(j-1))
              sub$lfp_cf_upper[sub$year == i & sub$age_id == z] <- sub$lfp_cf_upper[sub$year == i & sub$age_id == z]/(1-sub$rate.upper.dea[sub$year == (i-j) & sub$age_id == (z-j)]*sub$yl.ratio[sub$year == i & sub$age_id == (z-j)]*sub$p_upper[sub$year == i & sub$age_id == (z-j)]^(j-1))
              
            }
            
          }
          
        }
        
      }
      
      bind <- rbind(bind, sub)
      
    }
    
  }
  
  bind$lfp_cf[which(bind$lfp_cf >= 100)] <- 99.999999999
  bind$lfp_cf_lower[which(bind$lfp_cf_lower >= 100)] <- 99.999999999
  bind$lfp_cf_upper[which(bind$lfp_cf_upper >= 100)] <- 99.999999999
  bind$lfp_cf[which(bind$lfp_cf <= 0)] <- 0.000000001
  bind$lfp_cf_lower[which(bind$lfp_cf_lower <= 0)] <- 0.000000001
  bind$lfp_cf_upper[which(bind$lfp_cf_upper <= 0)] <- 0.000000001
  
  bind <- bind[, -c(22:25)]
  
  bind <- subset(bind, age != "0-4" & age != "5-9" & age != "10-14")
  
}
data <- lfp_cf(data)
remove(lfp_cf)
write.csv(data, "data_dp_n_l.csv")

###education------
data <- read.csv("data_dp_n_l.csv")
data <- data[, -1]
##edub
edubm <- read.csv("BL2013_M_v2.2.csv")
edubf <- read.csv("BL2013_F_v2.2.csv")
edub <- rbind(edubm, edubf)
remove(edubm, edubf)
edub <- edub[, c(2:6, 14)]
setdiff(unique(edub$country), intersect(unique(data$location), unique(edub$country)))
setdiff(unique(data$location), intersect(unique(data$location), unique(edub$country)))
edub$country[which(edub$country == "Cote dIvoire")] <- "Côte d'Ivoire"
edub$country[which(edub$country == "Swaziland")] <- "Eswatini"
edub$country[which(edub$country == "Dominican Rep.")] <- "Dominican Republic"
edub$country[which(edub$country == "USA")] <- "United States of America"
edub$country[which(edub$country == "Bolivia")] <- "Bolivia (Plurinational State of)"
edub$country[which(edub$country == "Venezuela")] <- "Venezuela (Bolivarian Republic of)"
edub$country[which(edub$country == "Taiwan")] <- "Taiwan (Province of China)"
edub$country[which(edub$country == "Turkey")] <- "Türkiye"
edub <- subset(edub, country != "China, Hong Kong Special Administrative Region" & country != "China, Macao Special Administrative Region" & country != "Reunion")
edub$sex[which(edub$sex == "M")] <- "Male"
edub$sex[which(edub$sex == "FALSE")] <- "Female"
edub <- subset(edub, ageto != 999 | (agefrom == 75 & ageto == 999))
edub_65 <- function(data) {
  
  bind <- NULL
  
  for (x in 1:length(unique(data$country))) {
    
    for (y in 1:length(unique(data$year))) {
      
      for (z in 1:length(unique(data$sex))) {
        
        sub <- subset(data, country == unique(data$country)[x] & year == unique(data$year)[y] & sex == unique(data$sex)[z])
        
        sub$yr_sch <- ifelse(sub$agefrom == 65, (sub$yr_sch[sub$agefrom == 65]+sub$yr_sch[sub$agefrom == 70]+sub$yr_sch[sub$agefrom == 75])/3, sub$yr_sch)
        
        sub <- subset(sub, agefrom != 70 & agefrom != 75)
        
        bind <- rbind(bind, sub)
        
      }
    }
  }
  
  bind
  
}
edub <- edub_65(edub)
remove(edub_65)
edub$age <- paste(edub$agefrom, edub$ageto, sep = "-")
edub$age[which(edub$age == "65-69")] <- "65+"
edub <- edub[, c(1, 2, 7, 3, 6)]
##edua
eduam <- read.csv("OUP_proj_M1564_v1.csv")
eduaf <- read.csv("OUP_proj_F1564_v1.csv")
edua <- rbind(eduam, eduaf)
remove(eduam, eduaf)
edua <- edua[, c(4:8, 16)]
setdiff(unique(edua$country), intersect(unique(data$location), unique(edua$country)))
setdiff(unique(data$location), intersect(unique(data$location), unique(edua$country)))
edua$country[which(edua$country == "Cote dIvoire")] <- "Côte d'Ivoire"
edua$country[which(edua$country == "Swaziland")] <- "Eswatini"
edua$country[which(edua$country == "Dominican Rep.")] <- "Dominican Republic"
edua$country[which(edua$country == "USA")] <- "United States of America"
edua$country[which(edua$country == "Bolivia")] <- "Bolivia (Plurinational State of)"
edua$country[which(edua$country == "Venezuela")] <- "Venezuela (Bolivarian Republic of)"
edua$country[which(edua$country == "Taiwan")] <- "Taiwan (Province of China)"
edua$country[which(edua$country == "Turkey")] <- "Türkiye"
edua$country[which(edua$country == "Iran")] <- "Iran (Islamic Republic of)"
edua$country[which(edua$country == "Syria")] <- "Syrian Arab Republic"
edua <- subset(edua, country != "Hong Kong, China" & country != "China, Macao Special Administrative Region" & country != "Reunion")
edua$sex[which(edua$sex == "M")] <- "Male"
edua$sex[which(edua$sex == "FALSE")] <- "Female"
##edu
setdiff(unique(edua$country), intersect(unique(edub$country), unique(edua$country)))
setdiff(unique(edub$country), intersect(unique(edub$country), unique(edua$country)))
edub$country[which(edub$country == "Libyan Arab Jamahiriya")] <- "Libya"
edup <- function(edub, edua) {
  
  bind <- NULL
  
  for (x in 1:length(unique(edub$country))) {
    
    for (y in 1:length(unique(edub$age))) {
      
      for (z in 1:length(unique(edub$sex))) {
        
        sub <- subset(edub, country == unique(edub$country)[x] & age == unique(edub$age)[y] & sex == unique(edub$sex)[z])
        
        gr <- (edua$yr_sch[edua$year == 2040 & edua$country == unique(edub$country)[x] & edua$sex == unique(edub$sex)[z]]/edua$yr_sch[edua$year == 2015 & edua$country == unique(edub$country)[x] & edua$sex == unique(edub$sex)[z]])^(1/5)-1
        
        yr_sch <- sub$yr_sch[sub$year == 2010]
        
        for (i in 1:((2050-2015)/5+1)) {
          
          sub[nrow(sub)+1, ] <- sub[nrow(sub), ]
          
          sub[nrow(sub), 2] <- 2010+i*5
          
          sub[nrow(sub), 5] <- yr_sch*(1+gr)
          
        }
        
        bind <- rbind(bind, sub)
        
      }
    }
  }
  
  bind
  
}
edu <- edup(edub, edua)
remove(edup, edub, edua)
colnames(edu) <- c("location", "year", "age", "sex", "ea")
edui <- function(edu) {
  
  bind <- NULL
  
  for (x in 1:length(unique(edu$location))) {
    
    for (y in 1:length(unique(edu$age))) {
      
      for (z in 1:length(unique(edu$sex))) {
        
        sub <- subset(edu, location == unique(edu$location)[x] & age == unique(edu$age)[y] & sex == unique(edu$sex)[z])
        
        for (i in 1:(length(unique(edu$year))-1)) {
          
          suby <- subset(sub, year == unique(edu$year)[i])
          
          for (j in 1:4) {
            
            suby[nrow(suby)+1, ] <- suby[nrow(suby), ]
            
            suby[nrow(suby), 2] <- unique(edu$year)[i]+j
            
            ea_b <- subset(sub, year == unique(edu$year)[i])
            
            ea_a <- subset(sub, year == unique(edu$year)[i+1])
            
            suby[nrow(suby), 5] <- ea_b[1, 5] + j*(ea_a[1, 5]-ea_b[1, 5])/5
            
          }
          
          bind <- rbind(bind, suby)
          
        }
        
        suby <- subset(sub, year == 2050)
        bind <- rbind(bind, suby)
        
      }
    }
  }
  
  bind
  
}
edu <- edui(edu)
remove(edui)
write.csv(edu, file = "education 1950-2050.csv")
na_edu <- setdiff(all_location, intersect(all_location, unique(edu$location)))
sd <- setdiff(unique(data$location), intersect(unique(data$location), unique(edu$location)))
for (i in 1:length(sd)) {
  
  data <- subset(data, location != sd[i])
  
}
remove(sd, i, edu)
write.csv(data, file = "data_dp_n_l_edu.csv")

####h------
###LIFE_EXPECTANCY
lebm <- read.csv("LIFE_EXPECTANCY_M 1950-2021.csv")
lebm <- lebm[17:20540, c(3, 9, 11, 77)]
leam <- read.csv("LIFE_EXPECTANCY_M 2022-2100.csv")
leam <- leam[17:22535, c(3, 9, 11, 77)]
setdiff(lebm$X.2, intersect(lebm$X.2, leam$X.2))
setdiff(leam$X.2, intersect(lebm$X.2, leam$X.2))
lem <- rbind(lebm, leam) 
remove(lebm, leam)
lem$sex <- "Male"
lebf <- read.csv("LIFE_EXPECTANCY_F 1950-2021.csv")
lebf <- lebf[17:20540, c(3, 9, 11, 77)]
leaf <- read.csv("LIFE_EXPECTANCY_F 2022-2100.csv")
leaf <- leaf[17:22535, c(3, 9, 11, 77)]
setdiff(lebf$X.2, intersect(lebf$X.2, leaf$X.2))
setdiff(leaf$X.2, intersect(lebf$X.2, leaf$X.2))
lef <- rbind(lebf, leaf)
remove(lebf, leaf)
lef$sex <- "Female"
setdiff(lef$X.2, intersect(lef$X.2, lem$X.2))
setdiff(lef$X.2, intersect(lef$X.2, lem$X.2))
le <- rbind(lem, lef) 
remove(lem, lef)
le <- subset(le, X.8 == "Country/Area")
le <- le[, c(1, 3, 5, 4)]
colnames(le) <- c("location", "year", "sex", "le")
setdiff(unique(le$location), intersect(unique(data$location), unique(le$location)))
setdiff(unique(data$location), intersect(unique(data$location), unique(le$location)))
le$location[which(le$location == "China, Taiwan Province of China")] <- "Taiwan (Province of China)"
le$location[which(le$location == "Czechia")] <- "Czech Republic"
###age_med
edu <- read.csv("education 1950-2050.csv")
edu <- edu[, -1]
age_med <- function(edu, le) {
  
  bind <- NULL
  
  h <- edu
  
  h <- mutate(h, age_med = ifelse(age == "15-19", 17, 
                                  ifelse(age == "20-24", 22, 
                                         ifelse(age == "25-29", 27, 
                                                ifelse(age == "30-34", 32, 
                                                       ifelse(age == "35-39", 37, 
                                                              ifelse(age == "40-44", 42, 
                                                                     ifelse(age == "45-49", 47, 
                                                                            ifelse(age == "50-54", 52, 
                                                                                   ifelse(age == "55-59", 57, 
                                                                                          ifelse(age == "60-64", 62, NA)))))))))))
  
  for (x in 1:length(unique(h$location))) {
    
    for (y in 1:length(unique(h$year))) {
      
      for (z in 1:length(unique(h$sex))) {
        
        h$age_med[h$age == "65+" & h$location == unique(h$location)[x] & h$year == unique(h$year)[y] & h$sex == unique(h$sex)[z]] <- (65+(65+as.numeric(le$le[le$location == unique(h$location)[x] & le$year == unique(h$year)[y] & le$sex == unique(h$sex)[z]])))/2
        
      }
    }
  }
  
  h
  
}
h <- age_med(edu, le)
remove(age_med, edu, le)
h <- mutate(h, h = exp(0.091*ea+0.1301*(age_med-ea-5)-0.0023*((age_med-ea-5)^2)))
write.csv(h, "h.csv")

####人力资本------
###合并
data <- read.csv("data_dp_n_l_edu.csv")
data <- data[, -1]
h <- read.csv("h.csv")
h <- h[, c(2:5, 8)]
setdiff(unique(h$location), intersect(unique(data$location), unique(h$location)))
setdiff(unique(data$location), intersect(unique(data$location), unique(h$location)))
na_lfp 
h <- subset(h, location != "Central African Republic" & location != "Libya")
data <- merge(data, h, by = c("location", "year", "age", "sex"), all.x = T)
remove(h)
###计算
hc_sum <- function(data) {
  
  bind <- NULL
  
  data$hc_sq <- data$pop_sq*(data$lfp_sq/100)*data$h
  data$hc_cf <- data$pop_cf*(data$lfp_cf/100)*data$h
  data$hc_cf_lower <- data$pop_cf_lower*(data$lfp_cf_lower/100)*data$h
  data$hc_cf_upper <- data$pop_cf_upper*(data$lfp_cf_upper/100)*data$h
  
  data <- data[, -c(6:22)]
  
  for (i in 1:length(unique(data$location))) {
    
    for (j in 1:length(unique(data$year))) {
      
      sub <- subset(data, location == unique(data$location)[i] & year == unique(data$year)[j])
      sub[nrow(sub)+1, ] <- c(unique(data$location)[i], unique(data$year)[j], "Total", "Total", sub[nrow(sub), 5], sum(sub$hc_sq), sum(sub$hc_cf), sum(sub$hc_cf_lower), sum(sub$hc_cf_upper))
      
      bind <- rbind(bind, sub)
      
    }
    
  }
  
  bind
  
}
data <- hc_sum(data)
remove(hc_sum)
write.csv(data, "data_hc.csv")

####事实GDP------
###预处理
data <- read.csv("data_hc.csv")
data <- subset(data, age == "Total")
data <- data[, -c(1, 4, 5)]
setwd("D:/S/基于健康增益宏观效应模型的疾病经济成本评估/Diabetes/实物资本及总产出")
gdp <- read.csv("gdp.csv") #2017国际美元
gdp <- gdp[, c(1, 60:66)]
gdp <- gather(gdp, key = "year", value = "gdp", 2:8)
colnames(gdp) <- c("location", "year", "gdp")
gdp$year <- as.numeric(gsub("X", "", gdp$year))
gdp <- subset(gdp, !is.na(gdp)) 
setdiff(unique(data$location), intersect(unique(gdp$location), unique(data$location)))
setdiff(unique(gdp$location), intersect(unique(gdp$location), unique(data$location)))
gdp$location[which(gdp$location == "Bolivia")] <- "Bolivia (Plurinational State of)"
gdp$location[which(gdp$location == "Cote d'Ivoire")] <- "Côte d'Ivoire"
gdp$location[which(gdp$location == "Congo, Rep.")] <- "Congo"
gdp$location[which(gdp$location == "Congo, Dem. Rep.")] <- "Democratic Republic of the Congo"
gdp$location[which(gdp$location == "Czechia")] <- "Czech Republic"
gdp$location[which(gdp$location == "Egypt, Arab Rep.")] <- "Egypt"
gdp$location[which(gdp$location == "Gambia, The")] <- "Gambia"
gdp$location[which(gdp$location == "Iran, Islamic Rep.")] <- "Iran (Islamic Republic of)"
gdp$location[which(gdp$location == "Kyrgyz Republic")] <- "Kyrgyzstan"
gdp$location[which(gdp$location == "Lao PDR")] <- "Lao People's Democratic Republic"
gdp$location[which(gdp$location == "Moldova")] <- "Republic of Moldova"
gdp$location[which(gdp$location == "Korea, Rep.")] <- "Republic of Korea"
gdp$location[which(gdp$location == "Slovak Republic")] <- "Slovakia"
gdp$location[which(gdp$location == "Turkiye")] <- "Türkiye"
gdp$location[which(gdp$location == "Tanzania")] <- "United Republic of Tanzania"
gdp$location[which(gdp$location == "United States")] <- "United States of America"
gdp$location[which(gdp$location == "Yemen, Rep.")] <- "Yemen"
na_gdp <- setdiff(all_location, intersect(unique(gdp$location), all_location))
setdiff(unique(data$location), intersect(unique(gdp$location), unique(data$location)))
data <- subset(data, location != "Cuba" & location != "Syrian Arab Republic")
sd <- setdiff(unique(gdp$location), intersect(unique(gdp$location), unique(data$location)))
for (i in 1:length(sd)) {
  
  gdp <- subset(gdp, location != sd[i])
  
}
remove(sd, i)
###gdp_predict
gdp_percent_change <- read.csv("GDP percent change.csv")
gdp_percent_change <- gdp_percent_change[, c(1, 6:34)]
gdp_percent_change <- gather(gdp_percent_change, key = "year", value = "gdp_percent_change", 2:30)
gdp_percent_change$year <- as.numeric(gsub("X", "", gdp_percent_change$year))
gdp_percent_change$gdp_percent_change[which(gdp_percent_change$gdp_percent_change == "n/a")] <- NA
gdp_percent_change$gdp_percent_change <- as.numeric(gdp_percent_change$gdp_percent_change)
colnames(gdp_percent_change) <- c("location", "year", "gdp_percent_change")
gdp_percent_change <- subset(gdp_percent_change, year >= 2022 & year <= 2028)
##与gdp数据location保持一致
setdiff(unique(gdp_percent_change$location), intersect(unique(gdp$location), unique(gdp_percent_change$location)))
setdiff(unique(gdp$location), intersect(unique(gdp$location), unique(gdp_percent_change$location)))
gdp_percent_change$location[which(gdp_percent_change$location == "Bolivia")] <- "Bolivia (Plurinational State of)"
gdp_percent_change$location[which(gdp_percent_change$location == "C?te d'Ivoire")] <- "Côte d'Ivoire"
gdp_percent_change$location[which(gdp_percent_change$location == "Republic of Congo")] <- "Congo"
gdp_percent_change$location[which(gdp_percent_change$location == "Korea")] <- "Republic of Korea"
gdp_percent_change$location[which(gdp_percent_change$location == "The Gambia")] <- "Gambia"
gdp_percent_change$location[which(gdp_percent_change$location == "Islamic Republic of Iran")] <- "Iran (Islamic Republic of)"
gdp_percent_change$location[which(gdp_percent_change$location == "Kyrgyz Republic")] <- "Kyrgyzstan"
gdp_percent_change$location[which(gdp_percent_change$location == "Lao P.D.R.")] <- "Lao People's Democratic Republic"
gdp_percent_change$location[which(gdp_percent_change$location == "Moldova")] <- "Republic of Moldova"
gdp_percent_change$location[which(gdp_percent_change$location == "Russia")] <- "Russian Federation"
gdp_percent_change$location[which(gdp_percent_change$location == "Slovak Republic")] <- "Slovakia"
gdp_percent_change$location[which(gdp_percent_change$location == "T?rkiye")] <- "Türkiye"
gdp_percent_change$location[which(gdp_percent_change$location == "United States")] <- "United States of America"
gdp_percent_change$location[which(gdp_percent_change$location == "Tanzania")] <- "United Republic of Tanzania"
gdp_percent_change$location[which(gdp_percent_change$location == "Vietnam")] <- "Viet Nam"
gdp_percent_change$location[which(gdp_percent_change$location == "Kosovo")] <- "Taiwan (Province of China)" 
gdp_percent_change$gdp_percent_change[gdp_percent_change$location == "Taiwan (Province of China)"] <- NA
gdp_percent_change$location[which(gdp_percent_change$location == "Lebanon")] <- "Venezuela (Bolivarian Republic of)" 
gdp_percent_change$gdp_percent_change[gdp_percent_change$location == "Venezuela (Bolivarian Republic of)"] <- NA
sd <- setdiff(unique(gdp_percent_change$location), intersect(unique(gdp$location), unique(gdp_percent_change$location)))
for (i in 1:length(sd)) {
  
  gdp_percent_change <- subset(gdp_percent_change, location != sd[i])
  
}
remove(sd, i)
na_gdp_pc <- subset(gdp_percent_change, is.na(gdp_percent_change))
na_gdp_pc <- unique(na_gdp_pc$location)
for (i in 2022:2028) {
  
  sub <- subset(gdp_percent_change, year == i)
  gdp_percent_change$gdp_percent_change[gdp_percent_change$year == i & (gdp_percent_change$location == "Afghanistan" | gdp_percent_change$location == "Sri Lanka" | gdp_percent_change$location == "Taiwan (Province of China)" | gdp_percent_change$location == "Venezuela (Bolivarian Republic of)")] <- mean(sub$gdp_percent_change, na.rm = T)
  
}
remove(i, sub, na_gdp_pc)
##predict
unique_gdp_location <- unique(gdp$location)
gdp_predict <- function(gdp, gdp_percent_change) {
  
  bind <- NULL
  
  for (i in 1:length(unique_gdp_location)) {
    
    sub <- subset(gdp, location == unique_gdp_location[i])
    
    
    for (j in 2022:2028) {
      
      sub[nrow(sub)+1, ] <- c(unique_gdp_location[i], j, sub$gdp[sub$year == j-1])
      sub$gdp[sub$year == j] <- as.numeric(sub$gdp[sub$year == j])*(1+gdp_percent_change$gdp_percent_change[gdp_percent_change$location == unique_gdp_location[i] & gdp_percent_change$year == j]/100)
      
    }
    
    bind <- rbind(bind, sub)
    
  }
  
  gdp <- bind
  bind <- NULL
  
  for (i in 1:length(unique_gdp_location)) {
    
    sub <- subset(gdp, location == unique_gdp_location[i])
    
    gdp_r <- (as.numeric(sub$gdp[sub$year == 2019])/as.numeric(sub$gdp[sub$year == 2015]))^(1/4)-1
    
    for (j in 2029:2050) {
      
      sub[nrow(sub)+1, ] <- c(unique_gdp_location[i], j, sub$gdp[sub$year == j-1])
      sub$gdp[sub$year == j] <- as.numeric(sub$gdp[sub$year == j])*(1+gdp_r)
      
    }
    
    bind <- rbind(bind, sub)
    
  }
  
  bind <- subset(bind, year >= 2019)
  
}
gdp <- gdp_predict(gdp, gdp_percent_change)
remove(gdp_predict, gdp_percent_change, unique_gdp_location)
write.csv(data, "data_hc_.csv")
write.csv(gdp, "data_gdp.csv")
remove(data, gdp)

####事实实物资本------
###预处理
data <- read.csv("data_hc_.csv")
data <- data[, -1]
cs <- read.csv("Capital stock.csv") #millions
cs <- cs[, c(2, 4, 15)]
colnames(cs) <- c("location", "year", "cs_sq")
cs$cs_sq <- cs$cs_sq*1000000
cs <- subset(cs, year == 2019)
setdiff(unique(data$location), intersect(unique(cs$location), unique(data$location)))
setdiff(unique(cs$location), intersect(unique(cs$location), unique(data$location)))
cs$location[which(cs$location == "D.R. of the Congo")] <- "Democratic Republic of the Congo"
cs$location[which(cs$location == "Lao People's DR")] <- "Lao People's Democratic Republic"
cs$location[which(cs$location == "Turkey")] <- "Türkiye"
cs$location[which(cs$location == "U.R. of Tanzania: Mainland")] <- "United Republic of Tanzania"
cs$location[which(cs$location == "United States")] <- "United States of America"
cs$location[which(cs$location == "Taiwan")] <- "Taiwan (Province of China)"
na_cs <- setdiff(all_location, intersect(unique(cs$location), all_location))
setdiff(unique(data$location), intersect(unique(cs$location), unique(data$location)))
data <- subset(data, location != "Afghanistan" & location != "Papua New Guinea" & location != "Tonga")
sd <- setdiff(unique(cs$location), intersect(unique(cs$location), unique(data$location)))
for (i in 1:length(sd)) {
  
  cs <- subset(cs, location != sd[i])
  
}
remove(sd, i)
###saving_rate
sav_rate <- read.csv("Gross savings (% of GDP).csv", header = F)
sav_rate <- sav_rate[-c(1:3), c(1, 55:64)]
colnames(sav_rate) <- c("location", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019")
setdiff(unique(data$location), intersect(unique(sav_rate$location), unique(data$location)))
setdiff(unique(sav_rate$location), intersect(unique(sav_rate$location), unique(data$location)))
sav_rate$location[which(sav_rate$location == "Bolivia")] <- "Bolivia (Plurinational State of)"
sav_rate$location[which(sav_rate$location == "Congo, Rep.")] <- "Congo"
sav_rate$location[which(sav_rate$location == "Congo, Dem. Rep.")] <- "Democratic Republic of the Congo"
sav_rate$location[which(sav_rate$location == "Cote d'Ivoire")] <- "Côte d'Ivoire"
sav_rate$location[which(sav_rate$location == "Czechia")] <- "Czech Republic"
sav_rate$location[which(sav_rate$location == "Egypt, Arab Rep.")] <- "Egypt"
sav_rate$location[which(sav_rate$location == "Gambia, The")] <- "Gambia"
sav_rate$location[which(sav_rate$location == "Iran, Islamic Rep.")] <- "Iran (Islamic Republic of)"
sav_rate$location[which(sav_rate$location == "Kyrgyz Republic")] <- "Kyrgyzstan"
sav_rate$location[which(sav_rate$location == "Lao PDR")] <- "Lao People's Democratic Republic"
sav_rate$location[which(sav_rate$location == "Korea, Rep.")] <- "Republic of Korea"
sav_rate$location[which(sav_rate$location == "Moldova")] <- "Republic of Moldova"
sav_rate$location[which(sav_rate$location == "Slovak Republic")] <- "Slovakia"
sav_rate$location[which(sav_rate$location == "Turkiye")] <- "Türkiye"
sav_rate$location[which(sav_rate$location == "Tanzania")] <- "United Republic of Tanzania"
sav_rate$location[which(sav_rate$location == "United States")] <- "United States of America"
sav_rate$location[which(sav_rate$location == "Venezuela, RB")] <- "Venezuela (Bolivarian Republic of)"
sav_rate$location[which(sav_rate$location == "Yemen, Rep.")] <- "Yemen"
na_sr <- subset(sav_rate, rowSums(is.na(sav_rate)) == 10)
na_sr <- unique(na_sr$location)
setdiff(unique(data$location), intersect(unique(sav_rate$location), unique(data$location)))
data <- subset(data, location != "Taiwan (Province of China)") 
cs <- subset(cs, location != "Taiwan (Province of China)")
na_sr <- c(na_sr, "Taiwan (Province of China)")
sd <- setdiff(unique(sav_rate$location), intersect(unique(sav_rate$location), unique(data$location)))
for (i in 1:length(sd)) {
  
  sav_rate <- subset(sav_rate, location != sd[i])
  
}
remove(sd, i)
na_sr_nb <- subset(sav_rate, rowSums(is.na(sav_rate)) == 10)
unique(na_sr_nb$location)
remove(na_sr_nb)
sav_rate <- gather(sav_rate, key = "year", value = "sav_rate", 2:11)
for (i in 2010:2019) {
  
  sub <- subset(sav_rate, year == i)
  sav_rate$sav_rate[sav_rate$year == i & sav_rate$location == "Maldives"] <- mean(sub$sav_rate, na.rm = T)
  sav_rate$sav_rate[sav_rate$year == i & sav_rate$location == "Mozambique"] <- mean(sub$sav_rate, na.rm = T)
  
}
remove(i, sub)
special_loc <- which(na_sr == "Maldives")
na_sr <- na_sr[-special_loc]
special_loc <- which(na_sr == "Mozambique")
na_sr <- na_sr[-special_loc]
remove(special_loc)
data <- subset(data, location != "United Arab Emirates" & location != "Guyana" & location != "Iran (Islamic Republic of)" & location != "Liberia" & location != "Myanmar" & location != "Malawi" & location != "Trinidad and Tobago")
cs <- subset(cs, location != "United Arab Emirates" & location != "Guyana" & location != "Iran (Islamic Republic of)" & location != "Liberia" & location != "Myanmar" & location != "Malawi" & location != "Trinidad and Tobago")
sav_rate <- subset(sav_rate, location != "United Arab Emirates" & location != "Guyana" & location != "Iran (Islamic Republic of)" & location != "Liberia" & location != "Myanmar" & location != "Malawi" & location != "Trinidad and Tobago")
###事实实物资本计算
gdp <- read.csv("data_gdp.csv")
gdp <- gdp[, -1]
unique_cs_location <- unique(cs$location)
cs_sq <- function(gdp, cs, sav_rate) {
  
  bind <- NULL
  
  for (i in 1:length(unique_cs_location)) {
    
    sub <- subset(cs, location == unique_cs_location[i])
    
    sub_sr <- subset(sav_rate, location == unique_cs_location[i])
    mean_sr <- mean(sub_sr$sav_rate, na.rm = T)
    
    for (j in 2020:2050) {
      
      sub[nrow(sub)+1, 1] <- unique_cs_location[i]
      sub[nrow(sub), 2] <- j
      sub[nrow(sub), 3] <- (1-0.05)*as.numeric(sub[nrow(sub)-1, 3])+as.numeric(gdp$gdp[gdp$location == unique_cs_location[i] & gdp$year == j-1])*mean_sr/100
      
    }
    
    bind <- rbind(bind, sub)
    
  }
  
  bind <- subset(bind, year >= 2021)
  
}
cs <- cs_sq(gdp, cs, sav_rate)
data_sr <- function(data, cs, sav_rate) {
  
  data$sav_rate <- NA
  
  for (i in 1:length(unique_cs_location)) {
    
    sub <- subset(cs, location == unique_cs_location[i])
    
    sub_sr <- subset(sav_rate, location == unique_cs_location[i])
    mean_sr <- mean(sub_sr$sav_rate, na.rm = T)
    
    data$sav_rate[data$location == unique_cs_location[i]] <- mean_sr
    
  }
  
  data
  
}
data <- data_sr(data, cs, sav_rate)
remove(sav_rate, unique_cs_location, cs_sq, data_sr)
gdp <- subset(gdp, year >= 2021)
data <- merge(data, gdp, by = c("location", "year"), all.x = T)
data <- merge(data, cs, by = c("location", "year"))
remove(cs, gdp)
names(data)[names(data) == "gdp"] <- "gdp_sq"
write.csv(data, "data_gdp_cs (all sq).csv")

####技术水平------
###弹性系数
data <- read.csv("data_gdp_cs (all sq).csv")
data <- data[, -1]
share_cs_gdp <- read.csv("Share of Labour Compensation in GDP.csv") 
share_cs_gdp$location[which(share_cs_gdp$location == "C?te d'Ivoire")] <- "Côte d'Ivoire"
share_cs_gdp$location[which(share_cs_gdp$location == "T\xa8\xb9rkiye")] <- "Türkiye"
setdiff(unique(data$location), intersect(unique(share_cs_gdp$location), unique(data$location)))
setdiff(unique(share_cs_gdp$location), intersect(unique(share_cs_gdp$location), unique(data$location)))
share_cs_gdp$share_cs_gdp[which(is.na(share_cs_gdp$share_cs_gdp))] <- mean(share_cs_gdp$share_cs_gdp, na.rm = T)
capital_share <- function(data, share_cs_gdp) {
  
  data$capital_share <- NA
  
  for (i in 1:length(unique(data$location))) {
    
    data$capital_share[data$location == unique(data$location)[i]] <- share_cs_gdp$share_cs_gdp[share_cs_gdp$location == unique(data$location)[i]]
    
  }
  
  data
  
}
data <- capital_share(data, share_cs_gdp)
remove(share_cs_gdp, capital_share)
###技术水平
data$tech_level <- data$gdp_sq/(data$cs_sq^(1-data$capital_share))/(data$hc_sq^data$capital_share)

####治疗费用------
###治疗费用在总健康支出中的占比
##beta
pop <- read.csv("Population (crude).csv")
pop1 <- pop$val[pop$year == 2016 & pop$location_name == "United States of America"]
pre <- read.csv("Prevalence (crude).csv")
pre1 <- pre$val[pre$year == 2016 & pre$location_name == "United States of America"]

beta <- A/(pre1/pop1)  #A <- proportion of fall
remove(pre1, pop1)
##疾病整体患病率预测
pop <- subset(pop, year >= 2011 & year <= 2020)
pop <- pop[, c(4, 11, 12)]
colnames(pop) <- c("location", "year", "pop")
pre <- subset(pre, year >= 2011 & year <= 2020)
pre <- pre[, c(4, 13, 14)]
colnames(pre) <- c("location", "year", "pre")
tc <- merge(pop, pre, by = c("location", "year"))

tc$rate.pre <- tc$pre/tc$pop
tc <- tc[, c(1, 2, 5)]
predict_for_rate <- function(data) {
  
  bind <- NULL
  
  for (i in 1:length(unique(data$location))) {
    
    sub <- subset(data, location == unique(data$location)[i])
    
    gr_pre <- (sub$rate.pre[sub$year == 2020]/sub$rate.pre[sub$year == 2011])^(1/9)-1
    gr_pre <- ifelse(gr_pre > 0.02, 0.02, gr_pre)
    
    for (j in 2021:2050) {
      
      sub[nrow(sub)+1, ] <- c(unique(data$location)[i], j, as.numeric(sub$rate.pre[sub$year == 2020])*(1+gr_pre)^(j-2020))
      
    }
    
    bind <- rbind(bind, sub)
    
  }
  
  bind$rate.pre <- ifelse(bind$rate.pre >= 1, 0.99, bind$rate.pre)
  
  bind <- subset(bind, year >= 2021)
  
}
tc <- predict_for_rate(tc)
remove(predict_for_rate)
tc$year <- as.numeric(tc$year)
tc$rate.pre <- as.numeric(tc$rate.pre)
##tc_share
tc$tc_share <- beta*tc$rate.pre
tc <- tc[, -3]
data <- merge(data, tc, by = c("location", "year"), all.x = T)
remove(beta, tc)
###人均健康支出
##总健康支出在GDP中的占比——用于计算人均健康支出的增长率
he_share <- read.csv("Current health expenditure (% of GDP).csv")
he_share <- he_share[, c(1, 45:64)]
names(he_share)[names(he_share) == "Country.Name"] <- "location"
setdiff(unique(data$location), intersect(unique(he_share$location), unique(data$location)))
setdiff(unique(he_share$location), intersect(unique(he_share$location), unique(data$location)))
he_share$location[which(he_share$location == "Bolivia")] <- "Bolivia (Plurinational State of)"
he_share$location[which(he_share$location == "Congo, Rep.")] <- "Congo"
he_share$location[which(he_share$location == "Congo, Dem. Rep.")] <- "Democratic Republic of the Congo"
he_share$location[which(he_share$location == "Cote d'Ivoire")] <- "Côte d'Ivoire"
he_share$location[which(he_share$location == "Czechia")] <- "Czech Republic"
he_share$location[which(he_share$location == "Egypt, Arab Rep.")] <- "Egypt"
he_share$location[which(he_share$location == "Gambia, The")] <- "Gambia"
he_share$location[which(he_share$location == "Kyrgyz Republic")] <- "Kyrgyzstan"
he_share$location[which(he_share$location == "Lao PDR")] <- "Lao People's Democratic Republic"
he_share$location[which(he_share$location == "Korea, Rep.")] <- "Republic of Korea"
he_share$location[which(he_share$location == "Moldova")] <- "Republic of Moldova"
he_share$location[which(he_share$location == "Slovak Republic")] <- "Slovakia"
he_share$location[which(he_share$location == "Turkiye")] <- "Türkiye"
he_share$location[which(he_share$location == "Tanzania")] <- "United Republic of Tanzania"
he_share$location[which(he_share$location == "United States")] <- "United States of America"
he_share$location[which(he_share$location == "Venezuela, RB")] <- "Venezuela (Bolivarian Republic of)"
he_share$location[which(he_share$location == "Yemen, Rep.")] <- "Yemen"
na_tc <- setdiff(all_location, intersect(unique(he_share$location), all_location))
sd <- setdiff(unique(he_share$location), intersect(unique(he_share$location), unique(data$location)))
for (i in 1:length(sd)) {
  
  he_share <- subset(he_share, location != sd[i])
  
}
remove(sd, i)
he_share <- gather(he_share, key = "year", value = "he_share", 2:21)
he_share$year <- as.numeric(gsub("X", "", he_share$year))
he_share <- subset(he_share, !is.na(he_share))
##读取2021年各国人均健康支出
he_pc <- read.csv("Current health expenditure per capita, PPP.csv")
he_pc <- he_pc[-c(1:3), c(1, 66)]
he_pc$year <- 2021
he_pc <- he_pc[, c(1, 3, 2)]
colnames(he_pc) <- c("location", "year", "he_pc")
he_pc <- subset(he_pc, !is.na(he_pc))
setdiff(unique(data$location), intersect(unique(he_pc$location), unique(data$location)))
setdiff(unique(he_pc$location), intersect(unique(he_pc$location), unique(data$location)))
he_pc$location[which(he_pc$location == "Bolivia")] <- "Bolivia (Plurinational State of)"
he_pc$location[which(he_pc$location == "Congo, Rep.")] <- "Congo"
he_pc$location[which(he_pc$location == "Congo, Dem. Rep.")] <- "Democratic Republic of the Congo"
he_pc$location[which(he_pc$location == "Cote d'Ivoire")] <- "Côte d'Ivoire"
he_pc$location[which(he_pc$location == "Czechia")] <- "Czech Republic"
he_pc$location[which(he_pc$location == "Egypt, Arab Rep.")] <- "Egypt"
he_pc$location[which(he_pc$location == "Gambia, The")] <- "Gambia"
he_pc$location[which(he_pc$location == "Kyrgyz Republic")] <- "Kyrgyzstan"
he_pc$location[which(he_pc$location == "Lao PDR")] <- "Lao People's Democratic Republic"
he_pc$location[which(he_pc$location == "Korea, Rep.")] <- "Republic of Korea"
he_pc$location[which(he_pc$location == "Moldova")] <- "Republic of Moldova"
he_pc$location[which(he_pc$location == "Slovak Republic")] <- "Slovakia"
he_pc$location[which(he_pc$location == "Turkiye")] <- "Türkiye"
he_pc$location[which(he_pc$location == "Tanzania")] <- "United Republic of Tanzania"
he_pc$location[which(he_pc$location == "United States")] <- "United States of America"
setdiff(unique(data$location), intersect(unique(he_pc$location), unique(data$location))) 
na_tc <- c(na_tc, "Yemen", "Venezuela")
data <- subset(data, location != "Yemen" & location != "Venezuela")
he_share <- subset(he_share, location != "Yemen" & location != "Venezuela")
##计算人均健康支出增长率
setdiff(unique(he_share$location), intersect(unique(he_pc$location), unique(he_share$location)))
setdiff(unique(he_pc$location), intersect(unique(he_pc$location), unique(he_share$location)))
unique_he_pc_location <- unique(he_pc$location)
he_rate <- function(he_share, he_pc) {
  
  for (i in 1:length(unique_he_pc_location)) {
    
    sub <- subset(he_share, location == unique_he_pc_location[i] & (year == 2019 | year == 2000))
    
    if (nrow(sub) == 0) {
      
      he_pc <- subset(he_pc, location != unique_he_pc_location[i])
      
    } else if (nrow(sub) == 1) {
      
      he_pc$he_rate[he_pc$location == unique_he_pc_location[i]] <- NA
      
    } else {
      
      he_pc$he_rate[he_pc$location == unique_he_pc_location[i]] <- (he_share$he_share[he_share$location == unique_he_pc_location[i] & he_share$year == 2019]/he_share$he_share[he_share$location == unique_he_pc_location[i] & he_share$year == 2000])^(1/19)-1
      
    }
    
  }
  
  he_pc
  
}
he_pc <- he_rate(he_share, he_pc)
remove(he_rate, unique_he_pc_location)
sub_na <- subset(he_pc, is.na(he_rate))
he_pc$he_rate[he_pc$location == "Iraq"] <- (he_share$he_share[he_share$location == "Iraq" & he_share$year == 2019]/he_share$he_share[he_share$location == "Iraq" & he_share$year == 2003])^(1/16)-1
he_pc$he_rate[he_pc$location == "Zimbabwe"] <- (he_share$he_share[he_share$location == "Zimbabwe" & he_share$year == 2019]/he_share$he_share[he_share$location == "Zimbabwe" & he_share$year == 2010])^(1/9)-1
remove(sub_na, he_share)
##计算2022-2050各国人均健康支出
unique_he_pc_location <- unique(he_pc$location)
he_pc_ <- function(he_pc) {
  
  bind <- NULL
  
  for (i in 1:length(unique_he_pc_location)) {
    
    sub <- subset(he_pc, location == unique_he_pc_location[i])
    
    for (j in 2022:2050) {
      
      sub[nrow(sub)+1, ] <- sub[nrow(sub), ]
      sub[nrow(sub), 2] <- j
      sub[nrow(sub), 3] <- sub[nrow(sub), 3]*(1+sub[nrow(sub), 4])
      
    }
    
    bind <- rbind(bind, sub)
    
  }
  
  bind
  
}
he_pc <- he_pc_(he_pc)
he_pc <- he_pc[, -4]
data <- merge(data, he_pc, by = c("location", "year"), all.x = T)
remove(he_pc, he_pc_, unique_he_pc_location)
###Population(2021-2050)
setwd("D:/S/基于健康增益宏观效应模型的疾病经济成本评估/Diabetes/人力资本")
pop <- read.csv("Population DESA.csv")
pop <- pop[, c(8, 10, 13, 15:17, 20)]
pop <- subset(pop, Time >= 2021 & Time <= 2050)
pop <- subset(pop, LocTypeName == "Country/Area")
pop$AgeGrp <- ifelse(pop$AgeGrpStart== 5, "5-9", 
                     ifelse(pop$AgeGrpStart == 10, "10-14", pop$AgeGrp))
pop <- pop[, c(2:4, 7)]
colnames(pop) <- c("location", "year", "age", "pop")
setdiff(unique(data$location), intersect(unique(pop$location), unique(data$location)))
setdiff(unique(pop$location), intersect(unique(pop$location), unique(data$location)))
pop$location[which(pop$location == "Czechia")] <- "Czech Republic"
pop$pop <- ifelse(pop$location == "China", pop$pop[pop$location == "China"] + pop$pop[pop$location == "China, Hong Kong SAR"] + pop$pop[pop$location == "China, Macao SAR"], pop$pop)
sd <- setdiff(unique(pop$location), intersect(unique(pop$location), unique(data$location)))
for (i in 1:length(sd)) {
  
  pop <- subset(pop, location != sd[i])
  
}
remove(sd, i)
pop_total <- function(data) {
  
  bind <- NULL
  
  for (i in 1:length(unique(data$location))) {
    
    for (j in 1:length(unique(data$year))) {
      
      sub <- subset(data, location == unique(data$location)[i] & year == unique(data$year)[j])
      
      sub[nrow(sub)+1, ] <- c(unique(data$location)[i], unique(data$year)[j], "Total", sum(sub$pop)*1000)
      
      bind <- rbind(bind, sub)
      
    }
    
  }
  
  bind <- subset(bind, age == "Total")
  
  bind
  
}
pop <- pop_total(pop)
pop <- pop[, -3]
pop$year <- as.numeric(pop$year)
pop$pop <- as.numeric(pop$pop)
data <- merge(data, pop, by = c("location", "year"), all.x = T)
remove(pop_total, pop)
###治疗费用
data$tc <- (data$tc_share/100)*data$he_pc*data$pop
setwd("D:/S/基于健康增益宏观效应模型的疾病经济成本评估/Diabetes/实物资本及总产出")
write.csv(data, "data_tl_tc.csv")

####反事实实物资本及总产出------
data <- read.csv("data_tl_tc.csv")
data <- data[, -1]
cf_cs_gdp <- function(data) {
  
  bind <- NULL
  
  data$gdp_cf <- NA
  data$gdp_cf_lower <- NA
  data$gdp_cf_upper <- NA
  
  data$cs_cf <- data$cs_sq
  data$cs_cf_lower <- data$cs_sq
  data$cs_cf_upper <- data$cs_sq
  
  for (i in 1:length(unique(data$location))) {
    
    sub <- subset(data, location == unique(data$location)[i])
    
    sub$gdp_cf[sub$year == 2021] <- sub$tech_level[sub$year == 2021]*(sub$cs_cf[sub$year == 2021]^(1-sub$capital_share[sub$year == 2021]))*(sub$hc_cf[sub$year == 2021]^sub$capital_share[sub$year == 2021])
    sub$gdp_cf_lower[sub$year == 2021] <- sub$tech_level[sub$year == 2021]*(sub$cs_cf_lower[sub$year == 2021]^(1-sub$capital_share[sub$year == 2021]))*(sub$hc_cf_lower[sub$year == 2021]^sub$capital_share[sub$year == 2021])
    sub$gdp_cf_upper[sub$year == 2021] <- sub$tech_level[sub$year == 2021]*(sub$cs_cf_upper[sub$year == 2021]^(1-sub$capital_share[sub$year == 2021]))*(sub$hc_cf_upper[sub$year == 2021]^sub$capital_share[sub$year == 2021])
    
    for (j in 2022:2050) {
      
      sub$cs_cf[sub$year == j] <- (sub$sav_rate[sub$year == j-1]/100)*(sub$gdp_cf[sub$year == j-1]+sub$tc[sub$year == j-1])+(1-0.05)*sub$cs_cf[sub$year == j-1]
      sub$cs_cf_lower[sub$year == j] <- (sub$sav_rate[sub$year == j-1]/100)*(sub$gdp_cf_lower[sub$year == j-1]+sub$tc[sub$year == j-1])+(1-0.05)*sub$cs_cf_lower[sub$year == j-1]
      sub$cs_cf_upper[sub$year == j] <- (sub$sav_rate[sub$year == j-1]/100)*(sub$gdp_cf_upper[sub$year == j-1]+sub$tc[sub$year == j-1])+(1-0.05)*sub$cs_cf_upper[sub$year == j-1]
      
      sub$gdp_cf[sub$year == j] <- sub$tech_level[sub$year == j]*(sub$cs_cf[sub$year == j]^(1-sub$capital_share[sub$year == j]))*(sub$hc_cf[sub$year == j]^sub$capital_share[sub$year == j])
      sub$gdp_cf_lower[sub$year == j] <- sub$tech_level[sub$year == j]*(sub$cs_cf_lower[sub$year == j]^(1-sub$capital_share[sub$year == j]))*(sub$hc_cf_lower[sub$year == j]^sub$capital_share[sub$year == j])
      sub$gdp_cf_upper[sub$year == j] <- sub$tech_level[sub$year == j]*(sub$cs_cf_upper[sub$year == j]^(1-sub$capital_share[sub$year == j]))*(sub$hc_cf_upper[sub$year == j]^sub$capital_share[sub$year == j])
      
    }
    
    bind <- rbind(bind, sub)
    
  }
  
  bind
  
}
data <- cf_cs_gdp(data)
remove(cf_cs_gdp)
write.csv(data, "data (crude_R).csv")

####经济成本------
###预处理
data <- read.csv("data (crude_R).csv")
data <- data[, c(4, 2, 3, 10, 18:20)]
setwd("D:/S/基于健康增益宏观效应模型的疾病经济成本评估/Diabetes/人力资本")
pop_DESA <- read.csv("Population DESA.csv")
pop_DESA <- pop_DESA[, c(8, 10, 13, 15:17, 20)]
pop_DESA <- subset(pop_DESA, Time >= 2021 & Time <= 2050)
pop_DESA <- subset(pop_DESA, LocTypeName == "Country/Area" | LocTypeName == "World")
pop_DESA$AgeGrp <- ifelse(pop_DESA$AgeGrpStart== 5, "5-9", 
                          ifelse(pop_DESA$AgeGrpStart == 10, "10-14", pop_DESA$AgeGrp))
pop_DESA <- pop_DESA[, c(2:4, 7)]
colnames(pop_DESA) <- c("location", "year", "age", "pop")
setdiff(unique(data$location), intersect(unique(pop_DESA$location), unique(data$location)))
setdiff(unique(pop_DESA$location), intersect(unique(pop_DESA$location), unique(data$location)))
pop_DESA$location[which(pop_DESA$location == "Czechia")] <- "Czech Republic"
pop_DESA$pop <- ifelse(pop_DESA$location == "China", pop_DESA$pop[pop_DESA$location == "China"] + pop_DESA$pop[pop_DESA$location == "China, Hong Kong SAR"] + pop_DESA$pop[pop_DESA$location == "China, Macao SAR"], pop_DESA$pop)
sd <- setdiff(unique(pop_DESA$location), intersect(unique(pop_DESA$location), unique(data$location)))
for (i in 1:length(sd)) {
  
  pop_DESA <- subset(pop_DESA, location != sd[i])
  
}
remove(sd, i)
pop_total <- function(data) {
  
  bind <- NULL
  
  for (i in 1:length(unique(data$location))) {
    
    for (j in 1:length(unique(data$year))) {
      
      sub <- subset(data, location == unique(data$location)[i] & year == unique(data$year)[j])
      
      sub[nrow(sub)+1, ] <- c(unique(data$location)[i], unique(data$year)[j], "Total", sum(sub$pop)*1000)
      
      bind <- rbind(bind, sub)
      
    }
    
  }
  
  bind <- subset(bind, age == "Total")
  
  bind
  
}
pop <- pop_total(pop_DESA)
remove(pop_total, pop_DESA)
pop$year <- as.numeric(pop$year)
pop$pop <- as.numeric(pop$pop)
###burden
burden <- function(data, pop) {
  
  ###先贴一下gdp_sq(之后会用到)
  
  data$gdp_sq002 <- NA
  data$gdp_sq003 <- NA
  data$gdp_sq005 <- NA
  
  for (i in 1:length(unique(data$location))) {
    
    for (j in 1:length(unique(data$year))) {
      
      data$gdp_sq002[data$location == unique(data$location)[i] & data$year == unique(data$year)[j]] <- data$gdp_sq[data$location == unique(data$location)[i] & data$year == unique(data$year)[j]]/((1+0.02)^j)
      data$gdp_sq003[data$location == unique(data$location)[i] & data$year == unique(data$year)[j]] <- data$gdp_sq[data$location == unique(data$location)[i] & data$year == unique(data$year)[j]]/((1+0.03)^j)
      data$gdp_sq005[data$location == unique(data$location)[i] & data$year == unique(data$year)[j]] <- data$gdp_sq[data$location == unique(data$location)[i] & data$year == unique(data$year)[j]]/((1+0.05)^j)
      
    }
    
  }
  
  ###Economic loss
  
  data$gdp_dif <- data$gdp_cf-data$gdp_sq
  data$gdp_dif_lower <- data$gdp_cf_lower-data$gdp_sq
  data$gdp_dif_upper <- data$gdp_cf_upper-data$gdp_sq
  
  data$gdp_dif002 <- NA
  data$gdp_dif002_lower <- NA
  data$gdp_dif002_upper <- NA
  data$gdp_dif003 <- NA
  data$gdp_dif003_lower <- NA
  data$gdp_dif003_upper <- NA
  data$gdp_dif005 <- NA
  data$gdp_dif005_lower <- NA
  data$gdp_dif005_upper <- NA
  
  for (i in 1:length(unique(data$location))) {
    
    for (j in 1:length(unique(data$year))) {
      
      data$gdp_dif002[data$location == unique(data$location)[i] & data$year == unique(data$year)[j]] <- data$gdp_dif[data$location == unique(data$location)[i] & data$year == unique(data$year)[j]]/((1+0.02)^j)
      data$gdp_dif002_lower[data$location == unique(data$location)[i] & data$year == unique(data$year)[j]] <- data$gdp_dif_lower[data$location == unique(data$location)[i] & data$year == unique(data$year)[j]]/((1+0.02)^j)
      data$gdp_dif002_upper[data$location == unique(data$location)[i] & data$year == unique(data$year)[j]] <- data$gdp_dif_upper[data$location == unique(data$location)[i] & data$year == unique(data$year)[j]]/((1+0.02)^j)
      
      data$gdp_dif003[data$location == unique(data$location)[i] & data$year == unique(data$year)[j]] <- data$gdp_dif[data$location == unique(data$location)[i] & data$year == unique(data$year)[j]]/((1+0.03)^j)
      data$gdp_dif003_lower[data$location == unique(data$location)[i] & data$year == unique(data$year)[j]] <- data$gdp_dif_lower[data$location == unique(data$location)[i] & data$year == unique(data$year)[j]]/((1+0.03)^j)
      data$gdp_dif003_upper[data$location == unique(data$location)[i] & data$year == unique(data$year)[j]] <- data$gdp_dif_upper[data$location == unique(data$location)[i] & data$year == unique(data$year)[j]]/((1+0.03)^j)
      
      data$gdp_dif005[data$location == unique(data$location)[i] & data$year == unique(data$year)[j]] <- data$gdp_dif[data$location == unique(data$location)[i] & data$year == unique(data$year)[j]]/((1+0.05)^j)
      data$gdp_dif005_lower[data$location == unique(data$location)[i] & data$year == unique(data$year)[j]] <- data$gdp_dif_lower[data$location == unique(data$location)[i] & data$year == unique(data$year)[j]]/((1+0.05)^j)
      data$gdp_dif005_upper[data$location == unique(data$location)[i] & data$year == unique(data$year)[j]] <- data$gdp_dif_upper[data$location == unique(data$location)[i] & data$year == unique(data$year)[j]]/((1+0.05)^j)
      
    }
    
  }
  
  bind <- NULL
  
  for (i in 1:length(unique(data$location))) {
    
    sub <- subset(data, location == unique(data$location)[i])
    
    sub[nrow(sub)+1, ] <- c(sub[nrow(sub), 1], unique(data$location)[i], "Total", 
                            sum(sub$gdp_sq), 
                            NA, NA, NA, 
                            sum(sub$gdp_sq002), sum(sub$gdp_sq003), sum(sub$gdp_sq005), 
                            sum(sub$gdp_dif), sum(sub$gdp_dif_lower), sum(sub$gdp_dif_upper), 
                            sum(sub$gdp_dif002), sum(sub$gdp_dif002_lower), sum(sub$gdp_dif002_upper), 
                            sum(sub$gdp_dif003), sum(sub$gdp_dif003_lower), sum(sub$gdp_dif003_upper), 
                            sum(sub$gdp_dif005), sum(sub$gdp_dif005_lower), sum(sub$gdp_dif005_upper))
    
    bind <- rbind(bind, sub)
    
  }
  
  data <- subset(bind, year == "Total")
  data <- data[, -c(3, 5:7)]
  

  for (i in 3:ncol(data)) {
    
    data[, i] <- as.numeric(data[, i])
    
  }
  
  ###Proportion of total GDP in 2021–50
  
  data$gdp_dif_prop <- data$gdp_dif/data$gdp_sq
  data$gdp_dif_prop_lower <- data$gdp_dif_lower/data$gdp_sq
  data$gdp_dif_prop_upper <- data$gdp_dif_upper/data$gdp_sq
  data$gdp_dif_prop002 <- data$gdp_dif002/data$gdp_sq002
  data$gdp_dif_prop002_lower <- data$gdp_dif002_lower/data$gdp_sq002
  data$gdp_dif_prop002_upper <- data$gdp_dif002_upper/data$gdp_sq002
  data$gdp_dif_prop003 <- data$gdp_dif003/data$gdp_sq003
  data$gdp_dif_prop003_lower <- data$gdp_dif003_lower/data$gdp_sq003
  data$gdp_dif_prop003_upper <- data$gdp_dif003_upper/data$gdp_sq003
  data$gdp_dif_prop005 <- data$gdp_dif005/data$gdp_sq005
  data$gdp_dif_prop005_lower <- data$gdp_dif005_lower/data$gdp_sq005
  data$gdp_dif_prop005_upper <- data$gdp_dif005_upper/data$gdp_sq005
  
  ###Per capita loss
  
  bind <- NULL
  
  unique_pop_location <- unique(pop$location)
  
  for (i in 1:length(unique_pop_location)) {
    
    sub_pop <- subset(pop, location == unique_pop_location[i])
    
    sub_pop[nrow(sub_pop)+1, ] <- c(unique_pop_location[i], "Median", "Total", median(sub_pop$pop))
    
    bind <- rbind(bind, sub_pop)
    
  }
  
  pop <- subset(bind, year == "Median")
  pop$pop <- as.numeric(pop$pop)
  pop <- pop[, c(1, 4)]
  
  data <- merge(data, pop, by = "location")
  
  data$gdp_dif_perc <- data$gdp_dif/data$pop
  data$gdp_dif_perc_lower <- data$gdp_dif_lower/data$pop
  data$gdp_dif_perc_upper <- data$gdp_dif_upper/data$pop
  data$gdp_dif_perc002 <- data$gdp_dif002/data$pop
  data$gdp_dif_perc002_lower <- data$gdp_dif002_lower/data$pop
  data$gdp_dif_perc002_upper <- data$gdp_dif002_upper/data$pop
  data$gdp_dif_perc003 <- data$gdp_dif003/data$pop
  data$gdp_dif_perc003_lower <- data$gdp_dif003_lower/data$pop
  data$gdp_dif_perc003_upper <- data$gdp_dif003_upper/data$pop
  data$gdp_dif_perc005 <- data$gdp_dif005/data$pop
  data$gdp_dif_perc005_lower <- data$gdp_dif005_lower/data$pop
  data$gdp_dif_perc005_upper <- data$gdp_dif005_upper/data$pop
  
  data
  
}
data <- burden(data, pop)
remove(burden, pop)
setwd("D:/S/基于健康增益宏观效应模型的疾病经济成本评估/Diabetes/Imputation")
write.csv(data, "data without imputation.csv")
###为插补作准备
length(na_lfp) <- length(all_location)
length(na_edu) <- length(all_location)
length(na_gdp) <- length(all_location)
length(na_sr) <- length(all_location)
length(na_cs) <- length(all_location)
length(na_tc) <- length(all_location)
na <- data.frame(all_location, na_lfp, na_edu, na_gdp, na_sr, na_cs, na_tc)
remove(all_location, na_lfp, na_edu, na_gdp, na_sr, na_cs, na_tc)
write.csv(na, "na.csv")

####Imputation预处理------
data <- read.csv("data without imputation.csv")
data <- data[, -c(1, 4:7, 32)]
na <- read.csv("na.csv")
na <- na[, -1]
na_loc <- setdiff(na$all_location, intersect(na$all_location, unique(data$location)))
for (i in 1:length(na_loc)) {
  
  data[nrow(data)+1, 1] <- na_loc[i]
  
}
remove(i, na_loc, na)
###合并发病和死亡数据
setwd("D:/S/基于健康增益宏观效应模型的疾病经济成本评估/Diabetes/人力资本")
pop <- read.csv("Population GBD.csv")
pop <- subset(pop, sex_id == 1 & age_id == 1 & year == 1990)
pop <- pop[, c(3, 4)]
colnames(pop) <- c("location_id", "location")
setwd("D:/S/基于健康增益宏观效应模型的疾病经济成本评估/Diabetes/Imputation")
inc <- read.csv("Incidence.csv")
inc <- inc[, c(3, 14)]
colnames(inc) <- c("location_id", "inc")
inc <- merge(inc, pop, by = "location_id", all.x = T)
inc <- inc[, c(3, 2)]
inc$inc <- inc$inc*(10)^(-5) 
dea <- read.csv("Death.csv")
dea <- dea[, c(3, 14)]
colnames(dea) <- c("location_id", "dea")
dea <- merge(dea, pop, by = "location_id", all.x = T)
dea <- dea[, c(3, 2)]
dea$dea <- dea$dea*(10)^(-5) 
dis <- merge(inc, dea, by = "location")
remove(pop, inc, dea)
setdiff(unique(dis$location), intersect(unique(dis$location), unique(data$location)))
setdiff(unique(data$location), intersect(unique(dis$location), unique(data$location)))
dis$location[which(dis$location == "Czechia")] <- "Czech Republic"
dis$location[which(dis$location == "United States Virgin Islands")] <- "Virgin Islands"
data <- merge(data, dis, by = "location")
remove(dis)
###合并人均GDP数据(间接计算)
##GDP
gdp <- read.csv("gdp.csv") #2017国际美元
gdp <- gdp[, c(1, 65)]
colnames(gdp) <- c("location", "gdp_2020")
gdp <- subset(gdp, !is.na(gdp_2020)) 
setdiff(unique(data$location), intersect(unique(gdp$location), unique(data$location)))
setdiff(unique(gdp$location), intersect(unique(gdp$location), unique(data$location)))
gdp$location[which(gdp$location == "Bolivia")] <- "Bolivia (Plurinational State of)"
gdp$location[which(gdp$location == "Bahamas, The")] <- "Bahamas"
gdp$location[which(gdp$location == "Cote d'Ivoire")] <- "Côte d'Ivoire"
gdp$location[which(gdp$location == "Congo, Rep.")] <- "Congo"
gdp$location[which(gdp$location == "Congo, Dem. Rep.")] <- "Democratic Republic of the Congo"
gdp$location[which(gdp$location == "Czechia")] <- "Czech Republic"
gdp$location[which(gdp$location == "Egypt, Arab Rep.")] <- "Egypt"
gdp$location[which(gdp$location == "Gambia, The")] <- "Gambia"
gdp$location[which(gdp$location == "Iran, Islamic Rep.")] <- "Iran (Islamic Republic of)"
gdp$location[which(gdp$location == "Kyrgyz Republic")] <- "Kyrgyzstan"
gdp$location[which(gdp$location == "Lao PDR")] <- "Lao People's Democratic Republic"
gdp$location[which(gdp$location == "Micronesia, Fed. Sts.")] <- "Micronesia (Federated States of)"
gdp$location[which(gdp$location == "Moldova")] <- "Republic of Moldova"
gdp$location[which(gdp$location == "Korea, Rep.")] <- "Republic of Korea"
gdp$location[which(gdp$location == "Slovak Republic")] <- "Slovakia"
gdp$location[which(gdp$location == "St. Kitts and Nevis")] <- "Saint Kitts and Nevis"
gdp$location[which(gdp$location == "St. Lucia")] <- "Saint Lucia"
gdp$location[which(gdp$location == "St. Vincent and the Grenadines")] <- "Saint Vincent and the Grenadines"
gdp$location[which(gdp$location == "Turkiye")] <- "Türkiye"
gdp$location[which(gdp$location == "Tanzania")] <- "United Republic of Tanzania"
gdp$location[which(gdp$location == "United States")] <- "United States of America"
gdp$location[which(gdp$location == "Yemen, Rep.")] <- "Yemen"
sd <- setdiff(unique(gdp$location), intersect(unique(gdp$location), unique(data$location)))
for (i in 1:length(sd)) {
  
  gdp <- subset(gdp, location != sd[i])
  
}
remove(sd, i)
data <- merge(data, gdp, by = "location", all.x = T)
remove(gdp)
##population
setwd("D:/S/基于健康增益宏观效应模型的疾病经济成本评估/Diabetes/人力资本")
pop <- read.csv("Population DESA.csv")
pop <- subset(pop, Time == 2020)
pop <- pop[, c("Location", "PopTotal")]
colnames(pop) <- c("location", "pop")
setdiff(unique(pop$location), intersect(unique(pop$location), unique(data$location)))
setdiff(unique(data$location), intersect(unique(pop$location), unique(data$location)))
pop$location[which(pop$location == "State of Palestine")] <- "Palestine"
pop$location[which(pop$location == "Micronesia (Fed. States of)")] <- "Micronesia (Federated States of)"
pop$location[which(pop$location == "United States Virgin Islands")] <- "Virgin Islands"
pop$location[which(pop$location == "Czechia")] <- "Czech Republic"
pop$location[which(pop$location == "Dem. People's Republic of Korea")] <- "Democratic People's Republic of Korea"
pop$location[which(pop$location == "China, Taiwan Province of China")] <- "Taiwan (Province of China)"
data_pop <- function(data, pop) {
  
  bind <- NULL
  
  for (i in 1:length(data$location)) {
    
    sub <- subset(data, location == data$location[i])
    sub_pop <- subset(pop, location == data$location[i])
    
    sub$pop <- sum(sub_pop$pop)*1000
    
    bind <- rbind(bind, sub)
    
  }
  
  bind
  
}
data <- data_pop(data, pop)
remove(pop, data_pop)
##人均gdp
data$gdp_pc <- data$gdp_2020/data$pop
data <- data[, -41] #人口数据还有用
###合并期望寿命数据
setwd("D:/S/基于健康增益宏观效应模型的疾病经济成本评估/Diabetes/Imputation")
le <- read.csv("Life expectancy at birth, total.csv")
le <- le[, c(1, 65)]
colnames(le) <- c("location", "le")
setdiff(unique(le$location), intersect(unique(le$location), unique(data$location)))
setdiff(unique(data$location), intersect(unique(le$location), unique(data$location)))
le$location[which(le$location == "Bahamas, The")] <- "Bahamas"
le$location[which(le$location == "Bolivia")] <- "Bolivia (Plurinational State of)"
le$location[which(le$location == "Congo, Rep.")] <- "Congo"
le$location[which(le$location == "Cote d'Ivoire")] <- "Côte d'Ivoire"
le$location[which(le$location == "Czechia")] <- "Czech Republic"
le$location[which(le$location == "Congo, Dem. Rep.")] <- "Democratic Republic of the Congo"
le$location[which(le$location == "Korea, Dem. People's Rep.")] <- "Democratic People's Republic of Korea"
le$location[which(le$location == "Egypt, Arab Rep.")] <- "Egypt"
le$location[which(le$location == "Gambia, The")] <- "Gambia"
le$location[which(le$location == "Iran, Islamic Rep.")] <- "Iran (Islamic Republic of)"
le$location[which(le$location == "Kyrgyz Republic")] <- "Kyrgyzstan"
le$location[which(le$location == "Lao PDR")] <- "Lao People's Democratic Republic"
le$location[which(le$location == "Micronesia, Fed. Sts.")] <- "Micronesia (Federated States of)"
le$location[which(le$location == "Korea, Rep.")] <- "Republic of Korea"
le$location[which(le$location == "Moldova")] <- "Republic of Moldova"
le$location[which(le$location == "St. Kitts and Nevis")] <- "Saint Kitts and Nevis"
le$location[which(le$location == "St. Lucia")] <- "Saint Lucia"
le$location[which(le$location == "St. Vincent and the Grenadines")] <- "Saint Vincent and the Grenadines"
le$location[which(le$location == "Slovak Republic")] <- "Slovakia"
le$location[which(le$location == "Taiwan Province of China")] <- "Taiwan (Province of China)"
le$location[which(le$location == "Turkiye")] <- "Türkiye"
le$location[which(le$location == "Tanzania")] <- "United Republic of Tanzania"
le$location[which(le$location == "United States")] <- "United States of America"
le$location[which(le$location == "Venezuela, RB")] <- "Venezuela (Bolivarian Republic of)"
le$location[which(le$location == "Virgin Islands (U.S.)")] <- "Virgin Islands"
le$location[which(le$location == "Yemen, Rep.")] <- "Yemen"
data <- merge(data, le, by = "location", all.x = T)
remove(le)
write.csv(data, "data without imputation_dgl.csv")

####建立预测模型------
library(sandwich)
library(lmtest)
data <- read.csv("data without imputation_dgl.csv")
data <- data[, -1]
###建模
model <- lm(formula = gdp_dif ~ inc + dea, data = data)
model <- lm(formula = gdp_dif_prop ~ inc +dea, data = data)
summary(model)
###线性回归假设验证
par(mfrow=c(2,2)) # Change the panel layout to 2 x 2
plot(model)
par(mfrow=c(1,1)) # Change back to 1 x 1
remove(model) #满足假设

####合并最终计算需要的gdp_sq------
###预处理
data <- read.csv("data without imputation_dgl.csv")
data <- data[, -1]
gdp <- read.csv("gdp.csv") #2017国际美元
gdp <- gdp[, c(1, 60:66)]
gdp <- gather(gdp, key = "year", value = "gdp", 2:8)
colnames(gdp) <- c("location", "year", "gdp")
gdp$year <- as.numeric(gsub("X", "", gdp$year))
gdp <- subset(gdp, !is.na(gdp))
setdiff(unique(data$location), intersect(unique(gdp$location), unique(data$location)))
setdiff(unique(gdp$location), intersect(unique(gdp$location), unique(data$location)))
gdp$location[which(gdp$location == "Bolivia")] <- "Bolivia (Plurinational State of)"
gdp$location[which(gdp$location == "Bahamas, The")] <- "Bahamas"
gdp$location[which(gdp$location == "Cote d'Ivoire")] <- "Côte d'Ivoire"
gdp$location[which(gdp$location == "Congo, Rep.")] <- "Congo"
gdp$location[which(gdp$location == "Congo, Dem. Rep.")] <- "Democratic Republic of the Congo"
gdp$location[which(gdp$location == "Czechia")] <- "Czech Republic"
gdp$location[which(gdp$location == "Egypt, Arab Rep.")] <- "Egypt"
gdp$location[which(gdp$location == "Gambia, The")] <- "Gambia"
gdp$location[which(gdp$location == "Iran, Islamic Rep.")] <- "Iran (Islamic Republic of)"
gdp$location[which(gdp$location == "Kyrgyz Republic")] <- "Kyrgyzstan"
gdp$location[which(gdp$location == "Lao PDR")] <- "Lao People's Democratic Republic"
gdp$location[which(gdp$location == "Micronesia, Fed. Sts.")] <- "Micronesia (Federated States of)"
gdp$location[which(gdp$location == "Moldova")] <- "Republic of Moldova"
gdp$location[which(gdp$location == "Korea, Rep.")] <- "Republic of Korea"
gdp$location[which(gdp$location == "Slovak Republic")] <- "Slovakia"
gdp$location[which(gdp$location == "St. Kitts and Nevis")] <- "Saint Kitts and Nevis"
gdp$location[which(gdp$location == "St. Lucia")] <- "Saint Lucia"
gdp$location[which(gdp$location == "St. Vincent and the Grenadines")] <- "Saint Vincent and the Grenadines"
gdp$location[which(gdp$location == "Turkiye")] <- "Türkiye"
gdp$location[which(gdp$location == "Tanzania")] <- "United Republic of Tanzania"
gdp$location[which(gdp$location == "United States")] <- "United States of America"
gdp$location[which(gdp$location == "Yemen, Rep.")] <- "Yemen"
sd <- setdiff(unique(gdp$location), intersect(unique(gdp$location), unique(data$location)))
for (i in 1:length(sd)) {
  
  gdp <- subset(gdp, location != sd[i])
  
}
remove(sd, i)
###gdp_predict
setwd("D:/S/基于健康增益宏观效应模型的疾病经济成本评估/Diabetes/实物资本及总产出")
gdp_percent_change <- read.csv("GDP percent change.csv")
gdp_percent_change <- gdp_percent_change[, c(1, 6:34)]
gdp_percent_change <- gather(gdp_percent_change, key = "year", value = "gdp_percent_change", 2:30)
gdp_percent_change$year <- as.numeric(gsub("X", "", gdp_percent_change$year))
gdp_percent_change$gdp_percent_change[which(gdp_percent_change$gdp_percent_change == "n/a")] <- NA
gdp_percent_change$gdp_percent_change <- as.numeric(gdp_percent_change$gdp_percent_change)
colnames(gdp_percent_change) <- c("location", "year", "gdp_percent_change")
gdp_percent_change <- subset(gdp_percent_change, year >= 2022 & year <= 2028)
##与gdp数据location保持一致
setdiff(unique(gdp_percent_change$location), intersect(unique(gdp$location), unique(gdp_percent_change$location)))
setdiff(unique(gdp$location), intersect(unique(gdp$location), unique(gdp_percent_change$location)))
gdp_percent_change$location[which(gdp_percent_change$location == "Bolivia")] <- "Bolivia (Plurinational State of)"
gdp_percent_change$location[which(gdp_percent_change$location == "The Bahamas")] <- "Bahamas"
gdp_percent_change$location[which(gdp_percent_change$location == "C?te d'Ivoire")] <- "Côte d'Ivoire"
gdp_percent_change$location[which(gdp_percent_change$location == "Republic of Congo")] <- "Congo"
gdp_percent_change$location[which(gdp_percent_change$location == "Korea")] <- "Republic of Korea"
gdp_percent_change$location[which(gdp_percent_change$location == "The Gambia")] <- "Gambia"
gdp_percent_change$location[which(gdp_percent_change$location == "Islamic Republic of Iran")] <- "Iran (Islamic Republic of)"
gdp_percent_change$location[which(gdp_percent_change$location == "Kyrgyz Republic")] <- "Kyrgyzstan"
gdp_percent_change$location[which(gdp_percent_change$location == "Lao P.D.R.")] <- "Lao People's Democratic Republic"
gdp_percent_change$location[which(gdp_percent_change$location == "Micronesia")] <- "Micronesia (Federated States of)"
gdp_percent_change$location[which(gdp_percent_change$location == "Moldova")] <- "Republic of Moldova"
gdp_percent_change$location[which(gdp_percent_change$location == "Russia")] <- "Russian Federation"
gdp_percent_change$location[which(gdp_percent_change$location == "S?o Tom? and Pr?ncipe")] <- "Sao Tome and Principe"
gdp_percent_change$location[which(gdp_percent_change$location == "Slovak Republic")] <- "Slovakia"
gdp_percent_change$location[which(gdp_percent_change$location == "St. Kitts and Nevis")] <- "Saint Kitts and Nevis"
gdp_percent_change$location[which(gdp_percent_change$location == "St. Lucia")] <- "Saint Lucia"
gdp_percent_change$location[which(gdp_percent_change$location == "St. Vincent and the Grenadines")] <- "Saint Vincent and the Grenadines"
gdp_percent_change$location[which(gdp_percent_change$location == "T?rkiye")] <- "Türkiye"
gdp_percent_change$location[which(gdp_percent_change$location == "United States")] <- "United States of America"
gdp_percent_change$location[which(gdp_percent_change$location == "Tanzania")] <- "United Republic of Tanzania"
gdp_percent_change$location[which(gdp_percent_change$location == "Vietnam")] <- "Viet Nam"
gdp_percent_change$location[which(gdp_percent_change$location == "Taiwan Province of China")] <- "Taiwan (Province of China)"
gdp_percent_change$location[which(gdp_percent_change$location == "Venezuela")] <- "Venezuela (Bolivarian Republic of)"
setdiff(unique(gdp_percent_change$location), intersect(unique(gdp$location), unique(gdp_percent_change$location)))
setdiff(unique(gdp$location), intersect(unique(gdp$location), unique(gdp_percent_change$location)))
gdp_percent_change$location[which(gdp_percent_change$location == "Kosovo")] <- "Bermuda" 
gdp_percent_change$gdp_percent_change[which(gdp_percent_change$location == "Bermuda")] <- NA
sd <- setdiff(unique(gdp_percent_change$location), intersect(unique(gdp$location), unique(gdp_percent_change$location)))
for (i in 1:length(sd)) {
  
  gdp_percent_change <- subset(gdp_percent_change, location != sd[i])
  
}
remove(sd, i)
na_gdp_pc <- subset(gdp_percent_change, is.na(gdp_percent_change))
na_gdp_pc <- unique(na_gdp_pc$location)
for (i in 2022:2028) {
  
  sub <- subset(gdp_percent_change, year == i)
  gdp_percent_change$gdp_percent_change[gdp_percent_change$year == i & (gdp_percent_change$location == "Afghanistan" | gdp_percent_change$location == "Bermuda" | gdp_percent_change$location == "Lebanon" | gdp_percent_change$location == "Sri Lanka" | gdp_percent_change$location == "Venezuela (Bolivarian Republic of)")] <- mean(sub$gdp_percent_change, na.rm = T)
  
}
remove(i, sub, na_gdp_pc)
##predict
unique_gdp_location <- unique(gdp$location) 
gdp_predict <- function(gdp, gdp_percent_change) {
  
  ###计算2022-2050gdp
  
  bind <- NULL
  
  for (i in 1:length(unique_gdp_location)) {
    
    sub <- subset(gdp, location == unique_gdp_location[i])

    
    for (j in 2022:2028) {
      
      sub[nrow(sub)+1, ] <- c(unique_gdp_location[i], j, sub$gdp[sub$year == j-1])
      sub$gdp[sub$year == j] <- as.numeric(sub$gdp[sub$year == j])*(1+gdp_percent_change$gdp_percent_change[gdp_percent_change$location == unique_gdp_location[i] & gdp_percent_change$year == j]/100)
      
    }
    
    bind <- rbind(bind, sub)
    
  }
  
  gdp <- bind
  bind <- NULL
  
  for (i in 1:length(unique_gdp_location)) {
    
    sub <- subset(gdp, location == unique_gdp_location[i])
    
    gdp_r <- (as.numeric(sub$gdp[sub$year == 2019])/as.numeric(sub$gdp[sub$year == 2015]))^(1/4)-1
    
    for (j in 2029:2050) {
      
      sub[nrow(sub)+1, ] <- c(unique_gdp_location[i], j, sub$gdp[sub$year == j-1])
      sub$gdp[sub$year == j] <- as.numeric(sub$gdp[sub$year == j])*(1+gdp_r)
      
    }
    
    bind <- rbind(bind, sub)
    
  }
  
  ###贴现
  
  gdp <- subset(bind, year >= 2021)
  colnames(gdp) <- c("location", "year", "gdp_sq")
  
  gdp$gdp_sq <- as.numeric(gdp$gdp_sq)
  
  gdp$gdp_sq002 <- NA
  gdp$gdp_sq003 <- NA
  gdp$gdp_sq005 <- NA
  
  for (i in 1:length(unique_gdp_location)) {
    
    for (j in 2021:2050) {
      
      gdp$gdp_sq002[gdp$location == unique_gdp_location[i] & gdp$year == j] <- gdp$gdp_sq[gdp$location == unique_gdp_location[i] & gdp$year == j]/((1+0.02)^(j-2020))
      gdp$gdp_sq003[gdp$location == unique_gdp_location[i] & gdp$year == j] <- gdp$gdp_sq[gdp$location == unique_gdp_location[i] & gdp$year == j]/((1+0.03)^(j-2020))
      gdp$gdp_sq005[gdp$location == unique_gdp_location[i] & gdp$year == j] <- gdp$gdp_sq[gdp$location == unique_gdp_location[i] & gdp$year == j]/((1+0.05)^(j-2020))
      
    }
    
  }
  
  ###求和
  
  bind <- NULL
  
  for (i in 1:length(unique_gdp_location)) {
    
    sub <- subset(gdp, location == unique_gdp_location[i])
    sub[nrow(sub)+1, ] <- c(unique_gdp_location[i], "Total", sum(sub$gdp_sq), sum(sub$gdp_sq002), sum(sub$gdp_sq003), sum(sub$gdp_sq005))
    sub <- subset(sub, year == "Total")
    
    bind <- rbind(bind, sub)
    
  }
  
  bind
  
}
gdp <- gdp_predict(gdp, gdp_percent_change)
remove(gdp_predict, gdp_percent_change, unique_gdp_location)
gdp <- gdp[, -2]
data <- merge(data, gdp, by = "location", all.x = T)
remove(gdp)

####合并最终计算需要的pop------
setwd("D:/S/基于健康增益宏观效应模型的疾病经济成本评估/Diabetes/人力资本")
pop_DESA <- read.csv("Population DESA.csv")
pop_DESA <- pop_DESA[, c(8, 10, 13, 15:17, 20)]
pop_DESA <- subset(pop_DESA, Time >= 2021 & Time <= 2050)
pop_DESA <- subset(pop_DESA, LocTypeName == "Country/Area" | LocTypeName == "World")
pop_DESA$AgeGrp <- ifelse(pop_DESA$AgeGrpStart== 5, "5-9", 
                          ifelse(pop_DESA$AgeGrpStart == 10, "10-14", pop_DESA$AgeGrp))
pop_DESA <- pop_DESA[, c(2:4, 7)]
colnames(pop_DESA) <- c("location", "year", "age", "pop_m")
setdiff(unique(data$location), intersect(unique(pop_DESA$location), unique(data$location)))
setdiff(unique(pop_DESA$location), intersect(unique(pop_DESA$location), unique(data$location)))
pop_DESA$location[which(pop_DESA$location == "Czechia")] <- "Czech Republic"
pop_DESA$location[which(pop_DESA$location == "Dem. People's Republic of Korea")] <- "Democratic People's Republic of Korea"
pop_DESA$location[which(pop_DESA$location == "Micronesia (Fed. States of)")] <- "Micronesia (Federated States of)"
pop_DESA$location[which(pop_DESA$location == "State of Palestine")] <- "Palestine"
pop_DESA$location[which(pop_DESA$location == "China, Taiwan Province of China")] <- "Taiwan (Province of China)"
pop_DESA$location[which(pop_DESA$location == "United States Virgin Islands")] <- "Virgin Islands"
pop_DESA$pop_m <- ifelse(pop_DESA$location == "China", pop_DESA$pop_m[pop_DESA$location == "China"] + pop_DESA$pop_m[pop_DESA$location == "China, Hong Kong SAR"] + pop_DESA$pop_m[pop_DESA$location == "China, Macao SAR"], pop_DESA$pop_m)
sd <- setdiff(unique(pop_DESA$location), intersect(unique(pop_DESA$location), unique(data$location)))
for (i in 1:length(sd)) {
  
  pop_DESA <- subset(pop_DESA, location != sd[i])
  
}
remove(sd, i)
pop_el <- function(data) {
  
  bind <- NULL
  
  for (i in 1:length(unique(data$location))) {
    
    for (j in 1:length(unique(data$year))) {
      
      sub <- subset(data, location == unique(data$location)[i] & year == unique(data$year)[j])
      
      sub[nrow(sub)+1, ] <- c(unique(data$location)[i], unique(data$year)[j], "Total", sum(sub$pop_m)*1000)
      
      bind <- rbind(bind, sub)
      
    }
    
  }
  
  data <- subset(bind, age == "Total")
  bind <- NULL
  
  for (i in 1:length(unique(data$location))) {
    
    sub <- subset(data, location == unique(data$location)[i])
    
    sub[nrow(sub)+1, ] <- c(unique(data$location)[i], "Mean", "Total", mean(as.numeric(sub$pop_m)))
    
    bind <- rbind(bind, sub)
    
  }
  
  data <- subset(bind, year == "Mean")
  
  data
  
}
pop <- pop_el(pop_DESA)
pop <- pop[, -c(2, 3)]
data <- merge(data, pop, by = "location", all.x = T)
remove(pop_el, pop_DESA, pop)
setwd("D:/S/基于健康增益宏观效应模型的疾病经济成本评估/Diabetes/Imputation")
write.csv(data, "data without imputation_dgl_g_p.csv")

####Imputation------
data <- read.csv("data without imputation_dgl_g_p.csv")
data <- data[, -1]
###Imputation
data_imp <- function(data) {
  
  data$ni <- ifelse(is.na(data$gdp_dif), 1, 0)
  data$sd <- (data$gdp_dif_prop_upper-data$gdp_dif_prop_lower)/2/1.96
  data$sd002 <- (data$gdp_dif_prop002_upper-data$gdp_dif_prop002_lower)/2/1.96
  data$sd003 <- (data$gdp_dif_prop003_upper-data$gdp_dif_prop003_lower)/2/1.96
  data$sd005 <- (data$gdp_dif_prop005_upper-data$gdp_dif_prop005_lower)/2/1.96
  
  ###Proportion of total GDP in 2021–50
  
  model <- lm(formula = gdp_dif_prop ~ inc + dea, data = data)
  data$gdp_dif_prop <- ifelse(data$ni == 1, model$coefficients[2]*data$inc+model$coefficients[3]*data$dea+model$coefficients[1], data$gdp_dif_prop)
  data$gdp_dif_prop_lower <- ifelse(data$ni == 1, data$gdp_dif_prop-1.96*mean(data$sd, na.rm = T), data$gdp_dif_prop_lower)
  data$gdp_dif_prop_upper <- ifelse(data$ni == 1, data$gdp_dif_prop+1.96*mean(data$sd, na.rm = T), data$gdp_dif_prop_upper)
  
  model <- lm(formula = gdp_dif_prop002 ~ inc + dea, data = data)
  data$gdp_dif_prop002 <- ifelse(data$ni == 1, model$coefficients[2]*data$inc+model$coefficients[3]*data$dea+model$coefficients[1], data$gdp_dif_prop002)
  data$gdp_dif_prop002_lower <- ifelse(data$ni == 1, data$gdp_dif_prop002-1.96*mean(data$sd002, na.rm = T), data$gdp_dif_prop002_lower)
  data$gdp_dif_prop002_upper <- ifelse(data$ni == 1, data$gdp_dif_prop002+1.96*mean(data$sd002, na.rm = T), data$gdp_dif_prop002_upper)
  
  model <- lm(formula = gdp_dif_prop003 ~ inc + dea, data = data)
  data$gdp_dif_prop003 <- ifelse(data$ni == 1, model$coefficients[2]*data$inc+model$coefficients[3]*data$dea+model$coefficients[1], data$gdp_dif_prop003)
  data$gdp_dif_prop003_lower <- ifelse(data$ni == 1, data$gdp_dif_prop003-1.96*mean(data$sd003, na.rm = T), data$gdp_dif_prop003_lower)
  data$gdp_dif_prop003_upper <- ifelse(data$ni == 1, data$gdp_dif_prop003+1.96*mean(data$sd003, na.rm = T), data$gdp_dif_prop003_upper)
  
  model <- lm(formula = gdp_dif_prop005 ~ inc + dea, data = data)
  data$gdp_dif_prop005 <- ifelse(data$ni == 1, model$coefficients[2]*data$inc+model$coefficients[3]*data$dea+model$coefficients[1], data$gdp_dif_prop005)
  data$gdp_dif_prop005_lower <- ifelse(data$ni == 1, data$gdp_dif_prop005-1.96*mean(data$sd005, na.rm = T), data$gdp_dif_prop005_lower)
  data$gdp_dif_prop005_upper <- ifelse(data$ni == 1, data$gdp_dif_prop005+1.96*mean(data$sd005, na.rm = T), data$gdp_dif_prop005_upper)
  
  ###Economic loss
  
  data$gdp_dif <- ifelse(data$ni == 1, data$gdp_dif_prop*data$gdp_sq, data$gdp_dif)
  data$gdp_dif_lower <- ifelse(data$ni == 1, data$gdp_dif_prop_lower*data$gdp_sq, data$gdp_dif_lower)
  data$gdp_dif_upper <- ifelse(data$ni == 1, data$gdp_dif_prop_upper*data$gdp_sq, data$gdp_dif_upper)
  
  data$gdp_dif002 <- ifelse(data$ni == 1, data$gdp_dif_prop002*data$gdp_sq002, data$gdp_dif002)
  data$gdp_dif002_lower <- ifelse(data$ni == 1, data$gdp_dif_prop002_lower*data$gdp_sq002, data$gdp_dif002_lower)
  data$gdp_dif002_upper <- ifelse(data$ni == 1, data$gdp_dif_prop002_upper*data$gdp_sq002, data$gdp_dif002_upper)
  
  data$gdp_dif003 <- ifelse(data$ni == 1, data$gdp_dif_prop003*data$gdp_sq003, data$gdp_dif003)
  data$gdp_dif003_lower <- ifelse(data$ni == 1, data$gdp_dif_prop003_lower*data$gdp_sq003, data$gdp_dif003_lower)
  data$gdp_dif003_upper <- ifelse(data$ni == 1, data$gdp_dif_prop003_upper*data$gdp_sq003, data$gdp_dif003_upper)
  
  data$gdp_dif005 <- ifelse(data$ni == 1, data$gdp_dif_prop005*data$gdp_sq005, data$gdp_dif005)
  data$gdp_dif005_lower <- ifelse(data$ni == 1, data$gdp_dif_prop005_lower*data$gdp_sq005, data$gdp_dif005_lower)
  data$gdp_dif005_upper <- ifelse(data$ni == 1, data$gdp_dif_prop005_upper*data$gdp_sq005, data$gdp_dif005_upper)
  
  ###Per capita loss
  
  data$gdp_dif_perc <- ifelse(data$ni == 1, data$gdp_dif/as.numeric(data$pop_m), data$gdp_dif_perc)
  data$gdp_dif_perc_lower <- ifelse(data$ni == 1, data$gdp_dif_lower/as.numeric(data$pop_m), data$gdp_dif_perc_lower)
  data$gdp_dif_perc_upper <- ifelse(data$ni == 1, data$gdp_dif_upper/as.numeric(data$pop_m), data$gdp_dif_perc_upper)
  
  data$gdp_dif_perc002 <- ifelse(data$ni == 1, data$gdp_dif002/as.numeric(data$pop_m), data$gdp_dif_perc002)
  data$gdp_dif_perc002_lower <- ifelse(data$ni == 1, data$gdp_dif002_lower/as.numeric(data$pop_m), data$gdp_dif_perc002_lower)
  data$gdp_dif_perc002_upper <- ifelse(data$ni == 1, data$gdp_dif002_upper/as.numeric(data$pop_m), data$gdp_dif_perc002_upper)
  
  data$gdp_dif_perc003 <- ifelse(data$ni == 1, data$gdp_dif003/as.numeric(data$pop_m), data$gdp_dif_perc003)
  data$gdp_dif_perc003_lower <- ifelse(data$ni == 1, data$gdp_dif003_lower/as.numeric(data$pop_m), data$gdp_dif_perc003_lower)
  data$gdp_dif_perc003_upper <- ifelse(data$ni == 1, data$gdp_dif003_upper/as.numeric(data$pop_m), data$gdp_dif_perc003_upper)
  
  data$gdp_dif_perc005 <- ifelse(data$ni == 1, data$gdp_dif005/as.numeric(data$pop_m), data$gdp_dif_perc005)
  data$gdp_dif_perc005_lower <- ifelse(data$ni == 1, data$gdp_dif005_lower/as.numeric(data$pop_m), data$gdp_dif_perc005_lower)
  data$gdp_dif_perc005_upper <- ifelse(data$ni == 1, data$gdp_dif005_upper/as.numeric(data$pop_m), data$gdp_dif_perc005_upper)
  
  data
  
}
data <- data_imp(data)
remove(data_imp)
data <- data[, -c(50:53)]
write.csv(data, "data.csv")

