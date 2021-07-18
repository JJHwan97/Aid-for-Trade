library("OECD")
library("dplyr")
library("tidyr")
library("ggplot")

setwd("E:/Aft")

temp = list.files(pattern="*.txt")
# myfiles = lapply(temp, read.delim)

library(readr)
for (i in 1:length(temp)){
a <- read_delim(temp[i], "|", escape_double = FALSE, trim_ws = TRUE)
assign(paste0("aft", i), a)
}

for (i in 1:19){
    sample <- paste0("aft", i) %>% get()
    aft_sector <- sample[,which(colnames(sample)=="PurposeCode")]  %>% as.data.frame() 
    aft_sector <- aft_sector[,1] %>% as.numeric()
    aft_sector <- aft_sector %in% c(21010,21020,21030,21040,21050,21061,21081,22010,22020,22030,22040,23110,23181,23182,23183,
        23210,23220,23230,23231,23232,23240,23250,23260,23270,23310,23320,23330,23340,23350,23360,
        23410,23510,23610,23620,23630,23631,23640,23641,23642) %>% which(.==TRUE)
    sample.sector <- sample[aft_sector,]
    assign(paste0("Aft.sector",i), sample.sector)}

Aft.total.sector <- rbind(get('Aft.sector1'),get('Aft.sector2'),get('Aft.sector3'),get('Aft.sector4'),get('Aft.sector5'),get('Aft.sector6'),
                   get('Aft.sector7'),get('Aft.sector8'),get('Aft.sector9'),get('Aft.sector10'),get('Aft.sector11'),get('Aft.sector12'),
                   get('Aft.sector13'),get('Aft.sector14'),get('Aft.sector15'),get('Aft.sector16'),get('Aft.sector17'),get('Aft.sector18'),get('Aft.sector19'))

Aft.sector.graph <- Aft.total.sector %>% colnames() %in% c("Year", "DonorCode", "DonorName", "AgencyCode", "AgencyName", "Bi_Multi", "RegionName",
                                                          "RecipientCode", "RecipientName", "FlowCode", "FlowName", 
                                                          "CurrencyCode", "PurposeCode", "PurposeName", "USD_Received_Defl") %>% which(.==TRUE) %>% Aft.total.sector[,.]



Aft.sector.graph.summary <- Aft.sector.graph %>% 
  group_by(RecipientName, Year) %>% 
  summarise(Received = sum(USD_Received_Defl, na.rm=T)) %>% 
  as.data.frame()

Aft.sector.graph.summary$Year <- Aft.sector.graph.summary$Year %>% as.integer()

# Aft.sector.multi.graph.summary <- Aft.sector.graph %>% 
#   group_by(RecipientName, Year, Bi_Multi) %>% 
#   summarise(Received = sum(USD_Received_Defl, na.rm=T)) %>% 
#   as.data.frame()
# 
# Aft.sector.multi.graph.summary$Year <- Aft.sector.multi.graph.summary$Year %>% as.integer()
# Aft.sector.multi.graph.summary$Bi_Multi <- Aft.sector.multi.graph.summary$Bi_Multi %>% as.character()
# 
# Aft.sector.multi.graph.summary <- Aft.sector.graph %>% 
#   group_by(RegionName, Year) %>% 
#   summarise(Received = sum(USD_Received_Defl, na.rm=T)) %>% 
#   as.data.frame()
# 
# Aft.sector.multi.graph.summary$Year <- Aft.sector.multi.graph.summary$Year %>% 
#   as.integer()
# 
Aft.sector.Full <- Aft.sector.graph %>% filter(CurrencyCode == 302) %>%
  group_by(RecipientName, Year) %>%
  summarise(Received = sum(USD_Received_Defl, na.rm=T)) %>%
  as.data.frame()
# 
Aft.sector.Bi <- Aft.sector.graph %>% filter(CurrencyCode == 302 & Bi_Multi == 1) %>%
  group_by(RecipientName, Year) %>%
  summarise(Received = sum(USD_Received_Defl, na.rm=T)) %>%
  as.data.frame()
# 
Aft.sector.Bi.country <- Aft.sector.graph %>% filter(CurrencyCode == 302 & DonorCode != 104 & DonorCode != 811 & DonorCode <900) %>%
  group_by(RecipientName, Year) %>%
  summarise(Received = sum(USD_Received_Defl, na.rm=T)) %>%
  as.data.frame()
# 
# Aft.sector.multi.graph.summary$Year <- Aft.sector.multi.graph.summary$Year %>% as.integer()

# Aft.sector.graph.summary %>% filter(RecipientName == "India" | RecipientName == "Bangladesh" | RecipientName == "Viet Nam" | RecipientName == "Argentina"| RecipientName == "Chile") %>%
#   ggplot(.,aes(x=Year, y=Received, group=RecipientName, colour=RecipientName)) +
#   geom_line(size=2.0) +
#   theme_jhp()
# 
# Aft.sector.multi.graph.summary %>% filter(RecipientName == "Zimbabwe") %>%
#   ggplot(.,aes(x=Year, y=Received, group=Bi_Multi, colour=Bi_Multi)) +
#   geom_line(size=2.0) +
#   theme_jhp()
# 
# Aft.sector.multi.graph.summary %>%
#   ggplot(.,aes(x=Year, y=Received, group=RegionName, colour=RegionName)) +
#   geom_line(size=2.0) +
#   theme_jhp()

# Aft.name <- Aft.sector.graph[,c("RecipientCode", "RecipientName")] %>% unique

# Aft.sector.graph.summary$ISO3N <- Aft.sector.graph.summary$RecipientCode %>% countrycode(origin = "imf", destination = "iso3n")

# Aft.name <- Aft.sector.graph$RecipientName %>% unique() %>% as.data.frame()
# write.table(Aft.name, "Aft.name.csv", row.names = FALSE, quote= FALSE)

# write.csv(IMF.name, "IMF.name.csv")



# 
# 
# dataset_list <- get_datasets()
# search_dataset("Creditor Reporting System", data = dataset_list)
# 
# dataset <- "CRS1"
# 
# dstruc <- get_data_structure(dataset)
# str(dstruc, max.level = 1)
# 
# aft_sector <- dstruc$SECTOR[,1]  %in% 
#   c(21010,21020,21030,21040,21050,21061,21081,22010,22020,22030,22040,23110,23181,23182,23183,
#     23210,23220,23230,23231,23232,23240,23250,23260,23270,23310,23320,23330,23340,23350,23360,
#     23410,23510,23610,23620,23630,23631,23640,23641,23642) %>% which(.==TRUE)
# 
# sector <- c(21010,21020,21030,21040,21050,21061,21081,22010,22020,22030,22040,23110,23181,23182,23183,
#   23210,23220,23230,23231,23232,23240,23250,23260,23270,23310,23320,23330,23340,23350,23360,
#   23410,23510,23610,23620,23630,23631,23640,23641,23642)
# 
# dstruc$SECTOR[aft_sector,]
# 
# filterlist = list(a,b,c,sector)
# 
# CRS <- get_dataset(dataset = dataset)
# 
# dstruc <- get_data_structure(dataset)
# b<-unique(dstruc$RECIPIENT)[1]
# b<-c(b[,names(b)])
# c<-unique(dstruc$DONOR)[1]
# c<-c(c[,names(c)])  
# 
# for (j in 1:length(sector)){
#   TYPE <- sector[j]
#   df <- get_dataset(dataset = dataset, filter = list(b,c,TYPE))
#   assign(paste0("aft",dataset,TYPE),df)
# }
# 
# df <- get_dataset(dataset = dataset)
Aft_name <- read_csv("E:/AfT/Aft.name.csv")
Aft_name <- as.data.frame(Aft_name)

Export_diversification <- read_csv("E:/AfT/Export diversification.csv")

Ex.div <- Export_diversification %>% filter(`Indicator Code` == "total_theil")

IMF.name <- Ex.div[,c("Country Name","Country Code")] %>% unique() %>% as.data.frame()

Ex.div.piv <- pivot_longer(Ex.div, cols = "1962":"2014", 
             names_to = "Year", values_to = "Val")

Ex.div.piv <- Ex.div.piv %>% as.data.frame()

Ex.div.piv$Year <- Ex.div.piv$Year %>% as.integer()

# Ex.div.piv %>% filter(`Country Name` == "China, P.R.: Mainland" | `Country Name` == "Ghana" | `Country Name` == "Korea, Republic of") %>%
#   ggplot(.,aes(x=Year, y=Val, group=`Country Name`, colour=`Country Name`)) +
#   geom_line(size=2.0) +
#   theme_jhp()

Aft_name[,3] = Aft_name[,3] %>% as.numeric() 
Aft_name[Aft_name == "#N/A"] <- NA

colnames(Aft.sector.graph.summary)[1] <- colnames(Aft_name)[1]
Aft.sector.graph.summary <- left_join(Aft.sector.graph.summary, Aft_name)

colnames(Aft.sector.Full)[1] <- colnames(Aft_name)[1]
Aft.sector.Full <- left_join(Aft.sector.Full, Aft_name)

colnames(Aft.sector.Bi)[1] <- colnames(Aft_name)[1]
Aft.sector.Bi <- left_join(Aft.sector.Bi, Aft_name)

colnames(Aft.sector.Bi.country)[1] <- colnames(Aft_name)[1]
Aft.sector.Bi.country <- left_join(Aft.sector.Bi.country, Aft_name)

colnames(Ex.div.piv)[2]<-colnames(Aft_name)[3]
Ex.div.piv <-left_join(Ex.div.piv, Aft_name)

# dat_argi[,length(dat_argi)+1] <- match(dat_argi[,2][[1]], Aft_name[,4][[1]]) %>% Aft_name[.,5]
####################### Saved as ISO
colnames(Aft.sector.graph.summary)[which((colnames(Aft.sector.graph.summary) == "Received"))] <- "aft.recevied"
colnames(Aft.sector.Full)[which((colnames(Aft.sector.Full) == "Received"))] <- "aft.recevied.full"
colnames(Aft.sector.Bi)[which((colnames(Aft.sector.Bi) == "Received"))] <- "aft.recevied.bi"
colnames(Aft.sector.Bi.country)[which((colnames(Aft.sector.Bi.country) == "Received"))] <- "aft.recevied.bi.country"

colnames(Ex.div.piv)[which((colnames(Ex.div.piv) == "Val"))] <- "export.div"
# colnames(dat_argi)[which((colnames(dat_argi) == "NV.AGR.TOTL.ZS"))] <- "agri.value.added"
# colnames(dat_argi)[which((colnames(dat_argi) == "date"))] <- "Year"

# agri.merge <- dat_argi[which(colnames(dat_argi)  %in%  c("Year", "ISO3C", "country", "agri.value.added"))]
export.merge <- Ex.div.piv[,c("Year", "ISO3N", "export.div")]
aft.merge <- Aft.sector.graph.summary[,c("Year", "ISO3N", "aft.recevied")]
aft.merge.bi <- Aft.sector.Bi[,c("Year", "ISO3N", "aft.recevied.bi")]
aft.merge.multi <- Aft.sector.Full[,c("Year", "ISO3N", "aft.recevied.full")]
aft.merge.bi.country <- Aft.sector.Bi.country[,c("Year", "ISO3N", "aft.recevied.bi.country")]

aft.merge <- aft.merge[-(which(is.na(aft.merge["ISO3N"]))),]
aft.merge.bi <- aft.merge.bi[-(which(is.na(aft.merge.bi["ISO3N"]))),]
aft.merge.multi <- aft.merge.multi[-(which(is.na(aft.merge.multi["ISO3N"]))),]
aft.merge.bi.country <- aft.merge.bi.country[-(which(is.na(aft.merge.bi.country["ISO3N"]))),]

export.merge$ISO3N <- export.merge$ISO3N %>% as.numeric()
export.merge <- drop_na(export.merge)
aft.merge$ISO3N <- as.numeric(aft.merge$ISO3N)
# a <- left_join(aft.merge, agri.merge)
##################cowipe

# load("E:/AfT/IPE/dataverse_files/3. Graham_Tucker_IPE_v4.rdata")
# cow_country <- ipe_v4[,c("ccode","ifs")] %>% unique()
AUT <- read.csv("E:/AfT/personalism.csv")
AUT$ISO3N <- AUT$cowcode %>% countrycode(origin = "cown", destination = "iso3n")
colnames(AUT)[which(colnames(AUT)=="year")] <- "Year"
AUT <- AUT[-(which(is.na(AUT["ISO3N"]))),]

AUT.merge <- AUT[,c("Year","cowcode","oilpc","oilpcl","allgwf_party", "allgwf_military", "allgwf_monarchy", "allgwf_personal", "allgwf_nonautocracy","ISO3N")]

library(WDI)

#GDP per cap
gdppc <- WDI(indicator = "NY.GDP.PCAP.CD")
gdppc$ISO3N <-gdppc$iso2c %>% countrycode(origin = "iso2c", destination = "iso3n")
gdppc <- drop_na(gdppc)
#Export of GDP
export <- WDI(indicator = "NE.EXP.GNFS.ZS")
export$ISO3N <-export$iso2c %>% countrycode(origin = "iso2c", destination = "iso3n")
export <- drop_na(export)
#FDI
fdi <- WDI(indicator = "BX.KLT.DINV.WD.GD.ZS")
fdi$ISO3N <-fdi$iso2c %>% countrycode(origin = "iso2c", destination = "iso3n")
fdi <- drop_na(fdi)
#natural
natrual <- WDI(indicator = "NY.GDP.TOTL.RT.ZS")
natrual$ISO3N <-natrual$iso2c %>% countrycode(origin = "iso2c", destination = "iso3n")
natrual <- drop_na(natrual)
#inflation
inflation <- WDI(indicator = "FP.CPI.TOTL.ZG")
inflation$ISO3N <-inflation$iso2c %>% countrycode(origin = "iso2c", destination = "iso3n")
inflation <- drop_na(inflation)
#arms
arms <- WDI(indicator = "MS.MIL.XPND.GD.ZS")
arms$ISO3N <-arms$iso2c %>% countrycode(origin = "iso2c", destination = "iso3n")
arms <- drop_na(arms)
#population
population <- WDI(indicator = "SP.POP.TOTL")
population$ISO3N <-population$iso2c %>% countrycode(origin = "iso2c", destination = "iso3n")
population <- drop_na(population)
#Balance
balance <- WDI(indicator = "BN.CAB.XOKA.GD.ZS")
balance$ISO3N <-balance$iso2c %>% countrycode(origin = "iso2c", destination = "iso3n")
balance <- drop_na(balance)
# GDP Growht
growth <-  WDI(indicator = "NY.GDP.MKTP.KD.ZG")
growth$ISO3N <-growth$iso2c %>% countrycode(origin = "iso2c", destination = "iso3n")
growth <- drop_na(growth)

worldbank <- full_join(gdppc, export)
worldbank <- full_join(worldbank, fdi)
worldbank <- full_join(worldbank, natrual)
worldbank <- full_join(worldbank , inflation)
worldbank <- full_join(worldbank, arms)
worldbank <- full_join(worldbank, population)
worldbank <- full_join(worldbank, balance)
worldbank <- full_join(worldbank, growth)

colnames(worldbank)[which(colnames(worldbank)=="year")] <- "Year"
worldbank$ISO3N <- worldbank$iso2c %>% countrycode(origin = "iso2c", destination = "iso3n")
#Ethnic
load("C:/Users/joshu/Desktop/HIEF_data.RData")
ethnic <- x

ethnic$ISO3N <- ethnic$Country %>% countrycode(origin = "country.name.en", destination = "iso3n")

ethnic <- ethnic %>% unique()

ethnic <- drop_na(ethnic)

#polity
library(rio)
url <- "http://www.systemicpeace.org/inscr/p5v2018.xls"

polity <- rio::import(file = url)

# ipe.sub <- ipe_v4[,c("year", "ifs", "gdppc_WDI" ,"growth_WDI", "MUSPC_RCS" ,"intwar_PV"
#                       , "civwar_PV" ,"region_PO", "natresource_rents_WDI", "fdi_inper_WDI", "exclpop_EP", "inflation_WDI", "risk_prem_WDI"
#                       ,"pop_WDI_PW", "exp_WDI"
#                       , "inflation_WDI", "trade_WDI", "poleff_SFI", "effect_SFI", "effect_SFI", "GE_EST_WGI", "risk_prem_WDI")]

polity <- polity %>% select(ccode, year, polity)
polity$ISO3N <- polity$ccode %>% countrycode(origin = "cown", destination = "iso3n")
polity <- drop_na(polity)

colnames(polity)[which(colnames(polity)=="year")] <- "Year"
# colnames(ipe.sub)[1:2] <- c("Year", "ISO3N")
# ipe.sub <- ipe.sub[-(which(is.na(ipe.sub["ISO3N"]))),]

# AUT.f <- left_join(AUT, ipe.sub)
# AUT.f %>% as.data.frame()
# AUT.f$intwar_PV[which((AUT.f$intwar_PV %>% is.na(.) == FALSE) & (AUT.f$intwar_PV != 0))] <- 1
# AUT.f$civwar_PV[which((AUT.f$civwar_PV %>% is.na(.) == FALSE) & (AUT.f$civwar_PV != 0))] <- 1

region <- Aft.sector.graph[,c("RecipientName","RegionName")] %>% unique()
colnames(region)[1] <- "AfT.name.old"
region.name <- left_join(region, Aft_name)

region.name <- region.name %>% as.data.frame()
region.name <- region.name[,c(2,5)]
region.name <- drop_na(region.name)
region.name$ISO3N <- region.name$ISO3C %>% countrycode(origin = 'iso3c', destination = "iso3n")
region.name <- drop_na(region.name)

aft.merge.bi$ISO3N <- aft.merge.bi$ISO3N %>% as.numeric()

final <- full_join(aft.merge.bi, export.merge )

final <- left_join(final, AUT.merge)
final <- left_join(final, worldbank)
final <- left_join(final, polity)

final <- left_join(final, region.name)
final <- left_join(final, ethnic)

# write.csv(final, paste0(getwd(), "/newfinal.csv"))
# 
# sample <- AUT.f
# AUT.f.last <- left_join(sample, region.name)

# for (i in 1: (AUT.f$allgwf_regimetype %>% unique(.) %>% length())){
#   type <- AUT.f$allgwf_regimetype %>% unique() %>% as.data.frame()
#   type <- type[,] %>% as.data.frame()
#   type.sub <- type [i,1]
#   AUT.f[which(AUT.f$allgwf_regimetype == type.sub),AUT.f %>% colnames() %>% length() +1] <- 1
#   AUT.f[is.na(AUT.f[,AUT.f %>% colnames %>% length()]), AUT.f %>% colnames() %>% length()] <- 0
#   colnames(AUT.f)[AUT.f %>% colnames() %>% length()] <- type.sub %>% as.character()
# }
# 
# for (i in 1: (AUT.f.last$RegionName %>% unique(.) %>% length() -1)){
#   type <- AUT.f.last$RegionName %>% unique() %>% as.data.frame()
#   type <- type[-1,] %>% as.data.frame()
#   type.sub <- type [i,1]
#   AUT.f.last[which(AUT.f.last$RegionName == type.sub),AUT.f.last %>% colnames() %>% length() +1] <- 1
#   AUT.f.last[is.na(AUT.f.last[,AUT.f.last %>% colnames %>% length()]), AUT.f.last %>% colnames() %>% length()] <- 0
#   colnames(AUT.f.last)[AUT.f.last %>% colnames() %>% length()] <- type.sub %>% as.character()
# }
# 
# AUT.f.last[which(AUT.f.last$gwf_nonautocracy == "democracy"),AUT.f.last %>% colnames() %>% length() +1] <- 1
# AUT.f.last[is.na(AUT.f.last[,AUT.f.last %>% colnames %>% length()]), AUT.f.last %>% colnames() %>% length()] <- 0
# colnames(AUT.f.last)[AUT.f.last %>% colnames() %>% length()] <- "democracy"
# 
# write.csv(AUT.f, "E:/AfT/AUTpersonalism.csv")
# 
pr_fail <- read_csv("E:/AfT/pr_fail.csv")
# 
# pr_fail %>% filter(gwf_country == "Argentina"| gwf_country == "Botswana"| gwf_country == "Hungary"| gwf_country == "Congo/Zaire") %>% filter (year > 1960) %>%
#   ggplot(.,aes(x=year, y= pr_fail, group= gwf_country, colour=gwf_country)) +
#   geom_line(size=2.0) +
#   theme_jhp()

# colnames(pr_fail)[(colnames(pr_fail)  %in%  c("year", "iso3n")) %>% which()] <- c("Year", "ISO3N")
# pr_fail <- pr_fail %>% as.data.frame()
# # pr_fail[,(colnames(pr_fail)  %in%  c("ISO3N")) %>% which()] <- pr_fail[,(colnames(pr_fail)  %in%  c("ISO3N")) %>% which()] %>% as.numeric()
# 
# ipe.sub2 <- ipe_v4[,c("year", "ifs", "exclpop_EP", "inflation_WDI", "risk_prem_WDI" 
#                       ,"pop_WDI_PW", "exp_WDI","natresource_rents_WDI","fdi_inper_WDI"
#                       , "inflation_WDI", "trade_WDI")]
# colnames(ipe.sub2)[1:2] <- c("Year", "ISO3N")
# ipe.sub2 <- ipe.sub2[-(which(is.na(ipe.sub2["ISO3N"]))),]
# agri.merge <- agri.merge[-(which(is.na(agri.merge["ISO3C"]))),]
# aft.merge <- aft.merge[-(which(is.na(aft.merge["ISO3C"]))),]
# export.merge <- export.merge[-(which(is.na(export.merge["ISO3C"]))),]
# agri.merge <- agri.merge[,1:4]
# 
# final <- left_join(export.merge, agri.merge)
# final <- left_join(final, aft.merge)
# final <- left_join(final, aft.merge.bi)
# final <- left_join(final, aft.merge.multi) 
# final <- left_join(final, aft.merge.bi.country)
# final[,final %>% colnames() %>% length() +1] <- match(final$ISO3C, Aft_name$ISO3C) %>% Aft_name[.,4]
# colnames(final)[final %>% colnames() %>% length()] <- "ISO3N"
# final <- left_join(final, ipe.sub)
pr_fail <- pr_fail[,c("year", "cowcode", "pr_fail")]
pr_fail$ISO3N <- pr_fail$cowcode %>% countrycode(origin= "cown", destination = "iso3n")
pr_fail <- pr_fail %>% select(!cowcode)
colnames(pr_fail)[which(colnames(pr_fail)=="year")] <- "Year" 
pr_fail <- pr_fail[-(which(is.na(pr_fail["ISO3N"]))),]




# write.csv(final, "E:/AfT/final_withnatural.csv")

## Adding Variables
library(OECD)
dataset_list <- get_datasets()
search_dataset("ODA", data = dataset_list)

dataset <- "REF_TOTALRECPTS"

dstruc <- get_data_structure(dataset)
str(dstruc, max.level = 1)

total.aid <- get_dataset(dataset = dataset)

total.aid.sum1 <- total.aid[which(total.aid$DONOR==20005),]
total.aid.sum2 <- total.aid.sum1 %>% group_by(RECIPIENT, obsTime) %>% summarise(obsValue = sum(obsValue))

total.aid.name <- dstruc$RECIPIENT %>% unique()

total.aid.sum2[,length(colnames(total.aid.sum2))+1] <- match(total.aid.sum2$RECIPIENT, total.aid.name$id) %>% total.aid.name[.,2]
colnames(total.aid.sum2)[length(colnames(total.aid.sum2))] <- "AfT.name.old"

total.aid.sum.new.name <- left_join(total.aid.sum2, Aft_name)

total.aid.sum2 <- total.aid.sum.new.name
total.aid.sum2 <- total.aid.sum2[-(which(is.na(total.aid.sum2["ISO3C"]))),]
total.aid.sum2 <- total.aid.sum2 %>% group_by(ISO3N, obsTime) %>% summarise(obsValue = sum(obsValue))


colnames(total.aid.sum2)[which(colnames(total.aid.sum2)=="obsTime")] <- "Year"
colnames(total.aid.sum2)[which(colnames(total.aid.sum2)=="obsValue")] <- "Totalaid"

total.aid.sum2$Year <- as.numeric(total.aid.sum2$Year)
total.aid.sum2$ISO3N <- as.numeric(total.aid.sum2$ISO3N)

total.merge <- total.aid.sum2 %>% as.data.frame()
total.merge[,2] <- total.merge[,2] %>% as.numeric()
# total.merge <- total.merge[-(which(is.na(total.merge["ISO3N"]))),]
# total.merge <- total.merge[-(which(is.na(total.merge["ISO3N"]))),]

aft.merge$Year <- aft.merge$Year %>% as.numeric()
aft.merge$ISO3N <- aft.merge$ISO3N %>% as.numeric()

final <- left_join(export.merge, aft.merge)

final <- left_join(final, AUT.merge)
final <- left_join(final, worldbank)
final <- left_join(final, polity)

final <- left_join(final, region.name)
final <- left_join(final, ethnic)
final <- left_join(final, pr_fail)
final <- left_join(final, total.merge)
# final[which(is.na(final[,length(colnames(final))])),length(colnames(final))] <- 0

## polity5
# pol.compt <- read_xls("E:/AfT/polity5.xls")
# pol.compt <- pol.compt %>% as.data.frame()
# colnames(pol.compt)[which(colnames(pol.compt)=="year")] <- "Year"
# pol.compt[,colnames(pol.compt)%>%length()+1] <- match(pol.compt[,1], cow_country[,1]) %>% cow_country[.,2]
# colnames(pol.compt)[length(colnames(pol.compt))] <- "ISO3N"
# pol.compt <- pol.compt[-(which(is.na(pol.compt["ISO3N"]))),]
# pol.compt <- pol.compt[-(which(pol.compt[,3] < 0)),]
# pol.compt <- drop_na(pol.compt)
# final <- left_join(final, pol.compt)

# final <- left_join(final, AUT.merge)

# write.csv(final, "E:/AfT/final_withnaturalwithcompetition.csv")

dreher <- read_dta("C:/Users/joshu/Desktop/DreherLanglotz2019_MonadicData.dta")

dreher_use <- dreher[,c("reccode","year","IV_preperiod_agg")]

countrycode::guess_field(dreher_use$reccode)

url <- "http://www.axel-dreher.de/UNSCdata.xls"

unsc <- rio::import(file = url, sheet = 2)

unsc$ISO3N <- unsc$code %>% countrycode(origin = "iso3c", destination = "iso3n")
colnames(unsc)[which(colnames(unsc) == "year")] <- "Year"
unsc <- unsc[,c(4,5,6)]

dreher_use$ISO3N <- dreher$reccode %>% countrycode(origin = "iso3c", destination = "iso3n")       
colnames(dreher_use)[which(colnames(dreher_use) == "year")] <- "Year"
dreher_use<- dreher_use[,c("Year","ISO3N","IV_preperiod_agg")]
final <- left_join(final, unsc)

write.csv(final, "E:/AfT/newfinal_0717.csv")

