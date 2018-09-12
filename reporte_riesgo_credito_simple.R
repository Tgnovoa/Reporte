####Reporte Riesgo Credito----
# rm(list = ls())
library(tidyverse)
library(magrittr)
library(stringr)
library(RDS)
library(xts)
# se volverá a separar el código en 2:
#   - bases de datos
#   - gráficas y resúmenes (tablas)
setwd("~/../Desktop/Santiago/Riesgos/Proyectos/Reporte Credito")
#### BASES DE DATOS ----
# texto
{
  file_1 <- 'Txts/resumen_periodo_030718_2.txt'
  # - Separar archivo en temas
  sep_names_file1 <- file_1 %>% 
    readLines() %>% 
    str_split_fixed(n = 2, pattern = ":") 
  # - Empresas que tuvieron cambios
  issuer_file1 <- sep_names_file1[which(!sep_names_file1[,2]==""),1]
  diff_group_change <- c(1,diff(which(!sep_names_file1[,2]=="")))>1
  sep_names_file1[c(1,which(!sep_names_file1[,2]=="")[diff_group_change]-1),1]
  text_df <- data.frame(Issuer = issuer_file1, line = which(!sep_names_file1[,2]==""))
  library(Hmisc)
  text_df$Commentary <- capitalize(substring(sep_names_file1[which(!sep_names_file1[,2]==""),2], 2))
  # text_df
  text_df <- text_df %>% 
    dplyr::mutate(
      section = case_when(line < which(!sep_names_file1[,2]=="")[diff_group_change][1] ~ sep_names_file1[c(1,which(!sep_names_file1[,2]=="")[diff_group_change]-1),1][1],
                          line < which(!sep_names_file1[,2]=="")[diff_group_change][2] ~ sep_names_file1[c(1,which(!sep_names_file1[,2]=="")[diff_group_change]-1),1][2],
                          TRUE ~ sep_names_file1[c(1,which(!sep_names_file1[,2]=="")[diff_group_change]-1),1][3]))
  file1_txt <- file_1 %>% readLines()
  Rnews <- file1_txt[(which(file1_txt=="Relevant News")+1):length(file1_txt)]
  Rnews <- as.data.frame(Rnews)
  names(Rnews) <- c("Relevant Information")
  Rnews$n <- 1:nrow(Rnews)
  Rnews <- Rnews %>% 
    dplyr::select(n,`Relevant Information`)
  Rnews$`Relevant Information` <- as.character(Rnews$`Relevant Information`)
  # text_df %>% write_csv("text_df.csv")
  # Rnews %>% write_lines("Rnews.txt")
}
# Asset and Accrual y EDF_lag
{
  library(data.table)
  dir_csv <- str_c("Csvs/",dir("Csvs/"))
  AaAD <- read_csv(dir_csv[which(dir_csv %>% str_detect(pattern = "AssetAndAccrual"))],
                   col_types = list(`Maturity Date` = col_date(format = "%m/%d/%Y"),
                                    `Pay Date` = col_date(format = "%m/%d/%Y")))
  EDF_port <- read_csv(dir_csv[which(dir_csv %>% str_detect(pattern="EDF_lag"))], skip = 3, 
                       col_types = list(Date = col_date(format = "%m/%d/%Y"),
                                        `Date-1` = col_date(format = "%m/%d/%Y")),
                       na = c("You do not have permission to view this data.","#N/A","NA/NA"))
  names(AaAD)<- gsub(names(AaAD), pattern = " ", replacement = "") %>% 
    gsub(pattern="/", replacement = "_")
  MarketValue_Bonds <-AaAD %>% 
    dplyr::select(ISIN, ReportingAccountName, SecurityDescription1, SectorName, MaturityDate, CountryName, BaseMarketValue, BaseCost, BaseNetIncomeReceivable, CouponRate, BaseUnrealizedGain_Loss, BaseNetUnrealizedGain_Loss, PayDate, PercentOfTotal) %>% 
    as.data.table()
  
  # - Tablas con peores Issuers con respecto a sus probabilidades de default y deterioro
  EDF_head <- EDF_port %>% 
    dplyr::filter(Date == "2018-06-29") %>% 
    dplyr::arrange(desc(EDF1)) %>%
    dplyr::mutate(n_char = nchar(Issuer),
                  Issuer = gsub(substring(Issuer, first = 1, last = n_char-17), pattern = "([0-9])", replacement = ""),
                  dEDF1 = EDF1-EDF1_LY,
                  dEDF5 = EDF5-EDF5_LY) %>% 
    dplyr::select(Issuer, EDF1, dEDF1, EDF5, dEDF5) %>% 
    head(25)
  PDet_head <- EDF_port %>% 
    dplyr::filter(Date == "2018-06-29") %>% 
    dplyr::arrange(desc(P_Det)) %>% 
    dplyr::mutate(n_char = nchar(Issuer),
                  Issuer = gsub(substring(Issuer, first = 1, last = n_char-17), pattern = "([0-9])", replacement = ""),
                  dP_Det = P_Det - P_Det_LY) %>% 
    dplyr::select(Issuer, P_Det, dP_Det) %>% 
    head(25)
  # EDF_head %>% write_csv("EDF_head.csv")
  # PDet_head %>% write_csv("PDet_head.csv")
}
# Bonds, prob_inc y EDF para crear tablas de PD(precio), gráfica cielo/infierno y tabla para unir a LGD
{
  # - Nombres de ISINs que están en A&A
  isin_prt <- MarketValue_Bonds$ISIN[!is.na(MarketValue_Bonds$ISIN)] %>% 
    unique() 
  isin_prt_df <- data.frame(ISIN_ID = isin_prt)
  # - Lectura de precios de bonos por Bond_id
  bonds_bdh <- read_rds("Csvs/bloomberg_bonos.rds")
  list_len <- length(bonds_bdh$Bond)
  #
  bond_read <- data.frame()
  for(i in 1:list_len){
    if(bonds_bdh[[1]][[i]] %>% nrow()>0){
      aux_df_bonds <- bonds_bdh[[1]][[i]]
      Bond_id <- bonds_bdh[[1]][i] %>% names()
      bond_read <- rbind(bond_read, data.frame(aux_df_bonds, Bond_id = Bond_id))
    }
  }
  # - tabla de IDs para bonos del portafolio (Raúl)
  isinID <- fread("Csvs/ISIN_ID_PORT.csv")
  isin_prt_df <- isin_prt_df %>% 
    dplyr::left_join(isinID)
  isin_prt_df <- isin_prt_df %>% 
    dplyr::mutate(ISIN_pre = gsub(ISIN_pre, pattern = " ISIN", replacement = ""))
  # - ISINs faltantes del A&A en IDs
  isin_missing <- isin_prt_df %>% dplyr::filter(is.na(Ticker)) %>% dplyr::select(ISIN_ID)
  ##  - - >                                     ## IMPORTANTE VER QUÉ ISINs FALTAN ##
  # - Emparejar ISINs del A&A con IDs
  MarketValue_Bonds <- MarketValue_Bonds %>% 
    dplyr::filter(!ISIN %in% isin_missing$ISIN_ID) %>% 
    dplyr::left_join(isin_prt_df, by = c("ISIN"="ISIN_ID"))
  # - Lectura de IDs para bonos del BENCHMARK y emparejamiento con ISINs del portafolio
  isin_bonds <- read_csv("Csvs/tabla_isin_tickers.csv",
                         na = c("#N/A N/A", "#N/A"),
                         col_types = list(Row = col_skip(), BANKS = col_skip(), FIN = col_skip()))
  isin_bonds <- isin_bonds %>% 
    dplyr::left_join(isin_prt_df, by = c("ISIN"="ISIN_pre")) %>% 
    dplyr::filter(!is.na(ISIN_ID)) %>% 
    as.data.table()
  # - Lectura precios bonos y emparejamiento por IDs de bonos con portafolio
  bond_bdp <- readRDS("Csvs/bloomberg_bonos_bdp.rds")
  rbind_bdp <- do.call("rbind", bond_bdp)
  rbind_bdp$Bond_id <- rownames(rbind_bdp) %>% gsub(pattern = "Bond.", replacement = "")
  names(rbind_bdp) <- c("Coupon", "Maturity", "DateOfIssue", "Bond_id") 
  bond_mkt <- bond_read %>% 
    dplyr::left_join(rbind_bdp) %>% 
    dplyr::left_join(isin_bonds %>% 
                       dplyr::select(BOND, ISIN_ID), 
                     by = c("Bond_id"="BOND")) %>% 
    dplyr::filter(!is.na(ISIN_ID))
  bond_mkt <- bond_mkt %>% 
    dplyr::group_by(Bond_id) %>% 
    dplyr::mutate(max_date = max(date))
  bond_join <- bond_mkt %>% 
    dplyr::filter(date == max_date)
  ### 
  # - Lectura de EDFs y precios históricos
  EDF <- readRDS(file = "Csvs/EDF_02052018.rds")
  bonds <- readRDS(file = "Csvs/benchmark_bonds.rds")
  bonds_prt <- bonds %>% 
    dplyr::left_join(isin_prt_df %>% 
                       dplyr::select(-c(Ticker,Issuer)), 
                     by = c("isin"="ISIN_ID")) %>% 
    dplyr::left_join(isin_prt_df %>% 
                       dplyr::select(Ticker, ISIN_ID)) %>% 
    dplyr::left_join(isin_prt_df %>% 
                       dplyr::mutate(ISIN_pre2 = ISIN_pre) %>% 
                       dplyr::select(Issuer, ISIN_pre2)) %>% 
    dplyr::filter(!is.na(ISIN_ID)| !is.na(ISIN_pre) | !is.na(ISIN_pre2)) %>% 
    dplyr::select(-c(ISIN_ID, ISIN_pre, ISIN_pre2)) %>% 
    dplyr::group_by(Ticker)
  names(EDF) <- c("Count", "ID", "Issuer", "ISIN", "Moodys", "Date", "Date_1", "EDF1", "EDF5", "EDF1_L", "EDF5_L", "d1", "d5", "dABS1", "dABS5", "slope")
  EDF <- EDF %>% as.data.frame()
  EDF[,sapply(EDF, is.character)] <-EDF[,sapply(EDF, is.character)] %>% lapply(factor) 
  EDFq <- EDF %>% 
    dplyr::mutate(DateQ = as.yearqtr(Date)) %>% 
    dplyr::select(-c(Date,Date_1, Moodys, ID, Count)) %>% 
    dplyr::group_by(DateQ, Issuer, ISIN) %>% 
    dplyr::summarise(
      EDF1 = mean(EDF1, na.rm = T),
      EDF5 = mean(EDF5, na.rm = T),
      EDF1_L = mean(EDF1_L, na.rm = T),
      EDF5_L = mean(EDF5_L, na.rm = T),
      d1 = mean(d1, na.rm = T),
      d5 = mean(d5, na.rm = T),
      dABS1 = mean(dABS1, na.rm = T),
      dABS5 = mean(dABS5, na.rm = T),
      slope = mean(slope, na.rm = T)
    )
  EDFq <- EDFq %>% 
    dplyr::filter(!is.na(EDF1)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(ISIN = factor(ISIN %>% gsub(pattern = "isin-", replacement = ""))) %>% 
    dplyr::arrange(Issuer, DateQ)
  bonds_prt[sapply(bonds_prt, is.character)] <- bonds_prt[sapply(bonds_prt, is.character)] %>% lapply(factor) 
  EDFq_prt <- EDFq %>% 
    dplyr::left_join(isinID) %>% 
    dplyr::filter(!is.na(Ticker))
  ## - Creación tabla comparación entre PD(precios) y EDF
  prt_PvsEDF <- bonds_prt %>% 
    dplyr::ungroup() %>% 
    dplyr::left_join(EDFq_prt %>% 
                       dplyr::select(-c(Issuer)),
                     by = c("Ticker" = "Ticker", "DateQ" = "DateQ")) 
  # isin_prt_df
  ID_b <- read_csv("Csvs/ID_BloombergPort.csv")
  # portB_PvsEDF, sólo bonos en el portafolio
  prtB_PvsEDF <- prt_PvsEDF %>%
    dplyr::left_join(ID_b %>% 
                       dplyr::mutate(BOND = str_c(substr(ID_BB, start = 1, stop = 8), " Corp")),
                     by = c("BOND" = "BOND")) %>%
    dplyr::filter(!is.na(ISIN.y)) 
  ### - Cálculo de EDF por plazo faltante ----
  EDFs_prt <- read_csv("Csvs/EDFs.csv", 
                       skip = 1, 
                       na = c("You do not have permission to view this data.","#N/A","NA/NA"),
                       col_types = list(Date = col_date(format = "%m/%d/%Y")))
  prtB_l <- prtB_PvsEDF %>% 
    dplyr::mutate(l = MaturityQ - DateQ, 
                  prob_inc = ifelse(prob_inc<0,0, prob_inc)) %>% 
    dplyr::filter(DateQ == as.yearqtr("2017 Q1"))
  EDFsQ_prt <- EDFs_prt %>% 
    dplyr::mutate(DateQ = as.yearqtr(Date)) %>% 
    dplyr::group_by(ISIN, DateQ) %>% 
    dplyr::summarise(EDF1 = mean(EDF1, na.rm = T),
                     EDF2 = mean(EDF2, na.rm = T),
                     EDF3 = mean(EDF3, na.rm = T),
                     EDF4 = mean(EDF4, na.rm = T),
                     EDF5 = mean(EDF5, na.rm = T),
                     EDF6 = mean(EDF6, na.rm = T),
                     EDF7 = mean(EDF7, na.rm = T),
                     EDF8 = mean(EDF8, na.rm = T),
                     EDF9 = mean(EDF9, na.rm = T),
                     EDF10 = mean(EDF10, na.rm = T)) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-c(DateQ))
  EDF_port$Issue_ID <- str_split_fixed(EDF_port$Issuer, pattern = ' ([0-9]).*', n = 2)[,1] %>% 
    gsub(pattern = " ", replacement = "_") %>% 
    gsub(pattern = "\\.", replacement = "") %>% 
    gsub(pattern = ",", replacement = "") %>% 
    gsub(pattern = "'", replacement = "") %>% 
    gsub(pattern = "&", replacement = "-") %>% 
    gsub(pattern = "/", replacement = "-")
  prtB_l$Issue_ID <- str_split_fixed(prtB_l$Issue, pattern = ' ([0-9]).*', n = 2)[,1] %>% 
    gsub(pattern = " ", replacement = "_") %>% 
    gsub(pattern = "\\.", replacement = "") %>% 
    gsub(pattern = ",", replacement = "") %>% 
    gsub(pattern = "'", replacement = "") %>% 
    gsub(pattern = "&", replacement = "-") %>% 
    gsub(pattern = "/", replacement = "-")
  EDF_Bonds <- prtB_l %>%
    dplyr::left_join(EDFsQ_prt, by = c("isin" = "ISIN")) %>% 
    dplyr::select(Ticker, BOND, EDF1.x, EDF5.x, EDF1.y:EDF10, l) %>% unique()
  aux_EDFb <- rep(NA_real_, nrow(EDF_Bonds))
  for(i in 1:nrow(EDF_Bonds)){
    aux_l <- EDF_Bonds[["l"]][i]
    diff_aux_l <- aux_l - floor(aux_l)
    if(is.na(EDF_Bonds[["EDF1.y"]][i])){
      aux_EDF <- EDF_Bonds[["EDF1.x"]][i] + ((EDF_Bonds[["EDF5.x"]][i] - EDF_Bonds[["EDF1.x"]][i])/4)*((aux_l-1))
    }else{
      aux_EDF <- EDF_Bonds[i,floor(aux_l)+4]*(1-diff_aux_l) + EDF_Bonds[i,ceiling(aux_l)+4]*(diff_aux_l)
    }
    aux_EDFb[i] <- aux_EDF %>% as.numeric()
  }
  EDF_Bonds$EDF <- aux_EDFb
  # EDF_Bonds
  prt <- prtB_l %>% 
    dplyr::left_join(EDF_Bonds %>%
                       dplyr::select(Ticker, BOND, EDF)) %>% 
    dplyr::select(-c(Issue, EDF1_L:ISIN.y, isin)) %>% 
    unique()
  ## - Tabla de PD(precio) para empresas con probabilidades más altas
  prtB_head <- prtB_l %>% 
    dplyr::left_join(EDF_Bonds %>%
                       dplyr::select(Ticker, BOND, EDF)) %>% 
    dplyr::mutate(Default_Probability_price = prob_inc) %>% 
    dplyr::select(Issuer, Default_Probability_price, EDF, l) %>% 
    dplyr::group_by(Issuer) %>% 
    dplyr::summarise(DP_price = mean(Default_Probability_price),
                     EDF = mean(EDF)/100,
                     l = mean(l)) %>% 
    dplyr::arrange(desc(DP_price)) %>% 
    head(25)
  
}
# Cálculo del EL 
{
  LGD_bench <- read_csv("Csvs/LGD_bench-port.csv",
                        na = c("--", "NA", "#N/A"))
  prt_LGD <- prt %>% 
    dplyr::left_join(LGD_bench %>% 
                       dplyr::select(Ticker, sector_lgd_sr_bond)) %>% 
    as.data.table() %>% 
    unique()
  prt_LGD <- prt_LGD %>% 
    dplyr::mutate(LGD = ifelse(!is.na(sector_lgd_sr_bond), sector_lgd_sr_bond, 0.6),
                  EL = EDF/100*LGD,
                  ExpectedLoss = ifelse(is.na(EL), median(EL,na.rm = T), EL))
  prt_EL <- prt_LGD %>% 
    dplyr::left_join(MarketValue_Bonds, by = c("Ticker" = "Ticker")) %>% 
    dplyr::filter(!is.na(ISIN)) %>% 
    dplyr::group_by(ReportingAccountName) %>% 
    dplyr::mutate(ExpectedLoss_MktV = ExpectedLoss*BaseMarketValue) %>% 
    dplyr::summarise(MktValue = sum(BaseMarketValue,na.rm=T),
                     ExpectedLoss_MktV = sum(ExpectedLoss_MktV, na.rm = T),
                     PercExpectedLoss = ExpectedLoss_MktV/MktValue) 
}
# Solvencia y chequeos 
{
  review_dates <- read_csv("Csvs/Watchlist_Review_Dates.csv", col_names = F)
  names(review_dates) <- c("Issuer", "Review_Date", "Commentary")
  solvencia <- read_csv("Csvs/tabla_completa12.csv")
  tabla_filtro <- solvencia %>% 
    dplyr::left_join(review_dates) %>% 
    dplyr::mutate(EBIT_prox12m = EBIT12.point, 
                  EBIT_ultTrim = EBIT_t) %>% 
    dplyr::select(Issuer, EBIT_NetDebt,EBIT_prox12m, EBIT_ultTrim, Review_Date, Commentary) %>% 
    dplyr::filter(Issuer %in% review_dates$Issuer)
}
#### GRáFICAS Y RESúMENES ----
# tresalia_palette
source(file = "../../../Bases_generales/tresalia_palettes.R")
# descriptivos portafolio
{
  ## MktValue
  library(scales)
  MarketValue_Bonds$SectorName <- factor(MarketValue_Bonds$SectorName,
                                         levels = MarketValue_Bonds$SectorName %>% table() %>% as.data.table() %>% dplyr::arrange(desc(N)) %>% .$`.`)
  # - Graficas barras tamaño de market value por sectores/regiones
  gg_Mkt_Sector <- MarketValue_Bonds %>%
    dplyr::group_by(SectorName) %>%
    dplyr::summarise(MktSum = sum(BaseMarketValue, na.rm = T)) %>%
    dplyr::arrange(desc(MktSum)) %>%
    ggplot(aes(x = SectorName, y = MktSum, colour = SectorName, fill = SectorName)) +
    geom_col(show.legend = F) +
    theme_tresalia + theme(axis.text.x = element_text(angle = 90, size = 7, hjust = 1, vjust = 0.5)) +
    scale_y_continuous(labels = dollar) + ylab("Market Value") + xlab("Sector") + ggtitle("Total of Market Value by Sector",subtitle = "CV Advisors Portfolio") +
    scale_fill_tresalia(palette = "main") + scale_colour_tresalia()

  MarketValue_Bonds$CountryName <- factor(MarketValue_Bonds$CountryName,
                                          levels = MarketValue_Bonds$CountryName %>% table() %>% as.data.table() %>% dplyr::arrange(desc(N)) %>% .$`.`)

  gg_Mkt_Country <- MarketValue_Bonds %>%
    dplyr::group_by(CountryName) %>%
    dplyr::summarise(MktSum = sum(BaseMarketValue, na.rm = T)) %>%
    dplyr::arrange(desc(MktSum)) %>%
    ggplot(aes(x = CountryName, y = MktSum, colour = CountryName, fill = CountryName)) +
    geom_col(show.legend = F) +
    theme_tresalia + theme(axis.text.x = element_text(angle = 90, size = 7, hjust = 1, vjust = 0.5),
                           axis.title.y = element_text(hjust = 0.4)) +
    scale_y_continuous(labels = dollar) + ylab("Market Value") + xlab("Country") + ggtitle("Total of Market Value by Country",subtitle = "CV Advisors Portfolio") +
    scale_fill_tresalia(palette = "main") + scale_colour_tresalia()
  library(lubridate)
  library(xts)
  gg_Mkt_Mat <- MarketValue_Bonds %>%
    dplyr::filter(!is.na(MaturityDate)) %>%
    dplyr::group_by(year(MaturityDate)) %>%
    dplyr::summarise(MktSum = sum(BaseMarketValue, na.rm = T)) %>%
    dplyr::arrange(desc(MktSum)) %>%
    ggplot(aes(x = `year(MaturityDate)`, y = MktSum,  fill = `year(MaturityDate)`)) +
    geom_col(show.legend = F) +
    theme_tresalia + theme(axis.text.x = element_text(angle = 90, size = 7)) +
    scale_y_continuous(labels = dollar) + xlab("Maturity Date") + ylab("Market Value") + ggtitle("Total of Market Value by Maturity",subtitle = "CV Advisors Portfolio") +
    scale_colour_tresalia(palette = "main", discrete = TRUE)
  ## EDF
  # - Graficas scatter comparando probabilidades de default ( y de deterioro ) actuales contra hace 1 año
  gg_EDF_PDet <- EDF_port %>%
    ggplot(aes(x = P_Det/100, y = EDF1/100), col = "myColour1") +
    geom_point() + coord_fixed(xlim = c(0,1), ylim = c(0,1), ratio = 1) + theme_tresalia +
    xlab("Deterioration Probability\n(actual value)") + ylab("EDF 1 year\n(actual value)") +
    scale_y_continuous(labels = percent) + scale_x_continuous(labels = percent) + ggtitle("Default Prob. vs.\nDeterioration prob.",subtitle = "CV Advisors Portfolio")
  gg_EDF_EDFLY <- EDF_port %>%
    ggplot(aes(x = EDF1_LY/100, y = EDF1/100), col = "myColour1") +
    geom_point() + coord_fixed(xlim = c(0,0.1), ylim = c(0,0.1), ratio = 1) + theme_tresalia +
    xlab("EDF 1 year\n(last year value)") + ylab("EDF 1 year\n(actual value)") +
    scale_y_continuous(labels = percent) + scale_x_continuous(labels = percent) + ggtitle("1 Year Change\n(default prob.)",subtitle = "CV Advisors Portfolio")
  gg_PDet_PDetLY <- EDF_port %>%
    ggplot(aes(x = P_Det_LY/100, y = P_Det/100), col = "myColour1") +
    geom_point() + coord_fixed(xlim = c(0,1), ylim = c(0,1), ratio = 1) + theme_tresalia +
    xlab("Deterioration Probability\n (last year value)") + ylab("Deterioration Probability\n (actual value)") +
    scale_y_continuous(labels = percent) + scale_x_continuous(labels = percent) + ggtitle("1 Year Change\n(deterioration prob.)",subtitle = "CV Advisors Portfolio")
}
# bonos portafolio y probabilidades de default
{
  # - Gráfica scatter que compara EDF1 contra Probabilidad implícita de default (para todos los bonos de las empresas en el portafolio)
  # - - por color y plazo
  gg_EDF_PD_f <- prt_PvsEDF %>%
    dplyr::mutate(l = MaturityQ - DateQ) %>%
    ggplot(aes(x = EDF1 ,  y = prob_inc, colour= factor(ceiling(l)))) +
    geom_point(alpha = 0.4) +
    facet_wrap(~year(DateQ), nrow = 2) +
    scale_fill_tresalia(palette = "main") + scale_colour_tresalia() +
    theme_tresalia +
    ylab("Implicit Default Probability \n(by bond price)") + xlab("Default Probabilty \n(1 year)") +
    theme(legend.title = element_blank()) +
    ggtitle("EDF vs price-implicit Default Probability\n grouped by date\n coloured by remaining years", subtitle = "CV Advisors Issuers\n(every bond from corps in portfolio)")
  # - Gráfica scatter que compara EDF1 contra Probabilidad implícita de default (para todos los bonos de las empresas en el portafolio)
  gg_EDF_PD <- prt_PvsEDF %>%
    dplyr::mutate(l = MaturityQ - DateQ,
                  prob_inc = ifelse(prob_inc<0,0, prob_inc)) %>%
    dplyr::filter(DateQ == as.yearqtr("2017 Q1")) %>%
    ggplot(aes(x = EDF1/100 ,  y = prob_inc, colour = Ticker)) +
    geom_abline(slope = 1, intercept = 0, col = "gray", lty = 2, size = 1.25) +
    geom_point(alpha = 0.25, size = 3.5, show.legend = F) +
    scale_fill_tresalia(palette = "main") + scale_colour_tresalia() +
    theme_tresalia  + xlab("Default Probabilty \n(1 year)") + ylab("Implicit Default Probability \n(by bond price)") +
    ggtitle("EDF vs price-implicit Default Probability", subtitle = "CV Advisors Issuers\n(every bond from corps in portfolio)")
  # - Gráfica scatter que compara plazo contra Probabilidad implícita de default (para todos los bonos de las empresas en el portafolio)
  gg_PD_l <- prt_PvsEDF %>%
    dplyr::mutate(l = MaturityQ - DateQ,
                  prob_inc = ifelse(prob_inc<0,0, prob_inc)) %>%
    dplyr::filter(DateQ == as.yearqtr("2017 Q1")) %>%
    ggplot(aes(x = l ,  y = prob_inc, colour = Ticker)) +
    geom_point(alpha = 0.25, size = 3.5, show.legend = F) +
    scale_fill_tresalia(palette = "main") + scale_colour_tresalia() +
    theme_tresalia + xlab("Years remaining until maturity") + ylab("Implicit Default Probability \n(by bond price)") +
    ggtitle("Time to maturity vs.\nprice-implicit Default Probability", subtitle = "CV Advisors Issuers\n(every bond from corps in portfolio)")
  # - Gráfica scatter que compara EDF1 contra Probabilidad implícita de default (para los bonos en el portafolio)
  # - - por color y plazo
  gg_EDF_PD_p_f <- prtB_PvsEDF %>%
    dplyr::mutate(l = MaturityQ - DateQ) %>%
    ggplot(aes(x = EDF1/100 ,  y = prob_inc, colour= factor(ceiling(l)))) +
    geom_point(alpha = 0.4) +
    facet_wrap(~year(DateQ), nrow = 2) +
    scale_fill_tresalia(palette = "main") + scale_colour_tresalia() +
    theme_tresalia +
    ylab("Implicit Default Probability \n(by bond price)") + xlab("Default Probabilty \n(1 year)") +
    theme(legend.title = element_blank()) +
    ggtitle("EDF vs price-implicit Default Probability\n grouped by date\n coloured by remaining years", subtitle = "CV Advisors Portfolio") +
    scale_y_continuous(labels = percent) + scale_x_continuous(labels = percent)
  ## - Gráfica del cielo vs el infierno
gg_EDF_PD_p <- prt %>%
  ggplot(aes(x = EDF/100 ,  y = prob_inc, colour = EDF1)) +
  geom_abline(slope = 1, intercept = 0, col = myColour2, lty = 2, size = 1.25) +
  geom_point(alpha = 0.25, size = 2.5, show.legend = T, shape = 20) +
  coord_fixed(xlim = c(0,1), ylim = c(0,1), ratio = 1) + theme_tresalia +
  xlab("Default Probability\n (Moody's)") + ylab("Default Probability\n (price-implied)") +
  scale_fill_tresalia(palette = "main", discrete = TRUE) + #scale_colour_tresalia(discrete = TRUE) +
  scale_y_continuous(labels = percent) + scale_x_continuous(labels = percent) + ggtitle("Moody's DP vs.\n price-implied DP", subtitle = "CV Advisors Portfolio")
}
