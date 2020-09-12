library(readxl)
library(data.table)
Sys.setenv(LANG = "en")
df_wb <- read_excel("Dados WB/API_ZAF_DS2_en_excel_v2_1346163.xls", 
                    skip = 2)

df_wb_t <- transpose(df_wb)

rownames(df_wb_t) = colnames(df_wb)
colnames(df_wb_t) = df_wb_t[4,]

wb.clean = df_wb_t[5:64,]

wb.clean = data.frame(wb.clean, (1960:2019))

names(wb.clean$X.1960.2019.) = "time"

library(ggplot2)
library(ggthemes)

library(plotly)

?plotly

gdp.plot <- plot_ly(data = wb_clean, x = ~(2007:2017), y = ~(wb.clean$NY.GDP.MKTP.KD.ZG[48:58]), type = "scatter", mode = "lines")

gdp.plot <- gdp.plot %>% layout(xaxis = list(title = "Tempo"), yaxis = list(title = "PIB, USD 2017"))
gdp.plot

wb.clean.lim = wb.clean[48:58,]

wb.clean.num = matrix(NA, nrow = nrow(wb.clean.lim), ncol = ncol(wb.clean.lim))

for (i in (1:11)) for (j in (1:1438)) {
  
  wb.clean.num[i,j] = as.numeric(wb.clean.lim[i,j])
  
}

wb.clean.num = data.frame(wb.clean.num)

rownames(wb.clean.num) = rownames(wb.clean.lim)
colnames(wb.clean.num) = colnames(wb.clean.lim)

# GDP Time Series

gdp.plot <- ggplot(wb.clean.lim) + geom_line(aes(x = (2007:2017), wb.clean.num$NY.GDP.MKTP.KD.ZG), color = "steelblue") + theme_few() + theme(text = element_text(size = 15)) + scale_color_few() + geom_hline(yintercept = 0) + geom_point(aes(x = (2007:2017), wb.clean.num$NY.GDP.MKTP.KD.ZG), color = "royalblue4") + xlab("Tempo") + ylab("Crescimento do PIB (%)")


ggplotly(gdp.plot) %>% layout(xaxis = list(dtick = 1, 
                                           tickmode = "linear"), font = list(
                                             family = "Helvetica",
                                             size = 20))

# Inflation time series

cpi.plot <- ggplot(wb.clean.lim) + geom_line(aes(x = (2007:2017), wb.clean.num$FP.CPI.TOTL.ZG), color = "steelblue") + theme_few() + theme(text = element_text(size = 15)) + scale_color_few() + geom_hline(yintercept = mean(wb.clean.num$FP.CPI.TOTL.ZG), linetype = 2, color = "deepskyblue3") + geom_point(aes(x = (2007:2017), wb.clean.num$FP.CPI.TOTL.ZG), color = "steelblue") + xlab("Tempo") + ylab("Crescimento do Índice de Preços ao Consumidor (%)") # mean in cyan


ggplotly(cpi.plot) %>% layout(xaxis = list(dtick = 1, 
                                           tickmode = "linear"), font = list(
                                             family = "Helvetica",
                                             size = 20))


# Real Interest time series

interest.plot <- ggplot(wb.clean.lim) + geom_line(aes(x = (2007:2017), wb.clean.num$FR.INR.RINR), color = "steelblue") + theme_few() + theme(text = element_text(size = 15)) + scale_color_few() + geom_hline(yintercept = mean(wb.clean.num$FR.INR.RINR), linetype = 2, color = "deepskyblue3") + geom_point(aes(x = (2007:2017), wb.clean.num$FR.INR.RINR), color = "steelblue") + xlab("Tempo") + ylab("Taxa de juros real (%)") # mean in cyan


ggplotly(interest.plot) %>% layout(xaxis = list(dtick = 1, 
                                           tickmode = "linear"), font = list(
                                             family = "Helvetica",
                                             size = 20))


# Interest and Inflation, joint plot

i_r.plot <- ggplot(wb.clean.num) + geom_line(aes(x = (2007:2017), wb.clean.num$FP.CPI.TOTL.ZG, color = "Inflação")) + geom_line(aes(x = (2007:2017), wb.clean.num$FR.INR.RINR, color = "Juros")) + theme_few() + theme(text = element_text(size = 15)) + geom_point(aes(x = (2007:2017), wb.clean.num$FR.INR.RINR, color = "Juros")) + geom_point(aes(x = (2007:2017), wb.clean.num$FR.INR.RINR), color = "steelblue") + geom_point(aes(x = (2007:2017), y = wb.clean.num$FP.CPI.TOTL.ZG, color = "Inflação")) + labs(x = "Tempo", y = "Taxa (%)", color = "Variável")


ggplotly(i_r.plot) %>% layout(xaxis = list(dtick = 1, 
                                                tickmode = "linear"), font = list(
                                                  family = "Helvetica",
                                                  size = 20))



# GDP components: Demand (%)

gdp_demand.df <- data.frame(wb.clean.num$NY.GDP.MKTP.KD.ZG, wb.clean.num$NE.CON.PRVT.KD.ZG, wb.clean.num$NE.GDI.TOTL.KD.ZG,  wb.clean.num$NE.CON.GOVT.KD.ZG,  wb.clean.num$NE.EXP.GNFS.KD.ZG, wb.clean.num$NE.IMP.GNFS.KD.ZG)

names(gdp_demand.df) <- c("GDP","C", "I", "G", "X", "M")

gdp_demand.plot <- plot_ly(gdp_demand.df, x = ~(2007:2017), y = ~C, type = "bar", name = "Consumo")

gdp_demand.plot <- gdp_demand.plot  %>% add_trace(y = ~I, name = "Investimento") %>% add_trace(y = ~G, name = "Gastos do Governo") %>% add_trace(y = ~(X-M), name = "Exportações Líquidas")  %>% add_trace(y = ~GDP, type = "scatter", mode = "line", name = "PIB") %>% layout(xaxis = list(title = "Tempo", dtick = 1, tickmode = "linear"), font = list(family = "Helvetica", size = 20), yaxis = list(title = "Variação anual (%)"), barmode = "group", legend = list(title = list(text = "<b> Variável <b>")))

# Exports x Imports plot (%)

exp_imp.plot <- plot_ly(gdp_demand.df, x = ~(2007:2017), y = ~X, type = "bar", name = "Exportações") %>% add_trace(y = ~(-M), name = "Importações") %>% layout(xaxis = list(title = "Tempo", dtick = 1, tickmode = "linear"), font = list(family = "Helvetica", size = 20), yaxis = list(title = "Variação anual (%)"), barmode = "overlay", legend = list(title = list(text = "<b> Variável <b>")))

# GDP components: Demand (absolute)

gdp_demand_abs.df <- data.frame(wb.clean.num$NY.GDP.MKTP.CD, wb.clean.num$NE.CON.PRVT.CD, wb.clean.num$NE.GDI.TOTL.CD,  wb.clean.num$NE.CON.GOVT.CD,  wb.clean.num$NE.EXP.GNFS.CD,  wb.clean.num$NE.IMP.GNFS.CD)

names(gdp_demand_abs.df) <- c("GDP","C", "I", "G", "X", "M")

gdp_demand_abs.plot <- plot_ly(gdp_demand_abs.df, x = ~(2007:2017), y = ~C, type = "bar", name = "Consumo")

gdp_demand_abs.plot <- gdp_demand_abs.plot  %>% add_trace(y = ~I, name = "Investimento") %>% add_trace(y = ~G, name = "Gastos do Governo") %>% add_trace(y = ~(X-M), name = "Exportações Líquidas")  %>% add_trace(y = ~wb.clean.num$NY.GDP.MKTP.CD, type = "scatter", mode = "line", name = "PIB") %>% layout(xaxis = list(title = "Tempo", dtick = 1, tickmode = "linear"), font = list(family = "Helvetica", size = 20), yaxis = list(title = "USD (Valores de 2017)"), barmode = "stack", legend = list(title = list(text = "<b> Variável <b>")))

# Exports x Imports plot (absolute)

exp_imp_abs.plot <- plot_ly(gdp_demand_abs.df, x = ~(2007:2017), y = ~X, type = "bar", name = "Exportações") %>% add_trace(y = ~(-M), name = "Importações") %>% layout(xaxis = list(title = "Tempo", dtick = 1, tickmode = "linear"), font = list(family = "Helvetica", size = 20), yaxis = list(title = "USD (Valores de 2017)"), barmode = "overlay", legend = list(title = list(text = "<b> Variável <b>")))


# GDP components: supply-side (%)

gdp_supply.df <- data.frame(wb.clean.num$NY.GDP.MKTP.KD.ZG, wb.clean.num$NV.AGR.TOTL.KD.ZG, wb.clean.num$NV.IND.TOTL.KD.ZG, wb.clean.num$NV.SRV.TOTL.KD.ZG, wb.clean.num$NV.IND.MANF.KD.ZG)

names(gdp_supply.df) = c("GDP", "Agricultura", "Indústria", "Serviços", "Manufatura")

gdp_supply.plot <- plot_ly(gdp_supply.df, x = ~(2007:2017), y = ~Agricultura, type = "bar", name = "Agricultura") %>% add_trace(y = ~Indústria, name = "Indústria") %>% add_trace(y = ~Serviços, name = "Serviços") %>% add_trace(y = ~Manufatura, name = "Manufatura") %>% add_trace(y = ~GDP, name = "PIB", type = "scatter", mode = "line") %>% layout(xaxis = list(title = "Tempo", dtick = 1, tickmode = "linear"), font = list(family = "Helvetica", size = 20), yaxis = list(title = "Variação anual (%)"), barmode = "group", legend = list(title = list(text = "<b> Variável <b>"))) # Na % não da pra ver a pequena divergencia.

# GDP components: supply-side (absolute)

gdp_supply_abs.df <- data.frame(wb.clean.num$NY.GDP.MKTP.CD, wb.clean.num$NV.AGR.TOTL.CD, wb.clean.num$NV.IND.TOTL.CD, wb.clean.num$NV.SRV.TOTL.CD, wb.clean.num$NV.IND.MANF.CD)

names(gdp_supply_abs.df) = c("GDP", "Agricultura", "Indústria", "Serviços", "Manufatura")

gdp_supply_abs.plot <- plot_ly(gdp_supply_abs.df, x = ~(2007:2017), y = ~Agricultura, type = "bar", name = "Agricultura") %>% add_trace(y = ~Indústria, name = "Indústria") %>% add_trace(y = ~Serviços, name = "Serviços") %>% add_trace(y = ~Manufatura, name = "Manufatura") %>% add_trace(y = ~GDP, name = "PIB", type = "scatter", mode = "line") %>% layout(xaxis = list(title = "Tempo", dtick = 1, tickmode = "linear"), font = list(family = "Helvetica", size = 20), yaxis = list(title = "USD (Valores de 2017)"), barmode = "stack", legend = list(title = list(text = "<b> Variável <b>"))) # tá dando divergencia com GDP total. Falta algum componente, provavelmente negativo.


# Credit % GDP plot.

credit_gdp.df <- data.frame(wb.clean.num$FS.AST.DOMS.GD.ZS)

names(credit_gdp.df) = "Credit"

credit_gdp_world.df <- read_excel("C:/Users/William/Downloads/API_FS.AST.DOMS.GD.ZS_DS2_en_excel_v2_1353420.xls", 
                                  skip = 2)

credit_gdp_world.df_t = transpose(credit_gdp_world.df)

rownames(credit_gdp_world.df_t) = colnames(credit_gdp_world.df)
colnames(credit_gdp_world.df_t) = credit_gdp_world.df_t[1,]

wb.cred.clean = credit_gdp_world.df_t[(52:62),]

wb.cred.num = matrix(NA, nrow = 11, ncol = 264)

for (i in (1:11)) for (j in (1:264)) {
  
  wb.cred.num[i,j] = as.numeric(wb.cred.clean[i,j])
  
}

wb.cred.num = data.frame(wb.cred.num)

rownames(wb.cred.num) = rownames(wb.cred.clean)
colnames(wb.cred.num) = colnames(wb.cred.clean)

credit_gdp.plot <- plot_ly(wb.cred.num, x = ~(2007:2017), y = ~`South Africa`, type = "scatter", mode = "line", name = "África do Sul") %>% add_segments(y = 100, yend = 100, x = 2007, xend = 2017, showlegend = FALSE, line = list(color = 'rgb(0, 0, 0)', width = 4, dash = 'dash')) %>% add_trace(y = ~`Argentina`, name = "Argentina") %>% add_trace(y = ~`United States`, name = "Estados Unidos")  %>% add_trace(y = ~`Chile`, name = "Chile") %>% add_trace(y = ~`Mexico`, name = "México") %>% layout(xaxis = list(title = "Tempo", dtick = 1, tickmode = "linear"), font = list(family = "Helvetica", size = 20), yaxis = list(title = "Crédito fornecido pelo setor financeiro (% PIB)"), barmode = "stack", legend = list(title = list(text = "<b> País <b>"))) 

















                   