nat_acct <- read_excel("Kbp6NA-June2020.xlsx")

gdp_demand.df = data.frame(as.character(nat_acct$...1), nat_acct$KBP6006D, nat_acct$KBP6007D, nat_acct$KBP6008D, nat_acct$KBP6009D, nat_acct$KBP6010D, nat_acct$KBP6011D, nat_acct$KBP6013D, nat_acct$KBP6014D)

names(gdp_demand.df) = c("index", "GDP", "C", "G", "I", "Inventories", "Residual", "X", "M")

gdp_demand.plot <- plot_ly(gdp_demand.df[189:229,], x = ~index, y = ~C, type = "bar", name = "Consumo")

gdp_demand.plot %>% add_trace(y = ~I, name = "Investimento") %>% add_trace(y = ~G, name = "Gastos do Governo") %>% add_trace(y = ~X, name = "Exportações") %>% add_trace(y = ~(-M), name = "Importações") %>% layout(xaxis = list(title = "Tempo", dtick = 1, tickmode = "linear"), font = list(family = "Helvetica", size = 20), yaxis = list(title = "USD (Valores de 2017)"), barmode = "stack") %>% add_trace(y = ~wb.clean.num$NY.GDP.MKTP.CD, type = "scatter", mode ="line", name = "PIB")
