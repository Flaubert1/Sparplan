```{r}
# versuch mit Liste
# erstelle leere Liste  
q <- c()
#q <- integer(14)

# erstelle matrizen
matrix <- data.frame(matrix(NA, nrow = years_save*12, ncol = length(years)-years_save+1))
matrix_2 <- data.frame(matrix(NA, nrow = years_save*12, ncol = length(years)-years_save+1))

for (i in years) {
  
  p <- dax_1 %>%
    filter(year >= i  & year <= i+years_save-1) %>%
    mutate(anteil = monthly_rate/adjusted) %>%
    mutate(anteil_cumsum = cumsum(anteil)) %>%
    mutate(wert = anteil_cumsum*adjusted) %>%
    mutate(ansparen = monthly_rate) %>%
    mutate(ansparen_cumsum = cumsum(ansparen))
  
  if (nrow(p) < years_save*12) next
  
  matrix[,i-i_date] <- p[,9]
  matrix_2[,i-i_date] <- p[,2]
  
  #erstelle Liste mit letzten Werten der verschiedenen Datenreihen
  q[i-i_date] <- p[nrow(p),9]
  #unlist die Liste, um Vektor zu erhalten
  q <- unlist(q, use.names = F)
  
}


# erstelle Farbpalette
pal <- got(5, option = "Jon_Snow")

for (i in years) {
  
  p <- dax_1 %>%
    filter(year >= i  & year <= i+years_save-1) %>%
    mutate(anteil = monthly_rate/adjusted) %>%
    mutate(anteil_cumsum = cumsum(anteil)) %>%
    mutate(wert = anteil_cumsum*adjusted) %>%
    mutate(ansparen = monthly_rate) %>%
    mutate(ansparen_cumsum = cumsum(ansparen))
  
  if (nrow(p) < years_save*12) next
  
  y <- ggplot(p)+
    geom_ribbon(aes(x = date, ymax = wert, ymin = ansparen_cumsum), fill = pal[5], alpha = .8)+
    geom_dl(aes(label = round(last(wert), 0), x = date, y = wert), method = list("last.qp"), color = pal[5])+
    
    geom_ribbon(aes(x = date, ymax = ansparen_cumsum, ymin = 0), fill = pal[1], alpha = .5)+
    geom_dl(aes(label = round(last(ansparen_cumsum), 0), x = date, y = ansparen_cumsum), method = "last.qp", color = pal[1])+
    
    theme_classic()+
    labs(title = paste0("Entwicklung von ", i, " bis ", i+years_save-1), x = "", y = "", caption = "Daten: https://finance.yahoo.com; Datenanalyse: jt")+
    scale_y_continuous(limits = c(0, max(matrix)), labels = dollar_format(suffix = " €", prefix = ""))
  
  # print(y)
  
  fp <- file.path(ansparen_dir_out, paste0(i, ".png"))
  
  ggsave(plot = y, 
         filename = fp, 
         device = "png",
         width = 20, height = 16, units = "cm")
}

# how to print all lines in one plot?
# https://github.com/nmaggiulli/of-dollars-and-data/blob/master/analysis/0001_simulate_hedge_fund_returns.R
```


```{r plot alle jahre 2, eval=FALSE, include=FALSE, fig.height = 4.7, fig.width = 6, out.width = "100%"}

pal <- got(1, option = "Jon_Snow")

## plot long dataframe
ggplot(t_4, aes(x = date, y = value, color = yrs))+
  geom_line(size=1)+
  
  geom_hline(yintercept = min(q), linetype = "dashed", color = pal)+
  geom_text(aes(x = first(date), y = min(q), label = paste0(round(min(q), 0), " € Min Ertrag")), vjust = -1, hjust = -0.1, color = pal, check_overlap = T, show.legend = F)+
  
  geom_hline(yintercept = max(q), linetype = "dashed", color = pal)+
  geom_text(aes(x = first(date), y = max(q), label = paste0(round(max(q), 0), " € Max Ertrag")),  vjust = 1.5, hjust = -0.1, color = pal, check_overlap = T, show.legend = F)+
  
  geom_hline(yintercept = mean(q), linetype = "dashed", color = pal)+
  geom_text(aes(x = first(date), y = mean(q), label = paste0(round(mean(q), 0), " € Durchschn. Ertrag")),  vjust = -1, hjust = -0.1, color = pal, check_overlap = T, show.legend = F)+
  
  geom_hline(yintercept = total_save, linetype = "dotted", color = pal)+
  geom_text(aes(x=first(date), y = total_save, label = paste0(total_save, " € Ansparen")),vjust = -1, hjust = -0.1, color = pal, check_overlap = T, show.legend = F)+
  
  scale_color_got(discrete = T, option = "Jon_Snow")+
  labs(title = "Alle Sparpläne in einer Visualisierung", x = "", y = "", color = "",  caption = "Daten: https://finance.yahoo.com; Datenanalyse: jt")+
  theme_classic()+
  theme(legend.position = "bottom",
        legend.direction = "horizontal")

```

```{r}
datatable(t_5, caption =  "Durchschn., Max und Min Performance", style = "bootstrap", rownames = F, options = list(dom = "t"))

datatable(t_6, caption =  "Entwicklung der verschiedenen Investitionszeiträume",  style = "bootstrap4", rownames = F, options = list(dom = "t"))
```


# % Zugewinn der benötigt wird, um % Verlust auszugleichen.
# Idee von: https://ofdollarsanddata.com/10-investing-lessons-from-2020/
```{r}
df <- df %>%
  mutate(c2 = 100-X1.100) %>%
  mutate(c3 = ((100/c2)-1)*100)
```

```{r}
df %>%
  filter(c3 <= 100) %>%
  ggplot()+
  geom_line(aes(X1.100,c3)) +
  scale_x_continuous(limits = c(0, 100)) +
  theme_classic()

```