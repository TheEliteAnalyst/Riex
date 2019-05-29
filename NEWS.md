# Riex 1.0.1

![](/Users/Myriam/Pictures/Riex_Logo_Final.PNG)

I'm excited to announce that `Riex` package is now available on [CRAN!](https://cran.r-project.org/) and you can install using:

```r
install.packages("Riex")
```
### **Introduction** ###

Main purpose of `RIEX` is to  efficiently and reliably retrieve stocks and market data via [`IEX Cloud API`](https://iexcloud.io/docs/api/). Platform is offered by Investors Exchange Group - [IEX Group](https://iextrading.com/). To subscribe, visit: [IEX Cloud](https://iexcloud.io/)

First `Riex` release includes basic functions that retrieve data in a standard format - *e.g.* Data frames. Future releases are expected to:

- Leverage `R` capabilities including existing packages to effectively provide financial and statistical analysis as well as visualization.
- Use `Reticuate` - an `R` package that interface with Python modules, classes and functions to further enhance the capability to process and analyze data.
- Optimize code to retrieve data for multiple symbols.


### **Get Started** ###

**Secret Key/ Token** is required for all API callS. It is available via Account Console and assigned the variable **sk** in the documentation. e.g. sk <- "sk_...".
   
Keep your **Secret Token** safe. Your **Secret Token** can make any API call on behalf of your account, including changes that may impact billing such as enabling pay-as-you-go charges. 

For more details about best practices to store and encrypt **Secret Key/ Token** check [Managing secrets](https://cran.r-project.org/web/packages/httr/vignettes/secrets.html) *by Hadley Wickham*

Load Package
```r
library(Riex)
```

Assign valid values to key parameters:

- sk <- "[SECRET KEY]". *e.g.* sk <- "sk_..."

- x <- "TSLA"

- r <- "1y"

### **Examples** ###

**Account usage details**

iex.key.usage()

```r
usage <- iex.key.usage(sk)
print(usage)

```

**Time series - OHLC**

```r
TSLA <- iex.chart(x, r, sk)

```

**Use `quantmod` package for visualization**

```r
library(quantmod)

```
Generate Barchart

```r
barChart(TSLA)
```
![](/Users/Myriam/Pictures/TSLA_Rplot_1.PNG)

To check available themes to customize  visualization

```r
names(quantmod:::.chart.theme)
```
To apply a different Theme

```r
barChart(TSLA, theme="white")
```
![](/Users/Myriam/Pictures/TSLA_Rplot_2.PNG)

```{r}
chartSeries(TSLA)
```
![](/Users/Myriam/Pictures/TSLA_Rplot_3.PNG)

**To create an interactive chart for multiple stocks**

*Credit*: [Chris Bow](https://medium.com/datadriveninvestor/interactive-time-series-plots-in-r-bceff3a7bb04)

1. Retrieve OHLC data

```r
TSLA <- iex.chart("TSLA", r, sk)
```
```r
GM <- iex.chart("GM", r, sk)
```
To view top records in time series for TSLA

```r
head(TSLA)
```
![](/Users/Myriam/Pictures/TSLA_OHLC_head.PNG)

2. Merge data for both stocks
```r
stocks <- cbind(TSLA$Close, GM$Close)
```
Change columns names to specify close by symbol
```r
colnames(stocks) <- paste0(c("TSLA", "GM"), ".Close")
```

3. Load the following packages

```r
library(dygraphs)
```
```r
library(dplyr)
```
4. Get the first and last date
```r
start(stocks)
```
[1] "2018-05-16"
```r
end(stocks)
```
[1] "2019-05-15"

5. Setup chart

```r
stocks_chart <- dygraph(stocks, main = "TSLA & GM Closing Price - 1 Year") %>%
  dySeries("GM.Close", axis = "y2") %>% 
  dyAxis("y", 
         label = "TSLA") %>%
  dyAxis("y2", 
         label = "GM",
         valueRange = c(20, 50),
         independentTicks = TRUE) %>%
  dyRangeSelector(dateWindow = c("2018-05-16", "2019-05-15")) %>%
  dyRoller()
```
6. Display Chart in Viewer pane

```r
stocks_chart
```
![](/Users/Myriam/Pictures/TSLA_GM.PNG)

7. To save in html format and display in browser, following are the steps:

Install and load `htmlwidgets`
```r
install.packages("htmlwidgets")
```
```r
library(htmlwidgets)
```
```r
saveWidget(stocks_chart, 
           "stocks_chart.html",
           selfcontained = TRUE)
```

### **Conclusion** ###

[IEX Group](https://iextrading.com/about/) mission **"We're building fairer markets."** is the motivation for this project and certainly hope to continue supporting their effort and bring value to the community. There has been significant enhancements since launching [`IEX Cloud API`](https://iexcloud.io/docs/api/) which makes this project really exciting!!

Your feedback and suggestions will be key to continuously improve `Riex` so it becomes relevant and practical to use. Please provide feedback and report any issues on [GitHub](https://github.com/TheEliteAnalyst/Riex).






