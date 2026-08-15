---
title: "Retail Analysis"
output:
  pdf_document: default
  html_document: default
date: "2026-08-11"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(readr)
library(tidyverse)
sales <- read.csv("/Users/elisakim/Downloads/RetailSalesAnalysis/datasets/sales data-set.csv")
features <- read.csv("/Users/elisakim/Downloads/RetailSalesAnalysis/datasets/Features data set.csv")
stores <- read.csv("/Users/elisakim/Downloads/RetailSalesAnalysis/datasets/stores data-set.csv")

```
## Introduction
This project analyzes historical retail sales from February of 2010 to November of 2012 and the different factors associated with changes in weekly retail sales. This specific data set focuses on holidays, markdowns, CPI (consumer price index), and other economic conditions related to consumer spending. This leads to the main research question:

What factors are associated with changes in weekly retail sales, and how do holidays and economic conditions influence retail sales?

The analysis combines three data sets: store, features, and sales. he Sales data set contains weekly sales by store and department. The Features data set contains information about economic conditions, weather, fuel prices, and promotional markdowns. The Stores data set contains information about store type and size.


### Data Overview

```{r data overview}
head(sales)
head(features)
head(stores)

str(sales)
str(features)
str(stores)

summary(sales)
summary(features)
summary(stores)
```
The Sales data set contains five variables. The weekly sales range from the negatives to over $600,000 with the average being close to $16,000. The data set also contains both holiday and non-holiday observations. Negative sales values are present in the data set and should be considered when interpreting the results because they may represent returns or other adjustments rather than ordinary retail sales.

The Features data set includes the most variables: Store (identification number), Date, Temperature (during the week), Fuel Price, Markdowns (up to five promotional markdowns during the week), CPI (Consumer Price Index), Unemployment Rate, and whether it was a holiday week. This data set is particularly important for the economic portion of this project because it includes CPI and unemployment whcih can be used to investigate whether other outside economic conditions are associated with the weekly retail spending. 

The Stores data set contains 45 stores and identifies the category of the store, while Size measures the physical size of the store. 


```{r data merge}
retail <- sales %>% left_join( features, by = c("Store", "Date", "IsHoliday") ) %>% left_join( stores, by = "Store" )
```
After combining the data sets, the new retail data set contains sales information along with economic conditions, promotional information, and store characteristics.

```{r data cleaning}
retail$Date <- as.Date(retail$Date, "%d-%m-%Y")
colSums(is.na(retail))

retail <- retail %>% mutate( MarkDown1 = replace_na(MarkDown1, 0), MarkDown2 = replace_na(MarkDown2, 0), MarkDown3 = replace_na(MarkDown3, 0), MarkDown4 = replace_na(MarkDown4, 0), MarkDown5 = replace_na(MarkDown5, 0) )

retail <- retail %>% mutate(TotalMarkdown = MarkDown1 + MarkDown2 + MarkDown3 + MarkDown4 + MarkDown5) 
summary(retail$TotalMarkdown)
```
Checking through the data to convert the Date variable so that it can be used for time-based analysis and, if displayed incorrectly, it is fixed. Also, going through the variables in the Features data set to see if there are any missing variables, especially for the markdown variables. We put a 0 instead of NA which indicates that there was no markdown recorded that week.  

The retail variable will be used later to investigate whether larger promotional markdowns are associated with higher weekly sales. 

### Plot Data Analysis

```{r total sales}
store_sales <- retail %>%
  group_by(Store) %>%
  summarize(TotalSales = sum(Weekly_Sales))
ggplot(store_sales,
       aes(x = reorder(Store, TotalSales),
           y = TotalSales)) +
  geom_col(fill = "#dce9f3") + 
  coord_flip() +
  labs(
    title = "Total Sales by Store",
    x = "Store",
    y = "Total Sales"
  ) +
  theme_minimal()
```

The first analysis examines the differences in total sales across stores. 

```{r top ten}
store_sales <- retail %>%
  group_by(Store) %>%
  summarize(TotalSales = sum(Weekly_Sales, na.rm = TRUE)) %>%
  arrange(desc(TotalSales)) %>%
  slice(1:10)

ggplot(store_sales,
       aes(x = reorder(as.factor(Store), TotalSales),
           y = TotalSales)) +
  geom_col(fill = "#97a483") +
  coord_flip() +
  labs(
    title = "Top 10 Stores by Total Sales",
    x = "Store",
    y = "Total Sales ($)"
  ) +
  theme_minimal()
```

However, with there being 45 stores, the number is too grand for the graph to interpret. Therefore, the analysis focuses on the ten stores with the highest total sales.

This graph shows which 10 stores generated the highest total sales during the period covered by the data set. The graph shows that Store 20 generated the most sales with Store 4 coming behind as a close second.

The differences between stores may reflect differences in store size, location, customer demand, or store type. 


Are weekly sales higher during holiday weeks than during non-holiday weeks?
```{r holiday sales}
monthly_sales <- retail %>%
  mutate(Month = format(Date, "%Y-%m")) %>%
  group_by(Month) %>%
  summarize(Sales = sum(Weekly_Sales, na.rm = TRUE))

holiday_sales <- retail %>%
  group_by(IsHoliday) %>%
  summarize(
    AverageSales = mean(Weekly_Sales, na.rm = TRUE)
  )

ggplot(holiday_sales,
       aes(x = IsHoliday,
           y = AverageSales)) +
  geom_col(fill = "#e7d6bc") + 
  labs(
    title = "Average Weekly Sales: Holiday vs. Non-Holiday",
    x = "Holiday Week",
    y = "Average Weekly Sales ($)"
  ) +
  theme_minimal()
```

The IsHoliday variable identifies whether a particular week contains a major holiday.

The graph compares weekly sales during holiday and non-holiday weeks. It shows that holiday weeks have a higher average of weekly sales whcih suggests that major holidays are associated with increased retail activity. 

However, this comparison alone does not prove that holidays are the sole reason for causing higher sales. Other factors, such as promotions or seasonal patterns, may also contribute to the difference. 

### Sales Overtime
```{r sales overtime}
sales <- sales %>%
  mutate(Date = as.Date(Date, format = "%d/%m/%Y"))

features <- features %>%
  mutate(Date = as.Date(Date, format = "%d/%m/%Y"))

retail <- sales %>%
  left_join(
    features,
    by = c("Store", "Date", "IsHoliday")
  ) %>%
  left_join(
    stores,
    by = "Store"
  )

retail <- retail %>%
  mutate(
    Month = format(Date, "%Y-%m")
  )

monthly_sales <- retail %>%
  mutate(
    Month = format(Date, "%Y-%m")
  ) %>%
  group_by(Month) %>%
  summarize(
    AverageWeeklySales = mean(
      Weekly_Sales,
      na.rm = TRUE
    )
  )

ggplot(
  monthly_sales,
  aes(
    x = Month,
    y = AverageWeeklySales,
    group = 1
  )
) +
  geom_line(
    color = "#e37574",
    linewidth = 1.2
  ) +
  geom_point(
    color = "#e37574",
    size = 2
  ) +
  labs(
    title = "Average Weekly Retail Sales by Month",
    x = "Month",
    y = "Average Weekly Sales ($)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )
```

This analysis helps identify seasonal patterns in retail sales and whether certain periods experience higher or lower sales. The graph shows that the average weekly says are relatively steady except for sudden peaks around November and December months and drops during January. 

This suggests that the previous graph on how holiday weeks result in higher average weekly sales supports this graph because of the peaks in the line graph. However, the steep drops in the line graph can suggest the results in the holiday shopping because of factors like less customer traffic or drops in consumer spending. 

### Economic Factors
The Features data set allows a deeper dive in the sales analysis and investigate economic factors. The main factors include: CPI (consumer price index), Unemployment (unemployment rate), and Fuel Price (average price of fuel associated with the observation).

The main goal is to determine whether these variables are associated with differences in weekly retail sales. 

***Unemployment***

Since the data set contains many observations, a random sample can be used for visualization so that the graph is easier to interpret.

```{r unemployment}
set.seed(123)

sample_data <- retail %>%
  sample_n(5000)



ggplot(
  sample_data,
  aes(
    x = Unemployment,
    y = Weekly_Sales
  )
) +
  geom_point(
    color = "#18fbff",
    alpha = 0.4
  ) + 
  geom_smooth(
    method = "lm",
    color = "#20b2aa"
  ) +
  labs(
    title = "Unemployment and Weekly Retail Sales",
    x = "Unemployment Rate",
    y = "Weekly Sales ($)"
  ) +
  theme_minimal()

cor( retail$Unemployment, retail$Weekly_Sales, use = "complete.obs" )
```

This analysis measures association rather than causation. The correlation measures the strength and direction of the relationship between unemployment and weekly sales. 

Since the correlation is negative, it indicates that higher unemployment is associated with lower weekly sales.

***CPI***

This analysis examines whether changes in Consumer Price Index are associated with differences in weekly retail sales.

A relationship between CPI and sales could reflect changes in consumer prices and spending patterns. 

```{r CPI}
ggplot(
  sample_data,
  aes(
    x = CPI,
    y = Weekly_Sales
  )
) +
  geom_point(
    color = "#c6c6e8",
    alpha = 0.4
  ) + 
  geom_smooth(
    method = "lm",
    color = "#9896bb"
  ) +
  labs(
    title = "CPI and Weekly Retail Sales",
    x = "Consumer Price Index",
    y = "Weekly Sales ($)"
  ) +
  theme_minimal()



cor(
  retail$CPI,
  retail$Weekly_Sales,
  use = "complete.obs"
)
```

The scatter plot shows that the correlation is extremely close to zero (horizontal line) which indicates that there is no linear relationship between a store's/region CPI and its weekly sales figures. This means that changes in CPI does not predict lower or higher weekly sales in this data set.

***Fuel Prices***

Fuel prices may influence consumer spending because transportation costs affect household budgets and travel behavior. 

```{r fuel prices}
ggplot( 
  sample_data, aes( x = Fuel_Price, y = Weekly_Sales 
                    ) 
  ) + geom_point( color = "#ff85a2", alpha = 0.4 
  ) + 
  geom_smooth( method = "lm", color = "#2ec4b6" ) + 
  labs( title = "Fuel Prices and Weekly Retail Sales", 
        x = "Fuel Price", 
        y = "Weekly Sales ($)" 
        ) + 
  theme_minimal()
```

The scatter plot shows that the completely flat regression line (teal) confirms that there is no linear correlation between fuel prices and weekly retail sales volume. All of the sales remain heavily concentrated below $50,000 no matter the fuel price. 

***Markdowns***

The data set contains five different markdown variables. A combined markdown variable was created to examine the overall amount of markdown activity. 

```{r markdown}

retail <- retail %>% 
  mutate( 
    TotalMarkdown = 
      MarkDown1 + 
      MarkDown2 + 
      MarkDown3 + 
      MarkDown4 + 
      MarkDown5 
  )

markdown_sample <- retail %>% filter(TotalMarkdown > 0) %>% 
  sample_n(5000)

ggplot(
  markdown_sample,
  aes(
    x = TotalMarkdown,
    y = Weekly_Sales
  )
) +
  geom_point(
    color = "#dcf6e6",
    alpha = 0.3
  ) + 
  geom_smooth(
    method = "lm",
    color = "#c7f3d0"
  ) +
  labs(
    title = "Promotional Markdowns and Weekly Sales",
    x = "Total Markdown ($)",
    y = "Weekly Sales ($)"
  ) +
  theme_minimal()



cor(
  retail$TotalMarkdown,
  retail$Weekly_Sales,
  use = "complete.obs"
)

```

The upward-sloping trend line (with a shaded confidence interval band) shows that higher markdowns correspond to a negligible average increase in weekly sales. 

***Store Size and Sales***

```{r store size and sales}
store_analysis <- retail %>%
  group_by(Store) %>%
  summarize(
    TotalSales = sum(Weekly_Sales, na.rm = TRUE)
  ) %>%
  left_join(
    stores,
    by = "Store"
  )



ggplot(
  store_analysis,
  aes(
    x = Size,
    y = TotalSales
  )
) +
  geom_point(
    color = "#dd7863",
    size = 3
  ) + 
  geom_smooth(
    method = "lm",
    color = "#e9b447"
  ) +
  labs(
    title = "Store Size and Total Sales",
    x = "Store Size",
    y = "Total Sales ($)"
  ) +
  theme_minimal()

```

According to the scatter plot, there is a clear, strong positive linear relationship between store size and total sales. Larger stores consistently generate significantly higher cumulative sales. 

Variances in total sales widen as store size grow. Stores with a size around 200000 square feet show a wider spread in performance compared to smaller stores.

***Store Type***

Comparing store types can help determine whether certan categories of stores have consistently different average sales.

```{r store type}
type_sales <- retail %>%
  group_by(Type) %>%
  summarize(
    AverageSales = mean(
      Weekly_Sales,
      na.rm = TRUE
    )
  )

ggplot(
  type_sales,
  aes(
    x = Type,
    y = AverageSales
  )
) +
  geom_col(fill = "#9ce9cb") +
  labs(
    title = "Average Weekly Sales by Store Type",
    x = "Store Type",
    y = "Average Weekly Sales ($)"
  ) +
  theme_minimal()
```

Store Type A generates the highest average weekly sales at roughly $20,000 followed by Type B at $12,200 and Type C with the lowest average $9,500.

## Predictive Modeling

The exploratory analysis identifies several variables that may be associated with weekly sales. A linear regression model can be used to evaluate several predictors simultaneously. 

```{r model}
model <- lm( 
  Weekly_Sales ~ 
    IsHoliday + 
    Temperature + 
    Fuel_Price + 
    CPI + 
    Unemployment + 
    TotalMarkdown, 
  data = retail 
  ) 
summary(model)

model_results <- data.frame(
  Variable = names(coef(model)),
  Coefficient = coef(model),
  P_Value = summary(model)$coefficients[, 4]
)

model_results
```

The model shows how the other variables are associated with sales while holding other variables constant. 

The Temperature coefficient shows that with every 1 degree of Fahrenheit increase, sales increase by about $58. With the Fuel Price, sales decrease by about $1637 every dollar increase in Fuel Price. Also, when the Unemployment rate increases by every one percent, sales decrease about $708. Holiday weeks are associated with about $805 lower sales, holding the other variables constant. $0.1048 increase per $1 increase in total markdown.

The Holiday insight is interesting because the previous graph showed that Holidays tend to drive better sales. This differs from a simple comparison of holiday and non-holiday sales because the regression accounts for several other factors that may influence sales.

The model only has R^2 of 0.01095, so these variables explain only about 1.1% of the variation in weekly sales. This shows that the economic and promotional variables available in this data set do not explain much of the store-level variation in weekly sales. 

### Conclusion

This analysis examined the factors associated with weekly retail sales by combining sales data with information on economic conditions, promotional markdowns, holidays, and store characteristics. The exploratory analysis showed that sales vary across stores and that holiday periods, store size, and store type can be useful when understanding differences in retail performance.

The linear regression model found that all of the included variables were statistically significant, but the model explained only about 1.1% of the variation in weekly sales. This suggests that many other factors not included in the model, such as individual store and department characteristics, may play a much larger role in determining sales. 

Working in retail, weekly sales are influenced more by employee behavior and store level operations then economic conditions. The way employees interact with the customer shapes their experiences and ultimately influences sales. Employee engagement, customer service, product knowledge, marketing displays, checkout efficiency, merchandising, and how well employees respond to customer needs can all affect how customers interact with a store. These factors are not captured in the current data set, but they can have a meaningful impact on daily and weekly performance.

These factors are difficult to capture in the available data set because it records the outcome of sales rather than behavior and the individual interactions that contribute to those sales. Two stores may experience the same economic conditions, have the same markdowns, and sell the same products, but still generate different sales because of differences in employee engagement, staffing, and how effectively employees communicate current promotions or selling priorities.

Overall, this project demonstrates how data science can be used to explore economic and business questions using real-world retail data. By combining exploratory data analysis, visualization, correlation analysis, and regression modeling, the analysis provides a better understanding of the factors associated with retail sales while also highlighting the limitations of using a small set of economic and promotional variables to predict complex consumer spending patterns.
