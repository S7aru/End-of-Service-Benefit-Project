End-of-Service Benefit Project
================
Abdulaziz Alsahhar
2026-06-10

# Objective

This project develops an **End-of-Service Benefit (EOSB) model** under
IAS-19 using the **Projected Unit Credit (PUC)** actuarial method in R. [To download the Excel model click here](https://raw.githubusercontent.com/S7aru/End-of-Service-Benefit-Project/main/model.xlsm)

The model workflow:  
- Import employee data from Excel (**ID**, **Date of Birth**, **Date of
Hire**, **Current Salary**)  
- Apply actuarial assumptions provided by the user (**Salary Growth
Rate**, **Discount Rate**, **Withdrawal Multiple**)  
- Project employees’ future salaries, service, and benefit accruals  
- Apply mortality and withdrawal probabilities from a two-decrement
table  
- Calculate the **Present Value of Defined Benefit Obligation
(PVDBO)**  
- Perform **sensitivity analysis** on key assumptions  
- Compute the **effective duration** of liabilities

## Data & Setup

### Importing Data

``` r
start <- Sys.time()
library(tidyverse)
library(readxl)
library(janitor)
library(tibble)
options(scipen = 999)

employee_data <- read_excel("EOSB Project.xlsm", sheet = 1)
employee_data <- employee_data %>% 
  select(c(2:5)) %>%
  clean_names() %>%
  mutate(date_of_birth = as.Date(date_of_birth), date_of_hire = as.Date(date_of_hire)) %>%
  filter(!is.na(employee_id))
```

- 
- 
- 

``` r
employee_data
```

    ## # A tibble: 50 × 4
    ##    employee_id date_of_birth date_of_hire salary
    ##          <dbl> <date>        <date>        <dbl>
    ##  1           1 1983-09-20    2016-02-13     3559
    ##  2           2 1956-01-24    2019-06-16     9166
    ##  3           3 1987-03-28    2005-03-12     3578
    ##  4           4 2000-01-17    2024-09-30     8232
    ##  5           5 1958-11-29    2015-11-07    21031
    ##  6           6 1966-01-26    1984-08-09    26864
    ##  7           7 2004-06-21    2022-02-03     5826
    ##  8           8 1992-12-10    2010-07-04    21405
    ##  9           9 1980-02-09    2012-11-18    25898
    ## 10          10 1970-07-07    2015-11-15     6945
    ## # ℹ 40 more rows

### - Assumptions

``` r
discount_rate <- 0.05
salary_growth <- 0.05
withdrawal_multiple <- 1
valuation_date = as.Date("2024-12-31")
```

### - Decrement Table (Mortality & Withdrawal)

``` r
decrement_table <- read_excel("EOSB Project.xlsm", sheet = "Decrements")
decrement_table <- decrement_table %>%
  select(7,8) %>%
  slice(-1)  %>%
  rename(mortality = 1, withdrawal = 2) %>%
  mutate(mortality=as.numeric(mortality),withdrawal=as.numeric(withdrawal)) %>%
  mutate(withdrawal = withdrawal*withdrawal_multiple) %>%
  mutate(age = c(20:65),.before = mortality) #To add an age column 
```

- 
- 
- 

``` r
decrement_table
```

    ## # A tibble: 46 × 3
    ##      age mortality withdrawal
    ##    <int>     <dbl>      <dbl>
    ##  1    20  0.000791     0.022 
    ##  2    21  0.000747     0.0216
    ##  3    22  0.000708     0.0212
    ##  4    23  0.000671     0.0208
    ##  5    24  0.000639     0.02  
    ##  6    25  0.000611     0.0192
    ##  7    26  0.000588     0.0188
    ##  8    27  0.00057      0.0176
    ##  9    28  0.000558     0.0168
    ## 10    29  0.000553     0.0156
    ## # ℹ 36 more rows

### - Normal Retirement Age

- As per **GOSI regulations (effective 3 July 2024)**, retirement age
  depends on the employee’s age on that date.

``` r
normal_retirement_age <- read_excel("EOSB Project.xlsm", sheet = "Decrements")
normal_retirement_age <- normal_retirement_age %>% 
  select(Age,Retirement) %>%
  slice(c(1:21)) %>%
  clean_names()
```

- 
- 
- 

``` r
normal_retirement_age
```

    ## # A tibble: 21 × 2
    ##      age retirement
    ##    <dbl>      <dbl>
    ##  1    28       65  
    ##  2    29       64.5
    ##  3    30       64.2
    ##  4    31       63.8
    ##  5    32       63.5
    ##  6    33       63.2
    ##  7    34       62.8
    ##  8    35       62.5
    ##  9    36       62.2
    ## 10    37       61.8
    ## # ℹ 11 more rows

We add **current age**, **years of service**, and **retirement age
assumption** (based on the previous table: normal_retirement_age).
Unnecessary columns (date of birth, date of hire) are removed

``` r
employee_data <- employee_data %>%
  mutate(current_age = as.numeric((valuation_date-date_of_birth)/365.25),.before = date_of_birth) %>%
  mutate(current_service = as.numeric((valuation_date-date_of_hire)/365.25),.before = date_of_birth) %>%
  mutate(age_at_gosi_laws = as.numeric(as.Date("2024-7-3")-date_of_birth)/365.25,.before = salary) %>%
  mutate(age_at_gosi_laws = trunc(pmin(pmax(age_at_gosi_laws,28),48))) %>%
  left_join(normal_retirement_age,by = c("age_at_gosi_laws"="age")) %>%
  select(-age_at_gosi_laws) %>%
  relocate(retirement,.before = salary) %>%
  select(-c(date_of_hire,date_of_birth))

employee_data
```

    ## # A tibble: 50 × 5
    ##    employee_id current_age current_service retirement salary
    ##          <dbl>       <dbl>           <dbl>      <dbl>  <dbl>
    ##  1           1        41.3           8.88        60.8   3559
    ##  2           2        68.9           5.54        58     9166
    ##  3           3        37.8          19.8         61.8   3578
    ##  4           4        25.0           0.252       65     8232
    ##  5           5        66.1           9.15        58    21031
    ##  6           6        58.9          40.4         58    26864
    ##  7           7        20.5           2.91        65     5826
    ##  8           8        32.1          14.5         63.8  21405
    ##  9           9        44.9          12.1         59.5  25898
    ## 10          10        54.5           9.13        58     6945
    ## # ℹ 40 more rows

## Future Projections

We project **age**, **service**, and **salary** for each employee until
retirement.

``` r
working_data <- employee_data %>%
  crossing(year = c(0:45)) %>%
  mutate(proj_year = year) %>%
  mutate(age_proj = current_age + year) %>%
  mutate(service_proj = current_service + year) %>%
  mutate(salary_proj = salary*(1+salary_growth)^(year+1)) %>%
  filter(if_else(current_age >= retirement,year == 0,age_proj<=retirement+1)) 

first_service <-working_data %>%
  group_by(employee_id) %>%
  summarise(first_service = first(service_proj)) 

first_age <-working_data %>%
  group_by(employee_id) %>%
  summarise(first_age = first(age_proj)) 

working_data <- working_data %>%
  left_join(first_service, by = "employee_id") %>%
  left_join(first_age, by = "employee_id") %>%
  mutate(age_proj = if_else(age_proj >= retirement & proj_year>0,true = retirement,false = age_proj)) %>%
  mutate(service_proj = if_else(age_proj==retirement,true = retirement-first_age + first_service,false = service_proj)) %>%
  mutate(salary_proj = if_else(age_proj==retirement,true = lag(salary_proj,n=1),false = salary_proj)) %>%
  select(employee_id,proj_year,age_proj,salary_proj,service_proj,retirement)
working_data
```

    ## # A tibble: 722 × 6
    ##    employee_id proj_year age_proj salary_proj service_proj retirement
    ##          <dbl>     <int>    <dbl>       <dbl>        <dbl>      <dbl>
    ##  1           1         0     41.3       3737.         8.88       60.8
    ##  2           1         1     42.3       3924.         9.88       60.8
    ##  3           1         2     43.3       4120.        10.9        60.8
    ##  4           1         3     44.3       4326.        11.9        60.8
    ##  5           1         4     45.3       4542.        12.9        60.8
    ##  6           1         5     46.3       4769.        13.9        60.8
    ##  7           1         6     47.3       5008.        14.9        60.8
    ##  8           1         7     48.3       5258.        15.9        60.8
    ##  9           1         8     49.3       5521.        16.9        60.8
    ## 10           1         9     50.3       5797.        17.9        60.8
    ## # ℹ 712 more rows

### Benefit Calculation Methedology

According to Saudi Labor Law (GOSI-based EOSB rules):

- Half monthly salary per year of service for the first 5 years

- One monthly salary per year of service thereafter

For simplicity, all exits are assumed to be **contract terminations**.

``` r
working_data <- working_data %>%
  mutate(EOSB = if_else(service_proj >5, 5*0.5*salary_proj + (service_proj - 5)*salary_proj,service_proj*0.5*salary_proj))
working_data
```

    ## # A tibble: 722 × 7
    ##    employee_id proj_year age_proj salary_proj service_proj retirement   EOSB
    ##          <dbl>     <int>    <dbl>       <dbl>        <dbl>      <dbl>  <dbl>
    ##  1           1         0     41.3       3737.         8.88       60.8 23848.
    ##  2           1         1     42.3       3924.         9.88       60.8 28964.
    ##  3           1         2     43.3       4120.        10.9        60.8 34532.
    ##  4           1         3     44.3       4326.        11.9        60.8 40585.
    ##  5           1         4     45.3       4542.        12.9        60.8 47156.
    ##  6           1         5     46.3       4769.        13.9        60.8 54283.
    ##  7           1         6     47.3       5008.        14.9        60.8 62005.
    ##  8           1         7     48.3       5258.        15.9        60.8 70364.
    ##  9           1         8     49.3       5521.        16.9        60.8 79403.
    ## 10           1         9     50.3       5797.        17.9        60.8 89171.
    ## # ℹ 712 more rows

### Applying Mortality & Withdrawal

Probabilities are applied to weight future cashflows.

``` r
working_data <- working_data %>%
  mutate(truncated_age_proj = as.integer(pmin(pmax(20,trunc(age_proj)),65))) %>%
  select(-any_of(c("mortality", "withdrawal"))) %>%
  left_join(decrement_table,by = c("truncated_age_proj"="age")) %>%
  group_by(employee_id) %>%
  mutate(survival = lag(cumprod(1 - mortality - withdrawal),default = 1)) %>%
  ungroup() %>%
  mutate(mortality  = mortality*survival, withdrawal = withdrawal*survival) %>%
  select(-truncated_age_proj) 

working_data
```

    ## # A tibble: 722 × 10
    ##    employee_id proj_year age_proj salary_proj service_proj retirement   EOSB
    ##          <dbl>     <int>    <dbl>       <dbl>        <dbl>      <dbl>  <dbl>
    ##  1           1         0     41.3       3737.         8.88       60.8 23848.
    ##  2           1         1     42.3       3924.         9.88       60.8 28964.
    ##  3           1         2     43.3       4120.        10.9        60.8 34532.
    ##  4           1         3     44.3       4326.        11.9        60.8 40585.
    ##  5           1         4     45.3       4542.        12.9        60.8 47156.
    ##  6           1         5     46.3       4769.        13.9        60.8 54283.
    ##  7           1         6     47.3       5008.        14.9        60.8 62005.
    ##  8           1         7     48.3       5258.        15.9        60.8 70364.
    ##  9           1         8     49.3       5521.        16.9        60.8 79403.
    ## 10           1         9     50.3       5797.        17.9        60.8 89171.
    ## # ℹ 712 more rows
    ## # ℹ 3 more variables: mortality <dbl>, withdrawal <dbl>, survival <dbl>

### Probability-Weighted Cashflows

At each projection year:

- Before retirement: EOSB\*(P(Death)+P(Withdrawal))

- At retirement: EOSB\*P(Surviving to that year)

**Note:** Employees who have already reached or exceeded the normal
retirement age at the valuation date  
(e.g., an employee aged 70 who is still in service) are assumed to
**retire immediately**.

``` r
working_data <- working_data %>%
  mutate(undiscounted = if_else(age_proj >= retirement, false = EOSB*(mortality+withdrawal),true = EOSB*survival))
working_data
```

    ## # A tibble: 722 × 11
    ##    employee_id proj_year age_proj salary_proj service_proj retirement   EOSB
    ##          <dbl>     <int>    <dbl>       <dbl>        <dbl>      <dbl>  <dbl>
    ##  1           1         0     41.3       3737.         8.88       60.8 23848.
    ##  2           1         1     42.3       3924.         9.88       60.8 28964.
    ##  3           1         2     43.3       4120.        10.9        60.8 34532.
    ##  4           1         3     44.3       4326.        11.9        60.8 40585.
    ##  5           1         4     45.3       4542.        12.9        60.8 47156.
    ##  6           1         5     46.3       4769.        13.9        60.8 54283.
    ##  7           1         6     47.3       5008.        14.9        60.8 62005.
    ##  8           1         7     48.3       5258.        15.9        60.8 70364.
    ##  9           1         8     49.3       5521.        16.9        60.8 79403.
    ## 10           1         9     50.3       5797.        17.9        60.8 89171.
    ## # ℹ 712 more rows
    ## # ℹ 4 more variables: mortality <dbl>, withdrawal <dbl>, survival <dbl>,
    ## #   undiscounted <dbl>

### Discounting to Present Value

Assuming mid-year payment timing.

``` r
working_data <- working_data %>%
  left_join(first_age,by = "employee_id") %>%
  mutate(discounted = if_else(retirement==age_proj,true = undiscounted*(1+discount_rate)^-(age_proj -first_age),false = undiscounted*(1+discount_rate)^-(0.5+proj_year))) 
  working_data
```

    ## # A tibble: 722 × 13
    ##    employee_id proj_year age_proj salary_proj service_proj retirement   EOSB
    ##          <dbl>     <int>    <dbl>       <dbl>        <dbl>      <dbl>  <dbl>
    ##  1           1         0     41.3       3737.         8.88       60.8 23848.
    ##  2           1         1     42.3       3924.         9.88       60.8 28964.
    ##  3           1         2     43.3       4120.        10.9        60.8 34532.
    ##  4           1         3     44.3       4326.        11.9        60.8 40585.
    ##  5           1         4     45.3       4542.        12.9        60.8 47156.
    ##  6           1         5     46.3       4769.        13.9        60.8 54283.
    ##  7           1         6     47.3       5008.        14.9        60.8 62005.
    ##  8           1         7     48.3       5258.        15.9        60.8 70364.
    ##  9           1         8     49.3       5521.        16.9        60.8 79403.
    ## 10           1         9     50.3       5797.        17.9        60.8 89171.
    ## # ℹ 712 more rows
    ## # ℹ 6 more variables: mortality <dbl>, withdrawal <dbl>, survival <dbl>,
    ## #   undiscounted <dbl>, first_age <dbl>, discounted <dbl>

### Straight-Line Attribution (IAS-19)

Distribute discounted benefits across years of service. That is: $$
PVDBO = \text{Discounted Cashflow} \times \frac{\text{Service Years at Valuation}}{\text{Total Service Years at Projection}}
$$

``` r
working_data <- working_data %>%
  left_join(first_service, by = "employee_id") %>%
  mutate(PVDBO = if_else(service_proj > 0, discounted * (first_service / service_proj),0))
working_data
```

    ## # A tibble: 722 × 15
    ##    employee_id proj_year age_proj salary_proj service_proj retirement   EOSB
    ##          <dbl>     <int>    <dbl>       <dbl>        <dbl>      <dbl>  <dbl>
    ##  1           1         0     41.3       3737.         8.88       60.8 23848.
    ##  2           1         1     42.3       3924.         9.88       60.8 28964.
    ##  3           1         2     43.3       4120.        10.9        60.8 34532.
    ##  4           1         3     44.3       4326.        11.9        60.8 40585.
    ##  5           1         4     45.3       4542.        12.9        60.8 47156.
    ##  6           1         5     46.3       4769.        13.9        60.8 54283.
    ##  7           1         6     47.3       5008.        14.9        60.8 62005.
    ##  8           1         7     48.3       5258.        15.9        60.8 70364.
    ##  9           1         8     49.3       5521.        16.9        60.8 79403.
    ## 10           1         9     50.3       5797.        17.9        60.8 89171.
    ## # ℹ 712 more rows
    ## # ℹ 8 more variables: mortality <dbl>, withdrawal <dbl>, survival <dbl>,
    ## #   undiscounted <dbl>, first_age <dbl>, discounted <dbl>, first_service <dbl>,
    ## #   PVDBO <dbl>

The first code (in comment) was calling first(service_proj) in every
single row, which was not efficient. The one used resulted in 45% cut in
run time at 100k employees level (10m rows)

### Total PVDBO

``` r
individual_results = working_data %>%
  group_by(employee_id) %>%
  summarise(total_PVDBO = sum(PVDBO)) 
individual_results
```

    ## # A tibble: 50 × 2
    ##    employee_id total_PVDBO
    ##          <dbl>       <dbl>
    ##  1           1      29255.
    ##  2           2      28592.
    ##  3           3      69410.
    ##  4           4       1875.
    ##  5           5     143308.
    ##  6           6    1043131.
    ##  7           7      15244.
    ##  8           8     294555.
    ##  9           9     288967.
    ## 10          10      52003.
    ## # ℹ 40 more rows

``` r
final_result = sum(individual_results$total_PVDBO)
final_result
```

    ## [1] 10864816

## Sensitivity Analysis & Effective Duration

We test ±1% to discount rate and salary growth.

``` r
base_val <- final_result

sg_base <- salary_growth
dr_base <- discount_rate

results <- tibble(
  Scenario = c("Base",
               "+1% Discount Rate", "-1% Discount Rate",
               "+1% Salary Growth", "-1% Salary Growth"),
  PVDBO = c(
    base_val,
    compute_pvdbo(dr_base + 0.01, sg_base),
    compute_pvdbo(dr_base - 0.01, sg_base),
    compute_pvdbo(dr_base,         sg_base + 0.01),
    compute_pvdbo(dr_base,         sg_base - 0.01)
  )
) %>%
  mutate(
    Delta     = PVDBO - first(PVDBO),
    Delta_pct = Delta/first(PVDBO)
  )

duration = (results$PVDBO[3]-results$PVDBO[2])/(2*results$PVDBO[1]*0.01)
results
```

    ## # A tibble: 5 × 4
    ##   Scenario              PVDBO    Delta Delta_pct
    ##   <chr>                 <dbl>    <dbl>     <dbl>
    ## 1 Base              10864816.       0     0     
    ## 2 +1% Discount Rate 10238181. -626635.   -0.0577
    ## 3 -1% Discount Rate 11678486.  813670.    0.0749
    ## 4 +1% Salary Growth 11725404.  860587.    0.0792
    ## 5 -1% Salary Growth 10183876. -680941.   -0.0627

``` r
duration
```

    ## [1] 6.628298

``` r
end <- Sys.time()
end - start
```

    ## Time difference of 5.868543 secs
