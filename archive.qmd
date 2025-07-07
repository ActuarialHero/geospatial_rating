## Liberty Mutual

Liberty Mutual Group (111) Liberty Insurance Corporation (42404)

Ranked 4th

NYP66556.pdf
Page 102
Rates - Page 107
H4/H6 - Page 109

## No. 5 - Allstate Insurance Company (19232) 

Territories are on scanned in sheets of paper

By County with a few cities carved out

## NYCM

Nycm Insurance Group (2518) New York Central Mutual Fire Insurance Company (14834)
Ranked 6th
NYP66579

By county
A few split counties. E.g. Buffalo part of Erie county
Nassau and Suffolk by ZIP
New York County is territory 05. 1.270 factor for HO04/HO06

## Travelers Insurance
Ranked 9th

66405

ZIP Code

## SSURGO - Manhattan - Flooding Frequency

```{r}
#| echo: true

muaggatt <- read_delim("data_in/ssurgo/NY061/tabular/muaggatt.txt", delim = "|",
                       col_names = muaggatt_names,
                       col_types = muaggatt_types)

ssurgo <- read_sf("data_in/ssurgo/NY061/spatial/soilmu_a_ny061.shp") %>%
  left_join(muaggatt, c ("MUKEY" = "mukey"))

drainage_codes <- unique(ssurgo$drclassdcd)

factpal <- colorFactor(topo.colors(4), ssurgo$flodfreqdcd)

leaflet(ssurgo) %>%
  addPolygons(weight = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~factpal(flodfreqdcd)) %>%
  addLegend(
    pal = factpal,
    values = ~flodfreqdcd,
    title = "Flooding Frequency Code")

```


## Filing Sample - Rate Rule

"Rule 965 - Advance Quote Factor

A factor will adjust the Base Premium for new business policies receiving a new business quote from Concord Group seven or more days prior to the policy effective date. The policyholder must have prior insurance without lapse or non-renewal within the past three years. The policy must have no lapse in coverage at new business; a lapse is defined as one or more days between the expiration date of the prior policy and the new business effective date of the Concord Group policy. This discount does not apply to cancel/rewrites or reinstated policies.

Apply the appropriate factor shown in rate section R965."

Concord General Mutual Insurance Company Filing SERFF #AOIC-134087738

::: {.notes}
Each of these steps has what's called a rate rule associate with it.

This is an example from a different company's filing.

They offer a discount if you get your quote in advance of the effective date

:::
  
  ## Filing Sample - Rates
  
  Advanced Quote Factors - Form 6

| Months Since<br>Original<br>Effective Date | Quoted >=7 Days<br>Prior To Original<br>Effective Date | Quoted 1-6 Days<br>Prior To Original<br>Effective Date | Quoted Quoted On Or After Original<br>Effective Date |
  |:---:|---:|---:|---:|
  | 0-11 | 0.930 | 1.000 | 1.000 |
  | 12-23 | 0.950 | 1.000 | 1.000 |
  | 24-35 | 0.970 | 1.000 | 1.000 |
  | 36-47 | 0.990 | 1.000 | 1.000 |
  | 48+ | 1.000 | 1.000 | 1.000 |
  ||
  
  ::: {.notes}
The rules often pair with the table of rating factors, like this.

Note that the first column is the only one with non-unity factors.

As you renew your policy in subsequent years, the discount goes away.

:::
  
  
  
  ## Filing Sample - Rate Rule {background-color="#A1D6F4"}
  
  "Rule 51 - Early Signing Discount"

To determine the appropriate factor for each premium component, refer to the Early Signing
Discount table shown in the Rate Pages.

If the following criteria are met, the policy qualifies for the Early Signing Discount.

- The application for the Wisconsin Allstate House & Home(SM) policy is completed 7 or more days before the policy effective date at New Business.
- The policyholder’s prior homeowners insurance carrier is not Allstate Insurance Company, Allstate New Jersey Insurance Company, Allstate Indemnity Company, Allstate New Jersey Property and Casualty Insurance Company, Allstate Property and Casualty Insurance Company, Allstate Fire and Casualty Insurance Company, Allstate Vehicle and Property Insurance Company, Castle Key Insurance Company, Castle Key Indemnity Company, Allstate Northbrook Indemnity Company, Allstate Texas Lloyd’s, Allstate County Mutual Insurance Company, or Allstate North American Insurance Company.

Exception: In the case of a customer move from another state, if the policy was receiving the Early Signing Discount in the previous state, the discount will also apply to the Wisconsin Allstate House & Home(SM) policy."

Allstate Vehicle and Property Insurance Company Filing SERFF #ALSE-132971474

::: {.notes}
Each of these steps has what's called a rate rule associate with it.

This is an example from a different company's filing.

They offer a discount if you get your quote in advance of the effective date

The won't give you this discount if you switch from one of the other listed companies in the group.

Think of rate rules like word document versions of data engineering logic

:::


## Survey

https://surveydown.org/templates


## Rate Order Calculation

![Allstate Rate Order Calculation Steps 1 - 42 of 123](images/roc1.png)


::: {.notes}
The first 42 steps of 123 for Allstate Premium Calculation in Wisconsin for Homeowners.

Each row is a step and each column is a Premium Component Group, which probably represents peril, but it's not in the publicly available document.
This is 6 of the 12 Premium Component Groups.

x represents multiple against the result of the previous step

Some interesting steps:
- Base Rate
- Deductible Factor
- Year Built Factor
- Canine Factor
- Education Factor
- Lots of Discounts
:::

## Rate Order Calculation - Version 2

```{r ROC 1}


```
```{r kable}
dt <- mtcars[1:5, 1:6]

kbl(dt) %>%
  kable_classic() %>%
  add_header_above(c(" " = 1, "Group 1" = 2, "Group 2" = 2, "Group 3" = 2))
```

::: {.notes}
The first 42 steps of 123 for Allstate Premium Calculation in Wisconsin for Homeowners.

Each row is a step and each column is a Premium Component Group, which probably represents peril, but it's not in the publicly available document.
This is 6 of the 12 Premium Component Groups.

x represents multiple against the result of the previous step

Some interesting steps:
- Base Rate
- Deductible Factor
- Year Built Factor
- Canine Factor
- Education Factor
- Lots of Discounts
:::

## Rate Order Calculation - Continuted

![Allstate Rate Order Calculation Steps 94 - 123 of 123](images/roc2.png)

::: {.notes}
Skipping a few steps... to the tail end: 94 - 123

Just one column, so policywide

Lots of additions for optional things you can buy on your policy:
- Business Property
- Cameras
- Home Daycare
- Golf Carts
- Musical Instruments

Add everything up at the end

:::


## Types of Premium & Losses

Direct
Assumed
Ceded
Net = Direct + Assumed - Ceded

From Travelers Insurance 2024 Annual Statement

Corporate Catastrophe Excess-of-Loss Reinsurance Treaty. This treaty covers the accumulation of certain property losses arising from one or multiple occurrences for the period January 1, 2025, through and including December 31, 2025. The treaty provides for recovery of 80% of each qualifying loss in excess of a $4.0 billion retention up to $5.0 billion, 95% of losses in excess of $5.0 billion up to $7.5 billion and 100% of losses in excess of $7.5 billion up to $8.0 billion. Therefore, the maximum recovery under the treaty would be $3.7 billion, or 92%, of the total $4.0 billion limit. Qualifying losses for each occurrence are after a $100 million deductible. The treaty covers all of the Company’s exposures in North America and all waters contiguous thereto. The treaty only provides coverage for terrorism events in limited circumstances and excludes entirely losses arising from nuclear, biological, chemical or radiological attacks. The treaty only provides coverage for cyber events and civil unrest in limited circumstances and excludes losses arising from communicable disease. The Company’s underlying insurance coverages generally exclude coverage for communicable disease.



st_intersection vs raster


