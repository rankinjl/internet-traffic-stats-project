# internet-traffic-stats-project
A statistical decomposition of internet traffic data (in bits) over time. Using RStudio I performed a Simple Trend Model, Multiplicative Classical Decomposition, Additive Classical Decomposition, and an ARIMA model.

To execute R code, may have to change line 3 (inttraffic <- read_csv("inttraffic.csv")) so that the .csv file is read from the correct directory.

This data was taken from https://datamarket.com/data/set/232n/internet-traffic-data-in-bits-from-a-private-isp-with-centres-in-11-european-cities-the-data-corresponds-to-a-transatlantic-link-and-was-collected-from-0657-hours-on-7-june-to-1117-hours-on-31-july-2005-data-collected-at-five-minute-intervals#!ds=232n&display=line
(Internet traffic data (in bits) from a private ISP with centres in 11 European cities. The data corresponds to a transatlantic link and was collected from 06:57 hours on 7 June to 11:17 hours on 31 July 2005. Data collected at five minute intervals.)
