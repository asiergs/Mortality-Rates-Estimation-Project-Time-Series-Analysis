## Mortality Rates Prediction Project Time Series Analysis

In this Project, an insurance company risk for the next year is going to be calculated by the estimation 
of the mortality rates of the policyholders. The objective of the model is to properly evaluate the 
premium risk (the risk of having more claims than expected) so the economic capital can be estimated. 

The insurance company to be modelled is composed by the following number of policies for each age: 

<center>
  
| Age | Policies |
|:-------:|:-------------:|
| 67      | 902           |
| 68      | 659           |
| 69      | 1471          |
| 70      | 978           |
| 71      | 675           |
| 72      | 850           |
| 73      | 882           |
| 74      | 1035          |
| 75      | 995           |

</center>

The historical mortality rates of the policyholders age is available since 1935 up to 2021. 
The project has four main parts: 

1. Mortality rates time series ARIMA functional form estimation by Box-Jenkins procedure 
2. Estimation and diagnosis of the ARIMA model 
3. One year period mortality rate prediction 
4. Cost estimation and economic capital calculation

With this model, the expected cost will be evaluated together with the VaR<sub>99</sub> and TVaR<sub>99</sub> to quantify the worst-case scenarios.
See the document [Mortality_Rates_Estimation_Time_Series_Analysis.pdf](https://github.com/asiergs/Mortality-Rates-Prediction-Project-Time-Series-Analysis/blob/main/Mortality_Rates_Estimation_Time_Series_Analysis.pdf) for the complete report about the project.

## How to run the code

To run the code, simply run the project.R script once. Since many equal checks need to be performed for the estimations, the main program has two loops, one for the ARIMA model estimation and the other for the simulation to predict by bootstrap.

Note the program will create folders and save the data into excel files when running.
