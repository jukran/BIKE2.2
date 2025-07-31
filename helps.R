###############################################
# HELP TEXTS FOR THE POP UP WINDOWS



help_file <- function(){
  tags$i()%>%
    helper(type="inline",
           size = "m",
           title = "Upload files",
           content = c(
             "Upload the data files into the corresponding fields.",
             "",
             "After the correct data file is uploaded a table with its content will be generated on the right side of the screen.",
             "",
             "NOTE: Consumption data can take two different forms: Food diary or Food frequency questionnaire. This should be indicated by selecting the correct option!",
             "",
             "<b>Example of an appropriate csv file:</b>",
             "",
             "<ul><i><b>Samplenum,Food,Type,Country,Year,Hazard,Concentration,LOQ,LOD,Unit</b></i>",
             "<i>1,poultry,broiler,EU,2020,campylobacter,NA,NA,0.500,cfu.p.gram</i>",
             "<i>2,poultry,broiler,EU,2020,campylobacter,0.572,0.500,0.500,cfu.p.gram</i>",
             "<i>3,poultry,broiler,EU,2020,campylobacter,2.141,0.500,0.500,cfu.p.gram</i></ul>",
             "",
             "More details about how to prepare the files are given 
             in <b>About</b> -> The model -> Requirements for the input data."
             
           ),
           buttonLabel = "OK", 
           easyClose = TRUE,
           icon = "info-circle",
           colour = "#343841",
           
           fade = FALSE)
}

help_parameters <- function(){
  tags$i()%>%
    helper(type="inline",
           size = "m",
           title = "Define the model settings",
           content = c(
             "<b>Consumption model</b>",
             "Consumption frequencies can be estimated using two alternative models.",
             "<ul>
             <li><b>Dependent days</b> assumes the reported days are consecutive days so that the 
             occurrence of consumption (yes/no) on any day may depend on the occurrence of consumption 
             (yes/no) on the previous day.  The long-term consumption frequency is estimated from a 
             Markov model as its stationary distribution.</li> 
             <li><b>Independent days</b> assumes the 
             occurrence of consumption (yes/no) on any day is independent of the occurrence of consumption 
             (yes/no) on any other day. The consumption frequency is estimated from a binomial model. </li></ul>",
             "<b>Priors for variances</b>",
             "There are two options for prior distributions for variance parameters.",
             "<ul>
             <li><b>Tau Gamma</b>, a conventional choice for the prior of inverse variance.</li> 
             <li>The restricted <b>Sigma Uniform</b> prior for standard deviation, determined from 
             an exaggerated upper bound based on data.</li></ul>",
             
             "<b>Number of MCMC iterations</b>",
             "The number of MCMC simulations affects the Monte Carlo accuracy of the results. It is recommended 
             that several iterations of different lengths are tried to gauge how Monte Carlo error could still 
             affect the results.",
             ""),
           buttonLabel = "OK", 
           easyClose = TRUE,
           icon = "info-circle",
           colour = "#CEB888",
           
           fade = FALSE)
}

help_params_indep <- function(){
  tags$i()%>%
    helper(type="inline",
           size = "m",
           title = "Additional settings for Independent days consumption model",
           content = c(
             
             "Two additional settings can be included when <b>Independent days</b> consumption model is selected.",
             "<ul>
             <li><b>Between-user variability in consumption frequencies</b></li>
             Independent days consumption model assumes two levels of variability: (1) variability of consumption days 
             (yes/no) per consumer, and (2) variability of (logit) frequencies between consumers. However, some consumption 
             data sets may contain food types that are reported as consumed on all days, for all consumers. In that case, 
             estimation of between consumer variability of the frequencies would not be possible. Even if there were a few 
             occurrences which manifest such variation, they may be too few for reliable estimation. Therefore, it is 
             recommended to check the MCMC output for the variance parameter from the corresponding plots and to be aware 
             of the quality of data used. <b>Between-user variability</b> should be considered as part of the model only if 
             there is enough data for estimating between consumer variance and if consumption frequencies need to be individual. 
             <li><b>Correlation model of consumption frequencies</b></li>
             With individual specific consumption (logit) frequencies, they may be allowed to be correlated between different foods, 
             but the correlation parameters are not individual specific (this would require much more reporting days per each 
             individual). 
             </ul>",
             
             ""),
           buttonLabel = "OK", 
           easyClose = TRUE,
           icon = "info-circle",
           colour = "#004F71",
           
           fade = FALSE)
}


help_params_serving <- function(){
  tags$i()%>%
    helper(type="inline",
           size = "m",
           title = "Correlation models for consumption amounts",
           content = c(
             "In addition to the Consumption model, one or two <b>Correlation models</b> could be included. ",
             "<ul>
             <li><b>Serving sizes</b> </li>
             With several food types, correlations may be allowed between daily (log) consumption amounts of 
             different foods, but the correlation parameters are not individual specific (this would require 
             much more reporting days per each individual) 
             <li><b>Mean serving sizes</b></li>
             With several food types, correlations may be allowed between daily mean (log) consumption amounts 
             of different foods, but the correlation parameters are not individual specific (this would require
             much more reporting days per each individual) 
             </ul>",
             ""
             
           ),
           buttonLabel = "OK", 
           easyClose = TRUE,
           icon = "info-circle",
           colour = "#004F71",
           
           fade = FALSE)
}


help_view1 <- function(){
  tags$i()%>%
    helper(type="inline",
           size = "m",
           title = "Select results to plot",
           content = c(
             "The selections for food types and/or hazards are section specific. When changing the food types and/or hazards 
             for which results to be visualized in the currently opened 
             section, the selections do not apply to the plots in the other sections.",
             "",
             "<b>Note:</b> The underlying model is running with the full lists of the food types and hazards.
             The selection only specifies which results are processed as outputs, i.e., a new selection 
             does not re-start the whole MCMC simulation."
             
           ),
           buttonLabel = "OK", 
           easyClose = TRUE,
           icon = "info-circle",
           colour = "#004F71",
           
           fade = FALSE)
}

help_view2 <- function(){
  tags$i()%>%
    helper(type="inline",
           size = "m",
           title = "Select the type of the plot",
           content = c(
             "The selections in this panel are section specific. When changing the <b>credible interval</b>, 
             <b>scale</b> or <b>distribution</b> to the plot in the currently opened 
             section, the selections do not apply to the plots in the other sections.",
             "",
             "<b>Note:</b> The selections only specifies how the results to be visualized, i.e., a new selection 
             does not re-start the whole MCMC simulation."
             
           ),
           buttonLabel = "OK", 
           easyClose = TRUE,
           icon = "info-circle",
           colour = "#004F71",
           
           fade = FALSE)
}

help_quantiles <- function(){
  
  tags$i() %>%
    helper(
      type = "inline",
      size = "m",
      title = "Quantile estimation",
      content = c(
        "From the selection list, choose the <b>quantile</b> for which total exposure to be estimated. 
        The total exposure will be computed as the summed exposure from the selected food types.",
        "",
        "The calculations employ 2D simulations, by taking the 'uncertainty-simulations' as a subset of 
        the MCMC-simulated parameter values (describing parameter uncertainty), and then for each parameter 
        value taking 'the variability-simulations' for the quantities that are considered as variables from the 
        conditional distributions <b><i>P(variable | parameter)</i></b>. 
        So this just partly utilizes the existing MCMC sample for getting the parameter uncertainty from it.",
        "",
        "The quantile for two exposure distributions will be estimated.",
        "<ul><b>a.</b> The worst case exposure that results if all the (selected) foods are always contaminated 
        and consumed daily (shown graphically and numerically in a legend).",
        "<b>b.</b> Accounting the prevalence of contamination for each food as well as the actual consumption 
        frequency of each food (shown only numerically in a legend).</ul>",
        
        "Since the distribution of summed positive exposure can only be simulated approximately, 
        the sample size to be used for producing a <b>variability distribution</b>, and <b>uncertainty distribution</b> 
        for the underlying n-tuple of parameters of each variability distribution, can be defined using the numeric input boxes.",
        "",
        "<b>Note</b>: The quantiles will be estimated only for those food-hazard pairs for which data is available. The text in the 
        figure does not specify the names of the selected foods used for the calculations."
      ),
      buttonLabel = "OK",
      easyClose = TRUE,
      icon = "info-circle",
      colour = "#004F71",
      
      fade = FALSE
    )
}

help_limits <- function(){
  tags$i()%>%
    helper(type="inline",
           size = "m",
           title = "Exposure limit analysis",
           content = c(
             "<b>Exposure limit analysis</b> presents the part of the population (1 = 100%) with exposure below the limit 
             given in the Occurrence dataset. The estimate is given for all days and for days with only positive consumption 
             of contaminated food. The exposure limit can be set as a health based guidance value, for example tolerable 
             daily intake TDI. Note that the exposure calculated by BIKE is in units per day (per bodyweight or as absolute), 
             and thus a health based guidance value given in units per a longer time period  should be adjusted  to units 
             per day before uploading data.",
             "",
             "From the defined critical exposure limits for the hazards in the input data,  
             <b>P(exposure < LIMIT)</b> is calculated for the selected hazard-food pairs. The table also contains the
             related results of the exposure Quantile <b>Q95(exposure)</b> estimate presented with its uncertainty 
             median (Q50%) and its uncertainty interval Q5%-Q95%.",
             "",
             "<b>Note:</b> The selection only specifies which results are processed as outputs, i.e., a new selection 
             does not re-start the whole MCMC simulation."
             
           ),
           buttonLabel = "OK", 
           easyClose = TRUE,
           icon = "info-circle",
           colour = "#004F71",
           
           fade = FALSE)
}

help_summary <- function(){
  tags$i()%>%
    helper(type="inline",
           size = "m",
           title = "Posterior predictive distributions",
           content = c(
             "<b>Posterior predictive distributions</b> present predictions where all uncertainties and variabilities are 
             integrated into one single probability distribution. This can be a useful summary for assessing what is 
             now probable, given all the data with all its variability and uncertainties. The distribution is obtained 
             by averaging (weighing) the possible variability distributions over the uncertainty distribution of their 
             parameters.",
             "",
             "<b>Note:</b> It is then not possible to separate what is due to variability or uncertainty 
             in the distribution, so there can be no uncertainty intervals for variability quantiles either.",
             "<b>Note:</b> The selection only specifies which results are processed as outputs, i.e., a new selection 
             does not re-start the whole MCMC simulation."
             
           ),
           buttonLabel = "OK", 
           easyClose = TRUE,
           icon = "info-circle",
           colour = "#004F71",
           
           fade = FALSE)
}

help_factors  <- function(){
  tags$i()%>%
    helper(type="inline",
           size = "m",
           title = "Adjustment factors",
           content = c(
             "To modify the modelled contamination when calculating exposure, adjustment factors can be assigned for 
             the concentration level (in <b>Concentration factors</b> table) and the prevalence (in <b>Prevalence factors</b> table). ",
             "",
             "The factors allow accounting for food processing effects which may either reduce the prevalence, and/or 
             reduce or increase the concentration. Processing effects may be due to cooking, heating, freezing, washing, 
             peeling, storage, etc. ",
             "",
             "The factor should be numerical but there is no automated check for the validity
or sensibility of the factors given by the user. A factor for
<b>concentrations</b> should be any positive number. A factor for <b>prevalence</b> should be between zero (0) and one (1)
to ensure that the multiplication produces a mathematically valid prevalence. (Hence, the factor can only decrease prevalence)",
             "",
             "<b>Note:</b> The changes in the adjustment factors lead only to generating new plot for exposures, as well as new 2D-simulation and new plot for quantiles, 
             i.e., new adjustment factors do not re-start the whole MCMC simulation."),
           buttonLabel = "OK", 
           easyClose = TRUE,
           icon = "info-circle",
           colour = "#CEB888",
           
           fade = FALSE)
}

help_report  <- function(){
  tags$i()%>%
    helper(type="inline",
           size = "m",
           title = "Download report",
           content = c(
             "The results could be downloaded as <b>rds</b> and <b>html</b> files.", 
             "",
             "<b>'MCMC samples' rds file</b>",
             "The file contains the MCMC samples for all parameters for all food types and hazards.",
             "",
             "<b>'Report hazard' html file</b>",
             "The file contains the input data, the settings used for the model, 
             and the results as figures and tables for all food-hazard combinations 
             for the selected hazard. A few plot view options are available.", 
             "The Quantile plot is optional.
             To be included in the report, total exposure quantile should be selected. 
             Also variability  and uncertainty sample size could be defined. Note that this will run 2D-simulation
             and it will take more time for the report to be generated.
             ",
             "",
             "<b>'Report view' html file</b>",
             "The file contains the input data, the settings used for the model, 
             and the results as figures and tables that have been last visible in the app sections."),
           buttonLabel = "OK", 
           easyClose = TRUE,
           icon = "info-circle",
           colour = "#5C442C",
           
           fade = FALSE)
}

help_report_mcmc  <- function(){
  tags$i()%>%
    helper(type="inline",
           size = "m",
           title = "'MCMC samples' rds file",
           content = c(
             
             "The file contains the MCMC samples for all parameters related to the selectedfor all food types and hazards.",
             "",
             "<b>Parameters</b> (<b>K</b> for chemical and <b>M</b> for microbiological hazards):",
             "
    mucK: mean for log-concentrations in each hazard-food pair  </br>
    mucM: mean for log-concentrations in each hazard-food pair </br>
    sigcK: standard deviation for log-concentrations in each hazard-food pair  </br>
    sigcM: standard deviation for log-concentrations in each hazard-food pair  </br>
    mus0: population mean for individual log- mean consumptions for each food </br>
    Ts0: inverse covariance matrix for mean consumption amounts of foods </br>
    Ts: inverse covariance matrix for consumption amounts of foods </br>
    logitp0: logit of mean consumption frequency (if food diary data), or of proportion of true consumers in population (if FFQ data) for each food </br>
    Tp: inverse covariance matrix for consumption frequencies of foods  </br>
    muw: mean of log-bodyweights </br>
    sigw: standard deviation of log-bodyweigths  </br>
    pK: prevalence of each hazard in each food  </br>
    pM: prevalence of each hazard in each food </br>
    n.sims: number of MCMC simulations
             ",
             "...to be updated"),
           buttonLabel = "OK", 
           easyClose = TRUE,
           icon = "info-circle",
           colour = "#5C442C",
           
           fade = FALSE)
}
