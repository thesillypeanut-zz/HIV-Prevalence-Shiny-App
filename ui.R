library(shiny)

# Choices for drop-downs
vars <- c(
  "CCPI" = "ccpi",
  "GDP" = "gdp",
  "Gini Coefficient" = "gini",
  "Affected Population" = "affectedpoppercent2015",
  "Literacy Rate" = "literacy2015",
  "HAQI" = "haqi2015"
)

navbarPage("Developing a Syndemic Framework for Understanding HIV Prevalence", id="nav",
           
           tabPanel("Interactive Map",
                    div(class="outer",
                        
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css"),
                          includeScript("gomap.js")
                        ),
                        
                        leafletOutput("map", width="100%", height="100%"),
                       
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = "auto", left = "auto", right = 20, bottom = 50,
                                      width = "330", height = "auto",
                                      
                                      h2("HIV Data Explorer"),
                                      
                                      selectInput("color", "Colour by", vars,selected="haqi2015"), 
                                      selectInput("size", "Size by", vars, selected = "affectedpoppercent2015")
                        )
                        
                        #div(id="cite",
                        #         'Interactive World Map for ', tags$em('STEM Fellowship Big Data Challenge 2017'), ' by Maliha Islam (DataBots, 2017). Special Thanks to Garrett Grolemund (R Studio)'
                        #)
                    )
           ),
           
          
           tabPanel("Global Climate Change",
                
                    fluidPage(
                      fluidRow(
                         shiny::column(9, 
                          h1("Climate Change on HIV Prevalence")
                         ),
                         shiny::column(3,
                           img(src="logo.png", height=100, width=100),align='right'
                          ),

                         shiny::column(6,
                                       htmlOutput("ccpiIntensityMap", align = 'right')
                         ),
                          shiny::column(6,
                               h3((em('"Environmental changes may foster conditions that are favorable 
                                     for disease transmission, including many infections that affect people living 
                                     with HIV/AIDS (e.g., malaria and diarrheal disease)...Infection with malaria and 
                                     other coinfections lead to significant increases in the amount of circulating virus in the 
                                     blood of HIV-infected individuals."')),"-Talman, A., Bolton, S., & Walson, J. L. (2013)", align='center')
                          )
                          
                        
                       ),
                      br(),
                      fluidRow(wellPanel(
                        h4("Climate Change Performance Index evaluates the climate protection performance of countries that 
                           are collectively responsible for more than 90% of global energy-related CO2 emissions. 
                           The index categories are weighted as following:",tags$ul(tags$li("Emissions level - 30%"),
                                                                                    tags$li("Recent emissions development - 30%"), 
                                                                                    tags$li("Renewable energy - 10%"), 
                                                                                    tags$li("Efficiency - 10%"),
                                                                                    tags$li("Climate policy assessments - 20%")))
                      )),
                      fluidRow(
                        wellPanel(
                            htmlOutput("ccpiBubbleChart", align = 'center')
                        )
                      ),
                      
                      fluidRow(img(src="kclusteredccpi.png", height=300, width=500),align='center'),
                      br(),
                      fluidRow(
                        shiny::column(6,
                                      h4(tags$strong("Cluster 1"),tags$ul(tags$li("CCPI in the range of 35.57 – 40.99, affected population in the range of 0.09 – 0.22 %"),
                                                                          tags$li("GDP is similar and within the range of 180,000 – 1,500,000 millions of USD"),
                                                                          tags$li("Adult literacy rates are similar and within the range of 74.99 – 79.72%"),
                                                                          tags$li("Positive correlation found between CCPI and GDP"),
                                                                          tags$li("Correlation found between low CCPI and moderate literacy rate"))),
                                      br(),
                                      h4(tags$strong("Cluster 2"),tags$ul(tags$li("CCPI in the range of 45.07 – 81.3, affected population in the range of 0.02 – 0.65%"),
                                                                          tags$li("GDP varies widely from 10,000 – 18,000,000 millions of USD"),
                                                                          tags$li("Adult literacy rates are similar within the range of 92.99 – 99.77%"),
                                                                          tags$li("Half of the countries are in East Asia & Pacific region")))
                        ),
                        shiny::column(6,
                                      h4(tags$strong("Cluster 3"),tags$ul(tags$li("CPI in the range of 54.46 – 63.07, affected population in the range of 0.01 – 14.5%",
                                                                                  tags$ul(tags$li("Germany’s affected population of 0.01% being the lowest"),
                                                                                          tags$li("South Africa’s affected population of 14.5% being the highest"))),
                                                                          tags$li("GDP varies widely from 16,000 to 3,400,000 millions of USD"),
                                                                          tags$li("Gini coefficients vary widely from 25.6 – 61.1%"),
                                                                          tags$li("9 out of the 15 countries are in Europe & Central Asia region"))),
                                      br(),
                                      h4(tags$strong("Cluster 4"),tags$ul(tags$li("CCPI in the range of 64.11 – 77.76, affected population in the range of 0.02 – 0.39%"),
                                                                          tags$li("GDP is similar and within the range of 100,000 – 2,400,000 millions of USD"),
                                                                          tags$li("Gini coefficients are similar and within the range of 41.5 – 45.7%")))
                        )
                      ),
                      br(),
                      h3("Key Results", align = 'center'),
                      h4(tags$ul(tags$li("Most countries with very poor to moderate CCPI have the highest affected population"),
                                 tags$li("Most countries with very good CCPI have very low affected population"),
                                 tags$li("Countries with moderate to good CCPI have the highest GDP"),
                                 tags$li("Countries in Europe & Central Asia have relatively high CCPI"))
                         ),
                      br(),
                      h3("Recommendations for Increasing CCPI", align = 'center'),
                      h4(tags$ul(tags$li(tags$strong("Reduce CO2 emission levels by switching to renewable energy sources.")))),
                      h4(em('"About 18 of the 35 developing countries ranked highest in renewable energy reserves, are in Africa."-Emelly Mutambatsere (African Development Bank, 2012)'),align='center'),
                      fluidRow(
                        
                        shiny::column(7,
                                      h4(em("Although Africa has a high potential for wind and solar energy, Africa's wind powered electricity
                                            generation accounted for less than 1% of the installed electricity generation capacity on the continent,
                                            as of 2011. If Africa switches over to wind or solar power, CO2 emission levels will be reduced and more
                                            jobs will be created.")),
                                      br(),
                                      h4(tags$ul(tags$li(tags$strong("Implement improved climate change policy.")))),
                                      h4(em("As of 2015, Denmark had the highest CCPI. Follow Denmark's progressive climate change policies
                                            such as demanding electricity generation from renewable sources and making energy saving
                                            obligations for manufacturing and construction companies."))
                                      ),
                        shiny::column(5,
                                      img(src="africawind.png", height=350, width=300),align='right'
                        )
                                      ),
                      br(),
                      br(),
                      br()
                    )

           ),
            
           tabPanel(fluid = TRUE,"Economic Factors",
                    fluidPage(
                      fluidRow(
                        shiny::column(8, 
                                      h1("Economic Factors on HIV Prevalence")
                        ),
                        shiny::column(4,
                                      img(src="logo.png", height=100, width=100),align='right'
                        ),
                        shiny::column(6,
                                      htmlOutput("gdpIntensityMap", align = 'right')
                        ),
                        shiny::column(6,
                                      h3((em('"The poor may be more susceptible to HIV infection as a result of 
                                             lack of education, general poor health and weak access to health 
                                             services."')),"-Barnett, T., & Weston, M. (2008)", align='center')
                                      )
                                      ),
                      br(),
                      fluidRow(wellPanel(
                        h4("To gauge the macroeconomics and microeconomics of a country, the metrics Gross Domestic Product 
                           (GDP) per capita and the Gini coefficient were used. These two metrics were chosen in order 
                           to measure the economic health of a country from two different aspects. GDP per capita measures 
                           the monetary value of goods and services produced per person in a country, while the Gini 
                           coefficient measures the degree of inequality in the distribution of family income in a country.")
                        )),
                      fluidRow(
                        wellPanel(
                          htmlOutput("gdpBubbleChart", align = 'center')
                        )
                      ),
                      fluidRow(
                        shiny::column(6,
                                      plotOutput("gdpPlot1")
                        ),
                        shiny::column(6,
                                      plotOutput("gdpPlot")
                        )
                      ),
                      br(),
                      h3("Key Results", align = 'center'),
                      h4(tags$ul(tags$li("Based on the log-log regression model, a weak negative trend was found that GDP ~ (HIV Prevalence)^-0.3094"),
                                 tags$li("Results indicate increasing the GDP of a country will decrease its HIV prevalence"),
                                 tags$li("Countries with the highest HIV prevalence (and thus the lowest GDP) originated from Sub-Saharan Africa"))
                      ),
                      br(),
                      h3("Recommendations for Increasing GDP", align = 'center'),
                      h4("The amount of arable land and labour force are correlated with the GDP of a country, thus by improving these factors 
                         could potentially affect the GDP of a given country. Thus, some potential 
                         suggestions may be:"),
                      h4(tags$ul(tags$li(tags$strong("Increase the amount of arable land, which would lead to higher agricultural cultivation.")))),
                      fluidRow(
                        
                        shiny::column(7,
                                      h4(em("Utilize deserted areas of African countries to expand the agricultural output of the country. 
                                            Currently, there is a large amount of African land that is unused.")),
                                      br(),
                                      h4(tags$ul(tags$li(tags$strong("Increase work opportunities through urbanization.")))),
                                      h4(em("Make work available to more parts of the country. Urban areas in Africa are very scarce. 
                                            Methods of improvement could include investments made into educating  children so they have
                                            easier access to work as well as more companies that would be willing to hire."))
                                      ),
                        shiny::column(5,
                                      img(src="arableland.png", height=300, width=400),align='right'
                        )
                                      ),
                      br(),
                      br(),
                      fluidRow(
                        plotOutput("giniPlot"), align = 'center'
                      ),
                      br(),
                      h3("Key Results", align = 'center'),
                      h4(tags$ul(tags$li("The cluster with the highest mean Gini coefficient also had the highest HIV prevalence."),
                                 tags$li("The cluster with the lowest mean Gini coefficient had the lowest HIV prevalence."),
                                 tags$li("Results indicate a positive correlation between the Gini coefficient and the HIV prevalence of a country."),
                                 tags$li("All countries from the cluster with the highest Gini coefficient originated from the Sub-Saharan African region."))
                      ),
                      br(),
                      h3("Recommendations for Decreasing Gini Coefficient", align = 'center'),
                      h4("The Gini coefficient measures how uniformly the wealth of a country is distributed. The desirable value wanted 
                         for minimizing HIV prevalence is 0. This means that the country should achieve a state where everyone makes the 
                         same amount of money. Some ways to achieve this could be:"),
                      h4(tags$ul(tags$li(tags$strong("Increase availability of education.")))),
                      h4(em("As per a 2015 report on the state of education in Africa, only about 20% of younger 
                            students are enrolled into primary education facilities, while only 6% of the population 
                            are enrolled into higher educational institutions (in contrast to the 26% global average). 
                            While there have been improvements to the system, it is still well below the accepted numbers, 
                            and this sector must be improved.")),
                      h4(tags$ul(tags$li(tags$strong("Make efforts to bridge the gap between the high-income and the low-income.")))),
                      h4(em("Make changes to the taxing system. This is somewhat already implemented where based on a given income the 
                            taxable rate may change from 18% to 41%, this should be further improved to invoke a greater 
                            change on the HIV prevalence.")),
                      
                      fluidRow(img(src="richandpoor.jpg", height=300, width=450),align='center'),
                      br(),
                      br(),
                      br()
                      
                      
                      
                      )
           ),
           

           tabPanel("Access to Education",
                    fluidPage(
                      fluidRow(
                        shiny::column(8, 
                                      h1("Access to Education on HIV Prevalence")
                        ),
                        shiny::column(4,
                                      img(src="logo.png", height=100, width=100),align='right'
                        ),
                        
                        shiny::column(6,
                                      htmlOutput("lrIntensityMap", align = 'right')
                        ),
                        shiny::column(6,
                                      h3((em('"If all children received a complete primary education, around 
                                             700,000 cases of HIV in young adults could be prevented each 
                                             year."')),"-The Global Campaign for Education (Advocacy Report, 2004)", align='center')
                                      )
                      ),
                      br(),
                      fluidRow(wellPanel(
                        h4("Although the definition of literacy is evolving, in this experiment, it was defined to be 
                          ability to read, write and do arithmetic. The adult literacy rate for countries were used.")
                        )),
                      fluidRow(
                        wellPanel(
                          htmlOutput("lrBubbleChart", align = 'center'),
                          sliderInput("lryear", "Year",min = min(literacydata$year), max = max(literacydata$year),
                             value = max(literacydata$year), animate = TRUE, step= 5)
                        )
                      ),
                      fluidRow(
                        shiny::column(6,
                            plotOutput("lrPlot")
                        ),
                        shiny::column(6,
                             img(src="plot2.png", height=380, width=400),align='center'
                        )
                      ),
                      br(),
                      fluidRow(
                        shiny::column(6,
                           htmlOutput("lrHistogram")
                        ),
                        shiny::column(6,
                           htmlOutput("adultPopHistogram")           
                        )
                      ),
                      br(),
                      h3("Key Results", align = 'center'),
                      h4(tags$ul(tags$li("Top 11 countries with the highest HIV prevalence has",
                                      tags$ul(tags$li("Literacy rate of 90-100%"),
                                              tags$li("Gender Parity Index lower than 1 (0.54 – 1.01) in favor of boys"),
                                              tags$li("36% Sub-Saharan African (4 out of 11) countries"),
                                              tags$li("18% Europe & Central Asia (2 out of 11) countries"),
                                              tags$li("18% East Asia & Pacific (2 out of 11) countries"),
                                              tags$li("18% Latin America & Caribbean (2 out of 11) countries"),
                                              tags$li("9% Middle East & North African (1 out of 11) countries")

                                      )),
                                 tags$li("Generally, increase in literacy rate leads to decrease in HIV prevalence"))
                      ),
                      br(),
                      h3("Recommendations for Increasing Literacy Rate", align = 'center'),
                      h4("Educating more people will help reduce HIV prevalence. It not only allows people 
                        to utilize available resources on HIV, but also more effectively change people’s behaviours. Some suggestions are:"),
                      h4(tags$ul(tags$li(tags$strong("Reduce gender inequality in education.")))),
                      h4(em('“A study in 72 capital cities found significantly higher infection rate where the literacy gap between women and men was large.” -Inon I. Schenker (2005)'),align='center'),
                      h4(em('Eliminate school tuition fee for primary and secondary education. Training to have more female educators at schools would help change 
                            classroom environment where girls may be treated unequal and face violence.')),
                      br(),
                      fluidRow(img(src="lrgender.jpg", height=300, width=500),align='center'),
                      br(),
                      h4(tags$ul(tags$li(tags$strong("Design programs for varying levels of literacy.")))),
                      h4(em('Currently, schools are primary institutions that provide HIV education. 
                            However, there should be a way to educate people who are out of school, such as by considering
                            varying levels of literacy abilities (e.g. take more visual and verbal approaches rater than written).
                            Services must be provided regularly and for free.')),
                      br(),
                      br(),
                      br()
                      
                      )
                      
           ),
           
           tabPanel("Access to Healthcare",
                    fluidPage(
                      fluidRow(
                        shiny::column(8, 
                                      h1("Access to Healthcare on HIV Prevalence")
                        ),
                        shiny::column(4,
                                      img(src="logo.png", height=100, width=100),align='right'
                        ),
                        
                        shiny::column(6,
                                      htmlOutput("haqiIntensityMap", align = 'right')
                        ),
                        shiny::column(6,
                                      h3((em('"AIDS is a disease 
                                             that the Western world can treat and the resource-limited countries cannot. 
                                             In the world scenario a total of 5 million patients with HIV/AIDS who need 
                                             treatment have no access to therapy."')),"-Giuliano M. & Vella S. (2007)", align='center')
                        )
                      ),
                      br(),
                      fluidRow(wellPanel(
                        h4("Healthcare Access and Quality Index (HAQI) measures the national level of personal 
                           healthcare access and quality by measuring mortality rates from causes that would otherwise 
                           not be fatal if medical care is effective. Highly standardised cause of death and risk factor 
                           estimates collected from Global Burden of Diseases, Injuries, and Risk Factors Study (GBD) 
                           was used to quantify this index for countries. ")
                        )),
                      fluidRow(
                        wellPanel(
                          htmlOutput("haqiBubbleChart", align = 'center')
                        )
                      ),
                      fluidRow(
                        shiny::column(6,
                                      plotOutput("haqiPlot")
                        ),
                        shiny::column(6,
                                      plotOutput("haqiPlot2")
                        )
                      ),
                      br(),
                      h3("Key Results", align = 'center'),
                      h4(tags$ul(tags$li("Weak negative correlation found between HAQI and HIV prevalence"),
                                tags$li("Sub-Saharan African countries have the lowest HAQI and highest affected population",
                                         tags$ul(tags$li("Top four countries with highest HIV prevalence are Botswana, Lesotho, Swaziland and
                                                         South Africa. They have low to moderate HAQI of 51.1, 35.7, 41.9 and 52.0, respectively.")
                                                
                                         )),
                                tags$li("Europe & Central Asian countries have the highest HAQI and low to moderate affected population ",
                                        tags$ul(tags$li("Germany has one of the lowest affected population and a high HAQI of 86.4")
                                                
                                        )),
                                 tags$li("Weak positive correlation found between GDP and HAQI"),
                                tags$li("Weak negative correlation found between Gini coefficient and HAQI"))
                      ),
                      br(),
                      h3("Recommendations for Increasing HAQI", align = 'center'),
                      h4("Countries with higher access to quality healthcare and treatment for HIV will have lower HIV prevalence. 
                          This is because increasing access to healthcare can improve quality of life and 
                          life expectancy of people with HIV by reducing illness and risk factors . Thus, some potential 
                          suggestions may be:"),
                      h4(tags$ul(tags$li(tags$strong("Government must invest more on healthcare.")))),
                      h4(em('“Africa bears one-quarter of the global disease burden, yet has only 2% of the world’s doctors.” -Joseph Jimenez (2015)'),align='center'),
                      h4(em('Technology is needed to improve the accessibility and quality of 
healthcare in Africa. Most African countries allocate less than 10% of their GDP on healthcare and less than 50% of African people have access to modern 
health facilities.  Increase the number of health care workers. Other than the government,
entrepreneurs should be encouraged to take healthcare initiatives in Africa.')),
                      br(),
                      fluidRow(img(src="healthcareafrica.jpg", height=400, width=700),align='center'),
                      br(),
                      h4(tags$ul(tags$li(tags$strong("Increase antiretroviral therapies.")))),
                      h4(em('Antiretroviral therapy (ART) uses antiretroviral (ARV) drugs to suppress HIV virus 
                            by slowing the rate at which HIV multiplies, thereby stopping the progression and transmission 
                            of the disease. ART has proven to significantly reduce death rates and infections when a potent 
                            ARV regimen is used. However for ART to be effective, it must be ensured that it is provided
                            in the early stages of the disease.')),
                      br(),
                      br(),
                      br()
                      
                    )
                    
           ),
           
           tabPanel("Syndemic Framework",
                    fluidPage(
                      fluidRow(
                        shiny::column(9, 
                                      h1("Syndemic Framework Constructed Based on Our Findings")
                        ),
                        shiny::column(3,
                                      img(src="logo.png", height=100, width=100),align='right'
                        )
                      ),
                      fluidRow(
                        img(src="framework.pdf", height=1000, width=800,align='center'),
                        h2("Summary of Recommendations"),
                        img(src="solutionframe.pdf", height=800, width=600,align='center'),align='center'
                      )
                    )
          ),
           tabPanel("References",
                    fluidPage(
                      fluidRow(
                        h1("References"),
                        br(),
                        h3("Sources of Datasets"),
                        h4(tags$ul(tags$li("Climate Change Performance Index (CCPI), 2015 – Germanwatch and Climate Action Network Europe "),
                                   tags$li("Gross Domestic Product (GDP) per capita, 2015 – The World Bank"),
                                   tags$li("Gini coefficient, 2013* – United Nations Development Programme"),
                                   tags$li("Adult literacy rate, 2015 – UNESCO Institute for Statistics "),
                                   tags$li("Healthcare Access and Quality Index, 2015 – GBD 2015 Healthcare Access and Quality Collaborators "))),
                        br(),
                        h3("Global Climate Change (CCPI)"),
                        h4(tags$ul(tags$li("http://www.indiaenvironmentportal.org.in/files/file/climate%20change%20performance%20index%202015.pdf"),
                                   tags$li("https://www.evwind.es/2014/09/16/irena-estimates-africas-renewable-energy-potential/47465"),
                                   tags$li("https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3558758/"))),
                        br(),
                        h3("Economic Factors (GDP & Gini Coefficient)"),
                        h4(tags$ul(tags$li("https://smartech.gatech.edu/bitstream/handle/1853/53303/hivandgdpecon3161.pdf"),
                                   tags$li("https://en.wikipedia.org/wiki/Gini_coefficient#Calculation"),
                                   tags$li("https://www.researchgate.net/publication/304064552_A_meta-analysis_and_statistical_modelling_of_nitrates_in_groundwater_at_the_African_scale"),
                                   tags$li("http://www.aaionline.org/wp-content/uploads/2015/09/AAI-SOE-report-2015-final.pdf"),
                                   tags$li("http://www.treasury.gov.za/documents/national%20budget/2016/sars/Budget%20PocketGuide%202016-17.pdf"))),
                        br(),
                        h3("Access to Education (Adult Literacy Rate)"),
                        h4(tags$ul(tags$li("https://www.unicef.org/education/bege_70640.html"),
                                   tags$li("http://unesdoc.unesco.org/images/0014/001460/146070e.pdf"),
                                   tags$li("http://unesdoc.unesco.org/images/0014/001461/146182e.pdf"))),
                        br(),
                        h3("Access to Healthcare (HAQI)"),
                        h4(tags$ul(tags$li("http://www.thelancet.com/journals/lancet/article/PIIS0140-6736(17)30818-8/fulltext?elsca1=tlpr"),
                                   tags$li("https://www.gsb.stanford.edu/insights/taking-challenges-health-care-africa"),
                                   tags$li("http://www.who.int/hiv/topics/treatment/en/"))),
                        br(),
                        h3("Special Thanks to Shiny Gallery by RStudio!", align='center'),
                        br(),
                        br(),
                        br()
                      )
                    )
           ),
           conditionalPanel("false", icon("crosshair"))
)
