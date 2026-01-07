# Load necessary libraries
library(shiny)
library(shinythemes)
library(DT)
library(ggplot2)


# Main Layout
ui <- fluidPage(
  navbarPage(
    title = "Poverty Risk Predictor",
    theme = shinytheme("cosmo"),
    id = "main-navbar",
    
    # Home Tab
    tabPanel("Home",
             fluidPage(
               # Title and Welcome Message
               titlePanel(strong("Welcome to the Poverty Risk Predictor", align = "center")),
               # Project Description
               h3("Addressing Poverty Through Predictive Machine Learning"),
               p("Poverty is a multifaceted challenge with far-reaching consequences for health, education, and economic growth. Our project employs advanced predictive modeling to quantify poverty risk using a comprehensive suite of socioeconomic, demographic, and digital indicators. By analyzing factors such as income levels, borrowing behavior, and financial transaction capabilities, we provide data-driven insights that bridge rigorous academic research with practical policymaking."),
               p("Recognizing that vulnerability is not uniform, our analysis specifically disaggregates poverty risk across distinct age groups—young (15–25), middle‑aged (26–45), and older (46+). This targeted approach enables policymakers to design interventions that address the unique challenges faced by each group, ensuring more effective allocation of resources and social welfare initiatives."),
               hr(),
               h4("What You Will Find on This Website:"),
               tags$ul(
                 tags$li(strong("Dataset:"), " Interactive visualizations and detailed analyses based on key socioeconomic indicators from created selected_vars dataset."),
                 tags$li(strong("Risk Predictor:"), " An interactive tool to compute and visualize predicted poverty risk using your own demographic and socioeconomic inputs, based on our poverty1_train data."),
                 tags$li(strong("Paper:"), "Access to our comprehensive research paper detailing our methodology, findings, and policy recommendations."),
                 tags$li(strong("About:"), " Information about our team, project background, and the strategic motivation behind our work.")
               ),
               p("Use the navigation bar at the top to explore each section. Whether you're a policymaker, researcher, or simply curious about data-driven insights on poverty, our website offers a range of resources to help you understand and address this critical issue.")
             )
    ),
    
    # Dataset Tab
    tabPanel("Dataset",
             fluidPage(
               # Data Description
               titlePanel("Data Description:"),
               p("For this study, the dataset Poverty Probability Index & Economic Indicators from Kaggle is utilized. The dataset includes the Poverty Probability Index (PPI), which was predicted for seven unknown countries using 60 variables. The countries are named A, C, D, F, G, I, and J in order to maintain the anonymity of the original project. Individuals were asked questions related to socioeconomic and demographic factors in these countries, where the poverty line of the individuals has $2.50/day threshold. The data consists of 12,600 observations collected from the Financial Inclusion Insights household surveys."),
               # Key Factors
               h4("Key Factors Driving Poverty Risk"),
               p("Our predictive model is built on a rigorous analysis of several key factors that are closely correlated with poverty risk. In our model, we consider:"),
               
               tags$ul(
                 tags$li(strong("Country:"), " This factor captures the broader macroeconomic and policy context in which individuals reside. Different countries offer varying levels of social protection and economic opportunities."),
                 tags$li(strong("Urbanization (is_urban):"), " Urban versus rural settings significantly impact access to services and infrastructure. Generally, urban areas provide better opportunities, which can lower poverty risk."),
                 tags$li(strong("Literacy:"), " Literacy is a strong proxy for educational attainment and is directly linked to employment opportunities and income potential. This relationship is well supported in the literature (e.g., Cadena-Palacios et al., 2024; Maloma, 2016)."),
                 tags$li(strong("Financial Activity:"), " The number of financial activities performed in the last year indicates the level of financial inclusion and economic engagement. Higher financial activity is often associated with improved economic management and reduced poverty risk."),
                 tags$li(strong("Employment Type in the Last Year:"), " This variable reflects the nature of an individual's employment (e.g., not working, self-employed, irregular seasonal, salaried, or unemployed). Salaried employment is generally linked to better wages, social security, and financial stability, while informal work often involves greater economic vulnerability."),
                 tags$li(strong("Age Group:"), " Age is a crucial determinant of economic behavior and income potential. Younger individuals (first age group) may face limited employment opportunities, while older individuals (third age group) may experience declining income and increased dependency."),
                 tags$li(strong("Gender (female):"), " Gender remains a significant factor influencing economic opportunities and financial outcomes. Women often face structural barriers to accessing education, employment, and financial services."),
                 tags$li(strong("Relationship to the Household:"), " This variable captures the individual's role within the household, such as head of household, spouse, child, or sibling. Household dynamics and decision-making power can significantly affect economic well-being."),
                 tags$li(strong("Education Level:"), " Educational attainment is strongly correlated with income levels and employment opportunities. There are four ranges of educational levels, from those who received no education to those who completed high school. Higher education (level 3) generally provides better job prospects and financial stability."),
                 tags$li(strong("Texting (can_text):"), " The ability to send and receive text messages serves as a proxy for digital literacy and mobile phone accessibility, which can enhance financial inclusion and access to information."),
                 tags$li(strong("Phone Technology:"), " Access to phone technology (e.g., smartphone or basic mobile phone) facilitates communication, financial transactions, and access to digital financial services, contributing to greater economic participation.")
               ),
               hr(),
               h3("Dataset Overview"),
               # Data Table
               DTOutput("data_table", width = "100%"),
               hr(),
               h3("Dataset Visualizations"),
               # Dummy Plot
               h4(em("Mean Values of Selected Dummy Variables:")),
               plotOutput("dummy_plot"),
               hr(),
               # Frequency Plots
               h4(em("Categorical Variables Frequency Plots:")),
               selectInput("plot_select", 
                           label = "Select Frequency Plot:", 
                           choices = c("Employment Type Frequency", 
                                       "Education Level Frequency", 
                                       "Relationship to Household Head", 
                                       "Phone Technology Frequency"),
                           selected = "Employment Type Frequency"),
               plotOutput("selected_plot"),
               hr(),
               # Correlation Matrix
               h3("Correlation Matrix"),
               DTOutput("correlation_matrix"),
               hr()
             )
    ),
    
    # Risk Predictor Tab
    tabPanel(
      "Risk Predictor",
      fluidPage(
        titlePanel("Risk Predictor - Interactive Tool"),
        p("Our Poverty Risk Predictor allows you to enter your specific demographic characteristics—including your country of residence, whether you live in an urban area, your literacy status, gender, marital status, and your level of financial activity. Using an advanced statistical model trained on poverty1_train data, the tool computes your risk of experiencing poverty. This interactive tool provides valuable insights into economic vulnerability, helping individuals and policymakers make informed decisions."),
        p("With this predictor, you can:"),
        tags$ul(
          tags$li(strong("Compare"), " your individual risk with group-level statistics."),
          tags$li(strong("Explore"), " how different factors influence economic vulnerability."),
          tags$li(strong("Identify"), " whether you fall into a low, medium, or high-risk category.")
        ),
        p("The interactive tool also visualizes three different age groups each represented by a unique color. Additionally, three dashed lines indicate risk levels:"),
        tags$ul(
          tags$li(strong("Low Risk:"), " 0 to 0.25"),
          tags$li(strong("Medium Risk:"), " 0.25 to 0.60"),
          tags$li(strong("High Risk:"), " 0.60 to 1.00")
        ),
        
        
        # Initially show the 'Start' button
        conditionalPanel(
          condition = "input.start == 0",
          div(style = "text-align:center;", actionButton("start", "Start"))
        ),
        
        # Once started, show inputs and prediction panel
        conditionalPanel(
          condition = "input.start > 0",
          hr(),
          sidebarLayout(
            sidebarPanel(
              checkboxInput("use_country", "Include Country?", FALSE),
              conditionalPanel(
                condition = "input.use_country == true",
                selectInput("country", "Country:", choices = sort(unique(poverty1_train$country)))
              ),
              checkboxInput("use_urban", "Include Urban Residence?", FALSE),
              conditionalPanel(
                condition = "input.use_urban == true",
                selectInput("is_urban", "Lives in Urban Area?", choices = c("Yes" = 1, "No" = 0))
              ),
              checkboxInput("use_literacy", "Include Literacy?", FALSE),
              conditionalPanel(
                condition = "input.use_literacy == true",
                selectInput("literacy", "Literate?", choices = c("Yes" = 1, "No" = 0))
              ),
              checkboxInput("use_female", "Include Gender?", FALSE),
              conditionalPanel(
                condition = "input.use_female == true",
                selectInput("female", "Female?", choices = c("Yes" = 1, "No" = 0))
              ),
              checkboxInput("use_married", "Include Marital Status?", FALSE),
              conditionalPanel(
                condition = "input.use_married == true",
                selectInput("married", "Married?", choices = c("Yes" = 1, "No" = 0))
              ),
              checkboxInput("use_financial", "Include Financial Activity?", FALSE),
              conditionalPanel(
                condition = "input.use_financial == true",
                sliderInput("num_financial_activities_last_year",
                            "# Financial Activities Last Year:",
                            min = 0, max = 10, value = 5)
              ),
              checkboxInput("use_employment", "Include Employment Type?", FALSE),
              conditionalPanel(
                condition = "input.use_employment == true",
                selectInput("employment_type_last_year", "Employment Type:",
                            choices = c("not_working", "irregular_seasonal", "self_employed", "salaried", "other"))
              ),
              
              checkboxInput("use_relationship", "Include Relationship to Household Head?", FALSE),
              conditionalPanel(
                condition = "input.use_relationship == true",
                selectInput("relationship_to_hh_head", "Relationship to HH Head:",
                            choices = sort(as.character(unique(poverty1_train$relationship_to_hh_head))))
              ),
              checkboxInput("use_education", "Include Education Level?", FALSE),
              conditionalPanel(
                condition = "input.use_education == true",
                selectInput("education_level", "Education Level:",
                            choices = sort(unique(poverty1_train$education_level)))
              ),
              checkboxInput("use_can_text", "Include Can Text?", FALSE),
              conditionalPanel(
                condition = "input.use_can_text == true",
                selectInput("can_text", "Can Text?", choices = c("Yes" = 1, "No" = 0))
              ),
              checkboxInput("use_phone", "Include Phone Technology?", FALSE),
              conditionalPanel(
                condition = "input.use_phone == true",
                selectInput("phone_technology", "Phone Technology?", choices = c(0, 1, 2, 3))
              )
            ),
            mainPanel(
              fluidRow(
                column(width = 8,
                       plotOutput("risk_distribution", height = "400px")),
                column(width = 4,
                       h3("Predicted Poverty Risk:"),
                       uiOutput("group_summary"),
                       br(),
                       uiOutput("age_group_interpretation"))
              )
            )
          )
        )
      )
    ),
    
    # Recommendations Tab
    tabPanel("Recommendations",
             fluidPage(
               titlePanel("Potential Recommendations"),
               p("This section presents policy recommendations aimed at reducing poverty risk across different age groups. These recommendations are based on selected variables, meaning alternative recommendations may be needed if different factors are considered."),
               h4(strong("Recommendations by Age Group:")),
               hr(),
               h4(strong("Young (15-26)")),
               p(strong("Scholarship Programs -")),
               p("Access to higher education is a key determinant in reducing poverty risk, particularly among young individuals aged 15–25. Socioeconomic disparities, especially in rural areas, along with limited educational opportunities, contribute significantly to the persistence of poverty. A well-designed scholarship program should include; financial aid, academic resources, mentorship programs, and career guidance (Cadena-Palacios, C. N., 2024). This holistic approach ensures that students not only receive funding but also the necessary support to successfully complete their education and transition into the labor market. Studies indicate that educational scholarships improve completion rates among low-income youth, reduce poverty risk and increasing their chances of securing stable job. Scholarship programs can help bridge the gap between individuals who live in rural areas, are female and those who lack access to phone technology. By offering students this program it will give them the opportunity to purse a high level of education in where they will be equipped with necessary skills, access to phone technology and help eliminate financial burden. While education is the first critical step it must be also be integrated with job training programs to ensure that the individuals can effectively apply their knowledge in the workforce. Governments can introduce job training and career development programs to further enhance employability (Wimer, C., 2020). This initiative can help individuals focus on skill development, job placement and career readiness. This ensures a seamless transition from education to the labor market, preventing employment gaps that could negatively impact economic stability."),
               hr(),
               h4(strong("Middle Age (26-45)")),
               p(strong("Community Literacy & Empowerment Workshops -")),
               p("Among middle-aged individuals, our analysis reveals a significantly higher poverty risk for illiterate, married women—a trend that aligns with findings from Cadena-Palacios et al. (2024) and Maloma (2016). In many low-income and rural contexts, married women carry disproportionate household responsibilities, often without access to education or stable income. Illiteracy compounds this vulnerability by limiting access to employment, social services, and financial literacy.
We propose implementing Community Literacy & Empowerment Workshops targeting married, illiterate women aged 26–45. These locally-run workshops should focus on practical literacy (e.g., reading, writing, navigating digital tools), financial education, and basic rights awareness. Studies show that adult literacy programs significantly improve household welfare, labor force participation, and self-reliance (Pokhriyal & Jacques, 2017; Wu et al., 2024).
Delivered through community centers, mobile libraries, or NGO partnerships, these workshops would reduce informational asymmetry, boost confidence, and empower women to participate in local economies or advocate for family resources. When combined with digital inclusion (e.g., phone use), literacy training can break the intergenerational cycle of poverty by equipping women to support their children's education and manage household finances more effectively.
"),
               hr(),
               h4(strong("Old (46+)")),
               p(strong("Digital Empowerment Hubs -")),
               p("Older individuals in rural areas face the highest predicted poverty risks according to our model—driven by reduced employment participation, isolation, and digital exclusion. While younger groups benefit from familial support or employment integration, older adults often lack both. Our analysis shows a strong link between digital literacy (proxied by texting ability) and lower poverty probabilities, affirming findings from Pokhriyal & Jacques (2017) and Wu et al. (2024) on digital access reducing economic vulnerability.
We recommend implementing Digital Empowerment Hubs—community-based evening centers in rural areas offering training on basic phone, texting, and laptop use. These hubs could empower older residents to access social protection services, mobile banking, and digital health tools. Evidence shows that improving digital inclusion enhances financial decision-making and promotes well-being (Hohberg et al., 2018; Zeller, 2013). While not aimed at labor market re-entry, the goal is to reduce social exclusion and increase autonomy in managing resources.
To maximize impact, hubs should be free, locally staffed, and paired with awareness campaigns. This initiative aligns with global digital inclusion goals and supports a shift from reactive to preventive poverty interventions.
"),
               hr(),
             )
    ),
    
    # Paper Tab
    tabPanel("Paper",
             fluidPage(
               titlePanel("Download Research Paper"),
               p("This document, provides a comprehensive overview of our methodology, 
                 results, and policy recommendations for addressing poverty risk across different age groups."),
               p(strong("Note:"),
                 "To download our full research paper, please open this Shiny application in your local web browser. 
                 Navigate to the 'Paper' tab and click on the 'Download Research Paper (PDF)' button. 
    "),
               downloadButton("downloadPaper", "Download Research Paper (PDF)")
             )
    ),
    
    # About Tab
    tabPanel("About", 
             fluidPage(
               # Project and Team Information
               titlePanel("About the Project & Team"),
               h3("Why This Project?"),
               p("Our project, ", strong("Can We Predict Poverty Risk? A Data-Driven Approach to Identifying Vulnerable Populations and Providing Targeted Assistance"), 
                 " was conceived as part of the Data Science Lab course at Utrecht University School of Economics. In an era where data is transforming decision-making across disciplines, we believe that harnessing advanced predictive analytics to address complex social issues is not only timely but essential. Poverty is a multifaceted challenge that impedes individual growth and undermines societal progress. By integrating socioeconomic, demographic, and digital indicators into our predictive framework, we aim to provide policymakers with actionable insights that can inform targeted interventions and ultimately contribute to a more equitable society."),
               h3("Our Approach"),
               p("Guided by the rigorous standards outlined in our course manual, our project has followed a comprehensive data science workflow—from formulating a research question to data preparation, analysis, and the development of an interactive Shiny tool. We designed our methodology to capture the nuanced ways in which poverty risk varies across different age groups—young (15–25), middle‑aged (26–45), and older (46+). This age-specific perspective is critical, as it reveals distinct patterns of vulnerability and resource needs, empowering decision-makers to design more effective, customized social welfare programs. Our approach not only bridges theoretical research and practical policy application but also showcases the transformative potential of data science in tackling real-world challenges."),
               h3("The Team"),
               fluidRow(
                 column(
                   width = 4,
                   tags$img(src = "ana.jpg", width = "150px", alt = "Ana's Photo"),
                   br(),
                   strong("Ana Sofia Polo Bleher")
                 ),
                 column(
                   width = 4,
                   tags$img(src = "aleksandra.jpg", width = "150px", alt = "Aleksandra's Photo"),
                   br(),
                   strong("Aleksandra Tatko")
                 ),
                 column(
                   width = 4,
                   tags$img(src = "valeria.jpg", width = "150px", alt = "Valeria's Photo"),
                   br(),
                   strong("Valeria Fuenmayor van Praag")
                 ),
                 
               ),
               
               p("Under the expert supervision of", strong(" Tina Dulam"), 
                 "our team is driven by a genuine desire to make a positive impact on society. Motivated by the challenges we observe in the world, we have combined our technical expertise and academic curiosity to create a tool that is both insightful and practical. We are committed to using data science as a force for good—providing clarity, supporting informed policy decisions, and ultimately contributing to poverty alleviation on a global scale.")
             )
    )
  )
)