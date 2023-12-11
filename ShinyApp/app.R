library(dplyr)
library(stringr)
library(ggplot2)
library(shiny)
library(plotly)
library(tidyr)

source("INFO_201_DataWrangling.R")

# Changing the chr values in Diff Age Groups for later graphing
for (count in 1:nrow(df)) {
  df$`18-29`[count] <- as.numeric(sub("%", "", df$`18-29`[count])) / 100
  df$`30-49`[count] <- as.numeric(sub("%", "", df$`30-49`[count])) / 100
  df$`50-64`[count] <- as.numeric(sub("%", "", df$`50-64`[count])) / 100
  df$`65+`[count] <- as.numeric(sub("%", "", df$`65+`[count])) / 100
}

df <- df %>%
  transform(`18-29` = as.numeric(`18-29`),
            `30-49` = as.numeric(`30-49`),
            `50-64` = as.numeric(`50-64`),
            `65+` = as.numeric(`65+`))

# filtering to make the suicide death rates by percentage
fil_df <- df %>%
  mutate(ESTIMATE_pec = ESTIMATE/ 100000)

# *************************************
# FluidPages for different perspectives
# *************************************
# FluidPage for OVERALL TREND
overall_trend <- fluidPage(
  titlePanel("Examine trend between Suicide Rate and Internet Usage over time"),
  sidebarLayout( 
    sidebarPanel(
      h2("Control Panel"),
      sliderInput(
        inputId = "year", 
        label = h3("Year"), 
        min = 2000, 
        max = 2018,
        value = 2000,
        sep = ""
      ),
      radioButtons(
        inputId = "GenderChoice",
        label = h3("Gender"),
        choices = list("Male", "Female", "Both"),
        selected = "Male"
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", h3("Overall Estimate Population of Suicide via Internet Usage Over Time"), plotlyOutput(outputId = "overall_scatter"))
      )
    )
  ),
  h2("What is the purpose of this graph?"),
  p("This chart is grounded in an estimated population of 1,000,000 individuals per year, accounting for gender-specific mortality rates and the average 
    percentage of internet usage across all age groups—meticulously adjusted for conversion. The goal is to comprehensively examine the interplay between 
    suicide rates and internet usage trends over a specified time span, shedding light on their correlation while considering the nuanced impact of gender variations.", 
    style = "font-size:18px;"),
  h2("Major Takeaways:"),
  p("1. There is a noticeable trend suggesting that, in correlation, internet usage has been linked to an increase in suicide deaths over time, regardless of gender."
    , style = "font-size:18px;"),
  p("2. Over 18 years, the average annual change in suicide rates influenced by the internet is approximately 1.94 for females, whereas for males, the average change 
    is 6.28 per year.", style = "font-size:18px;"),
  p("3. In the span of a year, the most significant increase in suicide rates influenced by the internet for males occurred during the most recent years, specifically in 
    2017-2018, while for females, the notable increase was observed between the years 2014-2015.", style = "font-size:18px;")
)

# Fluid Page for GENDER
gender_page <- fluidPage(
  titlePanel("Difference in Suicide Rate between Male and Female Internet Usage"),
  sidebarLayout( 
    sidebarPanel(
      h2("Control Panel"),
      selectInput(inputId = "gender", 
                  label = h3("Select an option for Gender"), 
                  choices = list("Male", "Female", "Both"),
                  selected = "Female"),
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Scatter Plot", h3("Internet Usage versus Suicide Death Rate based on Gender"), plotlyOutput(outputId = "gender_scatter")),
        tabPanel("Bar Plot", h3("Internet Usage versus Suicide Death Rate based on Gender"), plotlyOutput(outputId = "gender_plot")),
        tabPanel("FYI", br(), "The scatter plot serves as a visualization for the ongoing trend between x and y. While
                 the barplot serves as another visualization showcasing how suicide death rates (measured by the height of
                 the bars) tend to stack up more towards the right of the x-axis, which is the increase in internet usage.")
      )
    )
  ),
  br(),
  p("Through various observations across different gender groups and age brackets, a consistent trend emerges, 
  indicating a positive correlation between increased internet usage and higher suicide death rates. This trend
  is particularly obvious when examined solely by one gender. Likewise, in the first
  two graphs focusing only on males or females, a noticeable pattern emerges in both the bar and scatter plot 
  visualizations – higher internet usage (x-axis) corresponds to taller bars/ higher y values, symbolizing an 
  increase in suicide rates (y-axis)."),
  p("When both genders are considered together, an intriguing trend becomes apparent. While both genders exhibit 
  a clear rise in suicide rates with increased internet usage, an inherent disparity between females and males 
  is evident. The scatterplot showcasing both genders effectively shows that females generally tend to have lower 
  death rates compared to males, despite the overall increase in suicide rates."),
  p("The underlying reasons for this phenomenon are intricate and cannot be generalized. However, our speculation
    leans towards factors such as suicide methodologies and psychosocial elements. Males often face societal pressures
    that encourage the suppression of emotions, a stigma that could be amplified by the pervasive nature
    of social media. Consequently, they might be inclined towards more lethal or perilous suicide attempts 
    compared to females. It's crucial to note that this is just one among several potential factors contributing to 
    this observed trend."),
  
  p("Similar observations arise when analyzing different age groups' internet usage against suicide death rates."),
  br(),
  sidebarLayout( 
    sidebarPanel(
      h2("Control Panel"),
      radioButtons(inputId = "age_group",
                   label = h3("Choose an age group"),
                   choices = list("18-29", "30-49", "50-64", "65+", "All"),
                   selected = "All"),
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Age Scatter Plot", h3("Suicide Rate versus Internet Usage of Different Age Groups"), plotlyOutput(outputId = "age_group_plot")),
        tabPanel("FYI", br(), p("The plot displays two differing trends, this is due to the difference in suicide death rates
                          by gender. Connecting to our previous plot, the trend with higher death rate represents the Male
                          demograph, while the lower death rate trend represents the Females."))
      )
    )
  ),
  br(),
  p("The comprehensive graph portrays a consistent postive relationship between suicide deaths & internet usage, mirroring the pattern
    observed in the first graph. However, upon closer examination of individual graphs, interesting
    nuances emerge. For instance, the 18 to 29 age group graph demonstrates the general trend of growth, 
    with one point peaking at around (0.98, ~0.00023), implying a 0.023% death rate by suicide with a 98% internet usage.
    On the other hand, in the 65+ age group, an intriguing observation surfaces – despite lower internet usage 
    this age group also exhibits the growth in suicide rate with increased media usage. The group also has a 
    peaking suicide death rate at around 0.023%."),
  p("We believe that the general trend of younger adults exhibiting growing suicide death rate in face of 
    increased internet usage has a lot to do with the vulnerablity and inability to distinguish between
    accurate and inaccurate information. As younger individuals has less experience, they tend to be more susceptible
    to allures or misinformation, making it easier for the younger demographic to believe that what they see 
    on social media is the actually truths. Conversely, mature adults may possess greater knowledge and experience to 
    navigate the online sphere cautiously. However, even with their maturity, they remain susceptible to the impact of 
    widespread online information, particularly when it pertains to sensitive topics such as job loss. This 
    susceptibility is evident in the rising suicide rates across age groups, showcasing the profound influence 
    of online content despite one's maturity and accrued life experience."),
  p("However, we do want to acknowledge the potential existence of outliers which could alter the visualizations. 
    Nevertheless, the consistent depiction of increasing suicide rates with rising internet usage in the general plots 
    offers valuable insights despite these possible discrepancies and outliers.")
)

#Fluid Page for our CONCLUSION
conclusion_page <- fluidPage(
  mainPanel(
    h2("Summary"),
    plotlyOutput(outputId = "final_scatter_line"),
    br(),
    p("In conclusion, in each year we tend to see a difference between the rates in suicide for males and females. 
    The trend continues to show to be true when digging deeper and looking at more specific subgroups including a 
    range of income groups, gender, and internet usage. Our data being presented should be a wake up call. 
    There are many factors to take into account, but this just sheds light on the uncomfortable reality that parents, 
    teens, friends, family, and big companies should be aware of. Furthermore, social media companies should take this 
    data and dive deep to figure out the impact their applications have on individuals of all ages, not just the youth. 
    Teens and children should be aware of how social media can skew their thoughts and perceptions of the world, 
    while parents can also learn from this data by possibly questioning how much time their children actually 
    spend on the internet. The internet has only been around for a short period, less than 50 years. 
    With that being said, the new generations growing up on the internet have an unpredictable future as there still 
    isn’t enough research done. "), 
    p("As of now, even with our data, we may be uncertain of how the internet can affect each and every single of us, 
      but we do know that the rate of suicide is steadily increase with each year and we know where we can look to start 
      to make a change.")
  )
)

# Fluid Page for INCOME
income_page <- fluidPage(
  titlePanel("Death Rates by Suicide versus Income Groups"),
  sidebarLayout(
    sidebarPanel(
      h2("Control Panel"),
      radioButtons(inputId = "income", 
                   label = h3("Select Income Group"), 
                   choices = c("All income groups" = 1, "Less than $30,000" = 2,
                               "$30,000 to $49,999" = 3, "$50,000 to $74,999" = 4,
                               "$75,000+" = 5),
                   selected = 1)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotlyOutput(outputId = "income_plot"))
      )
    )
  ),
  br(),
  p("This graph is a comparison between income groups and the rate at which death by suicide occurs. 
  Throughout the years, a common trend was that as we went higher up the income groups, a higher percentage 
  of both males and females had access and would use the internet compared to those in lower income groups. 
  This may be due to a plethora of reasons including, obviously, socioeconomic differences with those in higher 
  income groups having the privilege of being able to more freely spend their money as they chose to. This includes 
  personal computers, laptops, gaming systems, and probably the most convenient of all, phones. All have different 
  yet similar purposes, but there is no denying that all of them allow users to access the internet. "),
  p("Although there are many reasons for why an individual may turn towards suicide our graph shows a clear correlation
    that suicide occurs at a higher rate for those that are living in higher income groups. It’s interesting and gives
    a different perspective on the saying, ‘money doesn’t buy happiness,’ yet it doesn’t fully support 
    the popular phrase as correlation does not equal causation.")
)

#FluidPage for INTRODUCTION
intro <- fluidPage(
  h2("Introduction"),
  p("In our swiftly advancing digital era, social media has seamlessly integrated into 
  our daily lives, fostering connections among millions while disseminating information at lightning 
  speed. Yet, concealed within this connectivity, a pressing concern looms – the adverse effects of 
  social media on cognitive development and mental well-being."),
  
  img(src = "https://cdn.pixabay.com/photo/2020/05/18/16/17/social-media-5187243_1280.png", height = "300px", width = "450px"),
  
  p("Extensive research has underscored the correlation between social media usage and mental health 
    influences. However, what occurs when someone's mental health deteriorates under the weight of social 
    media pressures? This question propels our narrative."),

  p("Our goal is to explore the uncharted territory of whether prolonged exposure to social media could 
  potentially contribute to, and in some cases trigger, a surge in suicide rates. Central to our story 
  is the relentless pressure imposed by social platforms – the ceaseless comparisons, the pressure 
  to conform to online personas and trends, the normalization of cyberbullying, and the self-imposed 
  isolation and distraction fostered by excessive media consumption. The constant stream of curated 
  content often showcases distorted realities, portraying seemingly perfect lives, thereby establishing 
  unattainable standards for consumers."),
  
  img(src = "https://cdni.iconscout.com/illustration/premium/thumb/customer-annoyed-by-online-ads-7301174-5979696.png?f=webp", height = "350px", width = "450px"),
  
  p("We aim to approach this issue from diverse perspectives: age, gender, income, and the overall trends. 
  Embracing varied viewpoints allows for a more nuanced comprehension of this poignant subject matter. 
  The issue is important for inspection as seen from its potential impact directly influencing countless lives globally, 
  notably the growing suicide rates and deaths. While numerous factors could contribute to this alarming trend, 
  we contend that social media, a hallmark of modern culture, stands as a significant role. Despite the growing awareness 
  of social media's negative impacts, individuals persist in its usage, often overshadowing its drawbacks. This omnipresence of 
  social media, coupled with the ceaseless influx of content, fuels its addictiveness, ensnaring users 
  in a perpetual cycle which poses particular challenges for younger audiences who are yet to develop the cognitive tools 
  necessary for discernment."),
  
  p("Thus, our pursuit is to ascertain and decipher potential links between social media usage and suicide rates. 
  Such revelations could serve as a clarion call, illuminating the darker facets of social media and 
  catalyzing collective action to monitor and address its impact."),
  
  p("For more information related to this topic, feel free to navigate following articles for more context:"),
  tags$a("Link 1: How Social Media Affects Teenagers", href = "https://childmind.org/article/how-using-social-media-affects-teenagers/"),
  br(),
  tags$a("Link 2: Study about link between Social Media & Mental Health", href = "https://mitsloan.mit.edu/ideas-made-to-matter/study-social-media-use-linked-to-decline-mental-health"),
  br(),
  tags$a("Link 3: Social Meida and Mental Health", href = "https://www.mcleanhospital.org/essential/it-or-not-social-medias-affecting-your-mental-health")
)

# **************************************
# The ACTUAL multiple page LAYOUT setting
# **************************************
ui <- navbarPage(
  "INFO 201 Final Project BH-8",
  tabPanel(
    "About",
    intro
  ),
  tabPanel("Gender & Age Groups", gender_page),
  tabPanel("Income", income_page),
  tabPanel("Overall", overall_trend),
  tabPanel("Conclusion", conclusion_page)
)


# ************************************
# Server for Outputs
# *************************************
server <- function(input, output) {
  
  # Overall Plots
  output$overall_scatter <- renderPlotly({
    if (input$GenderChoice == "Both") {
      yr <- filter(combo_df, Year <= input$year)
    } else {
      yr <- filter(combo_df, Year <= input$year, Gender == input$GenderChoice)
    }
    scatter <- ggplot(data = yr, aes(x = Year, y = death_vs_internet, color = Gender)) +
      geom_point() +
      geom_line() +
      labs(x = "Years", y = "Estimated Suicide Population via Internet")
    plot(scatter)
  })
  
  output$final_scatter_line <- renderPlotly({
    final_scatter <- ggplot(data = combo_df, aes(x = Year, y = death_vs_internet, color = Gender)) +
      geom_point() +
      geom_line() +
      ggtitle("Estimate Suicide Population via Internet Usage") +
      labs(x = "Years", y = "Estimated Suicide Population via Internet")
    plot(final_scatter)
  })
  
  output$internet_scatter <- renderPlotly({
    if (input$GenderChoice == "Both") {
      yr <- filter(combo_df, Year <= input$year)
    } else {
      yr <- filter(combo_df, Year <= input$year, Gender == input$GenderChoice)
    }
    scatter <- ggplot(data = yr, aes(x = Year, y = death_vs_internet, color = Gender)) +
      geom_point() +
      geom_line() +
      labs(x = "Years", y = "Estimated Suicide Population via Internet")
    plot(scatter)
  })
  
  # Gender Page Plots - Barplot
  output$gender_plot <- renderPlotly({
    
    fil_df <- fil_df %>% pivot_longer(cols=c('Male', 'Female'),
                                        names_to='Int_gender',
                                        values_to='Internet_Usage')
    
    if (input$gender == "Male" | input$gender == "Female") {
      fil_df <- filter(fil_df, Gender == input$gender)
    }
    
    gender <-  ggplot(data = fil_df, aes(x = Internet_Usage, y = ESTIMATE_pec, color = Gender)) + 
      geom_bar(stat = "identity") +
      labs(x = "Internet Usage by Gender (%)", 
           y = "Suicide Deaths per 100,000 resident population (%)",
           color = "Gender")
    
    
    plot(gender)

  })
  
  # Gender Plot - scatter
  output$gender_scatter <- renderPlotly({
    
    fil_df <- fil_df %>% pivot_longer(cols=c('Male', 'Female'),
                                      names_to='Int_gender',
                                      values_to='Internet_Usage')
    
    if (input$gender == "Male" | input$gender == "Female") {
      fil_df <- filter(fil_df, Gender == input$gender)
    }
    
    gender <-  ggplot(data = fil_df, aes(x = Internet_Usage, y = ESTIMATE_pec, color = Gender)) + 
      geom_point() +
      labs(x = "Internet Usage by Gender (%)", 
           y = "Suicide Deaths per 100,000 resident population (%)",
           color = "Gender")
    
    
    plot(gender)
    
  })
  
  # Age group Plots
  output$age_group_plot <- renderPlotly({
  
    ages <- fil_df %>%
      pivot_longer(cols = c('X18.29', 'X30.49', 'X50.64', 'X65.'),
                   names_to = 'age_groups', values_to = 'internet_usage_ages')
    
    if (input$age_group == "18-29") {
      ages <- ages %>%
        filter(str_detect(age_groups, "X18.29"))
    } else if (input$age_group == "30-49") {
      ages <- ages %>%
        filter(str_detect(age_groups, "X30.49"))
    } else if (input$age_group == "50-64") {
      ages <- ages %>%
        filter(str_detect(age_groups, "X50.64"))
    } else if (input$age_group == "65+") {
      ages <- ages %>%
        filter(str_detect(age_groups, "X65."))
    } 
    
    age_gp <- ggplot(ages, aes(x = internet_usage_ages, y = ESTIMATE_pec, color = age_groups)) +
      geom_point() +
      labs(title = paste("Media Usage and Suicide Rate by Age Group for", input$age_group),
           x = "Internet Usage by Age Groups (%)", y = "Suicide Deaths per 100,000 resident population (%)", 
           color = "Age Groups") 
    plot(age_gp)
    
  })
  
  #Income Page Plot
  output$income_plot <- renderPlotly({
    
    new_df <- fil_df %>% pivot_longer(cols=c('Less.than..30.000', 'X.30.000..49.999', 'X.50.000..74.999', 'X.75.000.'),
                                       names_to = 'Income_Group',
                                       values_to = 'Internet_usage_income')
    
    if (input$income == 2) {
      new_df <- new_df %>%
        filter(str_detect(Income_Group, "Less.than..30.000"))
    } else if (input$income == 3) {
      new_df <- new_df %>%
        filter(str_detect(Income_Group, "X.30.000..49.999"))
    } else if (input$income == 4) {
      new_df <- new_df %>%
        filter(str_detect(Income_Group, "X.50.000..74.999"))
    } else if (input$income == 5) {
      new_df <- new_df %>%
        filter(str_detect(Income_Group, "X.75.000."))
    }

    inc <- ggplot(new_df, aes(x = Internet_usage_income, y = ESTIMATE_pec, color = Income_Group)) +
      geom_point() +
      labs(x = "Internet Usage per Income Group (%)", 
           y = "Suicide Deaths per 100,000 resident population (%)",
           title = "Suicide Death Rates Compared to Income Groups",
           caption = "As mentioned previously, the two trends corresponds to the two genders (Male: Higher death rates; 
           Female: Lower death rates)",
           color = "Income Groups") 
    
    plot(inc)
  })
}

# ************************************
# Run the App
# *************************************
shinyApp(ui = ui, server = server)

