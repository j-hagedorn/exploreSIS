## ui.R ##

dashboardPage(
  dashboardHeader(
    title = "explore SIS"
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Productivity", 
        tabName = "productivity", 
        icon = icon("line-chart")
      ),
      menuItem(
        "Population Summary", 
        tabName = "support", 
        icon = icon("life-ring")
      ),
      menuItem(
        "Medical/Behavioral", 
        tabName = "med-beh", 
        icon = icon("medkit")
      ),
      menuItem(
        "Patterns of Need", 
        tabName = "pattern", 
        icon = icon("cubes")
      ),
      menuItem(
        "Compare Raters",
        tabName = "inter_rater",
        icon = icon("chain-broken")
      ),
      menuItem(
        "Use in Planning", 
        tabName = "planning", 
        icon = icon("paper-plane")
      ),
      menuItem(
        "Data Quality",
        tabName = "data_quality",
        icon = icon("database")
      ),
      menuItem(
        "Documentation", 
        tabName = "docs", 
        icon = icon("bookmark")
      ),
      selectInput(
        "region",
        label = "Pick a region:",
        choices = c("All", sort(levels(scrub_sis$PIHP))), 
        selected = "All"
      ),
      uiOutput(
        "agency"
      ),
      dateRangeInput(
        'dateRange',
        label = 'Date range:',
        start = begin, 
        end = max(as.Date(scrub_sis$sis_date)[as.Date(scrub_sis$sis_date) <= Sys.Date()])
      ),
      textOutput("valid_date"),
      br(),
      em(
        paste0("   Data updated ",max(as.Date(scrub_sis$sis_date)[as.Date(scrub_sis$sis_date) <= Sys.Date()]))
      )
    )
  ),
  dashboardBody(
    # Suppress errors from waiting on reactive dataframes to be built.
    tags$style(
      type = "text/css",
      ".shiny-output-error { visibility: hidden; }",
      ".shiny-output-error:before { visibility: hidden; }"
    ),
    tabItems(
      tabItem(
        tabName = "productivity",
        fluidRow(
          tabBox(
            width = 12,
            tabPanel(
              "Overall Completion",
              fluidRow(
                column(
                  width = 6,
                  valueBoxOutput(
                    "all_complete",
                    width = NULL
                  ),
                  valueBoxOutput(
                    "not_complete",
                    width = NULL
                  ),
                  box(
                    title = "About these indicators",
                    status = "warning",
                    collapsible = TRUE,
                    collapsed = TRUE,
                    width = NULL,
                    tabBox(
                      width = NULL,
                      tabPanel(
                        "Assessments Completed",
                        p(
                          "This indicator shows the total number of assessments 
                          that have completed.  Only the most recent assessment 
                          is counted for each person served, and assessments 
                          that have expired (",
                          em("e.g. those which are more than 3 years old"),
                          ") are not included in the count."
                        ),
                        p(
                          "Since the dataset does not indicate whether individuals 
                          are actively receiving services at the present time, 
                          this number may include some individuals who received 
                          an assessment but are no longer receiving services."
                        )
                      ),
                      tabPanel(
                        "Assessments Due",
                        p(
                          "This shows the total number of assessments currently 
                          due, including both initial assessments that have not 
                          yet been completed and re-assessments that are due but 
                          have not been completed."
                        ),
                        p(
                          "Note that re-assessments are shown as due for anyone 
                          who previously had a SIS assessment, and that this 
                          number does not account for individuals who may have 
                          left services."
                        ),
                        p(
                          "This number also does not include new individuals who 
                          have recently been admitted into services following the 
                          initial implementation of the SIS instrument between 
                          2014 and 2017."
                        )
                      )
                    )
                  )
                ),
                column(
                  width = 6,
                  plotlyOutput("per_mo_line"),
                  sliderInput(
                    "spread_init",
                    "Complete remaining initial assessments over ___ months:",
                    min = 6, max = 24,
                    value = 12
                  )
                )
              )
            ),
            tabPanel(
              "Ongoing Re-Assessment",
              fluidRow(
                column(
                  width = 6,
                  valueBoxOutput(
                    "overdue",
                    width = NULL
                  ),
                  valueBoxOutput(
                    "due30",
                    width = NULL
                  ),
                  valueBoxOutput(
                    "tocomplete",
                    width = NULL
                  ),
                  box(
                    title = "About these indicators",
                    status = "warning",
                    collapsible = TRUE,
                    collapsed = TRUE,
                    width = NULL,
                    tabBox(
                      width = NULL,
                      tabPanel(
                        "Overdue",
                        p("This indicator reflects the total number of assessments where the due
                          date has elapsed and a subsequent assessment has not yet been 
                          completed. The due date is defined as the maximum assessment date for
                          an individual plus 3 years (1,095 days).")
                      ),
                      tabPanel(
                        "Due in Next 30 Days",
                        p("This indicator reflects the total number of assessments that have a due
                          date that falls within the next 30 days. The due date is defined as the
                          maximum assessment date for an individual plus 3 years (1,095 days).")
                      ),
                      tabPanel(
                        "Complete ___ per month",
                        p("This indicator shows how many assessments will need to be completed 
                          this month in order to stay on track with the current workload. 
                          This metric is calculated by summing the number of overdue assessments 
                          and the number of assessments due within the next 12 months and dividing 
                          that total number by 12.  It also includes the remaining number 
                          of assessments due from initial implementation and distributes 
                          those over the 12-month period.")
                      )
                    ),
                    p(
                      "The indicators described above relate to the ongoing reassessment of all 
                      individuals who have received an initial SIS assessment.  Note that the 
                      following instances are not accounted for in these measures:",
                      tags$ul(
                        tags$li("If a person needs an initial SIS but has not received one"),
                        tags$li("If a person received an initial SIS but no longer meets criteria to receive one"),
                        tags$li("If a person has been newly admitted to services following the initial completion period")
                      )
                    )
                  )
                ),
                column(
                  width = 6,
                  box(
                    title = "Coming Up...", 
                    status = "warning",
                    collapsible = TRUE, 
                    width = NULL,
                    tabBox(
                      width = NULL,
                      tabPanel(
                        "By Month",
                        plotlyOutput("bymonth")
                      ),
                      tabPanel(
                        "By Days Due",
                        plotlyOutput("bydayct")
                      ),
                      tabPanel(
                        "About",
                        tabBox(
                          width = NULL,
                          tabPanel(
                            "Chart: By Month",
                            p("The chart here shows the total number of SIS assessments that have a due date
                              falling within each month. The due date is defined as the maximum assessment 
                              date for an individual plus 3 years (1,095 days).")
                          ),
                          tabPanel(
                            "Chart: By Days Due",
                            p("The chart here shows the total number of SIS assessments that have a due date
                              falling within the next X number of days. The due date is defined as the maximum
                              assessment date for an individual plus 3 years (1,095 days).")
                          )
                        )
                      )
                    )
                  )
                )
              )
            ),
            tabPanel(
              "Initial Implementation",
              fluidRow(
                column(
                  width = 6,
                  valueBoxOutput(
                    "rate",
                    width = NULL
                  ), 
                  valueBoxOutput(
                    "complete",
                    width = NULL
                  ), 
                  valueBoxOutput(
                    "needperwk",
                    width = NULL
                  ),
                  box(
                    title = "About these indicators", 
                    status = "warning",
                    collapsible = TRUE, 
                    collapsed = TRUE,
                    width = NULL,
                    tabBox(
                      width = NULL,
                      tabPanel(
                        "Completion Rate",
                        p("This basically answers the question", 
                          em("Am I on track?"), 
                          "  It's a comparison of 2 percentages: ",
                          br(),
                          strong("Numerator: % of assessments completed"),
                          "This calculates the number of unique clients interviewed in 
                          the selected date range as a percentage of the total clients 
                          currently meeting criteria for assessment", 
                          em("(from encounter data indicating services were received 
                             for a developmental disability diagnosis)"),
                          br(),
                          strong("Denominator: % of time elapsed"),
                          "This calculates the number of days between the first and last 
                          dates selected in the date range filter as a percentage of the 
                          number of days between the first date in the date range filter 
                          and the due date for initial SIS assessments.  Note that because 
                          the due date has passed, the denominator will continue to grow 
                          beyond 100%, since more than the required timeframe has elapsed."
                        ), 
                        p(
                          "So, for example, a score of 100% means that you're completing 
                          assessments at the rate needed to meet the goal, and a score 
                          of 200% would mean that you're completing assessments twice as 
                          quickly as needed to meet your goal, and so on..."
                        )
                        ),
                      tabPanel(
                        "% Complete",
                        p(
                          "This indicator calculates the number of unique clients 
                          interviewed in the selected date range as a percentage of the 
                          total clients currently meeting criteria for assessment."
                        ),
                        p(
                          "Assessments are indicated for people who are 18 years 
                          or older, who have an intellectual or developmental disability, 
                          have Medicaid/Healthy MI Medicaid, and are currently receiving 
                          defined services (",
                          em("i.e. Case Management, Supports Coordination, Respite, 
                             Home based or Assertive community treatment"),
                          ").  The denominator excludes individuals whose cases were 
                          closed, or who declined or refused the assessment, when 
                          these numbers have been reported to MDHHS."
                        ),
                        p(
                          em("P.S.  This is exactly the same as the numerator of the"), 
                          strong("Completion Rate")
                        )
                      ),
                      tabPanel(
                        "Complete ___ per week",
                        p(
                          "This indicator is no longer shown, since the due date 
                          for completion has passed and it is no longer possible 
                          to reach the goal.  All outstanding assessments must be 
                          completed in order to be in compliance."
                        )
                      )
                    )
                  )
                ),
                column(
                  width = 6,
                  box(
                    title = "How Overdue?", 
                    status = "warning",
                    collapsible = TRUE, 
                    width = NULL,
                    tabBox(
                      width = NULL,
                      tabPanel(
                        "Chart", 
                        dygraphOutput("on_track")
                      ),
                      tabPanel(
                        "Table",
                        dataTableOutput("num_dt"),
                        br(),
                        p("Note: Assessments with a negative duration time were removed.")
                      ),
                      tabPanel(
                        "What if...?",
                        p(
                          "Use the sliders below to understand the potential impact
                          that various changes might have on interviewer productivity:"
                        ),
                        strong("What if..."),
                        uiOutput("what_staff"),
                        uiOutput("what_prod"),
                        dygraphOutput("on_track_what_if")
                      ),
                      tabPanel(
                        "About",
                        tabBox(
                          width = NULL,
                          tabPanel(
                            "Chart",
                            br(),
                            strong("Due Date"),
                            p(
                              "When the SIS was selected by the state for implementation 
                              in 2014, an expectation was set that all individuals meeting 
                              criteria for assessment would be assessed within 3 years."
                            ),
                            p(
                              "Agencies implementing the SIS have been wondering how much 
                              person-time to devote to SIS assessments.  Should this be a 
                              full time person? Part time?  How many assessors do we need 
                              to cover the region on an ongoing basis?"
                            ),
                            strong("Projected Timeline"), 
                            p(
                              "The chart here shows the cumulative number of SIS 
                              assessments completed per week.  The green line shows actual 
                              historical data, while the dotted line shows what will 
                              happen if the region's historical SIS completion rate 
                              continues as it has for the past 3 months?"
                            ),
                            p(
                              "The future trendline is generated for as many weeks as 
                              will be needed in order to complete SIS assessments for the 
                              population initially identified as eligible.  Note that this 
                              projected completion date will vary depending on the PIHP/CMH 
                              selected.  The projected completion date for the state or for 
                              a given region assumes an even and consistent effort for all 
                              organizations within it, and if this is not possible then 
                              completion may take longer."
                            )
                          ),
                          tabPanel(
                            "Table",
                            br(),
                            strong("Assessments per Interviewer"),
                            p(
                              "In this table, you can see the total number of SIS 
                              assessments for each interviewer.  Only current interviewers 
                              are displayed."
                            ),
                            p(
                              "To define the interviewer for a given assessment, we use 
                              the field recommended by AAIDD for inter-rater reliability 
                              work.  This field may occasionally ascribe some assessments 
                              incorrectly due to errors in data entry."
                            ),
                            p(
                              "The average number of assessments per interviewer is",
                              round(scrub_sis %>% filter(current_int == T) %>%
                                      group_by(interviewer, agency) %>%
                                      summarize(n = n()) %>% ungroup() %>% 
                                      summarize(avg = mean(n, na.rm = T)), 
                                    digits = 1), 
                              ". Please remember that each interviewer has been completing 
                              assessments for various lengths of time and each have a 
                              different proportion of their position designated to 
                              completing SIS assessments."
                            ),
                            p(
                              "Clicking on the green 'plus' signs next to each row allows 
                              you to see the values for columns that don't fit in the 
                              view.  To change which columns are visible, click 'Show/Hide 
                              Columns' in the upper right corner."
                            ),
                            strong("Duration"),
                            p(
                              "In order to estimate how many people hours need to be 
                              dedicated to complete SIS assessments for eligible clients, 
                              we need to get a sense of how long it takes to do a SIS 
                              assessment."
                            ),  
                            p(
                              "While there are some issues which still need to be resolved 
                              with the data, assessments across the region have taken an 
                              average of ",
                              round(mean(scrub_sis$duration, na.rm = T), digits = 1), 
                              " minutes (median = ", 
                              round(median(scrub_sis$duration, na.rm = T), digits = 1), 
                              "). Adding some standard assumptions for travel time and 
                              documentation should allow for a basic estimate of the time 
                              required to complete a single SIS assessment.  From there, 
                              we can estimate the total number of hours which will need to 
                              be devoted to assessment in the region and compare that to 
                              the available hours of existing SIS interviewers in the 
                              region during the time remaining."
                            )
                          ),
                          tabPanel(
                            "What if...?",
                            br(),
                            h4("Sliders"),
                            p(
                              "You can use the sliding bars to look at what might 
                              happen using various scenarios where the number of 
                              interviewers or the productivity of those interviewers 
                              are either decreased or increased.  The first sliding 
                              bar starts at zero and lets you add or substract 
                              interviewers. The second starts at the current average 
                              per week and lets you change to half or 1.5 times that 
                              amount."
                            ),
                            h4("The Fine Print"),
                            p(
                              "The projections here make several assumptions in their 
                              calculations, which are helpful to understand:",
                              br(),
                              strong("Scheduling:"),
                              "It is possible that SIS interviews are not being 
                              completed quickly enough to cover the entire population 
                              due to issues with scheduling SIS assessments", 
                              em(
                                "(i.e. it's possible that you have enough people, with 
                                enough capacity, but there aren't appointments being 
                                scheduled in an efficient enough manner to keep everyone 
                                busy)"
                              ),
                              br(),
                              strong("Based on current practice:"), 
                              "The default min and max for the ",
                              em("assessments per week")," slider is 
                              based on the average number of assessments per 
                              interviewer over the past 3 months for the 
                              organization(s) selected. Users should be aware 
                              that drastic changes in productivity expectations 
                              without accompanying increases in staffing may have 
                              implications for the quality of assessments.",
                              br(),
                              strong("FTEs dedicated to SIS:"),
                              "The projection assumes that each interviewer has the 
                              same proportion of their position dedicated to SIS 
                              interviews, though this may not be accurate.  People may 
                              have other jobs besides SIS assessment, in which case it 
                              is not accurate to assume that everyone has the same 
                              capacity. Adding data about FTEs dedicated to SIS 
                              assessments would allow for this to be more accurate."
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "support",
        fluidRow(
          column(
            width = 6,
            box(
              title = "Support Needs Index", 
              status = "warning",
              collapsible = TRUE, 
              width = NULL,
              tabBox(
                width = NULL,
                tabPanel(
                  "Distribution",
                  plotlyOutput("hist_sni"),
                  br(),
                  box(
                    title = "Chart settings", 
                    color = "black",
                    collapsible = TRUE,
                    collapsed = T,
                    width = NULL,
                    radioButtons(
                      "central",
                      label = "Display:",
                      choices = c("Mean", "Median"), 
                      selected = "Mean",
                      inline = T
                    ),
                    sliderInput(
                      "sni_bins", 
                      "Number of bins:", 
                      min = 1, 
                      max = 30, 
                      value = 10
                    )
                  )
                ),
                tabPanel(
                  "Normal Distribution?",
                  plotlyOutput("norm_sni"),
                  br(),
                  box(
                    title = "Chart settings", 
                    color = "black",
                    collapsible = TRUE,
                    collapsed = T,
                    width = NULL,
                    sliderInput(
                      "norm_bins", 
                      "Number of bins:", 
                      min = 1, 
                      max = 30, 
                      value = 10
                    )
                  )
                ),
                tabPanel(
                  "Avg Subscale Scores",
                  dataTableOutput("dt_sni")
                ),
                tabPanel(
                  "About", 
                  tabBox(
                    width = NULL,
                    tabPanel(
                      "SIS Support Needs Scale",
                      p(
                        "The Support Needs Scale (Section 2 of the SIS) consists 
                        of 49 activities grouped into six domains: ",
                        em(
                          "Home Living, Community Living, Lifelong Learning, 
                          Employment, Health and Safety, and Social Activities."
                        ), 
                        "The ", em("Support Needs Index"), "is a composite score 
                        that reflects a person’s overall intensity of support 
                        needs across these domains, relative to the SIS scores 
                        of others with developmental disabilities."
                      ),
                      p(
                        "The SIS also measures exceptional Medical and Behavioral 
                        Support Needs, assessing 18 medical conditions and 12 
                        problem behaviors. Since certain medical conditions and 
                        challenging behaviors may require additional support, 
                        these items indicate instances where the ", 
                        em("Support Needs Index"), " may not fully reflect a 
                        person’s need for support."
                      )
                    ),
                    tabPanel(
                      "Distribution",
                      p(
                        "The graph shown in the 'Distribution' tab is called a 
                        histogram. A histogram groups numeric data into bins, 
                        displaying the bins as columns. They are used to show the 
                        distribution of a dataset, i.e. how often values fall 
                        into ranges."
                      ),
                      p(
                        "The histogram here shows the distribution of scores for 
                        all people assessed within the region.  Different colors 
                        are used to indicate the various agencies, and the dotted 
                        line shows the average for the selected agency"
                      )
                    ),
                    tabPanel(
                      "Normal Distribution?",
                      p(
                        "If you've been working with people who analyze data, you 
                        may have heard them use terms like", 
                        em("bell curve"), "or", em("normal distribution"), ".  ",
                        "That's what this chart looks at."
                      ),
                      p(
                        "The ", em("Support Needs Index"), " is normalized, which 
                        means that it is designed in such a way that its mean is 
                        100 and its standard deviation is 15, based on the 
                        initial group of people who were tested using the tool.  
                        If the ", 
                        em("Actual Scores"), " of the current population were 
                        distributed in the same way, they would fit under the ", 
                        em("bell curve"), "in the same way as the ", 
                        em("Standard Scores"), ".  Please note that agencies 
                        should not expect their data to match the bell curve 
                        until all of their population has been assessed."
                      ),
                      p(
                        "A density plot (which is what the lines are called) can 
                        be tricky to understand  You can think of it as a smoothed 
                        out histogram.  The y-axis measurement ", em("Density"),
                        " is also different.  It boils down to this: the area 
                        under the entire curve is 1, and the probability of a 
                        value being between any two points on the x-axis is the 
                        area under the curve between those two points.  If you 
                        hover over a single point on the x-axis that says 80 and the 
                        y-axis says .01, what that means is that there is a probability 
                        of getting an x value of 80 around 1% of the time.",
                        "For a more technical explanation, check out the ",
                        a(href = "https://en.wikipedia.org/wiki/Probability_density_function",
                          "probability density function"
                        ), 
                        "."
                      )
                    ),
                    tabPanel(
                      "Avg Subscale Scores",
                      p(
                        "The table in the 'Breakdown' tab shows the average scores 
                        for clients assessed by each agency across the sub-scales of 
                        the SIS which make up the ",
                        em("Support Needs Index"), ".  You can also look at the 
                        number of assessments completed by each agency."
                      ),
                      p(
                        "The width of the colored bars in the table corresponds to 
                        the numeric value within the cell, allowing an at-a-glance 
                        comparison of average scores in each sub-scale.  The table 
                        can be sorted by the values in any column."
                      ),
                      p(
                        "Clicking on the green 'plus' signs next to each row allows 
                        you to see the values for columns that don't fit in the 
                        view.  To change which columns are visible, click 'Show/Hide 
                        Columns' in the upper right corner."
                      )
                    )
                  )
                )
              )
            )
          ),
          column(
            width = 6,
            box(
              title = "Types of Need (Section 2: Supports)", 
              status = "warning",
              collapsible = TRUE,
              collapsed = F,
              width = NULL,
              selectInput(
                "select_area_q2",
                label = "Select life area:",
                choices = c("All", levels(as.factor(q2$section_desc)))
              ),
              tabBox(
                width = NULL,
                tabPanel(
                  "About",
                  tabBox(
                    width = NULL,
                    tabPanel(
                      "Understanding Type of Need",
                      p(
                        "While a global measure like the ", em("Support Needs Index"),
                        " can help to give an overall picture of the intensity 
                        of an individual’s support needs, the same score could 
                        be made up of various types of need (",
                        em("such as full physical support or coaching"),
                        ") in various areas of a person's life (",
                        em("such as eating food or social skills"),
                        ").  The data presented here allows you to start 
                        looking at the number of people who have similar support 
                        needs in any given life area."
                        ),
                      p(
                        "You could use this to ask the following questions (",
                        em("and more"),"):",
                        tags$ul(
                          tags$li("How many people need support with housekeeping? What type of support? How much support?  How often?"),
                          tags$li("Of the people who need full physical support with accessing emergency services, how many are in my region?"),
                          tags$li("Which specific needs related to living at home have the most variation across people who were assessed?")
                        )
                      )
                        ),
                    tabPanel(
                      "The Table",
                      p(
                        "The table shows the average (", em("All"), 
                        ") and standard deviation (", em("StDev"),
                        ") of raw scores for each item from the selected 
                        section of the SIS.  The standard deviation can be used 
                        to look at items whose total score has the greatest 
                        amount of variation across assessments.  You can then 
                        explore potential causes of variation using the 
                        visualization in the ", em("Chart"), " tab and selecting 
                        the item you want to investigate."
                      ),
                      p(
                        "When a specific agency is selected, the table will 
                        display the average (", em("All.Others"),
                        ") of scores for all agencies other than the one 
                        selected and the difference between the selected 
                        agency's average score and the average of other 
                        agencies (", em("Difference"), ") in order allow easy 
                        identification of areas where a given agency's scores 
                        are higher or lower than their peers."
                        )
                      ),
                    tabPanel(
                      "The Chart", 
                      br(),
                      strong("This wobbly-looking chart..."),
                      p(
                        "Take a deep breath.  Don't run away screaming from the 
                        'dancing octopus' chart that you see here. Let's take a 
                        moment to understand how it might help us out."
                      ),
                      p(
                        "Have you ever asked a question sounding like: ",
                        em(
                          "What percent of people are ____? What percent of those 
                          people are _____?"
                        ), 
                        " and so on...?  If so, then this is the visualization for 
                        you."
                        ), 
                      p(
                        "With this chart you can ask multiple questions such as:",
                        br(),
                        em(
                          "In the area of money management, how many people were 
                          assessed as needing prompting on a daily basis?  How 
                          many hours per day were they identified as needing it?"
                        ),
                        br(),
                        em(
                          "In the area of protection from exploitation, how many 
                          people were assessed as needing full physical support 
                          on a monthly basis? How many minutes were they 
                          identified as needing it?"
                        )
                      ),
                      p(
                        "When you click on", em("alpha"), " or ", em("size"), 
                        ", the chart sorts the variable alphabetically or by size.  
                        You can sort either of these from low-to-high (<<) or 
                        high-to-low (>>)."
                      ),
                      p(
                        "The different colors on the chart correspond to the 
                        variable at the top of the chart, so if 'Type of Support' is 
                        on the top, there will be one color from each type of 
                        support weaving down through the other variables."
                      )
                    ),
                    tabPanel(
                      "The Data", 
                      br(),
                      strong("Type of Support"),
                      p(
                        "When completing Section 2, the support needs for each 
                        activity addressed in the section are examined 
                        with regard to three measures of support need:",
                        br(),
                        tags$ul(
                          tags$li(
                            em("Frequency:"), 
                            "How often extraordinary support (i.e., support 
                            beyond that which is typically needed by most 
                            individuals without disabilities) is required for 
                            each targeted activity."
                          ),
                          tags$li(
                            em("Daily Support Time:"), 
                            "Amount of time that is typically devoted to support 
                            provision on those days when the support is provided."
                          ),
                          tags$li(
                            em("Type of Support:"), 
                            "The nature of support that would be needed by a 
                            person to engage in the activity in question."
                          )
                        )
                    ),
                    strong("Definitions"),
                    p(
                      "The chart uses short words and phrases for ease of use. 
                      Here are the full definitions from the SIS tool itself ",
                      a(
                        href = "http://aaidd.org/docs/default-source/sis-docs/sisfrequencyandscoringclarifications.pdf?sfvrsn=2",
                        "as defined by AAIDD"
                      ),":",
                      br(),
                      em("Frequency:"),
                      tags$ul(
                        tags$li(
                          "Hourly = hourly or more frequently" 
                        ),
                        tags$li(
                          "Daily = at least once a day but not once an hour"
                        ),
                        tags$li(
                          "Weekly = at least once a week, but not once a day"
                        ),
                        tags$li(
                          "Monthly = at least once a month, but not once a week"
                        ),
                        tags$li(
                          "None = none or less than monthly"
                        )
                      ),
                      br(),
                      em("Daily Support Time (DST):"),
                      tags$ul(
                        tags$li(
                          "Over 4 hrs = 4 hours or more"
                        ),
                        tags$li(
                          "2-4 hrs = 2 hours to less than 4 hours"
                        ),
                        tags$li(
                          "Under 2 hrs = 30 minutes to less than 2 hours"
                        ),
                        tags$li(
                          "Under 30 min = less than 30 minutes"
                        ),
                        tags$li(
                          "None = None"
                        )
                      )
                    )
                  )
                )
                ),
                tabPanel(
                  "Table",
                  dataTableOutput("q2_dt")
                ),
                tabPanel(
                  "Chart",
                  uiOutput('q2domain'),
                  parsetOutput("tos_q2")
                )
              )
            ),
            box(
              title = "Types of Need (Section 3: Protection)", 
              status = "warning",
              collapsible = TRUE, 
              collapsed = TRUE,
              width = NULL,
              tabBox(
                width = NULL,
                tabPanel(
                  "Summary",
                  dataTableOutput("q3_dt")
                ),
                tabPanel(
                  "Chart",
                  uiOutput('q3domain'),
                  parsetOutput("tos_q3")
                ),
                tabPanel(
                  "About",
                  tabBox(
                    width = NULL,
                    tabPanel(
                      "The Table",
                      p(
                        "The table shows the average (", em("All"), 
                        ") and standard deviation (", em("StDev"),
                        ") of raw scores for each item from section 3 of the SIS,
                        which addresses needs related to protection and advocacy.  
                        The standard deviation can be used 
                        to look at items whose total score has the greatest 
                        amount of variation across assessments.  You can then 
                        explore potential causes of variation using the 
                        visualization in the ", em("Chart"), " tab and selecting 
                        the item you want to investigate."
                      ),
                      p(
                        "When a specific agency is selected, the table will 
                        display the average (", em("All.Others"),
                        ") of scores for all agencies other than the one 
                        selected and the difference between the selected 
                        agency's average score and the average of other 
                        agencies (", em("Difference"), ") in order allow easy 
                        identification of areas where a given agency's scores 
                        are higher or lower than their peers."
                        )
                      ),
                    tabPanel(
                      "The Chart", 
                      br(),
                      strong("This wobbly-looking chart..."),
                      p(
                        "Take a deep breath.  Don't run away screaming from the 
                        'dancing octopus' chart that you see here. Let's take a 
                        moment to understand how it might help us out."
                      ),
                      p(
                        "Have you ever asked a question sounding like: ",
                        em(
                          "What percent of people are ____? What percent of those 
                          people are _____?"
                        ), 
                        " and so on...?  If so, then this is the visualization for 
                        you."
                        ), 
                      p(
                        "With this chart, you can ask multiple questions like:",
                        br(),
                        em(
                          "In the area of money management, how many people were 
                          assessed as needing prompting on a daily basis?  How 
                          many hours per day were they identified as needing it?"
                        ),
                        br(),
                        em(
                          "In the area of protection from exploitation, how many 
                          people were assessed asa needing full physical support 
                          on a monthly basis? How many minutes were they 
                          identified as needing it?"
                        )
                      ),
                      p(
                        "When you click on", em("alpha"), " or ", em("size"), 
                        ", the chart sorts the variable alphabetically or by size.  
                        You can sort either of these from low-to-high (<<) or 
                        high-to-low (>>)."
                      ),
                      p(
                        "The different colors on the chart correspond to the 
                        variable at the top of the chart, so if 'Type of Support' is 
                        on the top, there will be one color from each type of 
                        support weaving down through the other variables."
                      )
                    ),
                    tabPanel(
                      "The Data", 
                      br(),
                      strong("Type of Support"),
                      p(
                        "When completing Section 3, the support needs for each 
                        activity related to protection and advocacy are examined 
                        with regard to three measures of support need:",
                        br(),
                        em("Frequency:"), 
                        "How often extraordinary support (i.e., support beyond that 
                        which is typically needed by most individuals without 
                        disabilities) is required for each targeted activity.",
                        br(),
                        em("Daily Support Time:"), 
                        "Amount of time that is typically devoted to support 
                        provision on those days when the support is provided.",
                        br(),
                        em("Type of Support:"), 
                        "The nature of support that would be needed by a person to 
                        engage in the activity in question."
                    ),
                    strong("Definitions"),
                    p(
                      "The chart uses short words and phrases for ease of use. 
                      Here are the full definitions from the SIS tool itself ",
                      a(
                        href = "http://aaidd.org/docs/default-source/sis-docs/sisfrequencyandscoringclarifications.pdf?sfvrsn=2",
                        "as defined by AAIDD"
                      ),":",
                      br(),
                      em("Frequency:"),
                      "Hourly = hourly or more frequently; 
                      Daily = at least once a day but not once an hour; 
                      Weekly = at least once a week, but not once a day; 
                      Monthly = at least once a month, but not once a week; 
                      None = none or less than monthly",
                      br(),
                      em("Daily Support Time (DST):"),
                      "Over 4 hrs = 4 hours or more; 
                      2-4 hrs = 2 hours to less than 4 hours; 
                      Under 2 hrs = 30 minutes to less than 2 hours; 
                      Under 30 min = less than 30 minutes; 
                      None = None"
                    )
                    )
                  )
                )
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "med-beh",
        fluidRow(
          column(
            width = 12,
            box(
              title = "Chart settings", 
              color = "black",
              collapsible = TRUE,
              collapsed = T,
              width = NULL,
              radioButtons(
                "radio_mb",
                label = "Display:",
                choices = c("Medical", "Behavioral", "Either"), 
                selected = "Either",
                inline = T
              ),
              sliderInput(
                "mb_bins", 
                "Number of bins:", 
                min = 1, 
                max = 30, 
                value = 6
              )
            )
          ),
          column(
            width = 6,
            box(
              title = "How Many Medical & Behavioral Needs?", 
              status = "warning",
              collapsible = TRUE, 
              width = NULL,
              tabBox(
                width = NULL,
                tabPanel(
                  "Distribution",
                  plotlyOutput("hist_mb")
                ),
                tabPanel(
                  "About",
                  p(
                    "The graph shown in the 'Distribution' tab is called a 
                    histogram. A histogram groups numeric data into bins, 
                    displaying the bins as columns. They are used to show the 
                    distribution of a dataset, i.e. how often values fall into 
                    ranges.  Here, each bin has a range of one, so each column 
                    shows how many people have 1 condition, 2 conditions, and so 
                    on."
                  ),
                  p(
                    "The histogram here shows the distribution of conditions for 
                    people assessed within the region.  Since we're looking at 
                    medical and behavioral conditions which require a 
                    substantial amount of support, it isn't surprising that most 
                    people have only a few of these, falling on the left side of 
                    the histogram."
                  ),
                  p(
                    "When you select 'All' agencies, different colors are used 
                    to indicate the various agencies.  You can hover over the 
                    columns of histogram to see the exact number of people 
                    represented by each bin."
                  )
                )
              )
            )
          ),
          column(
            width = 6,
            box(
              title = "By Type", 
              status = "warning",
              collapsible = TRUE, 
              width = NULL,
              tabBox(
                width = NULL,
                tabPanel(
                  "Graph",
                  plotlyOutput("conditions")
                ),
                tabPanel(
                  "About",
                  h4("Section 1 (a.k.a. Exceptional Medical and Behavioral Support Needs)"),
                  p(
                    "Certain medical conditions and challenging behaviors are 
                    related to increased levels of support, regardless of 
                    support needs in other life areas. This section of the SIS 
                    looks at 18 medical conditions and 13 problem behaviors 
                    indicative of need for more intensive or more frequent 
                    supports due to higher risk of harm or vulnerability.  "
                  ),
                  h4("Scoring"),
                  p(
                    "The question that the items in this section attempt to 
                    answer is:",
                    em(
                      "How significant are the following medical/behavioral 
                      conditions for the extra support required for this 
                      person?"
                    )
                  ),
                  p(
                    "Each item in the section is scored with one of the 
                    following ratings:",
                    br(),
                    strong("No support needed (0) "),
                    "No support needed because the condition is not an issue, or 
                    requires no support", 
                    br(),
                    strong("Some Support Needed (1) "),
                    "Some support needed to address the condition(s).  People 
                    who support must be continuously aware of the condition to 
                    assure the individual’s health and safety.",
                    br(),
                    strong("Extensive Support Needed (2) "),
                    "Extensive support is needed to address the condition, such as significant physical effort or time required."
                  ),
                  br(),
                  em("Based on ",
                     a(
                       href = "https://www.ascendami.com/ami/Portals/1/VA%20SIS%20Resources/Supports_Intensity_Scale.pdf",
                       "guidance published by AAIDD"
                    )
                  )
                )
              )
            )
          ) 
        )
      ),
      tabItem(
        tabName = "pattern",
        column(
          width = 4,
          box(
            title = "Defining Patterns", 
            status = "warning",
            collapsible = TRUE, 
            width = NULL,
            tabBox(
              width = NULL,
              tabPanel(
                "What groups?",
                p(
                  "To design services that meet people where they are, it 
                  helps to understand patterns in the types of needs that 
                  people have in various areas of their lives. You can 
                  check out the ", em("Why group?"), " tab for examples of 
                  scenarios where this may be useful."  
                ),
                h4("How many groups of people?"),
                p(
                  "Depending on your particular situation, you may want to 
                  focus on greater or fewer groups of clients.  You can 
                  select the number of groups here: "
                ),
                numericInput(
                  inputId = "need_rows",
                  label = NULL, 
                  value = 5,
                  min = 1, 
                  max = 10,
                  width = '100px'
                ),
                p(
                  "Then, view the summary table and expand the other tabs to explore 
                  the groups in your population..."
                ),
                box(
                  title = "Picking a number", 
                  color = "black",
                  collapsible = TRUE, 
                  collapsed = T,
                  width = NULL,
                  p(
                    "This is called a scree plot, and it shows the amount of 
                    variation explained by each additional cluster added.  
                    You'll note that after a certain point, the line starts 
                    going straight across the bottom, which means that new 
                    clusters don't do much to identify coherent groups.  
                    To find the maximum number of meaningful clusters, don't go 
                    farther than the 'elbow' of the chart below..."
                  ),
                  plotlyOutput("need_scree")
                )
              ),
              tabPanel(
                "Why group?",
                p(
                  "It is important to note that planning for services in a 
                  community must take into account a variety of factors.  
                  Understanding individuals' support needs based on the SIS is 
                  just one aspect of planning a service delivery system or set 
                  of services and supports in a local community.   Individuals' 
                  preferences for who and how services and supports are 
                  delivered must be actively solicited in any collaborative 
                  service planning effort."
                ),
                p(
                  "With this in mind, the following scenarios provide examples 
                  of instances where it may be helpful to define patterns of 
                  need in the population being served:"
                ),
                box(
                  title = "Build Specialized Teams", 
                  color = "black",
                  collapsible = TRUE, collapsed = T, width = NULL,
                  p(
                    "A supervisor of a supports coordination team for persons 
                    with I/DD would like to start a pilot program providing 
                    intensive, multi-disciplinary team-based integrated care for 
                    persons with high behavioral health and physical health 
                    needs.  Clustering could be used to identify groups of 
                    individuals for whom this intervention could be considered. 
                    Different numbers of groups could be selected to assist in 
                    planning and understanding resource needs.",
                    em(
                      "(For instance, if she were trying to assign people to 
                      3 supports coordination programs, she may want to highlight 
                      3 groups)"
                    )
                  )
                ),
                box(
                  title = "Implement Best Practice Guidelines", 
                  color = "black",
                  collapsible = TRUE, collapsed = T, width = NULL,
                  p(
                    "A clinical director would like to identify practice 
                    guidelines to assist staff in recommending services and 
                    supports based on individuals’ needs. A first step could be 
                    taking the domains listed on the heat map and identifying 
                    any evidence-based or best practice interventions that may 
                    meet the identified needs for people with I/DD."
                  )
                ),
                box(
                  title = "Identify Training Needs", 
                  color = "black",
                  collapsible = TRUE, collapsed = T, width = NULL,
                  p(
                    "A manager could look at the clusters to determine which 
                    areas represent the highest need or most common needs within 
                    the I/DD population served to better understand the types of 
                    training that could benefit staff providing services and 
                    supports."
                  )
                )
              ),
              tabPanel(
                "How?",
                p(
                  "Clustering is an exploratory technique that allows similar 
                  (or very dissimilar) scores to be identified and grouped 
                  together in meaningful ways.  While it won't give you any 
                  conclusive results, it often generates insights and additional 
                  questions for analysis.  Here, we provide two different 
                  methods of clustering, ", em("k-means"), " and ", 
                  em("hierarchical clustering.")
                ),
                box(
                  title = "K-means clustering", 
                  color = "black",
                  collapsible = TRUE, collapsed = T, width = NULL,
                  p(
                    "This algorithm uses the following procedure to classify a 
                    given dataset into a certain number (k) of clusters:",
                    tags$ul(
                      tags$li("define k centroids, as far away from each other as possible"),
                      tags$li("take each point in the data and associate it to the nearest centroid"),
                      tags$li("re-calculate new centroids at the center of the cluster resulting from the previous step"),
                      tags$li("repeat until the centroids do not move any more")
                    ),
                    "The algorithm is faster and easier to use on larger datasets,
                    though it requires users to identify a number of clusters in 
                    order to be run."
                  ),
                  p(
                    "If you want to walk through a visual explanation of how k-means 
                    algorithms work, you can check out ",
                    a(
                      href = "http://stanford.edu/class/ee103/visualizations/kmeans/kmeans.html",
                      "this site from Stanford."
                    )
                  )
                ),
                box(
                  title = "Hierarchical clustering", 
                  color = "black",
                  collapsible = TRUE, collapsed = T, width = NULL,
                  p(
                    "Hierarchical clustering takes a ",
                    a(
                      href = "https://en.wikipedia.org/wiki/Distance_matrix",
                      "distance matrix"
                    ),
                    " and uses the following procedure to classify each item from 
                    a given dataset into a multiple levels of clusters:",
                    tags$ul(
                      tags$li("Assign each item to a cluster, so that if you have 
                              N items, you now have N clusters, each with one item"),
                      tags$li("Find the closest pair of clusters and merge them, 
                              so there is one less cluster"),
                      tags$li("Compute distances between the new cluster and each 
                              of the old clusters"),
                      tags$li("Repeat steps 2 and 3 until all items are in single 
                              cluster of size N")
                    )
                  )
                )
              )
            )
          )
        ),
        column(
          width = 8,
          box(
            title = "Summary of Clusters",
            status = "warning",
            collapsible = T,
            width = NULL,
            p(
              em("The table below shows information for each of the clusters, 
                 as well as the average scaled scores on each subscale of the  
                 SIS for people in that cluster:")
            ),
            DT::dataTableOutput("need_grp_dt"),
            box(
              title = "Settings", 
              color = "black",
              collapsible = TRUE, collapsed = T, width = NULL,
              radioButtons(
                "dt_clust_type",
                label = "Summarize groups using:",
                choices = c("k-means clusters","hierarchical clusters"), 
                selected = "k-means clusters",
                inline = T
              ),
              p(
                "To dig in and analyze the groupings created by the algorithm, 
                please visit the ", em("Visualizing Groups"), " tab, below."
              )
            )
          ),
          uiOutput("cluster_viz_ui")
        )
      ),
      tabItem(
        tabName = "inter_rater",
        fluidRow(
          column(
            width = 12,
            box(
              title = "Productivity Trends",
              status = "warning",
              collapsible = TRUE,
              width = 6,
              tabBox(
                width = NULL,
                tabPanel(
                  "Productivity Trends",
                  radioButtons(
                    "metric",
                    label = "Select a productivty metric:",
                    choices = c("# of Assessments","% of Total","Average Hours"),
                    selected = "# of Assessments",
                    inline = T
                  ),
                  plotlyOutput("int_prod"),
                  radioButtons(
                    "current_prod",
                    label = "Display:",
                    choices = c("Current assessors", "All Assessors"),
                    selected = "Current assessors",
                    inline = T
                  )
                ),
                tabPanel(
                  "About",
                  h4("Productivity Trends..."),
                  p(
                    "The charts here show a variety of weekly productivity 
                    metrics for each SIS interviewer. Different display options 
                    include:",
                    br(),
                    strong("% of Total: "),
                    "The percentage of total assessments per week that each 
                    interviewer comprises.",
                    br(),
                    strong("Average Hours: "),
                    "The average number of hours spent completing an assessment 
                    for the given week.",
                    br(),
                    strong("# of Assessments: "),
                    "The total number of assessments completed for the given week."
                  )
                )
              )
            ),
            box(
              title = "Subscale Comparison",
              status = "warning",
              collapsible = TRUE,
              width = 6,
              tabBox(
                width = NULL,
                tabPanel(
                  "Subscale Comparison",
                  uiOutput('box_opts'),
                  plotlyOutput("box_int")
                ),
                tabPanel(
                  "About",
                  h4("Subscale Comparison..."),
                  p(
                    "The boxplots here show a summary of scores. When viewing all regions,
                    the boxplots display each subscale score in aggregate. When a single
                    region is select, the boxplots show a summary of scores for all current
                    interviewers for the selected subscale.  A boxplot shows key information 
                    about the distribution of a measure, i.e. how it is spread out.  It is
                    made up of the following pieces:",
                    br(),
                    strong("Median: "),
                    "The mid-point of the data is shown by the line that divides
                    the box into two parts. Half the scores are greater than or
                    equal to this value, half are less.",
                    br(),
                    strong("Interquartile range: "),
                    "The middle 'box' represents the middle 50% of scores for
                    the group. The range of scores from lower to upper quartile
                    is referred to as the inter-quartile range.",
                    br(),
                    strong("Upper quartile: "),
                    "75% of the scores fall below the upper quartile. This is
                    the top of the box (or the right side if the boxplot is
                    displayed horizontally",
                    br(),
                    strong("Lower quartile: "),
                    "25% of scores fall below the lower quartile. This is the
                    bottom (left side) of the box.",
                    br(),
                    strong("Whiskers: "),
                    "The whiskers stretch to the greatest (top) and least
                    (bottom) values in the data, except for outliers.",
                    br(),
                    strong("Outliers: "),
                    "Outliers are defined as more than 1.5x the upper value or
                    less than 1.5x the lower value shown by the whiskers.",
                    br(),
                    "For more information, here's a ",
                    a(href = "http://flowingdata.com/2008/02/15/how-to-read-and-use-a-box-and-whisker-plot/",
                      "diagram from FlowingData"),
                    "showing the parts of a boxplot."
                    ),
                  br(),
                  h4("Interpreting them..."),
                  p(
                    "Box plots that are comparatively short show that an
                    interviewer's scores fall within a restricted range.
                    Comparatively tall box plots show a broader range of scores.
                    If one box plot is much higher or lower than all the others,
                    this may suggest either a difference between individuals
                    being assessed or some variation in the way that the
                    assessor is scoring individuals."
                  ),
                  p(
                    "Please recall that a broader range of scores by one
                    interviewer could be due to characteristics of the group
                    they assessed and is not automatically a concern with the
                    validity of scoring."
                  )
                )
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "planning",
        fluidRow(
          column(
            width = 12,
            box(
              title = "Individual Recommendations",
              status = "warning",
              collapsible = TRUE, 
              collapsed = F,
              width = NULL,
              p(
                "You can press the ", em("Select new"), 
                " button below to view the assessment details of a single 
                individual, selected at random and de-identified.  The various 
                tabs below show different ways of viewing the data with regard 
                to planning."
              ),
              uiOutput('id_drop'), # filter reactive based on selected filters
              br(),
              box(
                title = "Choose your preferences", 
                color = "black",
                collapsible = TRUE, collapsed = T, width = NULL,
                selectInput(
                  "filter_ipos",
                  label = "I want to focus on needs which are...",
                  choices = c(
                    "Important to or for this person",
                    "Important to/for, or in a higher risk area",
                    "All needs"
                  ), 
                  selected = "Important to/for, or in a higher risk area"
                ),
                selectInput(
                  "filter_ntwk_type",
                  label = "I want to see...",
                  choices = c(
                    "The simplest set of services to address my needs",
                    "All services which might be relevant"
                  ),
                  selected = "The simplest set of services to address my needs"
                ),
                selectInput(
                  "filter_community_based",
                  label = "I want my services to be provided in...",
                  choices = c(
                    "An independent community-based setting",
                    "A congregate facility-based setting is also acceptable"
                  ),
                  selected = "An independent community-based setting"
                ),
                box(
                  title = "More about these options", 
                  color = "black",
                  collapsible = TRUE, collapsed = T, width = NULL,
                  p(
                    "Below you can see what's included and excluded by each of
                    the filter options above:"
                  ),
                  p(
                    strong("Important to or for this person: "),
                    "The items displayed above were endorsed by either 
                    the person (", em("To"), 
                    "), members of their support system ", em("For"), 
                    "), or both (", em("To and For"), 
                    ") as being important in the person's life.",
                    "These items should be considered a priority during the 
                    person-centered planning process. Case managers can use 
                    the information here to:",
                    tags$ul(
                      tags$li("prompt the person to consider whether to pursue 
                              a new goal"),
                      tags$li("consider potential referrals for additional 
                              supports"),
                      tags$li("revisit items already addressed in the person's 
                              previous plan of service"),
                      tags$li("note items that will be addressed in future 
                              planning")
                      ),
                    "Items which were endorsed as important but which did not 
                    have any need indicated during the assessment (", 
                    em("i.e. where the score was zero"),
                    ") are not included in the list above."
                    ),
                  p(
                    "Items from the ", em("Behavioral Supports"), " and ", 
                    em("Medical Supports"), " sections do not have the option 
                    of being endorsed as important by the person or their 
                    support network.  These items are included in the ", 
                    em("Medical/Behavioral"), " tab."
                  ),
                  p(
                    strong("Important to/for, or in a higher risk area: "),
                    "This option will also display any items that are marked as 
                    important to or for the person, and also includes needs in 
                    any of the following areas: ",
                    em(paste(needs$item_desc[needs$need_svc == T 
                                             & needs$section != "Q1A" 
                                             & needs$section != "Q1B"],
                             sep = '',collapse = ', '))
                  ),
                  p(
                    strong("All needs:"),
                    "This option displays all needs with a score of greater 
                    than zero, regardless of whether they were marked as 
                    important to the person or if they are related to a higher 
                    risk area."
                  )
                )
              ),
              tabBox(
                width = NULL,
                tabPanel(
                  "Instructions",
                  box(
                    title = "Using the SIS in Person-Centered Planning", 
                    color = "black",
                    collapsible = TRUE, collapsed = T, width = NULL,
                    p(
                      "The needs identified by the Supports Intensity Scale® (SIS) 
                      are an important aspect of the person-centered planning 
                      process.  Results from the SIS should be used to inform the 
                      discussion about what natural supports, medically-necessary 
                      services, and community resources are needed to support 
                      an individual."
                    ),
                    p(
                      "While SIS data provides useful, standardized information 
                      about an individual's support needs, it should not be viewed 
                      in isolation nor should it be the the sole source of 
                      information used in planning. The Person-Centered planning 
                      process should take into account the individual's desires, 
                      preferences, experiences and goals in addition to their 
                      support needs."
                    ),
                    p(
                      "Case managers may find it helpful to use the areas of need
                      identified (see ", em("Area"), " column) to identify 
                      potential goals for consideration by persons served and 
                      their planning team.  Within each area, the specific 
                      needs identified (see ", em("Need")," column) can inform 
                      specific objectives related to the broader goals.  It may 
                      be helpful for case managers to review the full SIS report 
                      for additional details or comments provided by the SIS 
                      assessor."
                    ),
                    p(
                      "All needs identified in the SIS should be either addressed 
                      or deferred in the person centered plan. The reason for 
                      deferral should be stated explicitly in planning 
                      documentation."
                    )
                  ),
                  box(
                    title = "What's Included...", 
                    color = "black",
                    collapsible = TRUE, collapsed = T, width = NULL,
                    p(
                      "The output here does not represent all items from the SIS 
                      for every person.  Instead, it intends to highlight areas 
                      that may be most relevant to person-centered planning.  
                      While there are ", paste0(nrow(needs)), " needs assessed by 
                      the SIS, needs are displayed only when they meet 
                      the following criteria:",
                      tags$ul(
                        tags$li("The need has been identified as important by the 
                              person"),
                        tags$li("The need has been identified as important by the 
                              person's family, friends or other supports"),
                        tags$li("The need falls into a high-risk group which 
                              should be considered as part of treatment 
                              planning"),
                        tags$li("There is a need identified in that area, 
                              indicated by a score greater than 0")
                      )  
                    ),
                    p(
                      "While the person may have needs in other areas, this focus 
                      is intended to promote safety while also identifying 
                      personal priorities."
                    )
                  ),
                  box(
                    title = "Limitations of the SIS", 
                    color = "black",
                    collapsible = TRUE, collapsed = T, width = NULL,
                    p(
                      "Since the SIS evaluates the intensity of support needed for 
                      individuals to lead independent lives, it may require some 
                      interpretation to apply its findings to a non-independent 
                      setting.  In such instances, some level of family, CLS 
                      and/or PC supports would be available on a day-to-day basis.  
                      The structured setting provided by a family home or facility 
                      may therefore already be meeting some of the frequency and 
                      daily support time needed to support certain areas."
                    ),
                    p(
                      "The frequency and daily support time reflect the level of 
                      support needed for the individual to participate in a given 
                      activity just as another person of the same age in the 
                      community. These items do not indicate that services should 
                      be authorized with a specific scope, amount, or duration."
                    ),
                    p(
                      "The Supports Intensity Scale is not intended to be the sole 
                      source of information assisting in the development of an 
                      individualized plan of service."
                    )
                  )
                ),
                tabPanel(
                  "Support Needs",
                  dataTableOutput("ipos_need"),
                  br(),
                  radioButtons(
                    "pick_dom",
                    label = "Display needs by:",
                    choices = c("SIS Section", "QOL Domain"), 
                    selected = "SIS Section",
                    inline = T
                  ),
                  p(
                    "The numeric scores (", em("shaded by intensity of need"),
                    ") shown in the table are comprised of scores for the 
                    intensity of ",
                    em("Type"),", ", em("Frequency"), ", and ", 
                    em("Daily Support Time"), "related to the need."
                  )
                ),
                tabPanel(
                  "Medical/Behavioral",
                  p(
                    "In addition to the needs endorsed by the person or their 
                    supports, there are a number of exceptional medical and 
                    behavioral needs that the plan of service should also address (",
                    em("eating, for example"),").  These are listed below:"
                  ),
                  dataTableOutput("ipos_mb"),
                  br(),
                  p(
                    "Note that the ", em("Behavioral Supports"), " and ", 
                    em("Medical Supports"), " areas use a different scale (0-2) 
                    than the items in the other sections of the SIS.  Items with 
                    scores of 1 or 2 are highlighted for emphasis."
                  )
                ),
                tabPanel(
                  "PC/CLS",
                  p(
                    "The table below shows needs related to personal care and 
                    community living:"
                  ),
                  dataTableOutput("ipos_pccls"),
                  p(
                    "While this view identifies needs related to personal care 
                    and community living, the fact that an item is included 
                    here does not necessarily mean that there is a medical need 
                    for the provision of services related to these areas.  
                    Additional assessment may be required in order to determine 
                    whether a specific service is medically necessary for this 
                    person."
                  ),
                  p(
                    "Needs from the areas listed above which were endorsed by 
                    the person or their supports or included among additional 
                    needs are also included in the ",
                    em("PC/CLS"), " tab and are repeated here."
                  )
                ),
                tabPanel(
                  "Recommendations",
                  p(
                    "Based on the needs identified in the SIS assessment, 
                    the following services may be relevant in helping to address 
                    identified needs:"
                  ),
                  dataTableOutput("ind_ntwk_table"),
                  visNetworkOutput("ind_ntwk_graph"),
                  p(
                    "The services identified here are intended to prompt 
                    consideration and dialogue.  They are not intended to imply 
                    that any given service is either a current priority for the 
                    individual or medically necessary based on assessed needs.  
                    Services and supports should always be determined using the 
                    person-centered planning process."
                  )
                ),
                tabPanel(
                  "Potential Referrals",
                  p(
                    "Based on the needs identified in the SIS assessment, 
                    the individual may benefit from additional assessment 
                    in the area(s) indicated below to determine if any 
                    ongoing ancillary services are medically necessary or 
                    if any changes to existing care plans are needed.  Please 
                    note that only exceptional medical or behavioral needs cue 
                    the potential referrals listed below.  The list below is 
                    intended to prompt consideration and is not intended to 
                    imply that any given referral decision is clinically 
                    appropriate or inappropriate."
                  ),
                  dataTableOutput("ipos_refer")
                )
              )
            )
          ),
          column(
            width = 12,
            box(
              title = "Items for Focus", 
              status = "warning",
              collapsible = TRUE, 
              collapsed = TRUE,
              width = NULL,
              tabBox(
                width = NULL,
                tabPanel(
                  "Support Needs",
                  uiOutput('import'),
                  selectInput(
                    "need_import_q2_measure",
                    label = "Show the:",
                    choices = c("Number of people with need", 
                                "Average level of need"), 
                    selected = "Number of people with need"
                  ),
                  plotlyOutput("need_import_q2")
                ),
                tabPanel(
                  "Important To/For",
                  plotlyOutput("import_q2")
                ),
                tabPanel(
                  "Protection",
                  plotlyOutput("plans")
                ),
                tabPanel(
                  "About",
                  tabBox(
                    width = NULL,
                    tabPanel(
                      "Support Needs",
                      p(
                        "This chart shows all life domains ", 
                        em("i.e. questions"), " where a support need was 
                        identified through the SIS assessment or which the 
                        person or other SIS respondents endorsed as important to 
                        or important for the individual. The chart excludes item 
                        responses where no need was identified ", 
                        em("and"), " the item was not endorsed as important to 
                        or for the individual, since these instances are 
                        unlikely to be relevant for planning."
                      )
                    ),
                    tabPanel(
                      "Important To/For",
                      p(
                        strong("Important to the Person"), 
                        "Each item in this section can be endorsed as being of 
                        particular importance either ", em("to"), " or ", 
                        em("for"), " the person served.  Items endorsed by the 
                        individual are particularly relevant for service 
                        planning, as they are likely to be high priorities for 
                        action."
                      ),
                      p(
                        strong("Important for the Person"),
                        "Items marked as being important ", em("for"), 
                        " the person have been identified by family members or 
                        other informants as relevant for the individual.  These 
                        items may be relevant  for planning, as they indicate 
                        areas where key members of an individuals support system 
                        have noted needs."
                      )
                    ),
                    tabPanel(
                      "Protection",
                      p(
                        "Section 3 of the SIS (the Supplemental Protection and 
                        Advocacy Scale) measures 8 activities such as: ",
                        em(
                          "self-advocacy, money management, protecting self from 
                          exploitation, legal responsibilities, making choices and 
                          advocating for others."
                        ), 
                        "The score from this section is not used to determine the ", 
                        em("Support Needs Index"), " score."
                        ),
                      p(
                        "The top 4 items in this section of the SIS are intended to 
                        be included in person-centered planning.  This graph shows 
                        which items most frequently make it to the", 
                        em("top 4"), "list of clients in the selected agency. The 
                        ranking of top 4 items per person is done using a 
                        methodology that includes ties, so that items where the 
                        total score was the same across multiple questions are 
                        all included in the list."
                      ),
                      p(
                        "CMHSPs may be interested in identifying how these areas 
                        are addressed through the development of individual 
                        plans of service (IPOS)."
                      )
                    )
                  )
                )
              )
            )
          ),
          column(
            width = 12,
            box(
              title = "Related Services (In development)", 
              status = "warning",
              collapsible = TRUE, 
              collapsed = TRUE,
              width = NULL,
              tabBox(
                width = NULL,
                tabPanel(
                  "Network Map",
                  visNetworkOutput('sis_svc_ntwk')
                ),
                tabPanel(
                  "About",
                  p(
                    "While assessment data such as the information from the 
                    SIS can be helpful in painting a picture of the needs 
                    that exist, people want to meet those needs with services 
                    and practical solutions.  Questions like the following 
                    continue to arise:",  
                    em("How can we offer services which best address a person's 
                       needs?  How can we provide people with relevant service 
                       choices to supplement their natural supports and 
                       relationships in the community?"),  
                    "In order to even begin to answer these questions at a 
                    population level, it is important to understand which 
                    services may be relevant to meeting specific needs.  
                    That's where this chart comes in.  It's a complex picture 
                    to explore a complex set of questions..."
                  ),
                  strong("The network map:"),
                  p(
                    "Network maps show relationships between multiple pairs of items (", 
                    em("called 'nodes' on the map, and represented by circles"),
                    ").  A relationship between any two nodes is shown by a line 
                    drawn between those nodes.  In this map, a personal need 
                    identified by an item on the SIS (", em("blue node"),
                    ") is shown as being related to any service (",
                    em("yellow node"),") that could potentially address that need."
                  ),
                  p(
                    "The nodes are organized based on their connection to each 
                    other, which means needs that are not related to the same 
                    services will not appear near one another.  This doesn't 
                    mean that these needs (", 
                    em("e.g. physical fitness and emotional well-being"), 
                    ") aren't connected within an individual person's life, 
                    just that they are not addressed using the same services."
                  ),
                  p(
                    "On the network map here, the thickness of the line 
                    connecting a need to a service corresponds to the number 
                    of people who had this need identified on the SIS (",
                    em("i.e. received a score of greater than zero"),
                    ").  The size of a node indicates the number of other nodes 
                    it is connected to.  You can hover over a line to see the 
                    number of people that it represents and the average score 
                    on that item.  You can also click on individual nodes to 
                    highlight their relationships, and zoom in or out to inspect 
                    details."
                  ),
                  strong("A few words about service mappings:"),
                  p(
                    "The mappings connect each SIS need (",
                    em("i.e. each item from SIS sections 1-3"),
                    ") to each HCPCS code that might be used to address that 
                    need, either in full or in part.  Service codes are included 
                    if they were provided within the state according to the most 
                    recent Section 404 report.  A link does not imply 
                    eligibility for a given service, but merely indicates that 
                    the service might be relevant to help meet needs of this 
                    type when they do meet eligibility criteria.  In instances 
                    where the need was not related to a specific service but may 
                    require coordination by a supports coordinator (",
                    em("e.g. obtaining legal services"),
                    "), case management codes were mapped as potentially 
                    relevant."
                  ),
                  p(
                    "These mappings are not intended to confine a person's 
                    service options in any way, but to highlight services that 
                    are personalized based on individual needs, for 
                    consideration in person-centered planning."
                  ),
                  p(
                    "Because each need is mapped to all services that could 
                    possibly address it, the number of people with needs 
                    related to a given services is greater than the number 
                    who will use that service, since people are unlikely to 
                    use all services connected to the need, but will instead 
                    select from among these options.  It is also worth noting 
                    that the services shown here may not be available within 
                    every community."
                  )
                )
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "docs",
        fluidRow(
          column(
            width = 12,
            uiOutput("sis_docs")
          )
        )
      ),
      tabItem(
        tabName = "data_quality",
        fluidRow(
          column(
            width = 12,
            box(
              title = "Data Quality Issues",
              status = "warning",
              collapsible = TRUE,
              collapsed = FALSE,
              width = NULL,
              tabBox(
                width = NULL,
                tabPanel(
                  "Missing or Incorrect Entries",
                  radioButtons(
                    "current",
                    label = "Display:",
                    choices = c("Current assessors", "All Assessors"),
                    selected = "Current assessors",
                    inline = T
                  ),
                  dataTableOutput("dt_datqual")
                ),
                tabPanel(
                  "About",
                  p(
                    strong("Unmatched Mcaid IDs"), "counts the number of
                    instances in which the Medicaid ID from the SIS data does
                    not match with the attribution file."
                  ),
                  p(
                    strong("Missing Start Time"), "and", strong("Missing End Time"),
                    "count the number of times that no start/end time was entered
                    for the assessment, thereby making it impossible to
                    calculate the duration of the assessment."
                  ),
                  p(
                    strong("Missing Reason"),
                    "counts the number of instances in which no reason was given
                    for the completion of the SIS assessment.  Available reasons
                    include:",
                    em(
                      "Change in situation, First SIS, or Regularly scheduled 
                      assessment"
                    )
                  ),
                  p(
                    strong("No Intrvw Setting"),
                    "counts the number of instances in which the interview
                    setting was not specified for the SIS assessment."
                  ),
                  p(
                    strong("Missing Supports Info"),
                    "counts the number of instances in which information was
                    missing from the initial entry field for ",
                    em("Supports Relation Type."), " While multiple supports
                    can be recorded on the SIS assessment, there is no way of
                    knowing the actual number of supports that the individual
                    was receiving using the SIS data alone.  This count assumes
                    that individuals had at least one support at the time of
                    the interview."
                  ),
                  p(
                    strong("Missing Respondents"),
                    "counts the number of instances in which information was
                    missing from the initial entry field for ",
                    em("Respondent Relation Type."), " While multiple respondents
                    can be recorded on the SIS assessment, there is no way of
                    knowing the actual number of respondents that were present
                    using the SIS data alone.  This count assumes that
                    individuals had at least one respondent present for the
                    interview."
                  ),
                  p(
                    strong("State other than MI"),
                    "counts the number of times that a state other than Michigan
                    was identified as the living address of the person being
                    assessed.  This is one of a number of issues with data entry
                    that may make it difficult to correctly map proximity to
                    nearby resources."
                  ),
                  p(
                    strong("No Important To"),
                    "counts the number of assessments where no items were marked
                    as ", em("important to"), " the person being assessed."
                  ),
                  p(
                    strong("No Important For"),
                    "counts the number of assessments where no items were marked
                    as ", em("important for"), " the person being assessed."
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)
