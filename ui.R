library(googleVis)

shinyUI(pageWithSidebar(
        headerPanel("Visualizing State Growth on NAEP, 2003-2013"),
        sidebarPanel(
                h4("Choose Your Parameters"),
                selectInput("gender", "Gender:", choices =
                                    c("All", "Male", "Female")),

                selectInput("grade", "Grade Level:", choices = 
                                    c(4, 8))
                # possible tools:
                # region selector
                # gender selector (need add'l data)
                # grade level selector (need add'l data)
                # race selector (need add'l data)
                # economic status selector (need add'l data)
        ),
        mainPanel(
                tabsetPanel(
                        tabPanel("About",
                                 h3("About the NAEP Visualizer"),
                                 p(HTML("The <a href=\"http://nces.ed.gov/nationsreportcard/\">National Assessment of Educational
                                        Progress</a> (NAEP) is a standardized test
                                        administered by the National Center for
                                        Education Statistics at the U.S.
                                        Department of Education. Often termed
                                        the Nation's Report Card, it provides
                                        a wealth of information about how
                                        American public school students as a 
                                        whole perform in a variety of
                                        subjects.")),
                                 p(HTML("This visualizer uses State NAEP data
                                        for each of the 50 states, the District
                                        of Columbia, and the Department of
                                        Defense Education Activity (DoDEA) from
                                        2003 to 2013 in math and reading, grades
                                        4 and 8.")),
                                 p(HTML("<b>Choose a grade level and gender,
                                        switch to the 'Use the NAEP Visualizer'
                                        tab, and press play. The resulting
                                        motion plot shows how states'
                                        performance has evolved over
                                        time.</b>")),
                                 p(HTML("One nice feature of the motion chart
                                        is the ability to track an individual
                                        state over time. To focus on one state's
                                        performance, click on that state's
                                        bubble before pressing play and watch
                                        the plot draw trails highlighting that
                                        state's performance.")),
                                 p(HTML("Please note that while State NAEP in
                                        these subjects and grades is only
                                        administered every two years, Google
                                        Charts <a href=\"https://code.google.com/p/google-visualization-api-issues/issues/detail?id=102\">interpolates data for the
                                        even-numbered years</a> when NAEP is not
                                        administered if the user tracks
                                        individual states. Data for
                                        even-numbered years in trails should be
                                        disregarded.")),
                                 p(HTML("Also note that at this time, it is
                                        not possible to set min and max values
                                        on motion charts explicitly, so the
                                        scale of the chart may change depending
                                        on your choice of parameters."))),
                        tabPanel("Use the NAEP Visualizer", htmlOutput("view"))
                )
        )
))