library(googleVis)

shinyUI(pageWithSidebar(
        headerPanel("State Growth on the National Assessment of Educational
                    Progress, 2005-2013"),
        sidebarPanel(
                selectInput("gender", "Gender:", choices =
                                    c("All", "Male", "Female")),

                selectInput("grade", "Grade Level:", choices = 
                                    c(4, 8, 12))
                # possible tools:
                # region selector
                # gender selector (need add'l data)
                # grade level selector (need add'l data)
                # race selector (need add'l data)
                # economic status selector (need add'l data)
        ),
        mainPanel(
                htmlOutput("view")
        )
))