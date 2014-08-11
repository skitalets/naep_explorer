library(data.table)
library(reshape2)
library(xlsx)

# Before processing the data using this script, I downloaded the data according
# to the below queries on the NCES NAEP Data Explorer here:

# http://nces.ed.gov/nationsreportcard/naepdata/dataset.aspx

# I then manually copied the years from the first row in a given year in each
# resulting Excel file to all the relevant rows below it (which come from the
# NCES tool blank) through 2003, affecting cells B12 through B323 inclusive.
# This was much easier than messing with this one clean-up item
# programmatically. Otherwise, the rest of the clean-up is found in this file.

# This function is insanely ugly and should be refactored. Too lazy.

processFiles <- function(math, reading, math.gender, reading.gender, grade) {

        setnames(reading, c("year", "state", "scale.score.reading",
                            "SE.reading"))
        setnames(math, c("year", "state", "scale.score.math", "SE.math"))
        setnames(math.gender, c("year", "state", "scale.score.math.male",
                                "SE.math.male", "scale.score.math.female",
                                "SE.math.female"))
        setnames(reading.gender, c("year", "state", "scale.score.reading.male",
                                   "SE.reading.male",
                                   "scale.score.reading.female",
                                   "SE.reading.female"))

        reading$gender <- "all"

        reading.gender <- melt(reading.gender,
                               id = c("year", "state", "SE.reading.female",
                                      "SE.reading.male"))
        setnames(reading.gender, "variable", "gender")
        setnames(reading.gender, "value", "scale.score.reading")
        reading.gender[gender == "scale.score.reading.male",
                       gender := "male"]
        reading.gender[gender == "scale.score.reading.female",
                       gender := "female"]
        reading.gender <- melt(reading.gender,
                               id = c("year", "state", "gender",
                                        "scale.score.reading"))
        setnames(reading.gender, "value", "SE.reading")
        reading.gender[variable == "SE.reading.female",
                       variable := "female"]
        reading.gender[variable == "SE.reading.male",
                       variable := "male"]
        reading.gender$variable <- factor(reading.gender$variable)
        reading.gender$gender <- factor(reading.gender$gender)
        reading.gender <- reading.gender[gender == variable, !"variable",
                                         with = FALSE]
        setcolorder(reading, c("year", "state", "scale.score.reading",
                               "SE.reading", "gender"))
        setcolorder(reading.gender, c("year", "state", "scale.score.reading",
                                   "SE.reading", "gender"))

        reading <- rbindlist(list(reading, reading.gender))
        
        math$gender <- "all"
        
        math.gender <- melt(math.gender,
                            id = c("year", "state", "SE.math.female", "SE.math.male"))
        setnames(math.gender, "variable", "gender")
        setnames(math.gender, "value", "scale.score.math")
        math.gender[gender == "scale.score.math.male",
                       gender := "male"]
        math.gender[gender == "scale.score.math.female",
                       gender := "female"]
        math.gender <- melt(math.gender,
                               id = c("year", "state", "gender",
                                      "scale.score.math"))
        setnames(math.gender, "value", "SE.math")
        math.gender[variable == "SE.math.female",
                       variable := "female"]
        math.gender[variable == "SE.math.male",
                       variable := "male"]
        math.gender$variable <- factor(math.gender$variable)
        math.gender$gender <- factor(math.gender$gender)
        math.gender <- math.gender[gender == variable, !"variable",
                                         with = FALSE]
        setcolorder(math, c("year", "state", "scale.score.math", "SE.math",
                            "gender"))
        setcolorder(math.gender, c("year", "state", "scale.score.math",
                                     "SE.math", "gender"))
        
        math <- rbindlist(list(math, math.gender))
        
        math$gender <- factor(math$gender)
        reading$gender <- factor(reading$gender)
        
        all <- merge(reading, math, by = c("gender", "year", "state"))
        all$grade <- grade
        return(all)
}

## end of function definition

# Skip the first 10 rows and read.xlsx() magically finds the data
math.grade4 <- data.table(read.xlsx("data/Math-Grade4.xls", 1, startRow = 11,
                                    endRow = 323))
reading.grade4 <- data.table(read.xlsx("data/Reading-Grade4.xls", 1,
                                       startRow = 11, endRow = 323))
math.grade4.gender <- data.table(read.xlsx("data/Math-Grade4-Gender.xls", 1,
                                           startRow = 11, endRow = 323))
reading.grade4.gender <- data.table(read.xlsx("data/Reading-Grade4-Gender.xls",
                                              1, startRow = 11, endRow = 323))

math.grade8 <- data.table(read.xlsx("data/Math-Grade8.xls", 1, startRow = 11,
                                    endRow = 323))
reading.grade8 <- data.table(read.xlsx("data/Reading-Grade8.xls", 1,
                                       startRow = 11, endRow = 323))
math.grade8.gender <- data.table(read.xlsx("data/Math-Grade8-Gender.xls", 1,
                                           startRow = 11, endRow = 323))
reading.grade8.gender <- data.table(read.xlsx("data/Reading-Grade8-Gender.xls",
                                              1, startRow = 11, endRow = 323))

grade4 <- processFiles(math.grade4, reading.grade4, math.grade4.gender,
                       reading.grade4.gender, 4)
grade8 <- processFiles(math.grade8, reading.grade8, math.grade8.gender,
                       reading.grade8.gender, 8)

naep.clean <- rbindlist(list(grade4, grade8))

# Use Census-defined Regions to set region data -- doing this for each row
# is not ideal but I couldn't think of a good way to pull in a separate
# table at runtime when the plots are being generated
# http://nces.ed.gov/nationsreportcard/hsts/tabulations/regions.aspx

northeast <- c("Connecticut", "Maine", "Massachusetts", "New Hampshire",
               "New Jersey", "New York", "Pennsylvania", "Rhode Island",
               "Vermont")
south <- c("Alabama", "Arkansas", "Delaware", "District of Columbia", "Florida",
           "Georgia", "Kentucky", "Louisiana", "Maryland", "Mississippi",
           "North Carolina", "Oklahoma", "South Carolina", "Tennessee", "Texas",
           "Virginia", "West Virginia")
midwest <- c("Illinois", "Indiana", "Iowa", "Kansas", "Michigan", "Minnesota",
             "Missouri", "Nebraska", "North Dakota", "Ohio", "South Dakota",
             "Wisconsin")
west <- c("Alaska", "Arizona", "California", "Colorado", "Hawaii", "Idaho",
          "Montana", "Nevada", "New Mexico", "Oregon", "Utah", "Washington",
          "Wyoming")

naep.clean$region <- "Other" # default should only affect DoDEA after below runs
naep.clean[state %in% northeast, region := "Northeast"]
naep.clean[state %in% south, region := "South"]
naep.clean[state %in% midwest, region := "Midwest"]
naep.clean[state %in% west, region := "West"]

write.csv(naep.clean, file = "naep_clean.csv", row.names = FALSE)

# Reading-Grade4.xls
# Subject, Grade:Reading, Grade 4    
# Jurisdictions: Alabama, Alaska, Arizona, Arkansas, California, Colorado, Connecticut, Delaware, District of Columbia, Florida, Georgia, Hawaii, Idaho, Illinois, Indiana, Iowa, Kansas, Kentucky, Louisiana, Maine, Maryland, Massachusetts, Michigan, Minnesota, Mississippi, Missouri, Montana, Nebraska, Nevada, New Hampshire, New Jersey, New Mexico, New York, North Carolina, North Dakota, Ohio, Oklahoma, Oregon, Pennsylvania, Rhode Island, South Carolina, South Dakota, Tennessee, Texas, Utah, Vermont, Virginia, Washington, West Virginia, Wisconsin, Wyoming, DoDEA
# Measure: Composite scale
# Variable: All students
# Years: 2013, 2011, 2009, 2007, 2005, 2003

# Math-Grade4.xls
# Subject, Grade:Mathematics, Grade 4    
# Jurisdictions: Alabama, Alaska, Arizona, Arkansas, California, Colorado, Connecticut, Delaware, District of Columbia, Florida, Georgia, Hawaii, Idaho, Illinois, Indiana, Iowa, Kansas, Kentucky, Louisiana, Maine, Maryland, Massachusetts, Michigan, Minnesota, Mississippi, Missouri, Montana, Nebraska, Nevada, New Hampshire, New Jersey, New Mexico, New York, North Carolina, North Dakota, Ohio, Oklahoma, Oregon, Pennsylvania, Rhode Island, South Carolina, South Dakota, Tennessee, Texas, Utah, Vermont, Virginia, Washington, West Virginia, Wisconsin, Wyoming, DoDEA
# Measure: Composite scale
# Variable: All students
# Years: 2013, 2011, 2009, 2007, 2005, 2003

# Reading-Grade4-Gender.xls
# Subject, Grade:Reading, Grade 4    
# Jurisdictions: Alabama, Alaska, Arizona, Arkansas, California, Colorado, Connecticut, Delaware, District of Columbia, Florida, Georgia, Hawaii, Idaho, Illinois, Indiana, Iowa, Kansas, Kentucky, Louisiana, Maine, Maryland, Massachusetts, Michigan, Minnesota, Mississippi, Missouri, Montana, Nebraska, Nevada, New Hampshire, New Jersey, New Mexico, New York, North Carolina, North Dakota, Ohio, Oklahoma, Oregon, Pennsylvania, Rhode Island, South Carolina, South Dakota, Tennessee, Texas, Utah, Vermont, Virginia, Washington, West Virginia, Wisconsin, Wyoming, DoDEA
# Measure: Composite scale
# Variable: Gender
# Years: 2013, 2011, 2009, 2007, 2005, 2003

# Math-Grade4-Gender.xls
# Subject, Grade:Mathematics, Grade 4    
# Jurisdictions: Alabama, Alaska, Arizona, Arkansas, California, Colorado, Connecticut, Delaware, District of Columbia, Florida, Georgia, Hawaii, Idaho, Illinois, Indiana, Iowa, Kansas, Kentucky, Louisiana, Maine, Maryland, Massachusetts, Michigan, Minnesota, Mississippi, Missouri, Montana, Nebraska, Nevada, New Hampshire, New Jersey, New Mexico, New York, North Carolina, North Dakota, Ohio, Oklahoma, Oregon, Pennsylvania, Rhode Island, South Carolina, South Dakota, Tennessee, Texas, Utah, Vermont, Virginia, Washington, West Virginia, Wisconsin, Wyoming, DoDEA
# Measure: Composite scale
# Variable: Gender
# Years: 2013, 2011, 2009, 2007, 2005, 2003

# Reading-Grade8.xls
# Subject, Grade:Reading, Grade 8    
# Jurisdictions: Alabama, Alaska, Arizona, Arkansas, California, Colorado, Connecticut, Delaware, District of Columbia, Florida, Georgia, Hawaii, Idaho, Illinois, Indiana, Iowa, Kansas, Kentucky, Louisiana, Maine, Maryland, Massachusetts, Michigan, Minnesota, Mississippi, Missouri, Montana, Nebraska, Nevada, New Hampshire, New Jersey, New Mexico, New York, North Carolina, North Dakota, Ohio, Oklahoma, Oregon, Pennsylvania, Rhode Island, South Carolina, South Dakota, Tennessee, Texas, Utah, Vermont, Virginia, Washington, West Virginia, Wisconsin, Wyoming, DoDEA
# Measure: Composite scale
# Variable: All students
# Years: 2013, 2011, 2009, 2007, 2005, 2003, 2002, 1998, 1998¹, 1994¹, 1992¹

# Math-Grade8.xls
# Subject, Grade:Mathematics, Grade 8    
# Jurisdictions: Alabama, Alaska, Arizona, Arkansas, California, Colorado, Connecticut, Delaware, District of Columbia, Florida, Georgia, Hawaii, Idaho, Illinois, Indiana, Iowa, Kansas, Kentucky, Louisiana, Maine, Maryland, Massachusetts, Michigan, Minnesota, Mississippi, Missouri, Montana, Nebraska, Nevada, New Hampshire, New Jersey, New Mexico, New York, North Carolina, North Dakota, Ohio, Oklahoma, Oregon, Pennsylvania, Rhode Island, South Carolina, South Dakota, Tennessee, Texas, Utah, Vermont, Virginia, Washington, West Virginia, Wisconsin, Wyoming, DoDEA
# Measure: Composite scale
# Variable: All students
# Years: 2013, 2011, 2009, 2007, 2005, 2003, 2002, 1998, 1998¹, 1994¹, 1992¹

# Reading-Grade8-Gender.xls
# Subject, Grade:Reading, Grade 8    
# Jurisdictions: Alabama, Alaska, Arizona, Arkansas, California, Colorado, Connecticut, Delaware, District of Columbia, Florida, Georgia, Hawaii, Idaho, Illinois, Indiana, Iowa, Kansas, Kentucky, Louisiana, Maine, Maryland, Massachusetts, Michigan, Minnesota, Mississippi, Missouri, Montana, Nebraska, Nevada, New Hampshire, New Jersey, New Mexico, New York, North Carolina, North Dakota, Ohio, Oklahoma, Oregon, Pennsylvania, Rhode Island, South Carolina, South Dakota, Tennessee, Texas, Utah, Vermont, Virginia, Washington, West Virginia, Wisconsin, Wyoming, DoDEA
# Measure: Composite scale
# Variables: All students, Gender
# Years: 2013, 2011, 2009, 2007, 2005, 2003

# Math-Grade8-Gender.xls
# Subject, Grade:Mathematics, Grade 8    
# Jurisdictions: Alabama, Alaska, Arizona, Arkansas, California, Colorado, Connecticut, Delaware, District of Columbia, Florida, Georgia, Hawaii, Idaho, Illinois, Indiana, Iowa, Kansas, Kentucky, Louisiana, Maine, Maryland, Massachusetts, Michigan, Minnesota, Mississippi, Missouri, Montana, Nebraska, Nevada, New Hampshire, New Jersey, New Mexico, New York, North Carolina, North Dakota, Ohio, Oklahoma, Oregon, Pennsylvania, Rhode Island, South Carolina, South Dakota, Tennessee, Texas, Utah, Vermont, Virginia, Washington, West Virginia, Wisconsin, Wyoming, DoDEA
# Measure: Composite scale
# Variables: All students, Gender
# Years: 2013, 2011, 2009, 2007, 2005, 2003
