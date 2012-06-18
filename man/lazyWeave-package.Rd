\name{lazyWeave-package}
\alias{lazyWeave-package}
\alias{lazyWeave}
\docType{package}
\title{Generate Latex code from R}
\description{Arguments passed to functions are embedded in Latex code and can
  be output to a file.  Allows the use of Latex to write reports, functions
  for building tables, etc.  
}
\details{
Depending on the working directory (for Windows users), the user may encounter the error 
"texi2dvi.exe: Windows API error 5: Access is denied." when trying
to build documents.  This happens when the working directory is write protected.  It is advisable to change the working directory to 
something not write protected.  This can be done permanently by right clicking on the R shortcut icon, selecting properties, and changing
the directory in the "Start in:" box.

\code{lazyWeave} assumes the availability of the packages \code{xcolor, graphicx, colortbl, soul, lscape,} and \code{Sweave}.  
If these packages are not available, the package most likely will not function properly. 

It should be noted that \code{lazyWeave} is a rather inefficient way to go 
about writing reports with LaTeX.  It's only real advantage is it reduces the
amount of knowledge a user needs to have about LaTeX (and it could be 
debated if that's really an advantage). 

Use of \code{lazyWeave} could also be greatly supplemented by some basic
familiarity with LaTeX.  For example, knowing the commands for bolding 
(\\textbf\{\}), italicizing (\\empf\{\}), and underlining text 
(\\ul\{\}) can go a long way to improving the look of reports.  It also would
help to know how to subscript and superscript terms.  Most introductions
to LaTeX will cover these basics. 

\code{lazyWeave} is also only intended to provide the most basic functionality
of LaTeX, and I have no plans of extending it much further than what is 
available already.  If what is in the package now is not sufficient enough 
to satisfy your needs, then I strongly suggest you look into using 
\code{Sweave}.
}

\author{Benjamin Nutter <nutterb@ccf.org>}

\examples{
require(survival)
data(Scout)

#*** Did the scout earn first class in the first year
Scout$first.within.year <- ifelse(Scout$dt.first < 365.25, TRUE, FALSE)

#*** Define the scout's highest rank earned
Scout$highest.rank <- 
  ifelse(Scout$eagle \%in\% 1, "Eagle",
    ifelse(Scout$life \%in\% 1, "Life", 
      ifelse(Scout$star \%in\% 1, "Star", 
        ifelse(Scout$first \%in\% 1, "First Class",
          ifelse(Scout$second \%in\% 1, "Second Class",
            ifelse(Scout$tender \%in\% 1, "Tenderfoot",
              ifelse(Scout$scout \%in\% 1, "Scout", "No Rank")))))))
Scout$highest.rank <- factor(Scout$highest.rank,
  levels=c("No Rank", "Scout", "Tenderfoot", "Second Class", "First Class",
           "Star", "Life", "Eagle"))
  

#*** Subset to scouts age 18 and older (cannot advance any more)
ScoutComplete <- subset(Scout, age >= 18)


#*** Table of ranks
rank.table <- cbind(table(ScoutComplete$highest.rank),
                    round(prop.table(table(ScoutComplete$highest.rank))* 100, 2))
colnames(rank.table) <- c("N", "Percentage")


#*** Time to completion of Eagle Rank
km.eagle <- survfit(Surv(as.numeric(dt.first)/(365.25/12), eagle) ~ 1, 
    data=ScoutComplete)
    
pdf("TimeToEagle.pdf", height=6, width=6)
  plot(km.eagle, conf=FALSE, xaxt="n", xlab="Months Since Joining Scouts",
      ylab="Proportion Not Having Earned Eagle")
  axis(1, seq(0, 36, by=6))
dev.off()


#*** Cross tabulation of Eagle Scout and First Class within 1 year
first.eagle <- table(factor(ScoutComplete$eagle, 0:1, c("No", "Yes")), 
    ScoutComplete$first.within.year)
rownames(first.eagle) <- c("Did not earn Eagle", "Earned Eagle")
colnames(first.eagle) <- 
    c("More than 1 year to First Class", "Less than 1 year to First Class")
fisher.test(first.eagle)

#*** Time to completion of Eagle rank by First Class within 1 year
km.eagle.by.first <- survfit(Surv(as.numeric(dt.first)/(365.25/12), eagle) ~ 
    first.within.year, data=ScoutComplete)
    
pdf("TimeToEagleByFirst.pdf", height=6, width=6)
  plot(km.eagle.by.first, conf=FALSE, xaxt="n", xlab="Months Since Joining Scouts",
      ylab="Proportion Not Having Earned Eagle")
  axis(1, seq(0, 36, by=6))
dev.off()


#*** Write a brief report describing the data
lazy.write(
  lazy.file.start(docClass="report", 
    title="First Year Scout Advancement and Eagle Scout",
    author="Benjamin Nutter",
    page="roman"),

  lazy.toc(),
  lazy.toc("figures"),
  lazy.toc("tables"),
  lazy.page.break(),
  lazy.page.number("arabic"),
  lazy.counter("chapter", 1, fn="set"),

  #*** Introduction to scouting ranks
  lazy.section("Basics of Scout Advancement", 
      ordered=TRUE, label="advanceBasics"),
  lazy.text("Boys typically join Boy Scouting after turning 11 ", 
    "years old and completing fifth grade. The goal of Scouting ", 
    "is to provide an atmosphere in which boys can develop self ", 
    "confidence, leadership skills, and civic awareness.  One of ", 
    "the methods employed to achieve this goal is a system of ", 
    "ranks; recognitions for developing certain skill sets. ", 
    "The full list of ranks is:"),
  lazy.list(c("Scout", "Tenderfoot", "Second Class", "First Class", 
      "Star", "Life", "Eagle"),
    ordered=FALSE),
  lazy.text("It is commonly understood that the first year of ", 
    "scouting is the most important establishing a boy's ", 
    "participation.  Specifically, boys who obtain the rank of ", 
    "First Class within the first year of joining are believed ", 
    "to be more likely to complete the program having obtained ", 
    "the highest honor, Eagle Scout"),
    

  #*** Description of the data
  lazy.section("Selection of Data", ordered=TRUE, 
      label="dataSelection"),
  lazy.text("The data set \\tt{Scout} \\rm contains 105 records. ",
      "However, many of these records are of scouts who were ", 
      "active participants in the scouting program at the time ",
      "the data were captured.  Including all the records would ",
      "lead to a bias against obtaining Eagle Scout, since ", 
      "many of the boys could still be working toward that ", 
      "award.  Instead, the data need to be limited to the subset ",
      "of participants who cannot progress in the program any ",
      "further than they already have; that is, who are of age ",
      "18 or higher."),   

  #*** This is a little more challenging to read.  Take note of
  #*** how text and values are integrated within the lazy.text()
  #*** call.  
  lazy.text("Selecting this subset leaves a total of ",
    nrow(ScoutComplete), 
    " subjects to compare.  ",
    rank.table["Eagle", "N"], " (", rank.table["Eagle", "Percentage"], "\%)",  
    " of these subjects earned the Eagle rank (See Table ",
    lazy.ref("HighestRank"),
    " for complete distribution of highest ranks).  Table ",
    lazy.ref("EagleByFirst"), 
    " shows the cross-tabulation of these subjects by whether ",
      "or not they earned first class within one year or not. ", 
      "Fishers exact test indicates a statistically significant ",
      "difference between the percentage of scouts who earn ",
      "Eagle by completion of First Class within the first ",
      "year (p = ",
      round(fisher.test(first.eagle)$p.value, 4),
    "), lending strong support to the hypothesis that scouts ",
      "who earn First Class within the first year are more ", 
      "likely to go on to earn Eagle."),

  #*** Tables
  lazy.matrix(rank.table, caption="Highest Rank Earned by Scouts", 
      label="HighestRank", placement="h"),
  
  lazy.matrix(first.eagle, placement="h", 
      caption=paste("Completion of Eagle Scout by Completion of First Class",
                    "within 1 year"), label="EagleByFirst"),
               
  #*** Time to event analysis
  lazy.text("For a visual representation of the difference between ",
      "scouts who earn First Class within one year and those who do not, ",
      "we can use a Kaplan-Meier plot.  For this purpose we will ", 
      "define the event as completion of the Eagle rank and the time ",
      "to event as the time between registration and completion of ", 
      "the rank.  The overall curve is shown in Figure ",
    lazy.ref("KMAll"), 
    ".  The curves for each group are shown in Figure ",
    lazy.ref("KMGroup"), "."),
    
  #*** Including figures in the document
  lazy.figure("TimeToEagle.pdf", height=3, width=3, label="KMAll",
    caption="Proportion of Scouts Who Earn Eagle"),
  lazy.figure("TimeToEagleByFirst.pdf", height=3, width=3, label="KMGroup",
    caption=paste("Proportion of Scouts Who Earn Eagle By Completion of First",
                  "Class within 1 Year")),

 
  lazy.file.end(),
  OutFile="Scout.tex")
  
lazy.build("Scout.tex")

unlink("TimeToEagle.pdf")
unlink("TimeToEagleByFirst.pdf")
unlink("Scout.tex")
unlink("Scout.pdf")
}

\keyword{ package }

