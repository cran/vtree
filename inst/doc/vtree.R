## ---- eval=FALSE, echo=FALSE---------------------------------------------
#  #
#  # For future development.
#  #
#  For a vignette of examples, see [vtree_examples](vtree_examples.html).

## ---- echo=FALSE---------------------------------------------------------
library(vtree)

## ------------------------------------------------------------------------
PNGdir <- tempdir()

## ---- echo=FALSE---------------------------------------------------------
o_example1 <- vtree(FakeData,"Severity Sex",horiz=FALSE)
grVizToPNG(o_example1,folder=PNGdir,width=1000)
filepath <- file.path(PNGdir,"o_example1.png")

## ---- eval=FALSE---------------------------------------------------------
#  vtree(FakeData,"Severity",horiz=FALSE)

## ---- echo=FALSE---------------------------------------------------------
example1 <- vtree(FakeData,"Severity",horiz=FALSE)
grVizToPNG(example1,folder=PNGdir,width=1000)
filepath <- file.path(PNGdir,"example1.png")

## ---- eval=FALSE---------------------------------------------------------
#  vtree(FakeData,"Severity Sex",horiz=FALSE)

## ---- echo=FALSE---------------------------------------------------------
example1two <- vtree(FakeData,"Severity Sex",horiz=FALSE)
grVizToPNG(example1two,folder=PNGdir,width=1000)
filepath <- file.path(PNGdir,"example1two.png")

## ---- echo=FALSE---------------------------------------------------------
example1again <- vtree(FakeData,"Severity",horiz=FALSE)
grVizToPNG(example1again,folder=PNGdir,width=1000)
filepath <- file.path(PNGdir,"example1again.png")

## ---- eval=FALSE---------------------------------------------------------
#  vtree(FakeData,"Severity",vp=FALSE,horiz=FALSE)

## ---- echo=FALSE---------------------------------------------------------
example1alt <- vtree(FakeData,"Severity",vp=FALSE,horiz=FALSE)
grVizToPNG(example1alt,folder=PNGdir,width=1000)
filepath <- file.path(PNGdir,"example1alt.png")

## ---- eval=FALSE---------------------------------------------------------
#  vtree(FakeData,"Severity Sex",prune=list("Severity"=c("Mild","NA")))

## ---- echo=FALSE---------------------------------------------------------
example3 <- vtree(FakeData,"Severity Sex",prune=list("Severity"=c("Mild","NA")))
grVizToPNG(example3,folder=PNGdir,width=1000)
filepath <- file.path(PNGdir,"example3.png")

## ---- eval=FALSE---------------------------------------------------------
#  vtree(FakeData,"Severity Sex",prunebelow=list("Severity"=c("Mild","NA")))

## ---- echo=FALSE---------------------------------------------------------
example3below <- vtree(FakeData,"Severity Sex",prunebelow=list("Severity"=c("Mild","NA")))
grVizToPNG(example3below,folder=PNGdir,width=1000)
filepath <- file.path(PNGdir,"example3below.png")

## ---- eval=FALSE---------------------------------------------------------
#  vtree(FakeData,"Severity Sex",labelnode=list(Sex=(c("Male"="M","Female"="F"))),horiz=FALSE)

## ---- echo=FALSE---------------------------------------------------------
example4 <- vtree(FakeData,"Severity Sex",labelnode=list(Sex=(c("Male"="M","Female"="F"))),horiz=FALSE)
grVizToPNG(example4,folder=PNGdir,width=1000)
filepath <- file.path(PNGdir,"example4.png")

## ---- eval=FALSE---------------------------------------------------------
#  vtree(FakeData,"Severity Group",labelvar=c("Severity"="Severity on day 1"),horiz=FALSE)

## ---- echo=FALSE---------------------------------------------------------
varlabels1 <- vtree(FakeData,"Severity Group",labelvar=c("Severity"="Severity on day 1"),horiz=FALSE)

## ---- echo=FALSE---------------------------------------------------------
grVizToPNG(varlabels1,folder=PNGdir,width=1000)
filepath <- file.path(PNGdir,"varlabels1.png")

## ---- eval=FALSE---------------------------------------------------------
#  vtree(FakeData,"Severity",HTMLtext=TRUE,horiz=FALSE,
#    labelnode=list("Severity"=c(
#      "<B>Mild</B><BR ALIGN='LEFT'/>"="Mild",
#      "<I>Moderate</I><BR/>"="Moderate",
#      "Severe<FONT POINT-SIZE='10'><SUP>Superscript</SUP></FONT><BR/>"="Severe",
#      "NA<FONT POINT-SIZE='10'><SUB>Superscript</SUB></FONT><BR/>"="NA")),
#    title="<FONT FACE='Times-Roman' COLOR='red' POINT-SIZE='20'>Whole group</FONT><BR/>")

## ---- echo=FALSE---------------------------------------------------------
fancy <- vtree(FakeData,"Severity",HTMLtext=TRUE,horiz=FALSE,
  labelnode=list("Severity"=c(
    "<B>Mild</B><BR ALIGN='LEFT'/>"="Mild",
    "<I>Moderate</I><BR/>"="Moderate",
    "Severe<FONT POINT-SIZE='10'><SUP>Superscript</SUP></FONT><BR/>"="Severe",
    "NA<FONT POINT-SIZE='10'><SUB>Superscript</SUB></FONT><BR/>"="NA")),
  title="<FONT FACE='Times-Roman' COLOR='red' POINT-SIZE='20'>Whole group</FONT><BR/>")
grVizToPNG(fancy,folder=PNGdir,width=1000)
filepath <- file.path(PNGdir,"fancy.png")

## ---- eval=FALSE---------------------------------------------------------
#  vtree(FakeData,"Severity",horiz=FALSE,
#    labelnode=list("Severity"=c(
#      "**Mild**"="Mild",
#      "*Moderate*"="Moderate",
#      "Severe^Superscript^"="Severe",
#      "NA~subscript~"="NA")),
#    title="%%red Whole group%%")

## ---- echo=FALSE---------------------------------------------------------
fancy <- vtree(FakeData,"Severity",horiz=FALSE,
  labelnode=list("Severity"=c(
    "**Mild**"="Mild",
    "*Moderate*"="Moderate",
    "Severe^Superscript^"="Severe",
    "NA~subscript~"="NA")),
  title="%%red Whole group%%")
grVizToPNG(fancy,folder=PNGdir,width=1000)
filepath <- file.path(PNGdir,"fancy.png")

## ---- eval=FALSE---------------------------------------------------------
#  vtree(FakeData,"Severity Age",check.is.na=TRUE)

## ---- echo=FALSE---------------------------------------------------------
mv1 <- vtree(FakeData,"Severity Age",check.is.na=TRUE)

## ---- echo=FALSE---------------------------------------------------------
grVizToPNG(mv1,folder=PNGdir,width=1000)
filepath <- file.path(PNGdir,"mv1.png")

## ----eval=FALSE----------------------------------------------------------
#  vtree(FakeData,"Severity",horiz=FALSE,
#    text=list("Severity"=c("Mild"="Includes first-time visits")))

## ---- echo=FALSE---------------------------------------------------------
t_example1 <- vtree(FakeData,"Severity",text=list("Severity"=c("Mild"="\nIncludes first-time visits")),horiz=FALSE)

## ---- echo=FALSE---------------------------------------------------------
grVizToPNG(t_example1,folder=PNGdir,width=1000)
filepath <- file.path(PNGdir,"t_example1.png")

## ----eval=FALSE----------------------------------------------------------
#  vtree(FakeData,"Severity",summary="Score %mean%",horiz=FALSE)

## ---- echo=FALSE---------------------------------------------------------
ss_example1 <- vtree(FakeData,"Severity",summary="Score %mean%",horiz=FALSE)

## ---- echo=FALSE---------------------------------------------------------
grVizToPNG(ss_example1,folder=PNGdir,width=1000)
filepath <- file.path(PNGdir,"ss_example1.png")

## ----eval=FALSE----------------------------------------------------------
#  vtree(FakeData,"Severity",horiz=FALSE,showlevels=FALSE,
#    summary=c(
#      "Score \nScore\n mean(SD) %mean%(%SD%)",
#      "Pre \n\nPre\n range %min%, %max%"))

## ---- echo=FALSE---------------------------------------------------------
ss_example2 <- vtree(FakeData,"Severity",horiz=FALSE,showlevels=FALSE,
  summary=c(
    "Score \nScore\n mean(SD) %mean%(%SD%)",
    "Pre \n\nPre\n range %min%, %max%"))

## ---- echo=FALSE---------------------------------------------------------
grVizToPNG(ss_example2,folder=PNGdir,width=1000)
filepath <- file.path(PNGdir,"ss_example2.png")

## ---- eval=FALSE---------------------------------------------------------
#  vtree(FakeData,"Severity Viral",horiz=FALSE,showlevels=FALSE)

## ---- echo=FALSE---------------------------------------------------------
di_example1 <- vtree(FakeData,"Severity Viral",horiz=FALSE,showlevels=FALSE)

## ---- echo=FALSE---------------------------------------------------------
grVizToPNG(di_example1,folder=PNGdir,width=1000)
filepath <- file.path(PNGdir,"di_example1.png")

## ---- eval=FALSE---------------------------------------------------------
#  vtree(FakeData,"Severity",summary="Viral \nViral %npct%",horiz=FALSE,showlevels=FALSE)

## ---- echo=FALSE---------------------------------------------------------
di_example2 <- vtree(FakeData,"Severity",summary="Viral \nViral %npct%",
  horiz=FALSE,showlevels=FALSE)

## ---- echo=FALSE---------------------------------------------------------
grVizToPNG(di_example2,folder=PNGdir,width=1000)
filepath <- file.path(PNGdir,"di_example2.png")

## ----eval=FALSE----------------------------------------------------------
#  vtree(FakeData,"Severity",summary="id \nid = %list% %noroot%",horiz=FALSE)

## ---- echo=FALSE---------------------------------------------------------
ss_example3 <- vtree(FakeData,"Severity",summary="id \nid = %list% %noroot%",horiz=FALSE)

## ---- echo=FALSE---------------------------------------------------------
grVizToPNG(ss_example3,folder=PNGdir,width=1000)
filepath <- file.path(PNGdir,"ss_example3.png")

## ---- eval=FALSE---------------------------------------------------------
#  vtree(c("A","A","A","B","C"),horiz=FALSE)

## ---- echo=FALSE---------------------------------------------------------
exampleSingleVector <- vtree(c("A","A","A","B","C"),horiz=FALSE)
grVizToPNG(exampleSingleVector,folder=PNGdir,width=1000)
filepath <- file.path(PNGdir,"exampleSingleVector.png")

## ---- comment=""---------------------------------------------------------
dotscript <- vtree(FakeData,"Severity",getscript=TRUE)
cat(dotscript)

## ---- eval=FALSE---------------------------------------------------------
#  grVizToPNG(example1)

## ---- highlight=FALSE, eval=FALSE----------------------------------------
#  ![](example1.png){ height=3in }

## ---- highlight=FALSE, eval=FALSE----------------------------------------
#  ![](MyFolder/example1.png){ height=3in }

