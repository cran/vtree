## ---- echo=FALSE------------------------------------------------------------------------
suppressMessages(library(ggplot2))
library(vtree)
options(width=90)

## ---- echo=FALSE------------------------------------------------------------------------
PNGdir <- tempdir()

## ---- echo=FALSE------------------------------------------------------------------------
v1 <- vtree(FakeData,"Severity Sex",showlegend=FALSE,horiz=FALSE,width=600,height=200)
filepath <- grVizToPNG(v1,folder=PNGdir,width=800)

## ---- eval=FALSE------------------------------------------------------------------------
#  vtree(df,"v1 v2")

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(FakeData,"Severity")

## ----echo=FALSE-------------------------------------------------------------------------
v2 <- vtree(FakeData,"Severity",width=250,height=250)
filepath <- grVizToPNG(v2,folder=PNGdir,width=1000)

## ---- eval=FALSE------------------------------------------------------------------------
#  vtree(FakeData,"Severity Sex",horiz=FALSE,plain=TRUE)

## ---- echo=FALSE------------------------------------------------------------------------
v3 <- vtree(FakeData,"Severity Sex",horiz=FALSE,plain=TRUE,width=550,height=200)
filepath <- grVizToPNG(v3,folder=PNGdir,width=800)

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(FakeData,"Severity Sex",showlegend=TRUE,shownodelabels=FALSE)

## ----echo=FALSE-------------------------------------------------------------------------
v4 <- vtree(FakeData,"Severity Sex",showlegend=TRUE,shownodelabels=FALSE,width=550,height=450)
filepath <- grVizToPNG(v4,folder=PNGdir,width=800)

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(FakeData,"Severity Sex Viral",sameline=TRUE)

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(FakeData,"Severity Sex",prune=list(Severity=c("Mild","Moderate")))

## ----echo=FALSE-------------------------------------------------------------------------
v6 <- vtree(FakeData,"Severity Sex",prune=list(Severity=c("Mild","Moderate")),
  width=500,height=300)
filepath <- grVizToPNG(v6,folder=PNGdir,width=800)

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(FakeData,"Severity Sex",prunebelow=list(Severity=c("Mild","Moderate")))

## ----echo=FALSE-------------------------------------------------------------------------
v7 <- vtree(FakeData,"Severity Sex",prunebelow=list(Severity=c("Mild","Moderate")),
  width=500,height=300)
filepath <- grVizToPNG(v7,folder=PNGdir,width=800)

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(FakeData,"Severity Sex",horiz=FALSE,labelvar=c(Severity="Initial severity"))

## ----echo=FALSE-------------------------------------------------------------------------
v8 <- vtree(FakeData,"Severity Sex",horiz=FALSE,width=700,height=250,
  labelvar=c(Severity="Initial severity"))
filepath <- grVizToPNG(v8,folder=PNGdir,width=800)

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(FakeData,"Group Sex",horiz=FALSE,labelnode=list(Sex=c(Male="M",Female="F")))

## ----echo=FALSE-------------------------------------------------------------------------
v8a <- vtree(FakeData,"Group Sex",horiz=FALSE,labelnode=list(Sex=c(Male="M",Female="F")),
  width=700,height=250)
filepath <- grVizToPNG(v8a,folder=PNGdir,width=800)

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(FakeData,"Group Sex",horiz=FALSE,
#    labelnode=list(Group=c(Child="A",Adult="B")),
#    tlabelnode=list(
#      c(Group="A",Sex="F",label="girl"),
#      c(Group="A",Sex="M",label="boy"),
#      c(Group="B",Sex="F",label="woman"),
#      c(Group="B",Sex="M",label="man")))

## ----echo=FALSE-------------------------------------------------------------------------
v8b <- vtree(FakeData,"Group Sex",horiz=FALSE,width=700,height=250,
  labelnode=list(Group=c(Child="A",Adult="B")),
  tlabelnode=list(
    c(Group="A",Sex="F",label="girl"),
    c(Group="A",Sex="M",label="boy"),
    c(Group="B",Sex="F",label="woman"),
    c(Group="B",Sex="M",label="man")))
filepath <- grVizToPNG(v8b,folder=PNGdir,width=1000)

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(FakeData,"Group Severity",horiz=FALSE,showvarnames=FALSE,
#    text=list(Severity=c(Mild="\n*Excluding\nnew diagnoses*")))

## ----echo=FALSE-------------------------------------------------------------------------
v9 <- vtree(FakeData,"Group Severity",horiz=FALSE,showvarnames=FALSE,
  text=list(Severity=c(Mild="\n*Excluding\nnew diagnoses*")),
  width=450,height=150)
filepath <- grVizToPNG(v9,folder=PNGdir,width=1000)

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(FakeData,"Group Severity",horiz=FALSE,showvarnames=FALSE,
#    ttext=list(
#      c(Group="B",Severity="Mild",text="\n*Excluding\nnew diagnoses*"),
#      c(Group="A",text="\nSweden"),
#      c(Group="B",text="\nNorway")))

## ----echo=FALSE-------------------------------------------------------------------------
v9_a <- vtree(FakeData,"Group Severity",horiz=FALSE,showvarnames=FALSE,
  ttext=list(c(Group="B",Severity="Mild",text="\n*Excluding\nnew diagnoses*"),
    c(Group="A",text="\nSweden"),c(Group="B",text="\nNorway")),
  width=450,height=150)
filepath <- grVizToPNG(v9_a,folder=PNGdir,width=1000)

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(FakeData,"Severity",summary="Score \nmean score: %mean%",sameline=TRUE,horiz=FALSE)

## ----echo=FALSE-------------------------------------------------------------------------
v10 <- vtree(FakeData,"Severity",summary="Score \nmean score: %mean%",
  sameline=TRUE,horiz=FALSE,width=450,height=150)
filepath <- grVizToPNG(v10,folder=PNGdir,width=1000)

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(FakeData,"Severity",horiz=FALSE,showvarnames=FALSE,splitwidth=Inf,sameline=TRUE,
#    summary=c(
#      "Score \nScore: mean (SD) %mean% (%SD%)",
#      "Pre \nPre: range %min%, %max%"))

## ----echo=FALSE-------------------------------------------------------------------------
v11 <- vtree(FakeData,"Severity",horiz=FALSE,showvarnames=FALSE,splitwidth=Inf,sameline=TRUE,
  summary=c(
    "Score \nScore: mean (SD) %mean% (%SD%)",
    "Pre \nPre: range %min%, %max%"),
  width=650,height=350)
filepath <- grVizToPNG(v11,folder=PNGdir,width=1000)

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(FakeData,"Severity",summary="Viral \nViral %npct%",horiz=FALSE,
#    showvarnames=FALSE,sameline=TRUE)

## ----echo=FALSE-------------------------------------------------------------------------
v12 <- vtree(FakeData,"Severity",summary="Viral \nViral %npct%",horiz=FALSE,
  showvarnames=FALSE,sameline=TRUE,width=650,height=200)
filepath <- grVizToPNG(v12,folder=PNGdir,width=800)

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(FakeData,"Severity",summary="Category=single \n%pct% single",sameline=TRUE,horiz=FALSE)

## ----echo=FALSE-------------------------------------------------------------------------
v13x <- vtree(FakeData,"Severity",summary="Category=single \n%pct% single", 
  horiz=FALSE,sameline=TRUE,width=450,height=150)
filepath <- grVizToPNG(v13x,folder=PNGdir,width=800)

## ---------------------------------------------------------------------------------------
set.seed(1234)
FakeData$Feature1 <- rbinom(nrow(FakeData),1,0.5)
FakeData$Feature1[!FakeData$Viral | is.na(FakeData$Viral)] <- NA
FakeData$Feature2 <- rbinom(nrow(FakeData),1,0.5)
FakeData$Feature2[!FakeData$Viral | is.na(FakeData$Viral)] <- NA

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(FakeData,"Sex Category",sameline=TRUE,splitwidth=150,varminwidth=c(Category=6),
#    summary=c(
#      "Viral \nviral: %npct%%leafonly%",
#      "Feature1 , F1: %npct%%leafonly%",
#      "Feature2 , F2: %npct%%leafonly%"))

## ----echo=FALSE-------------------------------------------------------------------------
v13a <-
  vtree(FakeData,"Sex Category",sameline=TRUE,splitwidth=150,varminwidth=c(Category=6),
  summary=c(
    "Viral \nviral: %npct%%leafonly%",
    "Feature1 , F1: %npct%%leafonly%",
    "Feature2 , F2: %npct%%leafonly%"))
filepath <- grVizToPNG(v13a,folder=PNGdir,width=800)

## ---- eval=FALSE------------------------------------------------------------------------
#  function(x) TRUE

## ---- eval=FALSE------------------------------------------------------------------------
#  function(x) any(x$Viral,na.rm=TRUE)

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(FakeData,"Sex Category",sameline=TRUE,splitwidth=150,varminwidth=c(Category=6),
#    summary=c(
#      "Viral \nviral: %npct%%leafonly%",
#      "Feature1 , F1: %npct%%leafonly%",
#      "Feature2 , F2: %npct%%leafonly%"),
#    runsummary=list(
#      function(x) TRUE,
#      function(x) any(x$Viral,na.rm=TRUE),
#      function(x) any(x$Viral,na.rm=TRUE)))

## ----echo=FALSE-------------------------------------------------------------------------
v13b <- vtree(FakeData,"Sex Category",sameline=TRUE,splitwidth=150,varminwidth=c(Category=6),
  width=950,height=900,
  summary=c(
    "Viral \nviral: %npct%%leafonly%",
    "Feature1 , F1: %npct%%leafonly%",
    "Feature2 , F2: %npct%%leafonly%"),
  runsummary=list(
    function(x) TRUE,
    function(x) any(x$Viral,na.rm=TRUE),
    function(x) any(x$Viral,na.rm=TRUE)))
filepath <- grVizToPNG(v13b,folder=PNGdir,width=800)

## ---- eval=FALSE------------------------------------------------------------------------
#  vtree(FakeData,"Severity Sex")
#  vtree(FakeData,"Severity Sex",pattern=TRUE)

## ---- echo=FALSE------------------------------------------------------------------------
v14 <- vtree(FakeData,"Severity Sex",width=650,height=650)
filepath <- grVizToPNG(v14,folder=PNGdir,width=800)
v15 <- vtree(FakeData,"Severity Sex",pattern=TRUE,width=650,height=650)
filepath2 <- grVizToPNG(v15,folder=PNGdir,width=800)

## ---------------------------------------------------------------------------------------
vtree(FakeData,"Severity Sex",ptable=TRUE)

## ---------------------------------------------------------------------------------------
vtree(FakeData,"Severity Sex",summary=c("Score %mean%","Pre %mean%"),ptable=TRUE)

## ---- eval=FALSE------------------------------------------------------------------------
#  vtree(FakeData,"Ind1 Ind2 Ind3 Ind4",Venn=TRUE)

## ---- echo=FALSE------------------------------------------------------------------------
v21 <- vtree(FakeData,"Ind1 Ind2 Ind3 Ind4",Venn=TRUE,width=650,height=650)
filepath <- grVizToPNG(v21,folder=PNGdir,width=1000)

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(FakeData,"Ind1 Ind2 Ind3 Ind4",Venn=TRUE,pattern=TRUE,
#    palette=c(Ind1=1,Ind2=2,Ind3=3,Ind4=4))

## ----echo=FALSE-------------------------------------------------------------------------
v23 <- vtree(FakeData,"Ind1 Ind2 Ind3 Ind4",Venn=TRUE,pattern=TRUE,
  palette=c(Ind1=1,Ind2=2,Ind3=3,Ind4=4))
filepath <- grVizToPNG(v23,folder=PNGdir,width=800)

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(FakeData,"Severity Age Pre Post",check.is.na=TRUE)

## ----echo=FALSE-------------------------------------------------------------------------
v24 <- vtree(FakeData,"Severity Age Pre Post",check.is.na=TRUE,
  width=700,height=370)
filepath <- grVizToPNG(v24,folder=PNGdir,width=800)

## ---------------------------------------------------------------------------------------
vtree(FakeData,"Severity Age Pre Post",check.is.na=TRUE,ptable=TRUE)

## ---------------------------------------------------------------------------------------
vtree(FakeData,"Severity Age Pre Post",check.is.na=TRUE,summary="id %list%%trunc=15%",
  ptable=TRUE)

## ---------------------------------------------------------------------------------------
FakeRCT

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(FakeRCT,"eligible randomized group followup analyzed",plain=TRUE,
#    keep=list(eligible="Eligible",randomized="Randomized",followup="Followed up"),
#    horiz=FALSE,showvarnames=FALSE,title="Assessed for eligibility")

## ----echo=FALSE-------------------------------------------------------------------------
v26 <- vtree(FakeRCT,"eligible randomized group followup analyzed",plain=TRUE,width=230,height=500,
  keep=list(eligible="Eligible",randomized="Randomized",followup="Followed up"),
  horiz=FALSE,showvarnames=FALSE,title="Assessed for eligibility")
filepath <- grVizToPNG(v26,folder=PNGdir,width=800)

## ----eval=FALSE-------------------------------------------------------------------------
#  v7 <- vtree(FakeRCT,"eligible randomized group followup analyzed",plain=TRUE,
#    follow=list(eligible="Eligible",randomized="Randomized",followup="Followed up"),
#    horiz=FALSE,showvarnames=FALSE,title="Assessed for eligibility")

## ---- echo=FALSE------------------------------------------------------------------------
v27 <- vtree(FakeRCT,"eligible randomized group followup analyzed",plain=TRUE,width=400,height=500,
  follow=list(eligible="Eligible",randomized="Randomized",followup="Followed up"),
  horiz=FALSE,showvarnames=FALSE,title="Assessed for eligibility")
filepath <- grVizToPNG(v27,folder=PNGdir,width=800)

## ---- eval=FALSE------------------------------------------------------------------------
#  vtree(FakeRCT,"eligible randomized group followup analyzed",plain=TRUE,
#    follow=list(eligible="Eligible",randomized="Randomized",followup="Followed up"),
#    horiz=FALSE,showvarnames=FALSE,title="Assessed for eligibility",
#    summary="id \nid: %list% %noroot%")

## ---- echo=FALSE------------------------------------------------------------------------
v28 <- vtree(FakeRCT,"eligible randomized group followup analyzed",plain=TRUE,width=500,height=600,
  follow=list(eligible="Eligible",randomized="Randomized",followup="Followed up"),
  horiz=FALSE,showvarnames=FALSE,title="Assessed for eligibility",
  summary="id \nid: %list% %noroot%")
filepath <- grVizToPNG(v28,folder=PNGdir,width=800)

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(MyREDCapData,"IceCream___1 IceCream___2 IceCream___3")

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(MyREDCapData,"stem:IceCream")

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(MyREDCapData,"stem:IceCream",pattern=TRUE,showroot=FALSE)

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(FakeData,"Severity",edgeattr="arrowhead=none penwidth=4",sameline=TRUE)

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(FakeData,"Severity",nodeattr="width=2",sameline=TRUE)

## ---- comment=""------------------------------------------------------------------------
dotscript <- vtree(FakeData,"Severity",getscript=TRUE)
cat(dotscript)

## ---- eval=FALSE------------------------------------------------------------------------
#  vtree(esoph,"agegp=75+",
#    summary="ncases \nmean=%mean%%leafonly%",sameline=TRUE)

## ---- eval=FALSE------------------------------------------------------------------------
#  hec <- crosstabToCases(HairEyeColor)

## ---- eval=FALSE------------------------------------------------------------------------
#  vtree(hec,"Hair Eye=Green Sex")

## ---- eval=FALSE------------------------------------------------------------------------
#  titanic <- crosstabToCases(Titanic)

## ---- eval=FALSE------------------------------------------------------------------------
#  vtree(titanic,"Class Sex Age",summary="Survived=Yes \n%pct% survived",sameline=TRUE)

## ---- eval=FALSE------------------------------------------------------------------------
#  mt <- mtcars
#  mt$name <- rownames(mt)
#  rownames(mt) <- NULL

## ---- eval=FALSE------------------------------------------------------------------------
#  vtree(mt,"cyl gear carb",summary="hp \nmean (SD) HP %mean% (%SD%)%var=pattern%",pattern=TRUE)

## ---- eval=FALSE------------------------------------------------------------------------
#  vtree(mt,"gear carb",summary="name \n%list%%noroot%",splitwidth=50)

## ---- eval=FALSE------------------------------------------------------------------------
#  ucb <- crosstabToCases(UCBAdmissions)

## ---- eval=FALSE------------------------------------------------------------------------
#  vtree(ucb,"Dept Gender",summary="Admit=Admitted \n%pct% admitted",sameline=TRUE)

## ---- eval=FALSE------------------------------------------------------------------------
#  vtree(ChickWeight,"Diet Time",keep=list(Time=c("0","4")),summary="weight \nmean weight %mean%g")

## ---- eval=FALSE------------------------------------------------------------------------
#  vtree(InsectSprays,"spray",summary="count \nmean (SD) count %mean% (%SD%)",splitwidth=50)

## ---- eval=FALSE------------------------------------------------------------------------
#  vtree(ToothGrowth,"supp dose",summary="len>20 \n%pct% length > 20")

## ---- eval=FALSE------------------------------------------------------------------------
#  example1 <- vtree(FakeData,"Severity Sex")

## ---- eval=FALSE------------------------------------------------------------------------
#  grVizToPNG(example1)

## ---- highlight=FALSE, eval=FALSE-------------------------------------------------------
#  ![](example1.png){ height=3in }

## ---- highlight=FALSE, eval=FALSE-------------------------------------------------------
#  ![](MyFolder/example1.png){ height=3in }

