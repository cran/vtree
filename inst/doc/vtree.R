## ---- echo=FALSE------------------------------------------------------------------------
suppressMessages(library(ggplot2))
library(vtree)
options(width=90)

## ---- echo=FALSE------------------------------------------------------------------------
df <- build.data.frame(
  c("continent","population","landlocked"),
  list("Africa","Over 30 million","landlocked",2),
  list("Africa","Over 30 million","not landlocked",12),
  list("Africa","Under 30 million","landlocked",14),
  list("Africa","Under 30 million","not landlocked",26))

## ---- echo=FALSE, results="asis"--------------------------------------------------------
cat(vtree(FakeData,"Severity Sex",showlegend=FALSE,horiz=FALSE,
  width=600,height=200,pxwidth=1000,imageheight="2.2in"))

## ---- eval=FALSE------------------------------------------------------------------------
#  vtree(df,"v1 v2")

## ----eval=FALSE, results="asis"---------------------------------------------------------
#  vtree(FakeData,"Severity")

## ---- eval=FALSE------------------------------------------------------------------------
#  vtree(FakeData,"Severity Sex",horiz=FALSE,plain=TRUE)

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(FakeData,"Severity Sex",showlegend=TRUE,shownodelabels=FALSE)

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(FakeData,"Severity Sex Viral",sameline=TRUE)

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(FakeData,"Severity Sex",prune=list(Severity=c("Mild","Moderate")))

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(FakeData,"Severity",keep=list(Severity="Moderate"))

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(FakeData,"Severity")
#  vtree(FakeData,"Severity",vp=FALSE)

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(FakeData,"Severity",keep=list(Severity="Moderate"))
#  vtree(FakeData,"Severity",vp=FALSE,keep=list(Severity="Moderate"))

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(FakeData,"Severity Sex",prunebelow=list(Severity=c("Mild","Moderate")))

## ---- eval=FALSE------------------------------------------------------------------------
#  vtree(FakeData,"Severity Sex Age Category",sameline=TRUE)

## ---- eval=FALSE------------------------------------------------------------------------
#  vtree(FakeData,"Severity Sex Age Category",sameline=TRUE,prunesmaller=3)

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(FakeData,"Severity Sex",horiz=FALSE,labelvar=c(Severity="Initial severity"))

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(FakeData,"Group Sex",horiz=FALSE,labelnode=list(Sex=c(Male="M",Female="F")))

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(FakeData,"Group Sex",horiz=FALSE,
#    labelnode=list(Group=c(Child="A",Adult="B")),
#    tlabelnode=list(
#      c(Group="A",Sex="F",label="girl"),
#      c(Group="A",Sex="M",label="boy"),
#      c(Group="B",Sex="F",label="woman"),
#      c(Group="B",Sex="M",label="man")))

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(FakeData,"Group Severity",horiz=FALSE,showvarnames=FALSE,
#    text=list(Severity=c(Mild="\n*Excluding\nnew diagnoses*")))

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(FakeData,"Group Severity",horiz=FALSE,showvarnames=FALSE,
#    ttext=list(
#      c(Group="B",Severity="Mild",text="\n*Excluding\nnew diagnoses*"),
#      c(Group="A",text="\nSweden"),
#      c(Group="B",text="\nNorway")))

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(FakeData,"Severity",summary="Score \nmean score: %mean%",sameline=TRUE,horiz=FALSE)

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(FakeData,"Severity",summary="Score \nmean score: %mean%",cdigits=0,
#    sameline=TRUE,horiz=FALSE)

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(FakeData,"Severity",horiz=FALSE,showvarnames=FALSE,splitwidth=Inf,sameline=TRUE,
#    summary=c("Score \nScore: mean (SD) %mean% (%SD%)","Pre \nPre: range %min%, %max%"))

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(FakeData,"Severity",summary="Category=single \n%pct% single",sameline=TRUE,horiz=FALSE)

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(FakeData,"Severity Category",
#    summary="(Post-Pre)/Pre \nmean = %mean%",sameline=TRUE,horiz=FALSE,cdigits=1)

## ---- eval=FALSE------------------------------------------------------------------------
#  vtree(FakeData,"Severity Sex")
#  vtree(FakeData,"Severity Sex",pattern=TRUE)

## ---- echo=FALSE,results="asis"---------------------------------------------------------
cat(vtree(FakeData,"Severity Sex",
  width=650,height=650,pxwidth=500,imageheight="4in"))
#filepath <- grVizToPNG(v14,width=800))

cat("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;")

cat(vtree(FakeData,"Severity Sex",pattern=TRUE,
  width=650,height=650,pxwidth=600,imageheight="4in"))
#filepath2 <- grVizToPNG(v15,width=800)
#![](`r filepath`){ height=4in } &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; ![](`r filepath2`){ height=4in }

## ---------------------------------------------------------------------------------------
vtree(FakeData,"Severity Sex",ptable=TRUE)

## ---- eval=FALSE------------------------------------------------------------------------
#  vtree(FakeData,"Ind1 Ind2 Ind3 Ind4",Venn=TRUE,pattern=TRUE)

## ---------------------------------------------------------------------------------------
vtree(FakeData,"Ind1 Ind2",ptable=TRUE)

## ---------------------------------------------------------------------------------------
VennTable(vtree(FakeData,"Ind1 Ind2",ptable=TRUE))

## ---------------------------------------------------------------------------------------
print(VennTable(vtree(FakeData,"Ind1 Ind2",ptable=TRUE)),quote=FALSE)

## ---- eval=FALSE------------------------------------------------------------------------
#  `r VennTable(vtree(FakeData,"Ind1 Ind2",ptable=TRUE),markdown=TRUE)`

## ---------------------------------------------------------------------------------------
vtree(FakeData,"Severity Sex",summary=c("Score %mean%","Pre %mean%"),ptable=TRUE)

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(FakeData,"Severity Age Pre Post",check.is.na=TRUE)

## ---- eval=FALSE------------------------------------------------------------------------
#  `r VennTable(vtree(FakeData,"Severity Age Pre Post",check.is.na=TRUE,ptable=TRUE),
#     markdown=TRUE)`

## ---------------------------------------------------------------------------------------
vtree(FakeData,"Severity Age Pre Post",check.is.na=TRUE,summary="id %list%%trunc=15%",
  ptable=TRUE)

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(MyREDCapData,"IceCream___1 IceCream___2 IceCream___3")

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(MyREDCapData,"stem:IceCream")

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(MyREDCapData,"stem:IceCream",pattern=TRUE,showroot=FALSE)

## ---- eval=FALSE------------------------------------------------------------------------
#  vtree(MyREDCapData,"rc:IceCream___1 rc:IceCream___3",pattern=TRUE,showroot=FALSE)

## ---- comment=""------------------------------------------------------------------------
dotscript <- vtree(FakeData,"Severity",getscript=TRUE)
cat(dotscript)

## ---- eval=FALSE------------------------------------------------------------------------
#  `r vtree(FakeData,"Sex Severity")`

## ---- eval=FALSE------------------------------------------------------------------------
#  `r vtree(FakeData,"Severity Sex",pngknit=FALSE)`

## ---------------------------------------------------------------------------------------
build.data.frame(
  c("pet","breed","size"),
  list("dog","golden retriever","large",5),
  list("cat","tabby","small",2))

## ---- eval=FALSE------------------------------------------------------------------------
#  vtree(build.data.frame(
#    c("pet","breed","size"),
#    list("dog","golden retriever","large",5),
#    list("cat","tabby","small",2),
#    list("dog","Dalmation","various",101),
#    list("cat","Abyssinian","small",5),
#    list("cat","Abyssinian","large",22),
#    list("cat","tabby","large",86)))

## ---------------------------------------------------------------------------------------
FakeRCT

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(FakeRCT,"eligible randomized group followup analyzed",plain=TRUE,
#    keep=list(eligible="Eligible",randomized="Randomized",followup="Followed up"),
#    horiz=FALSE,showvarnames=FALSE,title="Assessed for eligibility")

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(FakeRCT,"eligible randomized group followup analyzed",plain=TRUE,
#    follow=list(eligible="Eligible",randomized="Randomized",followup="Followed up"),
#    horiz=FALSE,showvarnames=FALSE,title="Assessed for eligibility")

## ---- eval=FALSE------------------------------------------------------------------------
#  vtree(FakeRCT,"eligible randomized group followup analyzed",plain=TRUE,
#    follow=list(eligible="Eligible",randomized="Randomized",followup="Followed up"),
#    horiz=FALSE,showvarnames=FALSE,title="Assessed for eligibility",
#    summary="id \nid: %list% %noroot%")

## ---- eval=FALSE------------------------------------------------------------------------
#  vtree(esoph,"agegp=75+",sameline=TRUE,cdigits=0,
#    summary=c("ncases \ncases=%sum%%leafonly%","ncontrols  controls=%sum%%leafonly%"))

## ---- eval=FALSE------------------------------------------------------------------------
#  hec <- crosstabToCases(HairEyeColor)

## ---- eval=FALSE------------------------------------------------------------------------
#  vtree(hec,"Hair Eye=Green Sex",sameline=TRUE)

## ---- eval=FALSE------------------------------------------------------------------------
#  titanic <- crosstabToCases(Titanic)

## ---- eval=FALSE------------------------------------------------------------------------
#  vtree(titanic,"Class Sex Age",summary="Survived=Yes \n%pct% survived",sameline=TRUE)

## ---- eval=FALSE------------------------------------------------------------------------
#  mt <- mtcars
#  mt$name <- rownames(mt)
#  rownames(mt) <- NULL

## ---- eval=FALSE------------------------------------------------------------------------
#  vtree(mt,"cyl gear carb",summary="hp \nmean (SD) HP %mean% (%SD%)")

## ---- eval=FALSE------------------------------------------------------------------------
#  vtree(mt,"cyl gear carb",summary="hp mean (SD) HP %mean% (%SD%)",
#    cdigits=0,labelvar=c(cyl="# cylinders",gear="# gears",carb="# carburetors"),
#    ptable=TRUE)

## ---- eval=FALSE------------------------------------------------------------------------
#  vtree(mt,"gear carb",summary="name \n%list%%noroot%",splitwidth=50,sameline=TRUE,
#    labelvar=c(gear="# gears",carb="# carburetors"))

## ---- eval=FALSE------------------------------------------------------------------------
#  ucb <- crosstabToCases(UCBAdmissions)

## ---- eval=FALSE------------------------------------------------------------------------
#  vtree(ucb,"Dept Gender",summary="Admit=Admitted \n%pct% admitted",sameline=TRUE)

## ---- eval=FALSE------------------------------------------------------------------------
#  vtree(ChickWeight,"Diet Time",keep=list(Time=c("0","4")),summary="weight \nmean weight %mean%g")

## ---- eval=FALSE------------------------------------------------------------------------
#  vtree(ChickWeight,"Diet Time",keep=list(Time=c("0","4")),
#    labelnode=list(
#      Diet=c("Diet 1"="1","Diet 2"="2","Diet 3"="3","Diet 4"="4"),
#      Time=c("0 days"="0","4 days"="4")),
#    labelvar=c(Time="Days since birth"),summary="weight \nmean weight %mean%g")

## ---- eval=FALSE------------------------------------------------------------------------
#  vtree(InsectSprays,"spray",splitwidth=80,sameline=TRUE,
#    summary="count \ncounts: %list%%noroot%",cdigits=0)

## ---- eval=FALSE------------------------------------------------------------------------
#  vtree(ToothGrowth,"supp dose",summary="len>20 \n%pct% length > 20")

## ---- eval=FALSE------------------------------------------------------------------------
#  vtree(ToothGrowth,"supp dose",summary="len>20 \n%pct% length > 20",
#    labelvar=c("supp"="Supplement type","dose"="Dose (mg/day)"),
#    labelnode=list(supp=c("Vitamin C"="VC","Orange Juice"="OJ")))

