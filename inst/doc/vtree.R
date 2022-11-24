## ---- echo=FALSE------------------------------------------------------------------------
suppressMessages(library(ggplot2))
library(vtree)
#source("../source.R")
options(width=90)
options(rmarkdown.html_vignette.check_title = FALSE)

## ---- echo=FALSE------------------------------------------------------------------------
spaces <- function (n) {
  paste(rep("&nbsp;", n), collapse = "")
}

## ---- echo=FALSE------------------------------------------------------------------------
df <- build.data.frame(
  c("continent","population","landlocked"),
  list("Africa","Over 30 million","landlocked",2),
  list("Africa","Over 30 million","not landlocked",12),
  list("Africa","Under 30 million","landlocked",14),
  list("Africa","Under 30 million","not landlocked",26))

## ---------------------------------------------------------------------------------------
plot(0)

## ---- eval=FALSE------------------------------------------------------------------------
#  vtree(df,"v1 v2")

## ---- eval=FALSE------------------------------------------------------------------------
#  simple_tree <- vtree(df,"v1 v2")

## ---- eval=FALSE------------------------------------------------------------------------
#  simple_tree

## ---- eval=FALSE------------------------------------------------------------------------
#  vtree(df)

## ----eval=FALSE, results="asis"---------------------------------------------------------
#  vtree(FakeData,"Severity")

## ----eval=FALSE-------------------------------------------------------------------------
#  library(dplyr)
#  FakeData %>% rename("HowBad"=Severity) %>% vtree("HowBad")

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(FakeData,"Severity Sex",showlegend=TRUE,shownodelabels=FALSE)

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(FakeData,"Severity Sex")

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(FakeData,"Severity Sex",prune=list(Severity=c("Mild","Moderate")))

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(FakeData,"Severity Sex",keep=list(Severity="Moderate"))

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(FakeData,"Severity Sex",keep=list(Severity="Moderate"),vp=FALSE)

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(FakeData,"Severity Sex",prunebelow=list(Severity=c("Mild","Moderate")))

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(FakeData,"Sex Severity",tkeep=list(list(Sex="M",Severity="Moderate")))

## ---- eval=FALSE------------------------------------------------------------------------
#  tkeep=list(list(Sex="M",Severity=c("Moderate","Severe")),list(Sex=F",Severity="Mild"))

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
#  library(dplyr)
#  FakeData %>% mutate(missingScore=is.na(Score)) %>% vtree("missingScore")

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(FakeData,"is.na:Score")

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(FakeData,summary="Score")

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(FakeData,"Severity",summary="Score",horiz=FALSE)

## ----attributes-------------------------------------------------------------------------
vSeverity <- vtree(FakeData,"Severity",summary="Score",horiz=FALSE)
info <- attributes(vSeverity)$info
cat(info$Severity$Mild$.text)

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(FakeData,summary="Category")

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(FakeData,summary="Event")

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(FakeData,"Severity",summary="Category=single",horiz=FALSE)

## ----summary-pattern,eval=FALSE---------------------------------------------------------
#  vtree(FakeData,"Severity",summary="Ind*",sameline=TRUE,horiz=FALSE,just="l")

## ---- eval=FALSE------------------------------------------------------------------------
#  vtree(FakeData,"Severity Category",summary="Score<10 %var=Category%%node=single%",
#    sameline=TRUE, showlegend=TRUE, showlegendsum=TRUE)

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(FakeData,"Severity",summary="Score \nmean score\n%mean%",sameline=TRUE,horiz=FALSE)

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(FakeData,"Severity Category",
#    summary="(Post-Pre)/Pre \nmean = %mean%",sameline=TRUE,horiz=FALSE,cdigits=1)

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(FakeData,"Severity",horiz=FALSE,showvarnames=FALSE,splitwidth=Inf,sameline=TRUE,
#    summary=c("Score \nScore: mean (SD) %meanx% (%SD%)","Pre \nPre: range %range%"))

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(FakeData,"Age Sex",tsummary=list(list(Age="5",Sex="M","id \n%list%")),horiz=FALSE)

## ---- eval=FALSE------------------------------------------------------------------------
#  vtree(FakeData,"Severity Sex")
#  vtree(FakeData,"Severity Sex",pattern=TRUE)

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

## ---- eval=FALSE------------------------------------------------------------------------
#  vtree(FakeData,"Sex Severity",palette=c(3,4))

## ---- eval=FALSE------------------------------------------------------------------------
#  vtree(FakeData,"Sex Group Severity",revgradient=c(Sex=TRUE,Severity=TRUE))

## ---------------------------------------------------------------------------------------
dessert <- build.data.frame(
  c(   "group","IceCream___1","IceCream___2","IceCream___3"),
  list("A",     1,             0,             0,              7),
  list("A",     1,             0,             1,              2),
  list("A",     0,             0,             0,              1),
  list("A",     1,             1,             1,              1),
  list("B",     1,             0,             1,              1),
  list("B",     1,             0,             0,              2), 
  list("B",     0,             1,             1,              1),
  list("B",     0,             0,             0,              1))
attr(dessert$IceCream___1,"label") <- "Ice cream (choice=Chocolate)"
attr(dessert$IceCream___2,"label") <- "Ice cream (choice=Vanilla)"
attr(dessert$IceCream___3,"label") <- "Ice cream (choice=Strawberry)"

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(dessert,"r:IceCream___1")

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(dessert,"r:IceCream@")

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(dessert,"rnone:IceCream@")

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(dessert,"ri:IceCream@")

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(dessert,"IceCream___1 IceCream___2 IceCream___3",pattern=TRUE)

## ----eval=FALSE-------------------------------------------------------------------------
#  vtree(dessert,"stem:IceCream",pattern=TRUE)

## ---- eval=FALSE------------------------------------------------------------------------
#  vtree(dessert,summary="stem:IceCream",splitwidth=Inf,just="l")

## ---- eval=FALSE------------------------------------------------------------------------
#  vtree(dessert,"rc:IceCream___1 rc:IceCream___3",pattern=TRUE)

## ---- comment=""------------------------------------------------------------------------
dotscript <- vtree(FakeData,"Severity",getscript=TRUE)
cat(dotscript)

## ----echo=TRUE,message=FALSE,eval=FALSE-------------------------------------------------
#  v <- vtree(FakeData,"Group Viral",horiz=FALSE)
#  v

## ----echo=FALSE,message=FALSE-----------------------------------------------------------
v <- vtree(FakeData,"Group Viral",pxwidth=800,imageheight="2in",horiz=FALSE)
v

## ----echo=TRUE,message=FALSE------------------------------------------------------------
attributes(v)$info

## ---- eval=FALSE,echo=TRUE--------------------------------------------------------------
#  <unknown>:1919791: Invalid asm.js: Function definition doesn't match use

## ---- eval=FALSE------------------------------------------------------------------------
#  `r vtree(FakeData,"Sex Severity")`

## ---------------------------------------------------------------------------------------
build.data.frame(
  c("pet","breed","size"),
  list("dog","golden retriever","large",5),
  list("cat","tabby","small",2))

## ---- eval=FALSE------------------------------------------------------------------------
#  build.data.frame(
#    c("pet","breed","size"),
#    list("dog","golden retriever","large",5),
#    list("cat","tabby","small",2),
#    list("dog","Dalmation","various",101),
#    list("cat","Abyssinian","small",5),
#    list("cat","Abyssinian","large",22),
#    list("cat","tabby","large",86))

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
#  vtree(ChickWeight,"Diet Time",
#    keep=list(Time=c("0","4")),summary="weight \nmean weight %mean%g")

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

