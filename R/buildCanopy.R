buildCanopy <- function(z,root=TRUE,novars=FALSE,title="",parent=1,last=1,labels=NULL,tlabelnode=NULL,HTMLtext=FALSE,
  var,
  check.is.na=FALSE,
  labelvar=NULL,
  varminwidth=NULL,varminheight=NULL,varlabelloc=NULL,
  showvarinnode=FALSE,shownodelabels=TRUE,sameline=FALSE,
  prune=NULL,
  tprune=NULL,
  prunelone=NULL,prunesmaller=NULL,prunebigger=NULL,
  keep=NULL,tkeep=NULL,
  text=NULL,ttext=NULL,TopText="",showempty=FALSE,digits=0,cdigits=2,
  showpct=TRUE,
  showrootcount=FALSE,
  showcount=TRUE,
  prefixcount="",
  showvarnames=FALSE,
  pruneNA=FALSE,
  splitwidth=Inf,topcolor="black",color="blue",topfillcolor="olivedrab3",
  fillcolor="olivedrab2",vp=TRUE,rounded=FALSE,just="c",justtext=NULL,thousands="",
  showroot=TRUE,verbose=FALSE,sortfill=FALSE) {
#
# Write DOT code for a single-level {flow}chart of {cat}egories using the
# DiagrammeR framework.
#
# https://en.wikipedia.org/wiki/DOT_(graph_description_language)
#
  
  if (HTMLtext) {
    sepN <- "<BR/>"
  } else {
    sepN <- "\n"
  }

  if (is.na(shownodelabels)) shownodelabels <- TRUE

  if (is.logical(z)) {
    z <- factor(z, c("FALSE", "TRUE"))
  }

  categoryCounts <- table(z,exclude=NULL)
  names(categoryCounts)[is.na(names(categoryCounts))] <- "NA"
  
  sampleSize <- sum(categoryCounts[names(categoryCounts)!="NA"])
  
  #browser()
  
  if (is.null(prunesmaller)) {
    numsmallernodes <- 0
    sumsmallernodes <- 0
  } else {
    if (vp) {
      selectcount <- categoryCounts>=prunesmaller | names(categoryCounts)=="NA" 
    } else {
      selectcount <- categoryCounts>=prunesmaller
    }
    #browser()
    numsmallernodes <- sum(!selectcount & categoryCounts>0)
    sumsmallernodes <- sum(categoryCounts[!selectcount])
    categoryCounts <- categoryCounts[selectcount]
  }
  
  if (is.null(prunebigger)) {
    numbiggernodes <- 0
    sumbiggernodes <- 0
  } else {
    if (vp) {
      selectcount <- categoryCounts<=prunebigger | names(categoryCounts)=="NA" 
    } else {
      selectcount <- categoryCounts<=prunebigger
    }
    numbiggernodes <- sum(!selectcount & categoryCounts>0)
    sumbiggernodes <- sum(categoryCounts[!selectcount])
    categoryCounts <- categoryCounts[selectcount]
  }  
  
  if (length(categoryCounts)==0) {
    return(list(
      root=root,
      value="",
      n=NULL,
      pct=NULL,
      npctString=NULL,
      extraText="",
      levels="",
      nodenum=parent,
      edges="",
      labelassign="",
      lastnode=parent,
      numsmallernodes=numsmallernodes,
      sumsmallernodes=sumsmallernodes,      
      numbiggernodes=numbiggernodes,
      sumbiggernodes=sumbiggernodes))    
  }
  
  
  # Pre-pend the parent node
  categoryCounts <- c(length(z),categoryCounts)
  names(categoryCounts)[1] <- title

  if (novars) {
    showcount <- TRUE
  }

  if (vp & any(names(categoryCounts)=="NA")) { 
    cc <- categoryCounts[-1]
    if (length(cc)>0) {
      npctString <- rep("",length(cc))
      nString <- cc
      if (showcount) {
        for (i in 1:length(cc)) {
          npctString[i] <- format(cc[i],big.mark=thousands)
        }
        npctString <- paste0(prefixcount,npctString)
        #if (showpct) npctString <- paste0(npctString," ")
      }
      pctString <- ifelse(names(cc)=="NA","",around(100*cc/sampleSize,digits))
      if (showpct) {
        npctString <- ifelse(names(cc)=="NA",npctString,paste0(npctString," (",pctString,"%)"))
      }
    } else {
      npctString <- NULL
      nString <- NULL
      pctString <- NULL
    }
  } else {
    npctString <- rep("",length(categoryCounts[-1]))
    nString <- categoryCounts[-1]
    if (showcount) {
      numbers <- categoryCounts[-1]
      for (i in 1:length(numbers)) {
        npctString[i] <- format(numbers[i],big.mark=thousands)
      }      
      npctString <- paste0(prefixcount,npctString)
    }
    pctString <- around(100*categoryCounts[-1]/length(z),digits)
    if (showpct) {
      npctString <- paste0(npctString," (",pctString,"%)")
    }
  }
  
  npctString <- c(format(length(z),big.mark=thousands),npctString)
  nString <- c(length(z),nString)
  pctString <- c("",pctString)
  
  if (!showempty) {
    s <- categoryCounts>0
    categoryCounts <- categoryCounts[s]
    npctString <- npctString[s]
  }
  

  if (length(tprune)>0) {
    for (j in seq_len(length(tprune))) {
      if (length(tprune[[j]])==1 && any(names(tprune[[j]])==var)) {
        tpruneLevel <- names(categoryCounts[-1]) %in% unlist(tprune[[j]][names(tprune[[j]])==var])
        categoryCounts <- c(categoryCounts[1],categoryCounts[-1][!tpruneLevel])
        npctString <- c(npctString[1],npctString[-1][!tpruneLevel])
        pctString <- c(pctString[1],pctString[-1][!tpruneLevel])
        nString <- c(nString[1],nString[-1][!tpruneLevel])
      }
    }
  }
  
  if (length(tkeep)>0) {
    for (j in seq_len(length(tkeep))) {
      if (length(tkeep[[j]])==1 && any(names(tkeep[[j]])==var)) {
        matching <- match(unlist(tkeep[[j]][names(tkeep[[j]])==var]),names(categoryCounts)[-1])
        matching <- matching[!is.na(matching)]
        removed <- categoryCounts[-1][-matching]
        npctremoved <- npctString[-1][-matching]
        if (!vp) {
          if (any(names(removed)=="NA")) {
            NAremoved <- names(removed)=="NA"
            nr <- npctremoved[NAremoved]
            #if (nr>1) description <- "NAs" else description <- "NA"
            #warning(paste0(var,": keep removed ",npctremoved[NAremoved]," ",description),call.=FALSE)
          }  
        } else {
          newkeep <- c(names(categoryCounts[-1])[matching],"NA")
          matching <- match(newkeep,names(categoryCounts)[-1])
          matching <- matching[!is.na(matching)]        
        }
        #browser()
        categoryCounts <- c(categoryCounts[1],categoryCounts[-1][matching])
        npctString <- c(npctString[1],npctString[-1][matching])
        pctString <- c(pctString[1],pctString[-1][matching])
        nString <- c(nString[1],nString[-1][matching])
      }
    }
  }  
  
  if (!is.null(prune)) {
    matching <- names(categoryCounts)[-1] %in% prune
    removed <- categoryCounts[-1][matching]
    npctremoved <- npctString[-1][matching]
    if (any(names(removed)=="NA")) {
      NAremoved <- names(removed)=="NA"
      nr <- npctremoved[NAremoved]
      if (nr>1) description <- "NAs" else description <- "NA"
      warning(paste0(var,": prune removed ",npctremoved[NAremoved]," ",description),call.=FALSE)
    }  
    categoryCounts <- c(categoryCounts[1],categoryCounts[-1][!matching])
    npctString <- c(npctString[1],npctString[-1][!matching])
    pctString <- c(pctString[1],pctString[-1][!matching])
    nString <- c(nString[1],nString[-1][!matching])
  }

  if (!is.null(keep)) {
    matching <- match(keep,names(categoryCounts)[-1])
    matching <- matching[!is.na(matching)]
    removed <- categoryCounts[-1][-matching]
    npctremoved <- npctString[-1][-matching]
    if (!vp) {
      if (any(names(removed)=="NA")) {
        NAremoved <- names(removed)=="NA"
        nr <- npctremoved[NAremoved]
        #if (nr>1) description <- "NAs" else description <- "NA"
        #warning(paste0(var,": keep removed ",npctremoved[NAremoved]," ",description),call.=FALSE)
      }  
    } else {
      newkeep <- c(keep,"NA")
      matching <- match(newkeep,names(categoryCounts)[-1])
      matching <- matching[!is.na(matching)]        
    }
    #browser()
    categoryCounts <- c(categoryCounts[1],categoryCounts[-1][matching])
    npctString <- c(npctString[1],npctString[-1][matching])
    pctString <- c(pctString[1],pctString[-1][matching])
    nString <- c(nString[1],nString[-1][matching])
  }

  if (pruneNA) {
    m <- names(categoryCounts)[-1]!="NA"
    categoryCounts <- c(categoryCounts[1],categoryCounts[-1][m])
    npctString <- c(npctString[1],npctString[-1][m])
  }

  if (!is.null(prunelone)) {
    if (length(categoryCounts[-1])==1) {
      if (names(categoryCounts)[-1] %in% prunelone) {
        categoryCounts <- categoryCounts[1]
      }
    }
  }
  

  # Number of new nodes to add to the tree
  n <- length(categoryCounts)-1              # exclude the parent node

  if (n>0) {
    # Number the parent node and the additional nodes to be added
    nodenum <- c(parent,last+(1:n))
  } else {
    nodenum <- parent
  }
  nodenames <- paste0("Node_",nodenum)

  CAT <- names(categoryCounts)
  FILLCOLOR <- fillcolor[match(CAT[-1],names(fillcolor))]
  
  if (sortfill) {
    o <- order(categoryCounts[-1][CAT[-1]!="NA"])
    toChange <- (1:length(CAT[-1]))[CAT[-1]!="NA"]
    names(FILLCOLOR)[toChange] <- names(FILLCOLOR)[toChange][o]
    FILLCOLOR <- FILLCOLOR[CAT[-1]]
  }

  extraText <- rep("",length(CAT))

  # Match extra text to nodes
  if (TopText!="") extraText[1] <- TopText # paste0(sepN,TopText)
  for (label in names(text)) {
    if (label %in% names(categoryCounts)) {
      m <- match(label,names(categoryCounts))
      if (text[names(text)==label]!="") {
        extraText[m] <- paste0("",text[names(text)==label])
      }
    }
  }
  
  if (length(ttext)>0) {
    for (j in seq_len(length(ttext))) {
      if (length(ttext[[j]])==2 && any(names(ttext[[j]])==var)) {
        TTEXTposition <- CAT[-1] == ttext[[j]][names(ttext[[j]])==var]
        extraText[-1][TTEXTposition] <- ttext[[j]]["text"]
      }
    }
  }

  if (length(tlabelnode)>0) {
    for (j in seq_len(length(tlabelnode))) {
      if (length(tlabelnode[[j]])==2 && any(names(tlabelnode[[j]])==var)) {
        tlabelnode_position <- CAT[-1] == tlabelnode[[j]][names(tlabelnode[[j]])==var]
        CAT[-1][tlabelnode_position] <- tlabelnode[[j]]["label"]
      }
    }
  }
    
  displayCAT <- CAT
  
  if (HTMLtext) {
    displayCAT <- splitlines(displayCAT,width=splitwidth,sp="<BR/>",at=" ")
  } else {
    displayCAT <- splitlines(displayCAT,width=splitwidth,sp="\n",at = c(" ", ".", "-", "+", "_", "=", "/"))
  }

  if (check.is.na) {
    for (i in 2:length(displayCAT)) {
      varname <- gsub("^MISSING_(.+)", "\\1", var)
    }
  }

  # Relabel the nodes if labels have been specified
  for (label in labels) {
    if (label %in% names(categoryCounts)) {
      m <- match(label,names(categoryCounts))
      displayCAT[m] <- names(labels)[labels==label]
    }
  }

  # Write DOT code for the edges
  if (showroot & !novars) {
    edgeVector <- paste0(nodenames[1],"->",nodenames[-1])
    edges <- paste(edgeVector,collapse=" ")
  } else {
    edges <- ""
  }

  if (rounded) {
    styleString <- ' style="rounded,filled"'
  } else {
    styleString <- ' style=filled'
  }

  # Glue a space or a line break onto the non-empty elements of CAT
  if (sameline) {
    for (i in seq_len(length(displayCAT))) {
      if (showcount || showpct || extraText[i]!="") {
        if (displayCAT[i]!="") displayCAT[i] <- paste0(displayCAT[i],", ")
      }
    }
  } else {
    for (i in seq_len(length(displayCAT))) {
      if (displayCAT[i]!="") displayCAT[i] <- paste0(displayCAT[i],sepN)
    }
  }

  if (!shownodelabels) {
    for (i in 2:length(displayCAT)) displayCAT[i] <- ""
  }
  
  # Relabel the nodes if showvarinnode is TRUE
  if (showvarinnode) {
    if (is.null(labelvar)) {
      displayCAT[-1] <- paste0(var,": ",displayCAT[-1])
    } else {
      displayCAT[-1] <- paste0(labelvar,sepN,displayCAT[-1])
    }
  }
  
  extra_text <- extraText

  if (!HTMLtext) {
    # Move any linebreaks at the start of extraText to
    # the end of npctString, so that justification works right
    for (i in seq_len(length(extraText))) {
      if (length(grep("^\n",extraText[i]))>0) {
        npctString[i] <- paste0(npctString[i],"\n")
        extraText[i] <- sub("^(\n)","",extraText[i])
      }
    }
    displayCAT <- convertToHTML(displayCAT,just=just)
    npctString <- convertToHTML(npctString,just=just)
    extraText <- convertToHTML(extraText,just=justtext)
  }
  
  # Write DOT code for assigning labels (using the DiagrammeR framework)
  VARLABELLOC <- ""
  if (!is.null(varlabelloc) && !is.na(varlabelloc)) VARLABELLOC <- paste0("labelloc=",varlabelloc)
  VARMINWIDTH <- ""
  if (!is.null(varminwidth) && !is.na(varminwidth)) VARMINWIDTH <- paste0("width=",varminwidth)
  VARMINHEIGHT <- ""
  if (!is.null(varminheight) && !is.na(varminheight)) VARMINHEIGHT <- paste0("height=",varminheight)
  labelassign <- c()
  if (root) {
    if (showroot) {
      if (!showrootcount) {
        npctString[1] <- ""
      }
      rgb <- grDevices::col2rgb(topfillcolor)
      red <- rgb["red",]; green <- rgb["green",]; blue <- rgb["blue",]
      FONTCOLOR <- ifelse((red*0.299 + green*0.587 + blue*0.114) > 186,"#000000","#ffffff")      
      labelassign <- paste(paste0(
        nodenames[1],'[label=<',displayCAT[1],npctString[1],extraText[1],'>  fontcolor=<',FONTCOLOR,'> color=',topcolor,styleString,
        ' fillcolor=<',topfillcolor,'>]'),collapse='\n')
    }
    if (!novars) {
      rgb <- grDevices::col2rgb(FILLCOLOR)
      red <- rgb["red",]; green <- rgb["green",]; blue <- rgb["blue",]
      FONTCOLOR <- ifelse((red*0.299 + green*0.587 + blue*0.114) > 186,"#000000","#ffffff")
      labelassign <- paste0(labelassign,'\n',paste(paste0(
        nodenames[-1],'[label=<',displayCAT[-1],npctString[-1],extraText[-1],'>  fontcolor=<',FONTCOLOR,'> color=',color,styleString,
        ' fillcolor=<',FILLCOLOR,'>',VARLABELLOC,' ',VARMINWIDTH,' ',VARMINHEIGHT,']')),collapse='\n')
      #browser()
       
    }
  } else {
    rgb <- grDevices::col2rgb(FILLCOLOR)
    red <- rgb["red",]; green <- rgb["green",]; blue <- rgb["blue",]
    FONTCOLOR <- ifelse((red*0.299 + green*0.587 + blue*0.114) > 186,"#000000","#ffffff")    
    labelassign <- paste(paste0(
      nodenames[-1],'[label=<',displayCAT[-1],npctString[-1],extraText[-1],'>  fontcolor=<',FONTCOLOR,'> color=',color,styleString,
      ' fillcolor=<',FILLCOLOR,'>',VARLABELLOC,' ',VARMINWIDTH,' ',VARMINHEIGHT,']'),collapse='\n')
  }
  
  #browser()
  
  return(list(
    root=root,
    value=CAT[-1],
    n=as.numeric(nString[-1]),
    pct=as.numeric(pctString[-1]),
    npctString=npctString[-1],
    extraText=extra_text[-1],
    levels=names(categoryCounts)[-1],
    nodenum=nodenum[-1],
    edges=edges,
    labelassign=labelassign,
    lastnode=nodenum[length(nodenum)],
    numsmallernodes=numsmallernodes,
    sumsmallernodes=sumsmallernodes,
    numbiggernodes=numbiggernodes,
    sumbiggernodes=sumbiggernodes))
}
