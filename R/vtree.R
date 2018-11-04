#'
#'
#' vtree: Draw a variable tree
#'
#' @description
#' vtree is a tool for drawing variable trees.
#' Variable trees display information about nested subsets of a data frame,
#' in which the subsetting is defined by the values of categorical variables.
#'
#' @author Nick Barrowman <nbarrowman@cheo.on.ca>
#'
#' @param z           Data frame, or a single vector.
#' @param vars        Either a character string of space-separated variable names
#'                    or a vector variable names. (Does not need to be specified if \code{z} is a vector.)
#' @param horiz       Should the tree be drawn horizontally? (i.e. root node on the left, with the tree growing to the right)
#' @param labelnode   List of vectors used to change how variable values are displayed.
#'                    The name of each element of the
#'                    list is one of the variable names in \code{vars}.
#'                    Each element of the list is a vector of character strings.
#'                    The character strings represent the existing levels of the variable.
#'                    The names of the vector represent the corresponding new levels of the variable.
#' @param title       title for the top node of the tree. (If not specified "Sample" will be shown.)
#' @param prune       List of vectors to identify nodes to prune.
#'                    The name of each element of the
#'                    list is one of the variable names in \code{vars}.
#'                    Each element is a vector of character strings identifying values of the variable (nodes) to prune.
#' @param showlevels  Show the names of the variables at each level?
#' @param prunebelow  Same as prune but the nodes themselves are not pruned, just their descendants.
#' @param keep        Which nodes should be kept (i.e. not pruned).
#' @param follow      Which nodes should be "followed".
#'                    Here, "followed" means that the descendants of a node (and their descendants, etc.) will be shown.
#' @param labelvar    a named vector of labels for labeling nodes.
#'                    Each element is a header for nodes of the corresponding variable.
#' @param check.is.na Replace each variable with a logical vector indicating whether or not each of its values is missing.
#'                    This is one case where \code{vtree} can use continuous variables.
#' @param summary     A character string used to specify summary statistics to display in each node.
#'                    The first word in the character string is the name of the variable to be summarized.
#'                    The rest of the character string is the the string that will be displayed,
#'                    in which special codes are transformed into summary statistics, etc.
#'                    See Details for the special codes.
#' @param leafonly    Should the summary information only be shown in leaf nodes?
#' @param color       A vector of color names for the outline of the nodes at each level (see Graphviz `color` attribute).
#' @param fillcolor   A vector of color names for filling the nodes at each level (see Graphviz `fillcolor` attribute).
#' @param text        A list of vectors used to specify extra text to add to certain nodes.
#'                    (See Details for text formatting.)
#' @param HTMLtext    Is the text formatted in HTML?
#' @param splitwidth  The minimum number of characters before an automatic linebreak is created.
#' @param nodesep     Graphviz attribute: Node separation amount
#' @param ranksep     Graphviz attribute: Rank separation amount
#' @param vp          Use "valid percentages"?
#'                    Valid percentages are computed by first excluding any missing values,
#'                    i.e. restricting attention to the set of "valid" observations.
#'                    The denominator is thus the number of non-missing observations.
#'                    When \code{vp=TRUE}, nodes for missing values show the number of missing values
#'                    but do not show a percentage. The other nodes show valid percentages.
#'                    When \code{vp=FALSE}, all nodes (including nodes for missing values)
#'                    show percentages of the total number of observations.
#' @param nodefunc    A node function.
#' @param nodeargs    A list containing arguments for the node function.
#' @param rounded     Should the nodes have rounded boxes?
#' @param getscript   Instead of displaying the flowchart, return the DOT script as a character string?
#' @param showempty   Show empty (N=0) nodes?
#' @param levelshape  The shape of the boxes showing the levels of the tree
#' @param digits      Number of decimal digits to show in percentages
#' @param colornodes  Should the node boxes be colored?
#' @param fillnodes   Should the nodes be filled with color?
#' @param root        Is this the root node?
#' @param last        Is this the last node?
#' @param top         Is this the top?
#' @param width       width to be passed to grViz
#' @param height      height to be passed to grViz
#'
#' @details
#' Special codes for the \code{summary} argument:
#' \itemize{
#'  \item{\code{\%mean\%} }{mean}
#'  \item{\code{\%SD\%} }{standard deviation}
#'  \item{\code{\%min\%} }{minimum}
#'  \item{\code{\%max\%} }{maximum}
#'  \item{\code{\%pX\%} }{Xth percentile, e.g. p50 means the 50th percentile}
#'  \item{\code{\%median\%} }{median, i.e. p50}
#'  \item{\code{\%IQR\%} }{interquartile range, i.e. p25, p75}
#'  \item{\code{\%list\%} }{list of the individual values}
#'  \item{\code{\%mv\%} }{the number of missing values}
#'  \item{\code{\%v\%} }{the name of the variable}
#'  \item{\code{\%noroot\%} }{flag: Do not show summary in the root node.}
#'  \item{\code{\%leafonly\%} }{flag: Only show summary in leaf nodes.}
#' }
#'
#' Formatting codes for the \code{text} argument.
#' Also used by \code{labelnode} and \code{labelvar}.
#' \itemize{
#'  \item{\code{\\n} }{line break}
#'  \item{\code{*...*} }{italics}
#'  \item{\code{**...**} }{bold}
#'  \item{\code{^...^} }{superscript (using 10 point font)}
#'  \item{\code{~...~} }{subscript (using 10 point font)}
#'  \item{\code{\%\%red ...\%\%} }{display text in red (or whichever color is specified)}
#' }
#'
#' @examples
#' # A single-level hierarchy
#' vtree(FakeData,"Severity")
#'
#' # A two-level hierarchy
#' vtree(FakeData,"Severity Sex")
#'
#' # A two-level hierarchy with pruning of some values of Severity
#' vtree(FakeData,"Severity Sex",prune=list("Severity"=c("Moderate","NA")))
#'
#' # Rename some nodes
#' vtree(FakeData,"Severity Sex",labelnode=list(Sex=(c("Male"="M","Female"="F"))))
#'
#' @export

vtree <- function (z, vars, showempty = FALSE, title = "",
  prune=list(), prunebelow = list(), keep=list(), follow=list(),
  labelnode = list(),labelvar = NULL,
  text = list(),
  check.is.na = FALSE, nodefunc = NULL, nodeargs = NULL,
  HTMLtext = FALSE, showlevels = TRUE, levelshape = "none",
  digits = 0, splitwidth = 20, color = c("blue", "forestgreen",
      "red", "orange", "pink"), colornodes = FALSE, fillcolor = rep(c("#EFF3FF",
      "#C6DBEF", "#9ECAE1", "#6BAED6", "#4292C6", "#2171B5",
      "#084594"), 8), fillnodes = TRUE, getscript = FALSE,
  nodesep = 0.5, ranksep = 0.5, vp = TRUE, root = 1, last = 1,
  top = TRUE, horiz = TRUE, rounded = TRUE, summary = "",leafonly = FALSE,
  width=NULL,height=NULL)
{
  argname <- sapply(as.list(substitute({z})[-1]), deparse)

  if (!missing(vars) && length(vars)==1) {
    vars <- strsplit(vars," ")[[1]]
  }

  if (!all(summary=="")) {
    codevar <- gsub("^([^ ]+) (.+)$", "\\1", summary)
    codecode <- gsub("^([^ ]+) (.+)$", "\\2", summary)
    nodefunc <- nn
    nodeargs <- list(var = codevar, format = codecode)
  }
  if (check.is.na) {
      showlevels <- FALSE
  }
  makeHTML <- function(x) {
    if (is.list(x)) {
      lapply(x, convertToHTML)
    }
    else {
      convertToHTML(x)
    }
  }
  makeHTMLnames <- function(x) {
    if (is.list(x)) {
      x <- lapply(x,
        function(u) {
          names(u) <- convertToHTML(names(u))
          u
        })
    }
    else {
      names(x) <- convertToHTML(names(x))
    }
    x
  }
  if (top & length(labelvar) > 0) {
      namesvarheaders <- names(labelvar)
      labelvar <- splitlines(labelvar, splitwidth, sp = "\n")
      names(labelvar) <- namesvarheaders
  }
  if (top & length(labelnode) > 0) {
    for (i in 1:length(labelnode)) {
      names(labelnode[[i]]) <- splitlines(names(labelnode[[i]]),splitwidth,sp ="\n")
    }
  }
  if (top & !HTMLtext) {
    labelnode <- makeHTMLnames(labelnode)
    title <- makeHTML(title)
    labelvar <- makeHTML(labelvar)
  }
  if (!is.data.frame(z)) {
      z <- data.frame(z)
      if (!missing(vars))
          argname <- vars
      colnames(z)[1] <- argname
      vars <- argname
  }
  if (check.is.na) {
    NEWVARS <- c()
    for (v in vars) {
      newvar <- paste0("MISSING_", v)
      m <- is.na(z[[v]])
      z[[newvar]] <- factor(m, levels = c(FALSE, TRUE),
          c(paste(v, "not missing"), paste(v, "missing")))
      NEWVARS <- c(NEWVARS, newvar)
    }
    vars <- NEWVARS
  }
  if (missing(fillnodes) & !missing(fillcolor)) {
      fillnodes <- TRUE
  }
  fillcolor <- rep(fillcolor, length = length(vars) + 1)
  if (!colornodes)
      color <- rep("black", 100)
  if (!fillnodes)
      fillcolor <- rep("white", 100)

  z_names <- names(z)

  findvars <- vars %in% z_names
  if (any(!findvars)) {
      stop("The following variables were not found in the data frame: ",
          paste(vars[!findvars], collapse = ", "))
  }

  findvars <- names(labelvar) %in% z_names
  if (any(!findvars)) {
      stop("The following variables named in labelvar were not found in the data frame: ",
          paste(vars[!findvars], collapse = ", "))
  }

  findvars <- names(prunebelow) %in% z_names
  if (any(!findvars)) {
      stop("The following variables named in prunebelow were not found in the data frame: ",
          paste(names(prunebelow)[!findvars], collapse = ", "))
  }

  findvars <- names(prune) %in% z_names
  if (any(!findvars)) {
      stop("The following variables named in prune were not found in the data frame: ",
          paste(names(prune)[!findvars], collapse = ", "))
  }

  findvars <- names(follow) %in% z_names
  if (any(!findvars)) {
      stop("The following variables named in follow were not found in the data frame: ",
          paste(names(follow)[!findvars], collapse = ", "))
  }

  findvars <- names(keep) %in% z_names
  if (any(!findvars)) {
      stop("The following variables named in keep were not found in the data frame: ",
          paste(names(keep)[!findvars], collapse = ", "))
  }

  if (top & title == "") title <- "Sample"

  if (is.null(z) || is.null(vars)) {
    #cat("Return NULL because z is NULL or vars is NULL\n")
    return(NULL)
  }
  if (nrow(z) == 0 || length(vars) == 0) {
    #cat("Return NULL because z is empty or vars has zero length\n")
    return(NULL)
  }
  TopText <- ""
  if (!is.null(nodefunc) & (!leafonly | (length(vars) == 1 &
    leafonly))) {
    if (length(vars) == 1)
        nodeargs$leaf <- TRUE
    ThisLevelText <- c()
    qqq <- z[[vars[1]]]
    qqq <- as.character(qqq)
    qqq[is.na(qqq)] <- "NA"
    categoryCounts <- table(qqq, exclude = NULL)
    CAT <- names(categoryCounts)
    for (value in CAT) {
      nodetext <- nodefunc(z[qqq == value, ], vars[1],
        value, args = nodeargs)
      nodetext <- splitlines(nodetext, width = splitwidth,
        sp = "\n")
      ThisLevelText <- c(ThisLevelText, nodetext)
    }
    if (top) {
      topnodeargs <- nodeargs
      topnodeargs$top <- TRUE
      topnodeargs$leaf <- FALSE
      nodetext <- nodefunc(z, vars[1], value = NA, args = topnodeargs)
      nodetext <- splitlines(nodetext, width = splitwidth,sp = "\n")
      TopText <- nodetext
    }
    names(ThisLevelText) <- CAT
  }
  else {
    ThisLevelText <- text[[vars[1]]]
  }
  if (!HTMLtext)
      ThisLevelText <- makeHTML(ThisLevelText)
  if (!HTMLtext)
      TopText <- makeHTML(TopText)
  fc <- flowcat(z[[vars[1]]], top = top, title = title, root = root,
    last = last, labels = labelnode[[vars[1]]], varheader = labelvar[vars[1]],
    prunefull=prune[[vars[1]]],
    HTMLtext = HTMLtext, showlevels = showlevels,
    keep=keep[[vars[1]]],
    text = ThisLevelText, TopText = TopText, digits = digits,
    splitwidth = splitwidth, showempty = showempty, topcolor = color[1],
    color = color[2], topfillcolor = fillcolor[1], fillcolor = fillcolor[2],
    vp = vp, rounded = rounded)
  CurrentVar <- vars[1]
  if (CurrentVar %in% names(follow)) {
    followlevels <- follow[[CurrentVar]]
  } else {
    followlevels <- NULL
  }
  if (CurrentVar %in% names(prunebelow)) {
    prunebelowlevels <- prunebelow[[CurrentVar]]
  } else {
    prunebelowlevels <- NULL
  }
  i <- 0
  for (varlevel in fc$levels) {
    i <- i + 1
    if (!(varlevel %in% prunebelowlevels) & (is.null(followlevels) | (varlevel %in% followlevels))) {
      if (varlevel == "NA") {
          select <- is.na(z[[CurrentVar]])
      }
      else {
          select <- which(z[[CurrentVar]] == varlevel)
      }
      if (length(select)>0 & length(vars)>=1) {
        fcChild <- vtree(z[select, , drop = FALSE],
          vars[-1], root = fc$nodenum[i], last = max(fc$nodenum),
          labelnode = labelnode, showempty = showempty,
          top = FALSE, prune=prune, prunebelow = prunebelow, labelvar = labelvar,
          nodefunc = nodefunc, nodeargs = nodeargs, digits = digits,
          showlevels = showlevels,
          keep=keep,
          follow=follow,
          text = text,
          colornodes = colornodes, color = color[-1], fillnodes = fillnodes,
          fillcolor = fillcolor[-1], splitwidth = splitwidth,
          vp = vp, rounded = rounded)
        fc <- joinflow(fc,fcChild)
      }
    }
  }
  if (length(fc$nodenum) == 0) {
      cat("Setting fc to NULL\n")
      fc <- NULL
  }
  if (top) {
      if (showlevels) {
          VARS <- vars
          if (!is.null(labelvar)) {
              for (i in 1:length(vars)) {
                if (!is.na(labelvar[vars[i]])) {
                  VARS[i] <- labelvar[vars[i]]
                }
              }
          }
          nodelevels <- "Node_L0[style=invisible]\n"
          nodelevels <- paste0(nodelevels, paste0("Node_L",
              1:length(vars), "[label=<", VARS, ">", " shape=",
              levelshape, "]", collapse = "\n"))
          nodelevels <- paste0(nodelevels, paste0("\n\nedge[style=invis];\n",
              paste0("Node_L", 0:length(vars), collapse = "->")),
              "\n")
      }
      else {
          nodelevels <- ""
      }
      showflow(fc, getscript = getscript, nodesep = nodesep,
        ranksep = ranksep, nodelevels = nodelevels, horiz = horiz,
        width=width,height=height)
  }
  else {
      fc
  }
}




around_func <- function (x, digits = 2, tooLong = 10) {
    if (is.data.frame(x)) {
        for (i in 1:ncol(x)) {
            x[[i]] <- around(x[[i]], digits = digits)
        }
        x
    }
    else if (!is.numeric(x)) {
        x
    }
    else {
        if (digits == 0) {
            result <- formatC(x, digits = digits, drop0trailing = TRUE,
                format = "f", flag = "#")
            result[nchar(result) > tooLong] <- formatC(x[nchar(result) >
                tooLong], digits = digits, drop0trailing = TRUE,
                format = "g", flag = "#")
        }
        else {
            result <- formatC(x, digits = digits, drop0trailing = FALSE,
                format = "f", flag = "#")
            result[nchar(result) > tooLong] <- formatC(x[nchar(result) >
                tooLong], digits = digits, drop0trailing = FALSE,
                format = "g", flag = "#")
        }
        result[result == "-0"] <- "0"
        result[result == "-0.0"] <- "0.0"
        result[result == "-0.00"] <- "0.00"
        result[result == "-0.000"] <- "0.000"
        result
    }
}




flowcat <- function(z,top=TRUE,title="",root=1,last=1,labels=NULL,HTMLtext=FALSE,
varheader=NULL,
prunefull=NULL,
keep=NULL,
text=NULL,TopText="",sepN="<BR/>",showempty=FALSE,digits=0,
showlevels=FALSE,
splitwidth=Inf,topcolor="black",color="blue",topfillcolor="olivedrab3",fillcolor="olivedrab2",
vp=TRUE,rounded=FALSE) {
#
# Write DOT code for a single-level {flow}chart of {cat}egories using the
# DiagrammeR framework.
#
# https://en.wikipedia.org/wiki/DOT_(graph_description_language)
#
# z         a vector of values
# top       Is this the top node of the tree?
# title     title to display if this is the top node of the tree
# root      the node number of the root of this subtree
# last      the last node number used
# labels    category labels to use at this level
# varheader variable header to show on the line preceding the label of each child node
# sepN      separator between heading and sample size
# showempty Show empty (N=0) nodes?
# digits    Number of decimal digits to use in displaying percentages
# topcolor  Color of the top node
# color     Color of the children nodes of the top node

  if (is.logical(z)) {
    z <- factor(z, c("FALSE", "TRUE"))
  }

  categoryCounts <- table(z,exclude=NULL)
  names(categoryCounts)[is.na(names(categoryCounts))] <- "NA"

  # Pre-pend the root node
  categoryCounts <- c(length(z),categoryCounts)
  names(categoryCounts)[1] <- title

  # Use npct to calculate percentages, but don't use "valid percentages"
  # since the denominator should always be the number in the parent node.
  # npctString <- npct(z,includemiss=TRUE,vp=FALSE,pcs="%")
  # If there are no missing values, don't include the NA category
  # if (sum(is.na(z))==0) npctString <- npct(z,pcs="%")

  if (vp & any(is.na(z))) {
    cc <- categoryCounts[-1]
    cc <- cc[names(cc)!="NA"]
    if (length(cc)>0) {
      npctString <- paste0(cc," (",
        around_func(100*cc/sum(cc),digits),"%)")
    } else {
      npctString <- NULL
    }
    npctString <- c(npctString,categoryCounts["NA"])
  } else {
    npctString <- paste0(categoryCounts[-1]," (",
      around_func(100*categoryCounts[-1]/length(z),digits),"%)")
  }

  npctString <- c(length(z),npctString)
  names(npctString)[1] <- title

  if (!showempty) {
    s <- categoryCounts>0
    categoryCounts <- categoryCounts[s]
    npctString <- npctString[s]
  }

  if (!is.null(prunefull)) {
    matching <- names(categoryCounts)[-1] %in% prunefull
    categoryCounts <- c(categoryCounts[1],categoryCounts[-1][!matching])
    npctString <- c(npctString[1],npctString[-1][!matching])
  }

  if (!is.null(keep)) {
    matching <- match(keep,names(categoryCounts)[-1])
    matching <- matching[!is.na(matching)]
    categoryCounts <- c(categoryCounts[1],categoryCounts[-1][matching])
    npctString <- c(npctString[1],npctString[-1][matching])
  }

  # Number of new nodes to add to the tree
  n <- length(categoryCounts)-1              # exclude the root node

  if (n>0) {
    # Number the root node and the additional nodes to be added
    nodenum <- c(root,last+(1:n))
  } else {
    nodenum <- root
  }
  nodenames <- paste0("Node_",nodenum)

  CAT <- names(categoryCounts)

  if (HTMLtext) {
    CAT[-1] <- splitlines(CAT[-1],width=splitwidth,sp="<BR/>")
  } else {
    CAT[-1] <- splitlines(CAT[-1],width=splitwidth,sp="\n")
    CAT[-1] <- convertToHTML(CAT[-1])
  }

  extraText <- rep("",length(CAT))

  # Match extra text to nodes
  if (TopText!="") extraText[1] <- paste0("<BR/> ",TopText)
  for (label in names(text)) {
    if (label %in% names(categoryCounts)) {
      m <- match(label,names(categoryCounts))
      #browser()
      if (text[names(text)==label]!="") {
        extraText[m] <- paste("<BR/>",text[names(text)==label])
      }
    }
  }

  # Relabel the nodes if labels have been specified
  for (label in labels) {
    if (label %in% names(categoryCounts)) {
      m <- match(label,names(categoryCounts))
      CAT[m] <- names(labels)[labels==label]
    }
  }

  # Relabel the nodes if a varheader has been specified
  if (!showlevels) {
    if (!is.null(varheader)) {
      if (!is.na(varheader)) {
        CAT[-1] <- paste0(varheader,"<BR/>",CAT[-1])
      }
    }
  }

  # Write DOT code for the edges
  edges <- paste(paste0(nodenames[1],"->",nodenames[-1]),collapse=" ")

  if (rounded) {
    styleString <- " style=\"rounded,filled\""
  } else {
    styleString <- " style=filled"
  }

  # Write DOT code for assigning labels (using the DiagrammeR framework)
  labelassign <- c()
  if (top) {
    labelassign <- paste(paste0(
      nodenames[1],"[label=<",CAT[1],sepN,npctString[1],extraText[1],"> color=",topcolor,styleString," fillcolor=<",topfillcolor,">]"),collapse=" ")
    labelassign <- paste0(labelassign," ",paste(paste0(
      nodenames[-1],"[label=<",CAT[-1],sepN,npctString[-1],extraText[-1],"> color=",color,styleString," fillcolor=<",fillcolor,">]"),collapse=" "))
  } else {
    labelassign <- paste(paste0(
      nodenames[-1],"[label=<",CAT[-1],sepN,npctString[-1],extraText[-1],"> color=",color,styleString," fillcolor=<",fillcolor,">]"),collapse=" ")
  }

  return(list(
    levels=names(categoryCounts)[-1],
    nodenum=nodenum[-1],
    edges=edges,labelassign=labelassign,
    lastnode=nodenum[length(nodenum)]))
}




joinflow <- function(...) {
#
# {join} information (from the flowcat function) about two or more {flow}charts
#

  edges <- labelassign <- labelshow <- nodenum <- c()
  flows <- list(...)
  for (i in 1:length(flows)) {
    if (!is.null(flows[[i]])) {
      nodenum <- c(nodenum,flows[[i]]$nodenum)
      if (length(edges)==0) {
        edges <- flows[[i]]$edges
      } else {
        edges <- paste0(edges,"\n",flows[[i]]$edges)
      }
      if (length(labelassign)==0) {
        labelassign <- flows[[i]]$labelassign
      } else {
        labelassign <- paste0(labelassign,"\n",flows[[i]]$labelassign)
      }
    }
  }
  return(list(nodenum=nodenum,edges=edges,labelassign=labelassign,labelshow=labelshow))
}



splitlines <- function (x, width = 10, sp = "\n", at = c(" ", "-", "+"), same = FALSE) {

# NOTE: I removed forward slash from the default at argument,
# because it caused a problem with HTML where / is important.
# e.g. <BR/>

  n <- nchar(x)
  nsp <- nchar(sp)
  result <- rep("", length(x))
  splits <- rep(0, length(x))
  if (is.null(x) || (length(x)==0)) return(NULL)
  for (i in 1:length(x)) {
      count <- 0
      start <- 1
      for (j in 1:(n[i])) {
        count <- count + 1
        char <- substring(x[i], j, j)
        if (char %in% sp) {
          count <- 0
        } else {
          if (char %in% at) {
            if (count > width) {
              if (char == " ") {
                end <- j - 1
              }
              else {
                end <- j
              }
              result[i] <- paste0(result[i], substring(x[i],
                start, end), sp)
              start <- j + 1
              count <- 0
              splits[i] <- splits[i] + 1
            }
          }
        }
      }
      if (start <= n[i])
          result[i] <- paste0(result[i], substring(x[i], start,
              n[i]))
  }
  if (same) {
      maxsplits <- max(splits)
      for (i in 1:length(x)) {
          while (splits[i] < maxsplits) {
              result[i] <- paste0(result[i], sp)
              splits[i] <- splits[i] + 1
          }
      }
  }
  result
}



#' @title Node function: IDs
#' @author Nick J. Barrowman
#'
#' @param u           Data frame 
#' @param varname     The name of the variable in this node
#' @param value       The value of the variable in this node.
#' @param args        A list of arguments
#' 
#' @export

nodeids <- function(u,varname,value,args) {
  if (length(grep("not missing",value))>0) {
    ""
  } else {
    splitlines(paste0("\nIDs: ",paste0(u[[args$var]],collapse=", ")),10,sp="\n")
  }
}



around <- function (x, digits = 2, tooLong = 10)
{
    if (is.data.frame(x)) {
        for (i in 1:ncol(x)) {
            x[[i]] <- around(x[[i]], digits = digits)
        }
        x
    }
    else if (!is.numeric(x)) {
        x
    }
    else {
        if (digits == 0) {
            result <- formatC(x, digits = digits, drop0trailing = TRUE,
                format = "f", flag = "#")
            result[nchar(result) > tooLong] <- formatC(x[nchar(result) >
                tooLong], digits = digits, drop0trailing = TRUE,
                format = "g", flag = "#")
        }
        else {
            result <- formatC(x, digits = digits, drop0trailing = FALSE,
                format = "f", flag = "#")
            result[nchar(result) > tooLong] <- formatC(x[nchar(result) >
                tooLong], digits = digits, drop0trailing = FALSE,
                format = "g", flag = "#")
        }
        result[result == "-0"] <- "0"
        result[result == "-0.0"] <- "0.0"
        result[result == "-0.00"] <- "0.00"
        result[result == "-0.000"] <- "0.000"
        result
    }
}


#' @title Node function: mean and SD
#' @author Nick J. Barrowman
#'
#' @param u           Data frame 
#' @param varname     The name of the variable in this node
#' @param value       The value of the variable in this node.
#' @param args        A list of arguments
#' 
#' @export

nodeMeanSD <- function(u,varname,value,args) {
  if (is.null(args$digits)) args$digits <- 1   # default value

  paste0("\n",args$var,": mean (SD)\n",
    around(mean(u[[args$var]],na.rm=TRUE),args$digits),
    " (",around(sd(u[[args$var]],na.rm=TRUE),args$digits),")",
    " mv=",sum(is.na(u[[args$var]])))
}



convertToHTML <- function(x) {
  # Convert various text elements to their HTML entities.
  # Note that order matters here!

  x <- gsub("&","&amp;",x)
  x <- gsub("<=","&le;",x)
  x <- gsub(">=","&ge;",x)
  x <- gsub("<","&lt;",x)
  x <- gsub(">","&gt;",x)

  # Also convert character sequences for line breaks.

  x <- gsub("\n\\*l","<BR ALIGN='LEFT'/>",x)  
  
  x <- gsub("\\\\n","<BR/>",x)
  x <- gsub("\n","<BR/>",x)


  # Markdown-style formatting

  x <- gsub("\\*\\*(.+?)\\*\\*","<B>\\1</B>",x)
  x <- gsub("\\*(.+?)\\*","<I>\\1</I>",x)
  x <- gsub("_(.+?)_","<I>\\1</I>",x)

  # Special character sequence for colour!

  x <- gsub("%%([^ ]+?) (.+?)%%","<FONT COLOR='\\1'>\\2</FONT>",x)

  # Markdown-style formatting for superscript and subscript

  x <- gsub("\\^(.+?)\\^","<FONT POINT-SIZE='10'><SUP>\\1</SUP></FONT>",x)
  x <- gsub("~(.+?)~","<FONT POINT-SIZE='10'><SUB>\\1</SUB></FONT>",x)

  x
}




#' @importFrom stats median quantile sd

nn <- function (u, varname, value, args) {

  nAndpct <- function(w,vp=TRUE) {
    if (vp) {
      num <- sum(w==1,na.rm=TRUE)
      den <- length(w) - sum(is.na(w))
    } else {
      num <- sum(w==1,na.rm=TRUE)
      den <- length(w)     
    }
    npctString <- paste0(num," (",
      around_func(100*num/den,digits),"%)")    
    if (any(is.na(w))) 
      npctString <- paste0(npctString," mv=",sum(is.na(w)))
    npctString  
  }

  qntl <- function(x,...) {
    if (any(is.na(x))) {
      NA
    } else {
      stats::quantile(x,...)
    }
  }
  
  if (is.null(args$digits))
    args$digits <- 1
  if (is.null(args$na.rm))
    args$na.rm <- TRUE

  if (is.null(args$top)) {
    args$top <- FALSE
  }

  if (is.null(args$leaf)) {
    args$leaf <- FALSE
  }

  RESULT <- ""
  for (i in 1:length(args$var)) {
  
    y <- u[[args$var[i]]]
    format <- args$format[i]
    digits <- args$digits
    na.rm <- args$na.rm

    missingNum <- sum(is.na(y))
    if (na.rm) {
      x <- y[!is.na(y)]
      if (is.null(x)) x <- NA
    } else {
      x <- y
    }

    result <- format

    ShowNodeText <- TRUE

    if (!args$leaf) {
      if (length(grep("%leafonly%",result))>0) {
        ShowNodeText <- FALSE
      }
    }

    if (args$top) {
      if (length(grep("%noroot%",result))>0) {
        ShowNodeText <- FALSE
      }
    }

    # Format %list% output
    tabval <- table(y,exclude=NULL)
    countval <- paste0(" (n=",tabval,")")
    countval[tabval==1] <- ""
    listOutput <- paste0(paste0(names(tabval),countval),collapse=", ")

    if (ShowNodeText) {
      result <- gsub("%noroot%","",result)
      result <- gsub("%leafonly%","",result)
      result <- gsub("%v%",args$var[i],result)
      result <- gsub("%list%",listOutput,result)
      result <- gsub("%mv%",paste0(missingNum),result)
      if (is.numeric(x) | is.logical(x)) {
        # Note that y is used in the call to nAndpct
        # so that missing values can be handled as desired
        result <- gsub("%npct%",around(nAndpct(y),digits=digits),result)
        result <- gsub("%mean%", around(mean(x), digits = digits),
            result)
        result <- gsub("%median%", around(stats::median(x), digits = digits),
            result)
        result <- gsub("%SD%", around(stats::sd(x), digits = digits), result)
        result <- gsub("%min%", around(min(x), digits = digits), result)
        result <- gsub("%max%", around(max(x), digits = digits), result)
        result <- gsub("%IQR%",
          paste0(
            around(qntl(x,0.25), digits = digits),", ",
            around(qntl(x,0.75), digits = digits)),
          result)
        repeat {
            if (length(grep("%(p)([0-9]+)%", result)) == 0)
                break
            quant <- sub("(.*)%(p)([0-9]+)%(.*)", "\\3", result)
            if (quant != "") {
                qq <- around(qntl(x, as.numeric(quant)/100),
                    digits = digits)
                result <- sub(paste0("%p", quant,"%"), qq, result)
            }
        }
      }
    } else {
      result <- ""
    }
    RESULT <- paste0(RESULT,result)
  }
  RESULT
}



showflow <- function(flow,getscript=FALSE,nodesep=0.5,ranksep=0.5,nodelevels="",horiz=FALSE,width=NULL,height=NULL) {
#
# {show} a {flow}chart produced by flowcat or by hier.
#
# showscript Only show the script generated rather than displaying the flowchart? Useful for debugging.
# nodesep    The nodesep graph attribute.
# ranksep    The ranksep graph attribute.
#
  script <- paste0(
    "digraph boxes_and_circles {\n",
    "graph [layout = dot, compound=true, nodesep=",nodesep,", ranksep=",ranksep,", fontsize=12]\n",
    "node [fontname = Helvetica, fontcolor = black,shape = rectangle, color = black,margin =  0.2]\n")

  if (horiz) {
    script <- paste0(script,"rankdir=LR;\n")
  }

  script <- paste0(script, nodelevels)

  script <- paste0(script,"\nedge[style=solid];\n",
    flow$edges,"\n\n",flow$labelassign,sep="\n")

  script <- paste0(script,"\n}\n")
  if (getscript) { return(script) }
  flowchart <- DiagrammeR::grViz(script,width=width,height=height)
  flowchart
}



#' @title grVizToPNG
#'
#' @author Nick Barrowman
#'
#' @description
#'  \code{grVizToPNG} Export a grViz object into a PNG file.
#'
#' @param g      an object produced by the grViz function from the DiagrammmeR package
#' @param width  the pixel width of the bitmap
#' @param folder path to folder where the PNG file should stored
#'
#' @details
#'   First the grViz object is exported to an SVG file (using \code{DiagrammeRsvg::export_svg}).
#'   Then the SVG file is converted to a bitmap (using \code{rsvg::rsvg}).
#'   Then the bitmap is exported as a PNG file (using \code{png::writePNG}).
#'   Note that the SVG file and the PNG file will be named using the name of the \code{g} parameter
#'
#' @note
#'   In addition to the DiagrammmeR package, the following packages are used: \code{DiagrammeRsvg}, \code{rsvg}
#'
#' @export
#'


grVizToPNG <- function (g, width = 3000, folder = ".") {
  argname <- sapply(as.list(substitute({g})[-1]), deparse)
  message <- utils::capture.output(svg <- DiagrammeRsvg::export_svg(g))
  result <- rsvg::rsvg_png(charToRaw(svg),paste0(folder,"/",argname, ".png"), width = width)
  invisible(NULL)
}


