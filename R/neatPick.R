#' @title Interactive user modification of the arguments of a repeated function
#'
#' @description This opens a shiny app that will allow to manipulate the
#' arguments of a function interactively, with different conditions that the
#' user can provide a priori and modify at will
#'
#' @param fun the function to be applied n times.
#' @param n number of runs.
#' @param args the arguments to be supplied to fun. Should be a list of
#' each argument to be supplied to fun, having n elements stored
#' indiscriminately in list or in vector form.
#' @param class.args the class of the arguments, in a list. This is
#' useful when the starting arguments are NA
#' @param pick which arguments to be able to adapt interactively
#' @param fix which arguments that cannot be chosen interactively (if
#' pick is NA)
#' @param buttonswidth the width of the buttons panel (integer from 1 to
#' 12)
#' @param text which information to send to the text panel. The default
#' is the output of the current element (ni); "output". Can be the whole dataset
#'  of arguments; "all". Otherwise the panel does not show.
#' @param textwidth the width of the text panel (integer from 1 to 12)
#' @param plotwidth the width of the plot panel (arbitrary units)
#' @param plotheight the height of the plot panel (arbitrary units)
#' @param args.only whether to be only allowed to download and return
#' the arguments (this simplifies things and makes the workflow more efficient)
#' @param width,height,name,dir,gfile,openfile,folder,gfun,ext,gargs
#' arguments to be supplied to neatPicked, the equivalent
#' of neatPick without interactivity: it runs the function for each ni and
#' saves the output (normal and graphical). In neatPick this happens when the
#' button 'Run and Download Output' is clicked. See ?neatPicked function help
#' page for details.
#' @param pargs the arguments to transmit to par(), in neatPick and
#' neatPicked
#'
#' @details This is a complicated function. A few basics:
#'
#' neatPick works using the formals() function. It evaluates the arguments
#' and their default values of any function that you provide without
#' parentheses, like this for instance: \code{formals(multigons)}.
#'
#' neatPick is capable of providing interaction with arguments of class integer
#' or numeric (e.g. 10, or 13,58745),  character (e.g. "BlipBlapBLoup") and
#' logical (T or F), as long as for each iteration (n) the length of the
#' argument is one (you cannot use arguments like xlim = c(0,1), however you can
#' use xmin = 0 and xmax = 1 for instance). But you can provide a different
#' value for each iteration n (if n = 3, you can provide col = c("red", "blue",
#' "green") in the args list of arguments)
#'
#' You can chose which arguments are interactive or not using the 'pick' and
#' 'fix' arguments.
#'
#' To return the arguments or the output, you have to click on 'End & Return
#' Arguments' or 'End & Return Output', respectively.
#'
#' You can also save the obtained output and arguments via the download buttons:
#' you geta .RData file were the arguments are in the object saved.arguments and
#' the output is in the saved.output object. The arguments can also be found at
#' saved.output$args. The arguments can be provided to the args argument of the
#' same neatPick function to rework the changes you made.
#'
#' @examples
#' \dontrun{
#' # You create a simple function. The one below creates sinusoidal waves between
#' # x0 = 0 and x1 = 1. You want to personalise the amplitude (delta), the y
#' # offset (pos, see ?sinpoint for more details), the phase (phase, expressed
#' # in multiples of pi), the number of waves between x0 and x1, and the number
#' # of intervals between each discrete point (nint).
#' # So you set all these as arguments of the function. This function can also
#' # have a graphical output of one plot (which can be subdivided if necessary
#' # using par(mfrow)). And the function can return output.
#'
#' fun <- function(delta = 1, pos = 1, phase = 1.5, nwave = 1, nint = 50)
#' {
#'
#'   res <- sinpoint(1, 0, delta = delta, pos = pos, phase = phase,
#'                   nwave = nwave, nint = nint)
#'
#'   plot(res$x,res$y)
#'
#'   return(res)
#'
#' }
#'
#' # Once this simple function is coded, it can be integrated to neatPick(). The
#' # argument n defines to number of different realisations of the function.
#'
#' # WHEN YOU ARE HAPPY WITH THE OUTPUTS, click on 'END & RETURN ARGUMENTS'
#'
#' a <- neatPick(fun, n = 10, args.only = TRUE)
#'
#' # If you have clicked right (on the 'END & RETURN ARGUMENTS' button), the
#' # arguments will be returned and stored in a;
#'
#' a
#'
#' # These arguments can then serve for a more efficient function:
#'
#' seg <- sinpoint(1, 0, delta = a$delta, pos = a$pos, phase = a$phase,
#'                 nwave = a$nwave, nint = a$nint)
#'
#' # Basically neatPick applies a for loop to fun, but if you work on a large
#' # dataset, you can also create a function that can handle the arguments more
#' # efficiently. This is what sinpoint does here
#'
#' # Now you can see the results imported in R and do whatever you want with:
#'
#' plot(seg$x, seg$y, type = "n")
#'
#' multilines(seg$i, seg$x, seg$y)
#'
#' # You can even rework your initial changes:
#'
#' b <- neatPick(fun, n = 10, args.only = TRUE, args = a)}
#'
#' @export
#' @import shiny

neatPick <- function(fun, n, args = list(), class.args = list(),
                     pick = NA, fix = NA, buttonswidth = 2,
                     text = "output", textwidth = 4,
                     plotwidth = 800, plotheight = 600, args.only = F,
                     width = 10, height = 10, name = "fig",
                     dir = tempdir(), gfile = "onePDF", openfile = TRUE,
                     folder = "Rfig", gfun = "jpeg", ext = ".jpeg",
                     gargs = list(units = "in", res = 300),
                     pargs = list(ps = 12, cex = 1.5))
{

  # Preliminary manipulations

  gargs <- merge_list(gargs, list(units = "in", res = 300))

  pargs <- merge_list(pargs, list(ps = 12, cex = 1.5))

  # Define function arguments ----

  def.args  <- as.list(formals(fun))

  if(exists("...", where = def.args)) def.args$"..." <- NULL

  # Choose between function default arguments or adaptable arguments ----

  true.args  <- merge_list(args,def.args)
  true.names <- names(true.args)

  # Choose arguments to fix and to be able to pick ----

  if(!(length(pick) == 1 & is.na(pick[[1]][1]))){
    adp.args <- true.args[pick]
    fix.args <- true.args[setdiff(true.names,pick)]
  } else if (!(length(fix) == 1 & is.na(fix[[1]][1]))) {
    fix.args <- true.args[fix]
    adp.args <- true.args[setdiff(true.names,fix)]
  } else {
    adp.args <- true.args
    fix.args <- list()
  }

  # Deal with the number of elements to treat ----

  adp.names <- names(adp.args)
  adp.class <- lapply(adp.args, class)

  # Identify and treat sublists in adaptable arguments

  if(any(adp.class == "list")){

    adp.l <- unname(which(adp.class == "list"))

    for(i in adp.l){

      li  <- adp.args[[i]]
      lli <- lapply(li, length)
      cli <- lapply(li, class)

      if(all(lli == 1) & !any(cli == "list")) {

        adp.args[[adp.names[i]]] <- unlist(li)

      } else {

        stop(paste("Adaptable argument ", adp.names[i],
                   " should be in list or vector form, with each element",
                   " having a length of 1", sep = ""))

      }
    }
  }

  adp.class <- lapply(adp.args, class)

  adp.class <- merge_list(class.args, adp.class)[adp.names]

  adp.len   <- sapply(adp.args, length, simplify = TRUE, USE.NAMES = FALSE)
  adp.bad   <- adp.names[which(!(adp.len %in% c(1,n)))]

  if(length(adp.bad) != 0) warning(paste("Adaptable argument(s) ",
                                         paste(adp.bad, collapse = ", "),
                                         " should have",
                                         " 1 or n (", n,") elements",
                                         sep = ""))

  # Verify that each fixed argument is of length one or n

  fix.names <- names(fix.args)
  fix.len   <- sapply(fix.args, length, simplify = TRUE, USE.NAMES = FALSE)
  fix.bad   <- fix.names[which(!(fix.len %in% c(1,n)))]

  if(length(fix.bad) != 0) stop(paste("Fixed argument(s) ",
                                      paste(fix.bad, collapse = ", "),
                                      " should have",
                                      " 1 or n (", n,") elements",
                                      sep = ""))

  # Maybe another kind of error message should be send to the UI. Will see...

  if(n != 1){

    single   <- adp.names[which((adp.len %in% c(1)))]
    singlist <- adp.args[single]
    nlist    <- adp.args[setdiff(adp.names,single)]

    adp.args <- c(lapply(singlist, rep, n), nlist)[adp.names]

    single.fix   <- fix.names[which((fix.len %in% c(1)))]
    singlist.fix <- fix.args[single.fix]
    nlist.fix    <- fix.args[setdiff(fix.names,single.fix)]

    fix.args <- c(lapply(singlist.fix, rep, n), nlist.fix)[fix.names]
  }



  # UI part ----

  controlUI <- list()

  controlUI[[""]] <- h3("Arguments")

  # if(n > 1){
  controlUI[[""]] <- numericInput(inputId = "neatPick_element",
                                  label = paste("ni (1:", n, ")",
                                                sep = ""),
                                  value = 1, min = 1, max = n, step = 1)
  # }

  # Here is the sportive part: controlling the apparition of interactive
  # devices based on the arguments of the initial function

  for(i in seq_along(adp.args))
  {
    arg   <- adp.args[i]
    clarg <- adp.class[i]

    if(clarg == "name"){

      controlUI[[""]] <- p(paste("No value for the following parameter: ",
                                 names(arg),
                                 sep = ""),
                           style="color:red;")

    } else if(clarg == "logical"){

      controlUI[[""]] <- selectInput(inputId = names(arg),
                                     label = names(arg),
                                     choices = c(TRUE,FALSE),
                                     selected = arg[[1]][1])

    } else if (clarg == "numeric") {

      controlUI[[""]] <- numericInput(inputId = names(arg),
                                      label = names(arg),
                                      value = arg[[1]][1])

    } else if (clarg == "character") {

      controlUI[[""]] <- textInput(inputId = names(arg),
                                   label = names(arg),
                                   value = arg[[1]][1])

    } else {
      controlUI[[""]] <- p(paste("Invalid class (",
                                 clarg,
                                 ") for the following parameter: ",
                                 names(arg),
                                 sep = ""),
                           style="color:red;")
    }

  }

  # And here ends the sportive part

  controlUI[[""]] <- downloadButton('downloadArgs', 'Download Arguments',
                                    style ="width: 100%;")

  if(!args.only){

    controlUI[[""]] <- p(" ")

    controlUI[[""]] <- downloadButton('downloadOutput', 'Run & Download Output',
                                      style ="width: 100%;")

  }

  controlUI[[""]] <- p(" ")

  controlUI[[""]] <- actionButton('endArgs', 'End & Return Arguments',
                                  style ="width: 100%;")

  if(!args.only){

    controlUI[[""]] <- p(" ")

    controlUI[[""]] <- actionButton('endOutput', 'End & Return Output',
                                    style ="width: 100%;")

  }

  controlUI[["width"]] <- buttonswidth

  subUI  <- do.call(sidebarPanel,controlUI)
  plotUI <- plotOutput("plot1")

  if(text == "dataset"){

    helpUI <- sidebarPanel(h3("All Arguments"),
                           verbatimTextOutput("helptext",placeholder = T),
                           width = textwidth)

    UI     <- fluidPage(subUI, helpUI, plotUI)

  } else if(text == "output"){

    helpUI <- sidebarPanel(h3("Current Output"),
                           verbatimTextOutput("helptext",placeholder = T),
                           width = textwidth)

    UI     <- fluidPage(subUI, helpUI, plotUI)

  } else {

    UI     <- fluidPage(subUI, plotUI)

  }



  # Server Part; IT IS HERE THAT MAGIC HAPPENS BITCHES ----

  server <- function(input, output, clientData, session) {

    # Starting values ----

    app.args <- reactiveValues()

    app.args$dataset <- adp.args
    app.args$current <- lapply(adp.args,"[",1)

    currentoutput <- reactiveValues()

    # Defining the reaction to the argument buttons only (with eval, sorry) ----

    for(i in adp.names)
    {
      if(i == adp.names[1]) {
        accu <- paste("input$", i, sep = "")
      } else {
        accu <- paste(accu, paste("input$", i, sep = ""), sep = " \n ")
      }
    }

    accu <- paste("gloVar.adapt.buttons <- quote({\n", accu, "\n})", sep = "")

    eval(parse(text = accu))

    # Observe for changes in elements ----

    observeEvent(input$neatPick_element, {

      if(class(input$neatPick_element) == "numeric" |
         class(input$neatPick_element) == "integer"){
        if(input$neatPick_element >= 1 & input$neatPick_element <= n){

          current <- lapply(app.args$dataset,"[",
                            input$neatPick_element)

          for(i in adp.names)
          {
            if(adp.class[i] == "numeric"){

              updateNumericInput(session, i,
                                 value = unlist(current[i], use.names = FALSE))

            } else if(adp.class[i] == "character") {

              updateTextInput(session, i,
                              value = unlist(current[i], use.names = FALSE))

            } else if(adp.class[i] == "logical") {

              updateSelectInput(session, i,
                                selected = unlist(current[i], use.names = FALSE))

            }
          }
        }
      }
    })

    # Observe for changes in arguments ----

    observeEvent(gloVar.adapt.buttons, {

      isolate({

        if(class(input$neatPick_element) == "numeric" |
           class(input$neatPick_element) == "integer"){
          if(input$neatPick_element >= 1 &
             input$neatPick_element <= n){

            all <- reactiveValuesToList(app.args, all.names = TRUE)

            add <- reactiveValuesToList(input, all.names = TRUE)
            add <- add[adp.names]

            accu <- list()
            aseq <- seq_along(add)

            for(i in aseq){
              el        <- add[i]
              names(el) <- NULL
              class(el) <- adp.class[i]
              incre     <- unlist(all$dataset[adp.names[i]], use.names = FALSE)

              incre[input$neatPick_element] <- el
              accu[[adp.names[i]]] <- incre
            }

            app.args$dataset <- accu

            app.args$current <- lapply(accu,"[",
                                       input$neatPick_element)

          }
        }
      })
    }, event.quoted = TRUE)

    # Plot output ----

    output$plot1 <- renderPlot({

      all <- reactiveValuesToList(app.args, all.names = TRUE)

      par(pargs)

      subfix <- lapply(fix.args,"[",isolate({input$neatPick_element}))
      subfix <- lapply(subfix,unlist)

      currentoutput$a <- do.call(fun, c(all$current, subfix))

    },height = plotheight, width = plotwidth)

    # Text output for assistance ----

    if(text == "dataset"){

      output$helptext <- renderPrint({

        all <- reactiveValuesToList(app.args, all.names = TRUE)

        print(all$dataset)

      })

    } else if(text == "output"){

      output$helptext <- renderPrint({

        all <- reactiveValuesToList(currentoutput, all.names = TRUE)

        print(all$a)

      })

    }

    # Download arguments

    output$downloadArgs <- downloadHandler(

      filename = function() {

        paste(getwd(),"/Arguments", ".RData", sep = "")
      },

      content = function(file) {

        all <- reactiveValuesToList(app.args, all.names = TRUE)

        saved.arguments <- c(all$dataset, fix.args)

        save(saved.arguments, file = file)

      }
    )

    observeEvent(input$endArgs, {

      all <- reactiveValuesToList(app.args, all.names = TRUE)

      saved.arguments <- c(all$dataset, fix.args)

      stopApp(saved.arguments)

    })

    # Run and download output

    if(!args.only){

      output$downloadOutput <- downloadHandler(

        filename = function() {

          paste(getwd(),"/Output", ".RData", sep = "")
        },

        content = function(file) {

          all <- reactiveValuesToList(app.args, all.names = TRUE)

          saved.output <- neatPicked(fun, n, args = c(all$dataset, fix.args),
                                     width = width, height = width,
                                     output = "all", dir = dir,
                                     gfile = gfile, openfile = openfile,
                                     folder = folder, gfun = gfun, ext = ext,
                                     gargs = gargs, pargs = pargs)

          save(saved.output, file = file)

        }
      )

      observeEvent(input$endOutput, {

        all <- reactiveValuesToList(app.args, all.names = TRUE)

        saved.output <- neatPicked(fun, n, args = c(all$dataset, fix.args),
                                   width = width, height = width,
                                   output = "all", dir = dir,
                                   gfile = gfile, openfile = openfile,
                                   folder = folder, gfun = gfun, ext = ext,
                                   gargs = gargs, pargs = pargs)

        stopApp(saved.output)

      })

    }



    # ----

  }

  # Launch app; END OF MAGIC ----

  runApp(shinyApp(ui = UI, server = server), launch.browser = T)

}
