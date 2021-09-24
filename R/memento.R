#' @title Remembers and outputs the result of a slow function
#'
#' @description Memento mori; you do not have time to lose on unnecessary
#' calculations. This function remembers the output of a slow function, for
#' given arguments and, if asked politely, a given random seed. If they match
#' the previous arguments and seeds, the output is provided without delay. The
#' trade-off is to assign a folder to store the data.
#'
#' @param what a (slow) function
#' @param args a list of the the arguments to give to the function. If they
#' differ from saved values, the function will run again.
#' @param name the name of the folder where to store the info. THIS NEEDS TO BE
#' DIFFERENT FOR EACH IMPLEMENTATION OF THE FUNCTION IN IDENTICAL DIRECTORIES.
#' @param dir the directory. You can set it as the working directory via
#' \code{\link{getwd}}.
#' @param subdir a name for a subdirectory (useful when the function is used
#' several time in a script)
#' @param check.seed if TRUE, the value of the random seed in effect will be
#' taken into account; if it changes, the function will run again.
#' @param speak whether to signify when the (slow) function is running
#'
#' @return the output of the function
#'
#' @examples
#' \donttest{tf <- tempdir()
#'
#' testfun <- function(a = 1, time = 3){
#'
#'   Sys.sleep(time)
#'
#'   return(a  + 0.1 * abs(rnorm(1)))
#'
#' }
#'
#' # First time running; the function takes some time, memento needs the
#' # output to be generated, and will remember for later.
#' set.seed(43)
#' memento(testfun,  args = list(a = 7), name = "T1", dir = tf)
#'
#' set.seed(43)
#' testfun(7, time = 0)
#'
#' # Second time running: memento directly outputs the remembered results.
#' # In this case, the seed is ignored, so the result is different from what
#' # would be obtained with a different seed
#' set.seed(45)
#' memento(testfun,  args = list(a = 7), name = "T1", dir = tf)
#'
#' set.seed(45)
#' testfun(7, time = 0)
#'
#' # First time running while taking into account the random seed;
#' # the function takes some time to generate the result
#' set.seed(42)
#' memento(testfun,  args = list(a = 7), name = "T1", dir = tf, check.seed = TRUE)
#'
#' # Second time running with an identical random seed;
#' # memento directly outputs the results
#' set.seed(42)
#' memento(testfun,  args = list(a = 7), name = "T1", dir = tf, check.seed = TRUE)
#'
#' # The seed is changed: the result is computed anew
#' set.seed(47)
#' memento(testfun,  args = list(a = 7), name = "T1", dir = tf, check.seed = TRUE)
#' }
#'
#' @export

memento <- function(what, args, name, dir, subdir = "memento",
                    check.seed = F, speak = T)
{

  # conditions ----

  if(!inherits(what, "function")){
    stop("The 'what' argument should refer to a function")
  }

  if(!inherits(args, "list")){
    stop("The 'args' argument should refer to a list of arguments")
  }

  if(!inherits(name, "character")){
    stop("The 'name' argument should refer to a string of characters")
  }

  if(!inherits(dir, "character")){
    stop("The 'dir' argument should refer to a string of characters")
  }

  if(!(inherits(subdir, "character") | is.null(subdir))){
    stop("The 'subdir' argument should refer to a string of characters or be NULL")
  }

  # ----

  folderdir  <- paste(dir, subdir, name, sep="/")

  run.function <- function(gloVar.what.save = what, gloVar.args.save = args){

    what.run <- gloVar.what.save

    gloVar.what.save <- list(f = formals(gloVar.what.save),
                             b = toString(body(gloVar.what.save)))

    save(gloVar.args.save, file = paste(folderdir, "gloVar.args.save", sep="/"))
    save(gloVar.what.save, file = paste(folderdir, "gloVar.what.save", sep="/"))

    if(isTRUE(check.seed)) {

      gloVar.seed.save <- .Random.seed

      save(gloVar.seed.save, file = paste(folderdir, "gloVar.seed.save", sep="/"))

    }

    if(isTRUE(speak)) print("Function running")

    gloVar.out <- do.call(what.run, gloVar.args.save)

    save(gloVar.out, file = paste(folderdir, "gloVar.out", sep="/"))

    return(gloVar.out)

  }

  if(file.exists(folderdir)) {

    lf <- list.files(folderdir)

    if(isTRUE(check.seed)){

      if(any(!(c("gloVar.args.save", "gloVar.what.save",
                 "gloVar.seed.save", "gloVar.out") %in% lf))){

        out.now <- run.function()

        return(out.now)
        # print(out.now)

      } else {

        what.now <- what

        what.now.compare <- list(f = formals(what.now),
                                 b = toString(body(what.now)))

        args.now <- args
        seed.now <- .Random.seed

        load(paste(folderdir, "gloVar.args.save", sep="/"))
        load(paste(folderdir, "gloVar.what.save", sep="/"))
        load(paste(folderdir, "gloVar.seed.save", sep="/"))

        if(!(identical(what.now.compare, gloVar.what.save) &
             identical(args.now, gloVar.args.save) &
             identical(seed.now, gloVar.seed.save))){

          out.now <- run.function(what.now, args.now)

          # return(out.now)
          print(out.now)

        } else {

          load(paste(folderdir, "gloVar.out", sep="/"))

          return(gloVar.out)
          # print(gloVar.out)

        }

      }

    } else {


      if(any(!(c("gloVar.args.save", "gloVar.what.save", "gloVar.out") %in% lf))){

        out.now <- run.function()

        return(out.now)
        # print(out.now)

      } else {

        what.now <- what

        what.now.compare <- list(f = formals(what.now),
                                 b = toString(body(what.now)))

        args.now <- args

        load(paste(folderdir, "gloVar.args.save", sep="/"))
        load(paste(folderdir, "gloVar.what.save", sep="/"))

        if(!(identical(what.now.compare, gloVar.what.save) &
             identical(args.now, gloVar.args.save))){

          out.now <- run.function(what.now, args.now)

          # return(out.now)
          print(out.now)

        } else {

          load(paste(folderdir, "gloVar.out", sep="/"))

          return(gloVar.out)
          # print(gloVar.out)

        }

      }

    }

  } else {

    if(!is.null(subdir)){
      if(!file.exists(paste(dir, subdir, sep="/"))) {
        dir.create(paste(dir, subdir, sep="/"))
      }
    }

    dir.create(folderdir)

    out.now <- run.function()

    return(out.now)
    # print(out.now)

  }

}
