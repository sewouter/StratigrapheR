#' Creates a new folder where wanted if it does not exist yet
#'
#' @param dir directory containing the folder
#' @param name name of the folder
#' @return the directory of the folder itself
#' @examples
#' \donttest{folder(tempdir(),"test")}
#'
#' @export

folder<- function(dir,name)

{
  folderdir  <- paste(dir,name,sep="/")
  if (file.exists(folderdir)) {} else {dir.create(folderdir)}
  return(folderdir)
}
