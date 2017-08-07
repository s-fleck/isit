#' Check for Free Phyiscal Memory on Windows and Linux Machines
#'
#' @param x Minimum amount of allowed free physical memory
#' @param unit any of `b`, `kb`, `mb`, or, `gb`. The JDEC standard is used for
#'   unit conversion (so 1 kB = 1024 B). The default is `kb`, as this is what
#'   the operating system usually reports.
#'
#' @return `is_ram_available()` returns `TRUE` if at least `x` system memory 
#' is available, `FALSE` otherwise.
#' @seealso
#'   \url{https://stackoverflow.com/questions/27788968/how-would-one-check-the-system-memory-available-using-r-on-a-windows-machine}
#'   
#'   \url{https://stackoverflow.com/questions/36372397/r-how-to-get-amount-of-memory-available-to-r-on-linux}
#'   
#'   \url{https://superuser.com/questions/980820/what-is-the-difference-between-memfree-and-memavailable-in-proc-meminfo}
#' @export
#'
#' @examples
#' 
#' is_ram_available(2, "gb")
#' 
is_ram_available <- function(x, unit = "kb"){
  gc()
  x <= get_available_ram(unit = unit)
}




#' @return `get_available_ram()` returns a the available system memory  as a 
#'   `numeric` sclar. Be aware that this is not necessarily the memory 
#'   available to R.
#' @rdname is_ram_available
#' @export
#'
get_available_ram <- function(unit = "kb"){
  unit <- tolower(unit)
  os   <- Sys.info()[["sysname"]]

  if (os == "Windows") {
    get_available_ram_on_windows(unit)
  } else if (os == "Linux") {
    get_available_ram_on_linux(unit = unit)
  } else {
    stop("Only supported OS are Windows and Linux")
  }
}




get_available_ram_on_windows <- function(unit){
  x <- system2("wmic", args =  "OS get FreePhysicalMemory /Value", stdout = TRUE)
  x <- x[grepl("FreePhysicalMemory", x)]
  x <- gsub("FreePhysicalMemory=", "", x, fixed = TRUE)
  x <- gsub("\r", "", x, fixed = TRUE)
  res <- as.integer(x)

  convert_bytes(x, unit)
}




get_available_ram_on_linux <- function(unit){
  res <- as.integer(system("/usr/bin/awk '/MemAvailable/ {print $2}' /proc/meminfo", intern=TRUE))
  convert_bytes(res, unit)
}




convert_bytes <- function(x, unit){
  switch(unit,
         "b"  = x * 1024L,
         "kb" = x,
         "mb" = x / 1024,
         "gb" = x / 1024^2,
         stop("Unit not valid")
  )
}
