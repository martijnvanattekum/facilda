#' Creates an empty directory
#'
#' @param path The resulting empty directory. If not existing, dir will be created.
#' !! If existing, dir will be emptied. !!
#' @param mindepth Depth of output directory. Used as safety check to avoid emptying high-level dirs
#' @return The path to the directory
#' @examples
#' # New directory
#' directory <- paste0(tempdir(), "/example_creation_dir")
#' create_empty_dir(directory, 1)
#' # Existing non-empty directory (first example continues)
#' system(paste0("touch ", directory, "/newfile"))
#' create_empty_dir(directory, 1)
#' # -> newfile is deleted
#' @importFrom fs file_delete
#' @importFrom stringr str_extract_all
#' @export
create_empty_dir <- function(path, mindepth = 5) {

  stopifnot(length(stringr::str_extract_all(path, "/")[[1]]) > mindepth) # safety check

  if (fs::dir_exists(path)) {
    fs::file_delete(fs::dir_ls(path))
  } else {
    fs::dir_create(path, recurse = TRUE)
  }

  path

}

#' Writes writable objects from a (nested) list
#'
#' Descends into a (nested) list until it finds either a data.frame or ggplot
#' object. These objects are then written as csv or A4 format pdf respectively,
#' Using the names of the list as subsequent folder and lastly file names.
#' Typically used to gather different outputs in a list which can then be
#' written.
#' NOTE: !! dir_out will be overwritten !!
#'
#' @param output_list A nested list
#' @param dir_out The directory where the output will be written to (character or fs::path)
#' @param mindepth Depth of output directory. Used as safety check to avoid emptying high-level dirs
#' @return The path where the output was written to
#' @examples
#' library(ggplot2)
#' cars_df <- cars
#' cars_plot <- ggplot(cars, aes(x=speed, y=dist)) + geom_point()
#' my_output_list <- list(my_output = list(plots = list(cars = cars_plot),
#'                                         tables = list(cars = cars_df)))
#' example_output_directory <- paste0(tempdir(), "/example_output")
#' dir_out <- write_nested_output(my_output_list, example_output_directory, 1)
#' # Structure of output list
#' str(my_output_list, max.level = 3)
#' # Structure in the returned temporary folder, e.g.
#' system(paste0("tree ", dir_out))
#' @importFrom ggplot2 ggsave
#' @importFrom purrr walk2
#' @importFrom readr write_csv
#' @importFrom methods is
#' @export
write_nested_output <- function(output_list, dir_out, mindepth = 5) {

  create_empty_dir(fs::path(dir_out), mindepth)
  purrr::walk2(names(output_list), output_list, \(name, object) {

    if (methods::is(object, "ggplot")) {

      ggplot2::ggsave(filename = fs::path(dir_out, paste0(name, ".pdf")),
                      plot = object,
                      width = size_A4_mm[1], height = size_A4_mm[2], units = "mm")

    } else if (methods::is(object, "data.frame")) {

      readr::write_csv(x = object,
                       file = fs::path(dir_out, paste0(name, ".csv")))

      # recursion case
    } else if (methods::is(object, "list")) {

      dir_out <- fs::path(dir_out, name)
      write_nested_output(object, dir_out, mindepth)

    } else {

      stop(paste0("Cannot handle object of class ", class(object)))

    }

  }
  )
  return(dir_out)
}

#' Combine multiple pdfs into 1 file
#'
#' Finds all pdf files in path and combines them into one file with the name
# of the folder.
#'
#' @param path The path where the pdfs will be searched and output will be generated
#' @importFrom qpdf pdf_combine
#' @export
pdf_combine_from_path <- function(path) {

  filename <- paste0(basename(path), ".pdf")
  pdf_filenames <- fs::dir_ls(path, glob="*.pdf")
  if (length(pdf_filenames) > 1){qpdf::pdf_combine(pdf_filenames, fs::path(path, filename))}

}
