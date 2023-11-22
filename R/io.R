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
#' @importFrom readr write_csv write_rds
#' @importFrom methods is
#' @export
write_nested_output <- function(output_list, dir_out, mindepth = 5) {

  create_empty_dir(fs::path(dir_out), mindepth)
  purrr::walk2(names(output_list), output_list, \(name, object) {

    if (methods::is(object, "ggplot")) {

      ggplot2::ggsave(filename = fs::path(dir_out, paste0(name, ".pdf")),
                      plot = object,
                      width = size_A4_mm[1], height = size_A4_mm[2], units = "mm")
      readr::write_rds(x = object,
                       file = fs::path(dir_out, paste0(name, ".RDS")))

    } else if (methods::is(object, "data.frame")) {

      readr::write_csv(x = object,
                       file = fs::path(dir_out, paste0(name, ".csv")))
      readr::write_rds(x = object,
                       file = fs::path(dir_out, paste0(name, ".RDS")))

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

#' Reads RDS object from nested directories and creates a nested list from them
#'
#' Acts as a complement to write_nested_output(). The RDS files written to disk
#' by write_nested_output() will be read in to recreate the list originally
#' used by write_nested_output() to write the data to disk.
#'
#' @param dir_in The directory where the input RDS files are located (character or fs::path)
#' @return The nested list containing the content of the RDS files
#' @examples
#' # write output by running the example from write_any_output(), then run ...
#' \dontrun{nested_input <- read_nested_input(dir_out)}
#' @importFrom fs path_expand dir_ls
#' @importFrom purrr map map_int walk2 pluck
#' @importFrom readr write_csv write_rds
#' @importFrom stringr str_remove str_split_1
#' @export
read_nested_input <- function(dir_in) {

  dir_in <- fs::path_expand(dir_in)

  filenames <- fs::dir_ls(dir_in, recurse=TRUE, glob = "*.RDS")

  values_flat <- filenames %>%
    purrr::map(~read_rds(.))

  list_levels <- filenames %>%
    stringr::str_remove(paste0(dir_in, "/")) %>%
    stringr::str_remove(".RDS") %>%
    purrr::map(~stringr::str_split_1(., "/"))

  values_nested <- list()
  purrr::walk2(values_flat, list_levels, \(value, level_vec) purrr::pluck(values_nested, !!!level_vec) <<- value)
  values_nested

}

#' Combine multiple pdf files into 1 pdf file
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


#' Combine multiple csv files into 1 xlsx file
#'
#' Searches path for csv files. If csv files are found , an aggregated xlsx file
#' of them is generated, with the csv filenames as tabnames of the xlsx file.
#'
#' @param path The path where the csv's will be searched and output will be generated
#' @param recurse If TRUE, search for and combine csv files recursively from path
#' @examples
#' example_output_directory <- paste0(tempdir(), "/csv_files")
#' fs::dir_create(example_output_directory)
#' readr::write_csv(cars, fs::path(example_output_directory, "cars1.csv"))
#' readr::write_csv(cars, fs::path(example_output_directory, "cars2.csv"))
#' csv_combine_from_path(example_output_directory)
#' system(paste0("ls ", example_output_directory))
#' @importFrom fs dir_ls path_dir path_ext_remove path_file path
#' @importFrom purrr set_names map walk
#' @importFrom readr read_csv
#' @importFrom writexl write_xlsx
#' @export
csv_combine_from_path <- function(path, recurse = TRUE) {

  files <- fs::dir_ls(path, recurse = recurse, glob = "*.csv") %>%
    purrr::set_names(fs::path_dir(.))
  unique_folders <- names(files) %>% unique() %>% purrr::set_names()
  content <- purrr::map(unique_folders, \(folder) files %>% .[names(.) == folder] %>%
                          purrr::set_names() %>%
                          purrr::map(readr::read_csv) %>%
                          purrr::set_names(nm = fs::path_ext_remove(fs::path_file(names(.)))))

  purrr::walk(unique_folders, \(folder)
       writexl::write_xlsx(content[[folder]], path = fs::path(folder, "combined.xlsx")))

}
