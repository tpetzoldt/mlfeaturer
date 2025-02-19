#' @title Set Transformations
#'
#' @description Sets the transformation functions for a `preproc_data` object.
#'
#' @param object A `preproc_data` object.
#' @param fun_transform A named list of functions to apply to the data.  T
#'   he names should correspond to the columns, and the values should be the functions.
#' @param fun_inverse A named list of functions representing the inverse
#'   transformations.  The structure should be the same as `fun_transform`.
#'
#' @return A `preproc_data` object with the updated transformation functions.
#' @export
setGeneric("set_transformations",
           function(object, fun_transform = NULL,
                    fun_inverse = NULL) standardGeneric("set_transformations"))

# Implement the method
#' @describeIn set_transformations Method for setting transformations in a `preproc_data` object.
setMethod("set_transformations", signature = c(object = "preproc_data"),
          function(object, fun_transform = NULL, fun_inverse = NULL) {
            object@params@fun_transform <- fun_transform  # Update fun_transform in parameters
            object@params@fun_inverse <- fun_inverse      # Update fun_inverse in parameters

            ##Apply transformation if fun_transform is provided
            #if (!is.null(fun_transform)){
            #  object <- transform_data(object, fun_transform)
            #}
            return(object)
          })
