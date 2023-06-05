#' @title Connect to the regulondb web services.
#' @description This function creates a variable that allows the connection
#' to the RegulonDB web services hosted on Graphql, which will work for future queries.
#' This function requires an active Internet connection.
#' @author Pina Acosta Daniel, Valle Mondragon Sergio.
#' @return a connection variable global to web services
#' @examples
#' if (!exists("regulondb_conn")) regulondb_conn <- connect_database()
#' @export

connect_database <- function() {
  #We assign the value of url
  url <- "http://132.248.220.219:7000/graphql"
  return(url)
}
