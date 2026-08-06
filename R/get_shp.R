#' @export
#' @title get_shp
#' @name get_shp
#' @description this function is to extract shapefiles from the Cholera Taxonomy
#'   database. Optionally saves the result as a GeoParquet file.
#' @param location_period_id numeric: the location period ID to retrieve
#' @param username character: PostgreSQL username
#' @param password character: PostgreSQL password
#' @param host character: database host
#' @param port integer: database port
#' @param dbname character: database name
#' @param output_parquet character: optional file path to save result as
#'   GeoParquet (requires sfarrow package). If NULL, no file is written.
#' @return sf object with the shapefile geometry
get_shp <- function(location_period_id, username, password,
                    host = "db.cholera-taxonomy.middle-distance.com",
                    port = 5432, dbname = "CholeraTaxonomy_production",
                    output_parquet = NULL) {
  conn <- DBI::dbConnect(
    RPostgres::Postgres(),
    user = username,
    password = password,
    host = host,
    port = port,
    dbname = dbname
  )
  on.exit(DBI::dbDisconnect(conn))

  qsql_code <- paste0("select geojson from geojsons where location_period_id = ",
                      location_period_id, ";")
  sql_query <- DBI::dbSendQuery(conn, glue::glue_sql(.con = conn, qsql_code))
  shp <- sf::st_read(DBI::dbFetch(sql_query))
  shp$lctn_pr <- location_period_id

  if (!is.null(output_parquet)) {
    dir.create(dirname(output_parquet), recursive = TRUE, showWarnings = FALSE)
    sfarrow::st_write_parquet(shp, output_parquet)
  }
  return(shp)
}
