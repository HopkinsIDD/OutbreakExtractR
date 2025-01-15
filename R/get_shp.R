#' @export
#' @title get_shp
#' @name get_shp
#' @description this function is to extract shapefiles from
get_shp <- function (location_period_id,username=username,password=password,host="db.cholera-taxonomy.middle-distance.com",port=5432,dbname='CholeraTaxonomy_production'){
  conn=DBI::dbConnect(
    RPostgres::Postgres(),
    user= username,
    password = password,
    host = host,
    port = 5432,
    dbname = dbname
  )
  qsql_code=paste0("select geojson from geojsons where location_period_id = ",location_period_id,';')
  sql_query=DBI::dbSendQuery(conn,glue::glue_sql(.con=conn, qsql_code))
  shp=sf::st_read(DBI::dbFetch(sql_query))
  shp$lctn_pr <- location_period_id
  return(shp)
}
