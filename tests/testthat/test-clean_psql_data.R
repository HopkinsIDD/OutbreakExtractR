#test if averaging duplicates has been implemented
test_that("clean_psql_data works", {
  outbreak_ts<-readRDS("data/outbreak_data.rds")
  dup_outbreak_ts<-outbreak_ts[duplicated(outbreak_ts[,colnames(outbreak_ts)%in%c("TL","TR","location")])|duplicated(outbreak_ts[,colnames(outbreak_ts)%in%c("TL","TR","location")],fromLast = TRUE),]
  post_dup_outbreak_ts<-clean_psql_data(original_data=dup_outbreak_ts)
  testthat::expect_true(nrow(post_dup_outbreak_ts)<=nrow(dup_outbreak_ts)/2)
})

#test if all the duplicated rows have been averaged
test_that("clean_psql_data works", {
  outbreak_ts<-readRDS("data/outbreak_data.rds")
  dup_outbreak_ts<-outbreak_ts[duplicated(outbreak_ts[,colnames(outbreak_ts)%in%c("TL","TR","location")])|duplicated(outbreak_ts[,colnames(outbreak_ts)%in%c("TL","TR","location")],fromLast = TRUE),]
  post_dup_outbreak_ts<-clean_psql_data(original_data=dup_outbreak_ts)
  testthat::expect_true(
    nrow(post_dup_outbreak_ts[duplicated(post_dup_outbreak_ts[,colnames(post_dup_outbreak_ts)%in%c("TL","TR","location")])|duplicated(post_dup_outbreak_ts[,colnames(post_dup_outbreak_ts)%in%c("TL","TR","location")],fromLast = TRUE),])==0
  )
})