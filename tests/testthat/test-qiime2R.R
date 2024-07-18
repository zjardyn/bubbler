# devtools::load_all()

test_that("read_qza works", {
    fpath <- system.file("extdata/qiime", "table-dada2.qza", package = "bubbler")
    expect_no_error(read_qza(fpath))

})

# this function is meant to be run internally, so running it requires some set up
test_that("read_q2biom works",{
    file <- system.file("extdata/qiime", "table-dada2.qza", package = "bubbler")
    tmp <- tempdir()
    unzip(file, exdir=tmp)
    unpacked<-unzip(file, exdir=tmp, list=TRUE)
    artifact<-yaml::read_yaml(paste0(tmp,"/",
        paste0(gsub("/..+","", unpacked$Name[1]),"/metadata.yaml")))
    artifact$contents<-data.frame(files=unpacked)
    artifact$contents$size=sapply(paste0(tmp, "/", artifact$contents$files), file.size)
    artifact$version=read.table(paste0(tmp,"/",artifact$uuid, "/VERSION"))
    expect_no_error(read_q2biom(paste0(tmp, "/", artifact$uui,"/data/feature-table.biom")))

})

test_that("read_q2metadata works", {
    fpath <- system.file("extdata/qiime", "sample-metadata.tsv", package = "bubbler")
    expect_no_error(read_q2metadata(fpath))
})

test_that("is_q2metadata works", {
     fpath <- system.file("extdata/qiime", "sample-metadata.tsv", package = "bubbler")
     expect_no_error(is_q2metadata(fpath))
} )
