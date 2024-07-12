#' read qiime2 artifacts (.qza)
#'
#' extracts embedded data and object metadata into an R session
#'
#' @param file path to the input file, ex: file="~/data/moving_pictures/table.qza"
#' @param tmp a temporary directory that the object will be decompressed to (default="tempdir()")
#' @param rm should the decompressed object be removed at completion of function (T/F default=TRUE)
#' @return a named list.
#' @examples
#' fpath <- system.file("extdata/qiime", "table-dada2.qza", package = "bubbler")
#' read_qza(fpath)
#' @export
read_qza <- function(file, tmp, rm) {

    if(missing(tmp)){tmp <- tempdir()}
    if(missing(file)){stop("Path to artifact (.qza) not provided.")}
    if(!file.exists(file)){stop("Input artifact (",file,") not found. Please check path and/or use list.files() to see files in current working directory.")}
    if(missing(rm)){rm=TRUE} #remove the decompressed object from tmp
    if(!grepl("qza$", file)){stop("Provided file is not qiime2 artifact (.qza).")}

    utils::unzip(file, exdir=tmp)
    unpacked<-utils::unzip(file, exdir=tmp, list=TRUE)

    artifact<-yaml::read_yaml(paste0(tmp,"/", paste0(gsub("/..+","", unpacked$Name[1]),"/metadata.yaml")))
    artifact$contents<-data.frame(files=unpacked)
    artifact$contents$size=sapply(paste0(tmp, "/", artifact$contents$files), file.size)
    artifact$version=utils::read.table(paste0(tmp,"/",artifact$uuid, "/VERSION"))


    if(grepl("BIOMV", artifact$format)){
        artifact$data<-read_q2biom(paste0(tmp, "/", artifact$uui,"/data/feature-table.biom"))
    } else if (artifact$format=="TSVTaxonomyDirectoryFormat"){
        artifact$data<-utils::read.table(paste0(tmp,"/", artifact$uuid, "/data/taxonomy.tsv"), sep='\t', header=TRUE, quote="", comment="")
    }

    if(rm==TRUE){unlink(paste0(tmp,"/", artifact$uuid), recursive=TRUE)}
    return(artifact)
}

#' read qiime2 biom file (version 2.1)
#'
#' Loads a version 2.1 spec biom file (http://biom-format.org/documentation/format_versions/biom-2.1.html) as expected to be found within a qiime2 artifact.
#'
#' @param file path to the input file, ex: file="~/Downloads/3372d9e0-3f1c-43d8-838b-35c7ad6dac89/data/feature-table.biom"
#' @return a matrix of values
#'
#' @export
#' @examples
#' file <- system.file("extdata/qiime", "table-dada2.qza", package = "bubbler")
#' tmp <- tempdir()
#' unzip(file, exdir=tmp)
#' unpacked<-unzip(file, exdir=tmp, list=TRUE)
#' artifact<-yaml::read_yaml(paste0(tmp,"/",
#'     paste0(gsub("/..+","", unpacked$Name[1]),"/metadata.yaml")))
#' artifact$contents<-data.frame(files=unpacked)
#' artifact$contents$size=sapply(paste0(tmp, "/", artifact$contents$files), file.size)
#' artifact$version=read.table(paste0(tmp,"/",artifact$uuid, "/VERSION"))
#' read_q2biom(paste0(tmp, "/", artifact$uui,"/data/feature-table.biom"))

read_q2biom <- function(file) {
    if(missing(file)){stop("Path to biom file given")}
    if(!file.exists(file)){stop("File not found")}

    hdata<-rhdf5::h5read(file,"/")

    ftable<-
        Matrix::sparseMatrix(
            p=hdata$observation$matrix$indptr,
            j=hdata$observation$matrix$indices,
            x=as.numeric(hdata$observation$matrix$data),
            index1=FALSE,
            dims=c(length(hdata$observation$ids), length(hdata$sample$ids)),
            dimnames=list(hdata$observation$ids,hdata$sample$ids)
        )
    return(as.matrix(ftable))
}

#' read qiime2 metadata (.tsv)
#'
#' Loads a qiime2 metadata file wherein the 2nd line contains the #q2:types line dictating the type of variable (categorical/numeric)
#'
#' @param file path to the input file, ex: file="~/data/moving_pictures/table.qza"

#' @return a data.frame wherein the first column is SampleID
#' @export

#' @examples
#' fpath <- system.file("extdata/qiime", "sample-metadata.tsv", package = "bubbler")
#' read_q2metadata(fpath)

read_q2metadata <- function(file) {
    if(missing(file)){stop("Path to metadata file not found")}
    if(!is_q2metadata(file)){stop("Metadata does not define types (ie second line does not start with #q2:types)")}

    defline<-suppressWarnings(readLines(file)[2])
    defline<-strsplit(defline, split="\t")[[1]]

    defline[grep("numeric", tolower(defline))]<-"double"
    defline[grep("categorical|q2:types", tolower(defline))]<-"factor"
    defline[defline==""]<-"factor"

    coltitles<-strsplit(suppressWarnings(readLines(file)[1]), split='\t')[[1]]
    metadata<-utils::read.table(file, header=F, col.names=coltitles, skip=2, sep='\t', colClasses = defline, check.names = FALSE)
    colnames(metadata)[1]<-"SampleID"

    return(metadata)
}



#' checks if metadata is in qiime2 (.tsv)
#'
#' Checks to see if a file is in qiime2 metadata format, ie contains #q2:types line dictating the type of variable (categorical/numeric)
#'
#' @param file path to the input file, ex: file="~/data/moving_pictures/table.qza"

#' @return TRUE/FALSE
#' @export
#'
#' @examples
#' fpath <- system.file("extdata/qiime", "sample-metadata.tsv", package = "bubbler")
#' is_q2metadata(fpath)

is_q2metadata <- function(file){

    if (!file.exists(file)){stop("Input metadata file (",file,") not found. Please check path and/or use list.files() to see files in current working directory.")}

    suppressWarnings(
        if(grepl("^#q2:types", readLines(file)[2])){return(TRUE)}else{return(FALSE)}
    )
}
