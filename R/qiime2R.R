#' @export
read_qza <- function(file, tmp, rm) {

    if(missing(tmp)){tmp <- tempdir()}
    if(missing(file)){stop("Path to artifact (.qza) not provided.")}
    if(!file.exists(file)){stop("Input artifact (",file,") not found. Please check path and/or use list.files() to see files in current working directory.")}
    if(missing(rm)){rm=TRUE} #remove the decompressed object from tmp
    if(!grepl("qza$", file)){stop("Provided file is not qiime2 artifact (.qza).")}

    unzip(file, exdir=tmp)
    unpacked<-unzip(file, exdir=tmp, list=TRUE)

    artifact<-yaml::read_yaml(paste0(tmp,"/", paste0(gsub("/..+","", unpacked$Name[1]),"/metadata.yaml")))
    artifact$contents<-data.frame(files=unpacked)
    artifact$contents$size=sapply(paste0(tmp, "/", artifact$contents$files), file.size)
    artifact$version=read.table(paste0(tmp,"/",artifact$uuid, "/VERSION"))


    if(grepl("BIOMV", artifact$format)){
        artifact$data<-read_q2biom(paste0(tmp, "/", artifact$uui,"/data/feature-table.biom"))
    } else if (artifact$format=="TSVTaxonomyDirectoryFormat"){
        artifact$data<-read.table(paste0(tmp,"/", artifact$uuid, "/data/taxonomy.tsv"), sep='\t', header=TRUE, quote="", comment="")
    }

    if(rm==TRUE){unlink(paste0(tmp,"/", artifact$uuid), recursive=TRUE)}
    return(artifact)
}

#' @export
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



