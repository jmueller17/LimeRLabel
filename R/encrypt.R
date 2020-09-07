#' @import gpg
#'
#' @title Write GPG encrypted data
#'
#' @description Utility function to serialize and store data frame as encrypted file on disc.  Requires
#'  a valid gpg public key.
#'
#' @param df The data frame to be encrypted
#' @param file String. Name and path for writing file
#' @param receiver email address of receiver (which has gpg key associated)
#'
#' @return Nothing
#'
#' @export write_gpg
#'
write_gpg <- function(df, file="df.gpg", receiver){
    df.s <- serialize(df, con=NULL, ascii=T)
    df.enc <- gpg_encrypt(df.s, receiver=receiver)
    writeBin(df.enc, con=file)
}


#' @title Read GPG encrypted data
#'
#' @description Read and deserialize GPG encrypted file. Requires
#'  private key for decrypting file. Will prompt for password of private key.
#'
#'
#' @param file String. File path of file to be decrypted
#'
#' @return R data frame
#'
#' @export read_gpg
#'
read_gpg <- function(file, as_text=FALSE){
    df <- gpg_decrypt(file, as_text=as_text)
    df <- unserialize(df)
    df
}
