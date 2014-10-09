#Generic function to take HEX (BLOB) string data extracted from database and convert to
#raw for subsequent decoding
#NB BLOB data MUST have been extracted using SQL HEX() function

#This has been tested on a few thousand rows and columns and takes only a few seconds in such cases

#Convert hex string (resulting from SQL "SELECT HEX(x)..." to raw (bytes) vector

gtmBlobHexStrToRaw <- function(hexStr)
{
    # Define function to process individual hex strings (single row)
    # Current method is by the far the fastest out of various ways of doing this involving sapply or for loops
    convertSingleRow <- function(hexRow)
    {
        numChars    <- nchar(hexRow)
        tmpSplit    <- unlist(strsplit(hexRow, split="")) #yields vector of individual chars (we need pairs)
        # tmpSplit is now a character vector of length numChars
        # Now to paste adjacent pairs together
        oddInds     <- seq(1, numChars, by=2)
        evenInds    <- oddInds + 1
        tmpCharVec  <- paste(tmpSplit[oddInds], tmpSplit[evenInds], sep="")
        tmpRaw      <- as.raw(as.hexmode(tmpCharVec))
        return(tmpRaw)
    }

    # hexStr could contain multiple rows (e.g. be of length N)
    numRows <- length(hexStr)

    if (numRows > 1)
    {
        # Requires matrix
        lenStr      <- nchar(hexStr) # could be vector (of length numRows)
        numOutCols  <- lenStr/2      # could be vector (of length numRows)

        # A for loop here actually allows us to predefine a matrix of suitable size
        # (sapply was much slower)
        output <- matrix(as.raw(0), numRows, max(numOutCols))
        for (iRow in 1:numRows)
        {
            output[iRow,1:numOutCols[iRow]] <- convertSingleRow(hexStr[iRow])
        }
    } else
    {
        output <- convertSingleRow(hexStr)
    }
    return(output)
}

