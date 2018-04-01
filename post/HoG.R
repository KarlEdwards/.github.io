#!/usr/bin/env Rscript --vanilla

# Reference https://cran.r-project.org/web/packages/OpenImageR/vignettes/The_OpenImageR_package.html
# Reference https://gist.github.com/louist87/9248952#file-image-findhogfeatures

start_time <- Sys.time()  
 
# -----------------------------------------------
# Configuration
# -----------------------------------------------

cfg <- list()
cfg[[ 1 ]] <- list( c( '-c', '--cells' ), default = 3, type = 'integer' , help = 'Number of grid cells [default %default]' )
cfg[[ 2 ]] <- list( c( '-b', '--bins' ), default = 9, help = 'Number of histogram bins [default %default]' )
cfg[[ 3 ]] <- list( c( '-o', '--output' ), default = '', type = 'character', help = 'Output file' )
cfg[[ 4 ]] <- list( c( '-t', '--transform' ), default = '', type = 'character', help = 'Transformation to apply' )
options <- lapply( cfg, function(f) do.call( optparse::make_option, f ) )

# -----------------------------------------------
# Get command line options and print help, if warranted.
# -----------------------------------------------

parser <- optparse::OptionParser(usage = "%prog [options] file", option_list = options )

tryCatch(
    arguments <- optparse::parse_args( parser, positional_arguments = 1 )
  , error = function(e){
      system( './HoG.R -h' )
      stop('\n-----------------\nSee Usage, above.\n-----------------\n\n./HoG.R -h')
    }
)
  
opt <- arguments$options
input_file <- arguments$args
print( input_file )
  
# https://www.mathworks.com/help/vision/ref/extracthogfeatures.html?requestedDomain = true
# To capture large-scale spatial information, increase the cell size. When you increase the cell size, you may lose small-scale detail.
# To encode finer orientation details, increase the number of bins. Increasing this value increases the size of the feature vector, which requires more time to process.

IMAGE_FILE_EXTENSIONS <- '.(png|jpeg|tiff|tif)$'

# -----------------------------------------------
# File not found
# -----------------------------------------------

if( !file.exists( input_file ))
  stop( 'Specified file does not exist' )
  
# -----------------------------------------------
#     Histogram of Gradients (HoG) function
# -----------------------------------------------

HoG <- function( f, bins=3, cells=6 ){
  # Length of resulting vector is bins x cells x cells
  input_image <- OpenImageR::readImage( f )
  normalized_image <- input_image * 255
  hog <- OpenImageR::HOG( normalized_image, cells  = cells, orientations  =  bins )
  data.frame( matrix( hog, nrow  =  1 ) )
}

get_im <- function( f ) OpenImageR::readImage( f )

find_edges <- function( im, method = 'Sobel' ) OpenImageR::edge_detection( im, method = method, conv_mode = 'same' )

threshold <- function( im, h=0.25 ) OpenImageR::image_thresholding( im, h )

downsample <- function( im, by=2.0 ) OpenImageR::down_sample_image( im, factor=by, gaussian_blur = TRUE, gauss_sigma = 1, range_gauss = 2 )

transformation_A <- function( f ){
  im <- downsample( find_edges( threshold( get_im( f ), 0.35 ),'Scharr' ), 5 )
  data.frame( matrix( im, nrow  =  1 ) )
}

# -----------------------------------------------
# One file, or many?
# -----------------------------------------------

is.directory <- function( f ) as.logical( file.info( f )[ 'isdir' ] )
if( is.directory( input_file ) ){
  print('DIRECTORY')
  input_file <- dir( path  = input_file, pattern = IMAGE_FILE_EXTENSIONS, full.names = TRUE )
  cat(sprintf( 'Found %d files\n', length( input_file )))
} else {
  if( !any( grepl( tools::file_ext( input_file ), IMAGE_FILE_EXTENSIONS ) ) ){
    stop( sprintf( 'File must be one of these types: %s\n', IMAGE_FILE_EXTENSIONS ) )
  }
}

# -----------------------------------------------
# No image files found
# -----------------------------------------------

if( !length( input_file ) )
  stop( paste( 'No image files found in', input_file ) )
  
# -----------------------------------------------
# We have one or more images to process, proceed!
# -----------------------------------------------

N <- length( input_file )
cat( sprintf( 'Processing %d files...\n', N ) )

if( opt$transform %in% c( 'a', 'b' )){
  df <- do.call( rbind, lapply( input_file, transformation_A ))
  
} else {
  df <- do.call( rbind, lapply( input_file, function(f) HoG( f, opt$bins, opt$cells) ) )
}

# -----------------------------------------------
# Show a sample of the results
# -----------------------------------------------

rownames( df ) <- basename( input_file )
print(df[ ,1:6 ] )
print( dim( df ))

if( nchar( opt$output ) > 0 ){
  cat(sprintf( 'Saving results to %s\n', opt$output ))
  write.table( df, file = opt$output, col.names = FALSE, sep = ',', quote = FALSE )
}

end_time   <- Sys.time()  
time_spent <- end_time - start_time
cat( sprintf( 'Time elapsed: ' ))
print( time_spent )

# Tests
# ./HoG.R  --cells=4 ./data/2_images/frame_00031.png
# ./HoG.R  --cells=4 ./data/0_training/fansinstands/
# ./HoG.R  -o test.csv --cells=4 ./data/0_training/fansinstands/
# ./HoG.R  -t 'a' ./data/2_images/frame_00031.png