#!/bin/bash

# step one : convert to tif
for image in *.png; do
    convert $image ${image/.png/.tif}
done

# step two : unpaper
for image in *.tif; do
    unpaper $image ${image/.tif/-unp.tif}
done

# step three : ocr
count=1
for page in *-unp.tif; do
    no=$(printf "%02d" $count)
    tesseract $page ${page/unp.tif/$no} -l lat
    let count=$count+1
    rm $page
done

# step four : combine
for file in *.txt; do
    cat $file >>binder.txt
    rm $file
done

rm *.tif
