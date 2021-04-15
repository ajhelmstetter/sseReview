#!/bin/bash
ls -1 ./ | \
while read sample; \
do 
	pdftotext $sample
done
