#!/bin/bash

if [ "$1" == "-c" ]; then
    dotnet run -c sgSettings.config buildLCClassGraph datasets/LOC/Class.thru.d130228.records-JPFIX.xml.gz
    start=1
elif [ "$1" == "-s" ]; then
    start=$2
else
    start=1
fi

if [ "$start" == 1 ]; then
    dotnet run -c sgSettings.config addBooksToClassGraph output/ClassGraph.sgb ~/datasets/LOC/2016-booksall/BooksAll.2016.part01.xml.gz output/books01-01.sgb
    start=2
fi

for i in $(seq $start 43); do
    num=$(printf "%02d" $i)
    prev=$(printf "%02d" $((i-1)))
    dotnet run -c sgSettings.config addBooksToClassGraph output/books01-$prev.sgb ~/datasets/LOC/2016-booksall/BooksAll.2016.part$num.xml.gz output/books01-$num.sgb
done
