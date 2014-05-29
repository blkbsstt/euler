#!/usr/bin/env zsh
working=./src/main/scala/euler
completed=./src/main/completed/euler
while getopts b name
do
    case $name in
        b) tmp=$working
           working=$completed
           completed=$tmp;;
    esac
done
shift $((OPTIND-1))
prob=prob${(l:3::0:)1}.scala
    
mv -v $working/$prob $completed/ 
