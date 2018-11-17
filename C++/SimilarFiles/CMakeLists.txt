cmake_minimum_required(VERSION 2.8)

project(similarFiles)

set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTOUIC ON)
set(CMAKE_INCLUDE_CURRENT_DIR ON)

find_package(Qt5Widgets REQUIRED)

add_executable(similarFiles main.cpp mainwindow.h mainwindow.cpp parameters.h)
qt5_use_modules(similarFiles Widgets)
