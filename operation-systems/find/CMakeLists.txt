cmake_minimum_required(VERSION 2.8)

project(find)

set(CMAKE_CXX_STANDARD 17)

add_executable(${PROJECT_NAME} find.cpp filter.cpp)

find_package(Boost REQUIRED COMPONENTS program_options)
target_link_libraries(${PROJECT_NAME} ${Boost_LIBRARIES})
