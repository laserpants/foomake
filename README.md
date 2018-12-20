# foomake

Wouldn't it be nice to have a more declarative way to express CMake build requirements? So that, instead of&hellip;

```cmake
cmake_minimum_required(VERSION 3.2)

project(example VERSION 0.1.0 LANGUAGES CXX)

set(default_build_type "Release")

add_library(grok STATIC src/laserpants/grok.cpp)

target_include_directories(grok PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}/include")

add_executable(main src/main.cpp)

target_link_libraries(main grok)
```

&hellip;we could write something like:

```yaml
name:           example
description:    An example of a more declarative CMakeLists configuration
homepage:       http://github.com/laserpants/foomake#readme
languages:      CXX
version:        0.1.0

cmake: 
  minimumRequired:
    version: "3.2"

targets:
  executables:
    - main:
      files: 
        - "src/main.cpp"
      linkLibraries:
        - grok

  libraries:
    - grok: 
      type: static
      files: 
        - "src/laserpants/grok.cpp"
      includeDirs: 
        - path: "${CMAKE_CURRENT_SOURCE_DIR}/include"
          scope: public

variables:
  default_build_type: Release
```
