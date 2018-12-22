# foomake

Wouldn't it be nice to have a more declarative way to express CMake build requirements? So that, instead of&hellip;

```cmake
# CMakeLists.txt

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
name:                  example
version:               0.1.0
description:           An example of a declarative CMakeLists configuration
homepage:              http://github.com/laserpants/foomake#readme
languages:             CXX
cmakeMinimumRequired:  '3.2'

executables:
  main:
    files:
      - src/main.cpp
    linkLibraries:
      - grok

libraries:
  grok:
    type: static
    files:
      - src/laserpants/grok.cpp
    includeDirs:
      - path: '${CMAKE_CURRENT_SOURCE_DIR}/include'
        scope: public

variables:
  default_build_type: Release
```

Or, for those who prefer JSON:

```json
{
    "name": "example",
    "version": "0.1.0",
    "description": "An example of a declarative CMakeLists configuration",
    "homepage": "http://github.com/laserpants/foomake#readme",
    "languages": ["CXX"],
    "cmakeMinimumRequired": "3.2",
    "executables": {
        "main": {
            "files": [
                "src/main.cpp"
            ],
            "linkLibraries": [ "grok" ]
        }
    },
    "libraries": {
        "grok": {
            "type": "static",
            "files": [
                "src/laserpants/grok.cpp"
            ],
            "includeDirs": [{
                "path": "${CMAKE_CURRENT_SOURCE_DIR}/include",
                "scope": "public"
            }]
        }
    },
    "variables": {
        "default_build_type": "Release"
    }
}
```

## Top-level properties

| Property             | Type                     | Description |
|----------------------|--------------------------|-------------|
| name                 | `string`                 | Sets the `PROJECT_NAME` variable |
| version              | `string`                 | Sets the `PROJECT_VERSION` variable |
| description          | `string`                 |   |
| homepage             | `string`                 |   |
| languages            | `array` &vert; `string`  |   |
| cmakeMinimumRequired | `object` &vert; `string` |   |
| executables          | `object`                 | Executable targets (binaries) |
| libraries            | `object`                 | Library targets               |
| variables            | `object`                 |   |
| install              | `object`                 |   |

None of the top-level properties are required.

### `name`

### `version`

### `description`

### `homepage`

### `languages`

```yaml
languages:
  - CXX
  - Fortran
```

```yaml
languages: CXX
```

### `cmakeMinimumRequired`

```yaml
cmakeMinimumRequired:  
  version: '3.2'
```

```yaml
cmakeMinimumRequired: '3.2'
```

### `executables`

### `libraries`

### `variables`

### `install`
