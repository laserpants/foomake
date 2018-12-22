# foomake

Wouldn't it be nice to have a more grokkable way to express CMake build requirements? So that, instead of&hellip;

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

cmakeMinimumRequired:
  version: '3.2'

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

<!--
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
-->

## Top-level keys

| Key                     | Type                     | Description                                     |
|-------------------------|--------------------------|-------------------------------------------------|
| name<sup>&dagger;</sup> | string                   | Sets the `PROJECT_NAME` variable                |
| version                 | string                   | Sets the `PROJECT_VERSION` variable             |
| description             | string                   | Sets the `CMAKE_PROJECT_DESCRIPTION` variable   |
| homepage                | string                   | Sets the `CMAKE_PROJECT_HOMEPAGE_URL` variable  |
| languages               | list (or string)         | Sets the project `LANGUAGES`                    |
| cmakeMinimumRequired    | dict (or string)         | Set the minimum required version of CMake       |
| executables             | dict                     | Executable targets (binaries)                   |
| libraries               | dict                     | Library targets                                 |
| variables               | dict                     |                                                 |
| install                 | dict                     |                                                 |
| configure               | list                     | Copy and perform variable substitution on files |

&dagger;) **required**

---

### `name`, `version`, `description`, `homepage`

These properties set the project details. For example,

```yaml
name: Your project
version: '1.3'
description: One project to rule them all
```

translates to:

```cmake
project(Your project
  VERSION
    1.3
  DESCRIPTION
    One project to rule them all
  )
```

---

### `languages`

A list of languages that your project supports. For example,

```yaml
name: example
languages:
  - CXX
  - Fortran
```

translates to:

```cmake
project(example LANGUAGES CXX Fortran)
```

A string can be used as a shorthand for singleton lists:

```yaml
languages: CXX
```

---

### `cmakeMinimumRequired`

```yaml
cmakeMinimumRequired:
  version: '3.2'
```

The following form is also accepted:

```yaml
cmakeMinimumRequired: '3.2'
```

Or to specify a range:

```yaml
cmakeMinimumRequired: '3.1...3.13'
```

---

### `executables`

```yaml
executables:
  foo:
    # ...
  baz:
    # ...
```

See [Targets](#Targets)

---

### `libraries`

```yaml
libraries:
  grok:
    # ...
  bot:
    # ...
```

See [Targets](#Targets)

---

### `variables`

```yaml
variables:
  MY_VARIABLE: rocks
```

```cmake
set(MY_VARIABLE "rocks")
```

#### Dictionary form

```yaml
variables:
  MY_VARIABLE:
    value: rocks
```

---

### `install`

---

### `configure`

A list of files to perform variable substitution on. See `[configure_file](https://cmake.org/cmake/help/latest/command/configure_file.html?highlight=configure_file)`.

List entries should be dictionaries of the following form.

| Key                  | Type                     | Required | Description                                        |
|----------------------|--------------------------|:--------:|----------------------------------------------------|
| input                | string                   | yes      |                                                    |
| output               | string                   | yes      |                                                    |
| arguments            | dict                     |          | Arguments accepted by the `configure_file` command |

Example:

```yaml
name: example
configure:
  - input:  'config.h.in'
    output: 'config.h'
    arguments:
      @ONLY: true
```

---

## Targets

### Executables

| Key                  | Type                     | Required | Default | Alias        | Description                                    |
|----------------------|--------------------------|:--------:|---------|--------------|------------------------------------------------|
| files                | list                     |          |         |              |                                                |
| includeDirectories   | list                     |          |         | includeDirs  |                                                |

---

### `files`

---

### `includeDirectories`

---

### Libraries

| Key                  | Type                               | Required | Default | Alias        | Description                                   |
|----------------------|------------------------------------|:--------:|---------|--------------|-----------------------------------------------|
| files                | list                               |          |         |              |                                               |
| includeDirectories   | list                               |          |         | includeDirs  |                                               |
| type                 | static &vert; shared &vert; module |          | static  |              |                                               |

---

### `files`

---

### `includeDirectories`

---

### `type`

---

## What's missing?

A lot! What about those cases where control flow and conditional statements are unavoidable?

## Examples

The following examples are adapted from https://cmake.org/cmake-tutorial/

### Step 1

```cmake
cmake_minimum_required (VERSION 2.6)
project (Tutorial)

# the version number
set (Tutorial_VERSION_MAJOR 1)
set (Tutorial_VERSION_MINOR 0)

# configure a header file to pass some of the CMake settings to the source code
configure_file (
  "${PROJECT_SOURCE_DIR}/TutorialConfig.h.in"
  "${PROJECT_BINARY_DIR}/TutorialConfig.h"
  )

# add the binary tree to the search path for include files so that we will find TutorialConfig.h
include_directories("${PROJECT_BINARY_DIR}")

# add the executable
add_executable(Tutorial tutorial.cxx)
```

```yaml
name: Tutorial
version: '1.0'                      # the version number

cmakeMinimumRequired:
  version: '2.6'

executables:
  Tutorial:                         # add the executable
    files:
      - tutorial.cxx
    includeDirectories:
      - '${PROJECT_BINARY_DIR}'     # add the binary tree to the search path for include files so
                                    # that we will find TutorialConfig.h
configure:
  - input:  '${PROJECT_SOURCE_DIR}/TutorialConfig.h.in'
    output: '${PROJECT_BINARY_DIR}/TutorialConfig.h'
```
