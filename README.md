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

| Key                    | Type                     | Description                                    |
|------------------------|--------------------------|------------------------------------------------|
| name<sup>&dagger;</sup> | string                   | Sets the `PROJECT_NAME` variable               |
| version                | string                   | Sets the `PROJECT_VERSION` variable            |
| description            | string                   | Sets the `CMAKE_PROJECT_DESCRIPTION` variable  |
| homepage               | string                   | Sets the `CMAKE_PROJECT_HOMEPAGE_URL` variable |
| languages              | list (or string)         | Sets the project `LANGUAGES`                   |
| cmakeMinimumRequired   | dict (or string)         | Set the minimum required version of CMake      |
| executables            | dict                     | Executable targets (binaries)                  |
| libraries              | dict                     | Library targets                                |
| variables              | dict                     |                                                |
| install                | dict                     |                                                |

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
    One project to rule them all)
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

## Targets

### Executables

| Key                  | Type                     | Required | Default | Alias        | Description                                    |
|----------------------|--------------------------|:--------:|---------|--------------|------------------------------------------------|
| files                | list                     |          |         |              |                                                |
| includeDirectories   | dict                     |          |         | includeDirs  |                                                |

---

### `files`

---

### `includeDirectories`

---

### Libraries

| Key                  | Type                               | Required | Default | Alias        | Description                                   |
|----------------------|------------------------------------|:--------:|---------|--------------|-----------------------------------------------|
| files                | list                               |          |         |              |                                               |
| includeDirectories   | dict                               |          |         | includeDirs  |                                               |
| type                 | static &vert; shared &vert; module |          | static  |              |                                               |

---

### `files`

---

### `includeDirectories`

---

### `type`
