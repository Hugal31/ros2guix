# ROS2Guix

Run `./ros2guix.scm -r galactic PACKAGE` to convert PACAKGE or
`./ros2guix.scm -r galactic -a` to convert the whole distribution.

You can also grep packages recursively with `-R`. Use `--help`for more
information.

This project doest *not* work. I advise you to start small (e.g.
`./ros2guix.scm -r galactic -R xacro` and test on package at a time.

## Dependencies

* guile-lib
* guile-gnutls
* guix

## Features

[x] Fetch the ROS cache repository.
[x] Very early prototype converstion from package.xml format to guix format.
[x] Recursive dependency fetching.
[ ] Ament guix build type
