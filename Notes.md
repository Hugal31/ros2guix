# Fixes

- fastrtps needs foonathan-memory
- ament-cmake-core needs its dependencies to be propagated.
  - sames go for ament-cmake-lint-cmake, and probably all ament-cmake-* packages
- ament-cmake should propagate-input all ament-cmake-* packages, because they are declaring it as exports.
- launch misses python-pyyaml as an propagated-input, and osrf-pycommon as a native-input
- Create a ament target with all extensions, as with colcon.
