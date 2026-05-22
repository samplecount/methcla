# Consumer tests

Minimal projects that verify the two supported consumption modes build end-to-end without the consumer adding any include paths or library flags manually.

## Consumer A — `add_subdirectory`

```sh
cmake -S tests/consumers/add_subdirectory -B /tmp/consumer-a -DMETHCLA_BUILD_TESTS=OFF
cmake --build /tmp/consumer-a --target consumer
/tmp/consumer-a/consumer
```

## Consumer B — `find_package`

Requires methcla to be installed first:

```sh
cmake -S . -B /tmp/methcla-build -DMETHCLA_BUILD_TESTS=OFF
cmake --build /tmp/methcla-build
cmake --install /tmp/methcla-build --prefix /tmp/methcla-install
```

Then configure and build the consumer:

```sh
cmake -S tests/consumers/find_package -B /tmp/consumer-b \
      -DCMAKE_PREFIX_PATH=/tmp/methcla-install
cmake --build /tmp/consumer-b --target consumer
/tmp/consumer-b/consumer
```
