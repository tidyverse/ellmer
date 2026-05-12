# llmjson (0.1.0)

* GitHub: <https://github.com/DyfanJones/llmjson>
* Email: <mailto:dyfan.r.jones@gmail.com>
* GitHub mirror: <https://github.com/cran/llmjson>

Run `revdepcheck::cloud_details(, "llmjson")` for more info

## In both

*   checking whether package ‘llmjson’ can be installed ... ERROR
     ```
     Installation failed.
     See ‘/tmp/workdir/llmjson/new/llmjson.Rcheck/00install.out’ for details.
     ```

## Installation

### Devel

```
* installing *source* package ‘llmjson’ ...
** this is package ‘llmjson’ version ‘0.1.0’
** package ‘llmjson’ successfully unpacked and MD5 sums checked
** using staged installation
Using cargo 1.75.0
Using rustc 1.75.0 (82e1608df 2023-12-21) (built from a source tarball)
Building for CRAN.
Writing `src/Makevars`.
`tools/config.R` has finished.
** libs
...
export CARGO_HOME=/tmp/workdir/llmjson/new/llmjson.Rcheck/00_pkg_src/llmjson/src/.cargo && \
export PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/root/.cargo/bin" && \
RUSTFLAGS=" --print=native-static-libs" cargo build -j 2 --offline --lib --release --manifest-path=./rust/Cargo.toml --target-dir ./rust/target
error: package `indexmap v2.12.1` cannot be built because it requires rustc 1.82 or newer, while the currently active rustc version is 1.75.0
Either upgrade to rustc 1.82 or newer, or use
cargo update indexmap@2.12.1 --precise ver
where `ver` is the latest version of `indexmap` supporting rustc 1.75.0
make: *** [Makevars:28: rust/target/release/libllmjson.a] Error 101
ERROR: compilation failed for package ‘llmjson’
* removing ‘/tmp/workdir/llmjson/new/llmjson.Rcheck/llmjson’


```
### CRAN

```
* installing *source* package ‘llmjson’ ...
** this is package ‘llmjson’ version ‘0.1.0’
** package ‘llmjson’ successfully unpacked and MD5 sums checked
** using staged installation
Using cargo 1.75.0
Using rustc 1.75.0 (82e1608df 2023-12-21) (built from a source tarball)
Building for CRAN.
Writing `src/Makevars`.
`tools/config.R` has finished.
** libs
...
export CARGO_HOME=/tmp/workdir/llmjson/old/llmjson.Rcheck/00_pkg_src/llmjson/src/.cargo && \
export PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/root/.cargo/bin" && \
RUSTFLAGS=" --print=native-static-libs" cargo build -j 2 --offline --lib --release --manifest-path=./rust/Cargo.toml --target-dir ./rust/target
error: package `indexmap v2.12.1` cannot be built because it requires rustc 1.82 or newer, while the currently active rustc version is 1.75.0
Either upgrade to rustc 1.82 or newer, or use
cargo update indexmap@2.12.1 --precise ver
where `ver` is the latest version of `indexmap` supporting rustc 1.75.0
make: *** [Makevars:28: rust/target/release/libllmjson.a] Error 101
ERROR: compilation failed for package ‘llmjson’
* removing ‘/tmp/workdir/llmjson/old/llmjson.Rcheck/llmjson’


```
