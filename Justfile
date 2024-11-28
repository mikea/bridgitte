DDS_HOME := "$HOME/src/github.com/dds-bridge/dds"

alias w := watch

watch +WATCH_TARGET='run':
    watchexec -rc -w . --ignore *.results -- just {{WATCH_TARGET}}

run:
    cargo build -r
    # bermuda hand
    time target/release/bridgitte analyze-deal --declarer e --strain d --stats "854.Q873.Q984.65 KQ32.T6.T72.AJ93 9.AJ542.J653.T87 AJT76.K9.AK.KQ42"

clippy:
    cargo clippy -- -D clippy::pedantic -A clippy::missing-panics-doc -A clippy::module_name_repetitions

build:
    cargo build

test:
    cargo test
    cargo test -r

clean:
    rm -rf target

bench: test
    cargo bench

callgrind: build-valgrind
    rm -f callgrind.out.* cachegrind.out.*
    valgrind --tool=callgrind --dump-instr=yes --collect-jumps=yes target/release/bridgitte analyze-deal --declarer N --strain S  "854.Q873.Q984.65 KQ32.T6.T72.AJ93 9.AJ542.J653.T87 AJT76.K9.AK.KQ42"

cachegrind: build-valgrind
    rm -f callgrind.out.* cachegrind.out.*
    valgrind --tool=cachegrind target/release/bridgitte analyze-deal --declarer N --strain S  "854.Q873.Q984.65 KQ32.T6.T72.AJ93 9.AJ542.J653.T87 AJT76.K9.AK.KQ42"

build-valgrind:
    # use build-std to get nice symbols
    cargo +nightly build -r -Z build-std --target x86_64-unknown-linux-gnu

test-suite:
    cargo build -r && time cargo run -r test {{DDS_HOME}}/hands/list10.txt

doc:
    cargo doc

fmt:
    cargo fmt