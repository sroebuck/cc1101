Note:  This is a fork of [dsvensson/cc1101](https://github.com/dsvensson/cc1101) set up to try to figure out how to use the cc1101 for receiving ASK/OOK coded FS20 messages from FHT protocol radiator controllers.  See <https://sourceforge.net/p/opentrv/wiki/FHT%20Protocol/>.

The code changes are currently a mess - reflecting the trial and error nature of just figuring out how things work.  I hope to return to this to tidy it up and them submit a pull request.

For what it's worth, this takes the original library and adds some additional control functions around features of the cc1101 required for ASK/OOK as well as adding quite a lot of functionality for reading and reporting the current settings of the cc1101 chip to help with debugging what is happening.

The following comment is the original comment from library pre-fork:

# `cc1101`

> A platform agnostic driver to interface with the CC1101 (Sub-1GHz RF Transceiver)   

<a href="https://travis-ci.org/dsvensson/cc1101" alt="Build Status">
        <img src="https://travis-ci.org/dsvensson/cc1101.svg?branch=master" /></a>
<a href="https://crates.io/crates/cc1101" alt="Crates.io">
        <img src="https://img.shields.io/crates/v/cc1101.svg" /></a>
<a href="https://tldrlegal.com/license/apache-license-2.0-(apache-2.0)" alt="License">
        <img src="https://img.shields.io/crates/l/cc1101.svg" /></a>

## What works

- Configuring radio for reception, and reading data.

## TODO

- [ ] Sending data, but only have one module.

## License

Apache License, Version 2.0 ([LICENSE](LICENSE) or http://www.apache.org/licenses/LICENSE-2.0)

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in the
work by you, as defined in the Apache-2.0 license, shall be licensed as above, without any
additional terms or conditions.
