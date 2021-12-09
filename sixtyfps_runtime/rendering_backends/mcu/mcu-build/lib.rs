/* LICENSE BEGIN
    This file is part of the SixtyFPS Project -- https://sixtyfps.io
    Copyright (c) 2021 Olivier Goffart <olivier.goffart@sixtyfps.io>
    Copyright (c) 2021 Simon Hausmann <simon.hausmann@sixtyfps.io>

    SPDX-License-Identifier: GPL-3.0-only
    This file is also available under commercial licensing terms.
    Please contact info@sixtyfps.io for more information.
LICENSE END */

pub fn configure_linker() {
    #[cfg(feature = "pico-st7789")]
    {
        println!("cargo:rustc-link-arg=--nmagic");
        println!("cargo:rustc-link-arg=-Tlink.x");
        println!("cargo:rustc-link-arg=-Tdefmt.x");
        let memory_x_path: std::path::PathBuf =
            [env!("CARGO_MANIFEST_DIR"), "pico_st7789"].iter().collect();
        println!("cargo:rustc-link-search={}", memory_x_path.to_string_lossy());
    }
}
