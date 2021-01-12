/* LICENSE BEGIN
    This file is part of the SixtyFPS Project -- https://sixtyfps.io
    Copyright (c) 2020 Olivier Goffart <olivier.goffart@sixtyfps.io>
    Copyright (c) 2020 Simon Hausmann <simon.hausmann@sixtyfps.io>

    SPDX-License-Identifier: GPL-3.0-only
    This file is also available under commercial licensing terms.
    Please contact info@sixtyfps.io for more information.
LICENSE END */
use super::CanvasRc;
use sixtyfps_corelib::graphics::FontRequest;
use std::cell::RefCell;

thread_local! {
    /// Database used to keep track of fonts added by the application
    static APPLICATION_FONTS: RefCell<fontdb::Database> = RefCell::new(fontdb::Database::new())
}

/// This function can be used to register a custom TrueType font with SixtyFPS,
/// for use with the `font-family` property. The provided slice must be a valid TrueType
/// font.
pub fn register_application_font_from_memory(
    data: &'static [u8],
) -> Result<(), Box<dyn std::error::Error>> {
    APPLICATION_FONTS.with(|fontdb| fontdb.borrow_mut().load_font_data(data.into()));
    Ok(())
}

pub(crate) fn try_load_app_font(
    canvas: &CanvasRc,
    request: &FontRequest,
) -> Option<femtovg::FontId> {
    let family = if request.family.is_empty() {
        fontdb::Family::SansSerif
    } else {
        fontdb::Family::Name(&request.family)
    };

    let query = fontdb::Query {
        families: &[family],
        weight: fontdb::Weight(request.weight.unwrap() as u16),
        ..Default::default()
    };
    APPLICATION_FONTS.with(|font_db| {
        let font_db = font_db.borrow();
        font_db.query(&query).and_then(|id| font_db.face_source(id)).map(|(source, _index)| {
            // pass index to femtovg once femtovg/femtovg/pull/21 is merged
            match source.as_ref() {
                fontdb::Source::Binary(data) => canvas.borrow_mut().add_font_mem(&data).unwrap(),
                #[cfg(not(target_arch = "wasm32"))]
                fontdb::Source::File(path) => canvas.borrow_mut().add_font(path).unwrap(),
            }
        })
    })
}

pub(crate) fn load_system_font(canvas: &CanvasRc, request: &FontRequest) -> femtovg::FontId {
    let family_name = if request.family.len() == 0 {
        font_kit::family_name::FamilyName::SansSerif
    } else {
        font_kit::family_name::FamilyName::Title(request.family.to_string())
    };

    let handle = font_kit::source::SystemSource::new()
        .select_best_match(
            &[family_name, font_kit::family_name::FamilyName::SansSerif],
            &font_kit::properties::Properties::new()
                .weight(font_kit::properties::Weight(request.weight.unwrap() as f32)),
        )
        .unwrap();

    // pass index to femtovg once femtovg/femtovg/pull/21 is merged
    match handle {
        font_kit::handle::Handle::Path { path, font_index: _ } => {
            canvas.borrow_mut().add_font(path)
        }
        font_kit::handle::Handle::Memory { bytes, font_index: _ } => {
            canvas.borrow_mut().add_font_mem(bytes.as_slice())
        }
    }
    .unwrap()
}