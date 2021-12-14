/* LICENSE BEGIN
    This file is part of the SixtyFPS Project -- https://sixtyfps.io
    Copyright (c) 2021 Olivier Goffart <olivier.goffart@sixtyfps.io>
    Copyright (c) 2021 Simon Hausmann <simon.hausmann@sixtyfps.io>

    SPDX-License-Identifier: GPL-3.0-only
    This file is also available under commercial licensing terms.
    Please contact info@sixtyfps.io for more information.
LICENSE END */

use crate::diagnostics::BuildDiagnostics;
use crate::expression_tree::Expression;
use crate::object_tree::*;
use std::rc::Rc;

pub fn embed_glyphs<'a>(
    component: &Rc<Component>,
    all_docs: impl Iterator<Item = &'a crate::object_tree::Document> + 'a,
    diag: &mut BuildDiagnostics,
) {
    if std::env::var("SIXTYFPS_EMBED_GLYPHS").is_err() {
        return;
    }

    let mut fontdb = fontdb::Database::new();
    fontdb.load_system_fonts();

    // add custom fonts
    for doc in all_docs {
        for (font_path, import_token) in doc.custom_fonts.iter() {
            if let Err(e) = fontdb.load_font_file(&font_path) {
                diag.push_error(format!("Error loading font: {}", e), import_token);
            }
        }
    }

    // TODO: improve heuristics in choice of which fonts to embed. use default-font-family, etc.
    let default_family =
        component.root_element.borrow().bindings.get("default-font-family").and_then(|binding| {
            match &binding.borrow().expression {
                Expression::StringLiteral(family) => Some(family.clone()),
                _ => None,
            }
        });

    let family = match default_family.as_ref() {
        Some(family_name) => fontdb::Family::Name(family_name),
        None => fontdb::Family::SansSerif,
    };

    let query = fontdb::Query { families: &[family], ..Default::default() };
    let face_id =
        fontdb.query(&query).expect("internal error: could not match any font for embedding");
    fontdb.with_face_data(face_id, |font_data, face_index| {
        let font = fontdue::Font::from_bytes(
            font_data,
            fontdue::FontSettings { collection_index: face_index, scale: 40. },
        )
        .expect("internal error: fontdb returned a font that ttf-parser/fontdue could not parse");
        embed_font(component, font);
    });
}

fn embed_font(component: &Rc<Component>, font: fontdue::Font) {
    // TODO: configure pixel size and coverage
    // support font_sizes=14,54,32 env

    for codepoint in (b'a'..b'z').map(char::from) {
        let (metrics, bitmap) = font.rasterize(codepoint, 12.);
        // TODO
    }
}
