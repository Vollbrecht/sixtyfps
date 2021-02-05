/* LICENSE BEGIN
    This file is part of the SixtyFPS Project -- https://sixtyfps.io
    Copyright (c) 2020 Olivier Goffart <olivier.goffart@sixtyfps.io>
    Copyright (c) 2020 Simon Hausmann <simon.hausmann@sixtyfps.io>

    SPDX-License-Identifier: GPL-3.0-only
    This file is also available under commercial licensing terms.
    Please contact info@sixtyfps.io for more information.
LICENSE END */
/*!
This module contains path related types and functions for the run-time library.
*/

#[cfg(feature = "rtti")]
use crate::rtti::*;
use const_field_offset::FieldOffsets;
use sixtyfps_corelib_macros::*;

use super::*;

#[repr(C)]
#[derive(FieldOffsets, Default, SixtyFPSElement, Clone, Debug, PartialEq)]
#[pin]
/// PathMoveTo describes the event of setting the cursor on the path to use as starting
/// point for sub-sequent events, such as `LineTo`. Moving the cursor also implicitly closes
/// sub-paths and therefore beings a new sub-path.
pub struct PathMoveTo {
    #[rtti_field]
    /// The x coordinate where the current position should be.
    pub x: f32,
    #[rtti_field]
    /// The y coordinate where the current position should be.
    pub y: f32,
}

#[repr(C)]
#[derive(FieldOffsets, Default, SixtyFPSElement, Clone, Debug, PartialEq)]
#[pin]
/// PathLineTo describes the event of moving the cursor on the path to the specified location
/// along a straight line.
pub struct PathLineTo {
    #[rtti_field]
    /// The x coordinate where the line should go to.
    pub x: f32,
    #[rtti_field]
    /// The y coordinate where the line should go to.
    pub y: f32,
}

#[repr(C)]
#[derive(FieldOffsets, Default, SixtyFPSElement, Clone, Debug, PartialEq)]
#[pin]
/// PathArcTo describes the event of moving the cursor on the path across an arc to the specified
/// x/y coordinates, with the specified x/y radius and additional properties.
pub struct PathArcTo {
    #[rtti_field]
    /// The x coordinate where the arc should end up.
    pub x: f32,
    #[rtti_field]
    /// The y coordinate where the arc should end up.
    pub y: f32,
    #[rtti_field]
    /// The radius on the x-axis of the arc.
    pub radius_x: f32,
    #[rtti_field]
    /// The radius on the y-axis of the arc.
    pub radius_y: f32,
    #[rtti_field]
    /// The rotation along the x-axis of the arc in degress.
    pub x_rotation: f32,
    #[rtti_field]
    /// large_arc indicates whether to take the long or the shorter path to complete the arc.
    pub large_arc: bool,
    #[rtti_field]
    /// sweep indicates the direction of the arc. If true, a clockwise direction is chosen,
    /// otherwise counter-clockwise.
    pub sweep: bool,
}

#[repr(C)]
#[derive(FieldOffsets, Default, SixtyFPSElement, Clone, Debug, PartialEq)]
#[pin]
/// PathCubicTo describes a smooth Bézier curve from the path's current position
/// to the specified x/y location, using two control points.
pub struct PathCubicTo {
    #[rtti_field]
    /// The x coordinate of the curve's end point.
    pub x: f32,
    #[rtti_field]
    /// The y coordinate of the curve's end point.
    pub y: f32,
    #[rtti_field]
    /// The x coordinate of the curve's first control point.
    pub control_1_x: f32,
    #[rtti_field]
    /// The y coordinate of the curve's first control point.
    pub control_1_y: f32,
    #[rtti_field]
    /// The x coordinate of the curve's second control point.
    pub control_2_x: f32,
    #[rtti_field]
    /// The y coordinate of the curve's second control point.
    pub control_2_y: f32,
}

#[repr(C)]
#[derive(FieldOffsets, Default, SixtyFPSElement, Clone, Debug, PartialEq)]
#[pin]
/// PathCubicTo describes a smooth Bézier curve from the path's current position
/// to the specified x/y location, using one control points.
pub struct PathQuadraticTo {
    #[rtti_field]
    /// The x coordinate of the curve's end point.
    pub x: f32,
    #[rtti_field]
    /// The y coordinate of the curve's end point.
    pub y: f32,
    #[rtti_field]
    /// The x coordinate of the curve's control point.
    pub control_x: f32,
    #[rtti_field]
    /// The y coordinate of the curve's control point.
    pub control_y: f32,
}

#[repr(C)]
#[derive(Clone, Debug, PartialEq)]
/// PathElement describes a single element on a path, such as move-to, line-to, etc.
pub enum PathElement {
    /// The MoveTo variant sets the current position on the path.
    MoveTo(PathMoveTo),
    /// The LineTo variant describes a line.
    LineTo(PathLineTo),
    /// The PathArcTo variant describes an arc.
    ArcTo(PathArcTo),
    /// The CubicTo variant describes a Bézier curve with two control points.
    CubicTo(PathCubicTo),
    /// The QuadraticTo variant describes a Bézier curve with one control point.
    QuadraticTo(PathQuadraticTo),
    /// Indicates that the path should be closed now by connecting to the starting point.
    Close,
}

#[repr(C)]
#[derive(Clone, Debug, PartialEq)]
/// PathEvent is a low-level data structure describing the composition of a path. Typically it is
/// generated at compile time from a higher-level description, such as SVG commands.
pub enum PathEvent {
    /// The beginning of the path.
    Begin,
    /// A straight line on the path.
    Line,
    /// A quadratic bezier curve on the path.
    Quadratic,
    /// A cubic bezier curve on the path.
    Cubic,
    /// The end of the path that remains open.
    EndOpen,
    /// The end of a path that is closed.
    EndClosed,
}

struct ToLyonPathEventIterator<'a> {
    events_it: std::slice::Iter<'a, PathEvent>,
    coordinates_it: std::slice::Iter<'a, Point>,
    first: Option<&'a Point>,
    last: Option<&'a Point>,
}

impl<'a> Iterator for ToLyonPathEventIterator<'a> {
    type Item = lyon::path::Event<lyon::math::Point, lyon::math::Point>;
    fn next(&mut self) -> Option<Self::Item> {
        use lyon::path::Event;

        self.events_it.next().map(|event| match event {
            PathEvent::Begin => Event::Begin { at: self.coordinates_it.next().unwrap().clone() },
            PathEvent::Line => Event::Line {
                from: self.coordinates_it.next().unwrap().clone(),
                to: self.coordinates_it.next().unwrap().clone(),
            },
            PathEvent::Quadratic => Event::Quadratic {
                from: self.coordinates_it.next().unwrap().clone(),
                ctrl: self.coordinates_it.next().unwrap().clone(),
                to: self.coordinates_it.next().unwrap().clone(),
            },
            PathEvent::Cubic => Event::Cubic {
                from: self.coordinates_it.next().unwrap().clone(),
                ctrl1: self.coordinates_it.next().unwrap().clone(),
                ctrl2: self.coordinates_it.next().unwrap().clone(),
                to: self.coordinates_it.next().unwrap().clone(),
            },
            PathEvent::EndOpen => Event::End {
                first: self.first.unwrap().clone(),
                last: self.last.unwrap().clone(),
                close: false,
            },
            PathEvent::EndClosed => Event::End {
                first: self.first.unwrap().clone(),
                last: self.last.unwrap().clone(),
                close: true,
            },
        })
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.events_it.size_hint()
    }
}

impl<'a> ExactSizeIterator for ToLyonPathEventIterator<'a> {}

struct TransformedLyonPathIterator<EventIt> {
    it: EventIt,
    transform: lyon::math::Transform,
}

impl<EventIt: Iterator<Item = lyon::path::Event<lyon::math::Point, lyon::math::Point>>> Iterator
    for TransformedLyonPathIterator<EventIt>
{
    type Item = lyon::path::Event<lyon::math::Point, lyon::math::Point>;
    fn next(&mut self) -> Option<Self::Item> {
        self.it.next().map(|ev| ev.transformed(&self.transform))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.it.size_hint()
    }
}

impl<EventIt: Iterator<Item = lyon::path::Event<lyon::math::Point, lyon::math::Point>>>
    ExactSizeIterator for TransformedLyonPathIterator<EventIt>
{
}

/// PathDataIterator is a data structure that acts as starting point for iterating
/// through the low-level events of a path. If the path was constructed from said
/// events, then it is a very thin abstraction. If the path was created from higher-level
/// elements, then an intermediate lyon path is required/built.
pub struct PathDataIterator {
    it: LyonPathIteratorVariant,
    transform: lyon::math::Transform,
}

enum LyonPathIteratorVariant {
    FromPath(lyon::path::Path),
    FromEvents(crate::SharedVector<PathEvent>, crate::SharedVector<Point>),
}

impl PathDataIterator {
    /// Create a new iterator for path traversal.
    #[auto_enum(Iterator)]
    pub fn iter<'a>(
        &'a self,
    ) -> impl Iterator<Item = lyon::path::Event<lyon::math::Point, lyon::math::Point>> + 'a {
        match &self.it {
            LyonPathIteratorVariant::FromPath(path) => {
                TransformedLyonPathIterator { it: path.iter(), transform: self.transform }
            }
            LyonPathIteratorVariant::FromEvents(events, coordinates) => {
                TransformedLyonPathIterator {
                    it: ToLyonPathEventIterator {
                        events_it: events.iter(),
                        coordinates_it: coordinates.iter(),
                        first: coordinates.first(),
                        last: coordinates.last(),
                    },
                    transform: self.transform,
                }
            }
        }
    }

    fn fit(&mut self, width: f32, height: f32) {
        if width > 0. || height > 0. {
            let br = lyon::algorithms::aabb::bounding_rect(self.iter());
            self.transform = lyon::algorithms::fit::fit_rectangle(
                &br,
                &Rect::from_size(Size::new(width, height)),
                lyon::algorithms::fit::FitStyle::Min,
            );
        }
    }
}

#[repr(C)]
#[derive(Clone, Debug, PartialEq)]
/// PathData represents a path described by either high-level elements or low-level
/// events and coordinates.
pub enum PathData {
    /// None is the variant when the path is empty.
    None,
    /// The Elements variant is used to make a Path from shared arrays of elements.
    Elements(crate::SharedVector<PathElement>),
    /// The Events variant describes the path as a series of low-level events and
    /// associated coordinates.
    Events(crate::SharedVector<PathEvent>, crate::SharedVector<Point>),
}

impl Default for PathData {
    fn default() -> Self {
        Self::None
    }
}

impl PathData {
    /// This function returns an iterator that allows traversing the path by means of lyon events.
    pub fn iter(self) -> PathDataIterator {
        PathDataIterator {
            it: match self {
                PathData::None => LyonPathIteratorVariant::FromPath(lyon::path::Path::new()),
                PathData::Elements(elements) => LyonPathIteratorVariant::FromPath(
                    PathData::build_path(elements.as_slice().iter()),
                ),
                PathData::Events(events, coordinates) => {
                    LyonPathIteratorVariant::FromEvents(events, coordinates)
                }
            },
            transform: Default::default(),
        }
    }

    /// This function returns an iterator that allows traversing the path by means of lyon events.
    pub fn iter_fitted(self, width: f32, height: f32) -> PathDataIterator {
        let mut it = self.iter();
        it.fit(width, height);
        it
    }

    fn build_path(element_it: std::slice::Iter<PathElement>) -> lyon::path::Path {
        use lyon::geom::SvgArc;
        use lyon::math::{Angle, Point, Vector};
        use lyon::path::traits::SvgPathBuilder;
        use lyon::path::ArcFlags;

        let mut path_builder = lyon::path::Path::builder().with_svg();
        for element in element_it {
            match element {
                PathElement::MoveTo(PathMoveTo { x, y }) => {
                    path_builder.move_to(Point::new(*x, *y));
                }
                PathElement::LineTo(PathLineTo { x, y }) => {
                    path_builder.line_to(Point::new(*x, *y));
                }
                PathElement::ArcTo(PathArcTo {
                    x,
                    y,
                    radius_x,
                    radius_y,
                    x_rotation,
                    large_arc,
                    sweep,
                }) => {
                    let radii = Vector::new(*radius_x, *radius_y);
                    let x_rotation = Angle::degrees(*x_rotation);
                    let flags = ArcFlags { large_arc: *large_arc, sweep: *sweep };
                    let to = Point::new(*x, *y);

                    let svg_arc = SvgArc {
                        from: path_builder.current_position(),
                        radii,
                        x_rotation,
                        flags,
                        to,
                    };

                    if svg_arc.is_straight_line() {
                        path_builder.line_to(to);
                    } else {
                        path_builder.arc_to(radii, x_rotation, flags, to)
                    }
                }
                PathElement::CubicTo(PathCubicTo {
                    x,
                    y,
                    control_1_x,
                    control_1_y,
                    control_2_x,
                    control_2_y,
                }) => {
                    path_builder.cubic_bezier_to(
                        Point::new(*control_1_x, *control_1_y),
                        Point::new(*control_2_x, *control_2_y),
                        Point::new(*x, *y),
                    );
                }
                PathElement::QuadraticTo(PathQuadraticTo { x, y, control_x, control_y }) => {
                    path_builder.quadratic_bezier_to(
                        Point::new(*control_x, *control_y),
                        Point::new(*x, *y),
                    );
                }
                PathElement::Close => path_builder.close(),
            }
        }

        path_builder.build()
    }
}

pub(crate) mod ffi {
    #![allow(unsafe_code)]

    use super::super::*;
    use super::*;

    #[allow(non_camel_case_types)]
    type c_void = ();

    #[no_mangle]
    /// This function is used for the low-level C++ interface to allocate the backing vector for a shared path element array.
    pub unsafe extern "C" fn sixtyfps_new_path_elements(
        out: *mut c_void,
        first_element: *const PathElement,
        count: usize,
    ) {
        let arr = crate::SharedVector::from(std::slice::from_raw_parts(first_element, count));
        core::ptr::write(out as *mut crate::SharedVector<PathElement>, arr.clone());
    }

    #[no_mangle]
    /// This function is used for the low-level C++ interface to allocate the backing vector for a shared path event array.
    pub unsafe extern "C" fn sixtyfps_new_path_events(
        out_events: *mut c_void,
        out_coordinates: *mut c_void,
        first_event: *const PathEvent,
        event_count: usize,
        first_coordinate: *const Point,
        coordinate_count: usize,
    ) {
        let events =
            crate::SharedVector::from(std::slice::from_raw_parts(first_event, event_count));
        core::ptr::write(out_events as *mut crate::SharedVector<PathEvent>, events.clone());
        let coordinates = crate::SharedVector::from(std::slice::from_raw_parts(
            first_coordinate,
            coordinate_count,
        ));
        core::ptr::write(out_coordinates as *mut crate::SharedVector<Point>, coordinates.clone());
    }
}