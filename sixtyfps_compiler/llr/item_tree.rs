/* LICENSE BEGIN
    This file is part of the SixtyFPS Project -- https://sixtyfps.io
    Copyright (c) 2021 Olivier Goffart <olivier.goffart@sixtyfps.io>
    Copyright (c) 2021 Simon Hausmann <simon.hausmann@sixtyfps.io>

    SPDX-License-Identifier: GPL-3.0-only
    This file is also available under commercial licensing terms.
    Please contact info@sixtyfps.io for more information.
LICENSE END */

use std::collections::HashMap;
use std::num::NonZeroUsize;
use std::rc::Rc;

use crate::langtype::Type;

use super::Expression;

// Index in the `SubComponent::properties`
pub type PropertyIndex = usize;

#[derive(Debug)]
pub enum Animation {
    /// The expression is a Struct with the animation fields
    Static(Expression),
    Transition(Expression),
}

#[derive(Debug)]
pub struct BindingExpression {
    pub expression: Expression,
    pub animation: Option<Animation>,
}

#[derive(Debug)]
pub struct GlobalComponent {
    pub name: String,
    pub properties: Vec<Property>,
    pub init_values: Vec<Option<Expression>>,
}

/// a Reference to a property, in the context of a SubComponent
#[derive(Clone, Debug)]
pub enum PropertyReference {
    /// A property relative to this SubComponent
    Local { sub_component_path: Vec<usize>, property_index: PropertyIndex },
    /// A property in a Native item
    InNativeItem { sub_component_path: Vec<usize>, item_index: usize, prop_name: String },
    /// The properties is a property relative to a parent ItemTree (`level` level deep)
    InParent { level: NonZeroUsize, parent_reference: Box<PropertyReference> },
    /// The property within a GlobalComponent
    Global { global_index: usize, property_index: usize },
}

#[derive(Debug)]
pub struct Property {
    pub name: String,
    pub ty: Type,
    //pub binding: Option<BindingExpression>,
}

#[derive(Debug)]
pub struct RepeatedElement {
    pub model: Expression,
    /// Within the sub_tree's root component
    pub index_prop: PropertyIndex,
    /// Within the sub_tree's root component
    pub data_prop: PropertyIndex,
    pub sub_tree: ItemTree,
}

#[derive(Debug)]
pub struct ItemType {
    // cpp_name: String,
// rust_name: String,
// cpp_init_function: String,
// mouse_function: String,
// extra_data_type: String,
}

#[derive(Debug)]
pub struct Item {
    pub ty: Rc<ItemType>,
}

#[derive(Debug)]
pub struct TreeNode {
    pub sub_component_path: Vec<usize>,
    pub item_index: usize,
    pub repeated: bool,
    pub children: Vec<TreeNode>,
}

#[derive(Debug)]
pub struct SubComponent {
    pub name: String,
    pub properties: Vec<Property>,
    pub items: Vec<Item>,
    pub repeated: Vec<RepeatedElement>,
    pub popup_windows: Vec<ItemTree>,
    pub sub_components: Vec<SubComponentInstance>,
    pub property_init: Vec<(PropertyReference, BindingExpression)>,
    pub two_way_bindings: Vec<(PropertyReference, PropertyReference)>,
}

pub struct SubComponentInstance {
    pub ty: Rc<SubComponent>,
    //pub property_values: Vec<(PropertyReference, BindingExpression)>,
}

impl std::fmt::Debug for SubComponentInstance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.ty.name)
    }
}

#[derive(Debug)]
pub struct ItemTree {
    pub root: SubComponent,
    pub tree: TreeNode,
    /// This tree has a parent. e.g: it is a Repeater or a PopupMenu whose property can access
    /// the parent ItemTree.
    /// The String is the type of the parent ItemTree
    pub parent_context: Option<String>,
}

#[derive(Debug)]
pub struct PublicComponent {
    pub item_tree: ItemTree,
    pub sub_components: HashMap<String, Rc<SubComponent>>,
    pub globals: Vec<GlobalComponent>,
}
