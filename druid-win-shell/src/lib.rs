// Copyright 2018 The xi-editor Authors.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

//! Windows-specific application shell used for xi editor.

#[macro_use]
extern crate winapi;
extern crate direct2d;
extern crate wio;
#[macro_use]
extern crate lazy_static;

mod dcomp;
pub mod dialog;
pub mod menu;
pub mod paint;
pub mod tray;
pub mod util;
pub mod win_main;
pub mod window;

pub use util::Error;

pub use util::init;
