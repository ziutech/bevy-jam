use bevy::{app::App, asset::embedded_asset};

pub fn setup(app: &mut App) {
    embedded_asset!(app, "src/", "plus.png");
    embedded_asset!(app, "src/", "dot.png");
    embedded_asset!(app, "src/", "cross.png");
}
