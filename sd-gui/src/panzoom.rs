use eframe::epaint::{Pos2, Vec2};

const ZOOM_FACTOR: f32 = 1.25;

pub struct Panzoom {
    pub translation: Pos2,
    pub zoom: f32,
}

impl Default for Panzoom {
    fn default() -> Self {
        Self {
            translation: Pos2::default(),
            zoom: 50.0,
        }
    }
}

impl Panzoom {
    pub fn reset(&mut self, size: Vec2) {
        self.translation = (size / 2.0).to_pos2();
        self.zoom = 50.0;
    }

    pub fn zoom_in(&mut self) {
        self.zoom *= ZOOM_FACTOR;
    }

    pub fn zoom_out(&mut self) {
        self.zoom /= ZOOM_FACTOR;
    }
}
