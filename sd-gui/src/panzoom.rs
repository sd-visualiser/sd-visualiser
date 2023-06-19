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

    pub fn zoom(&mut self, zoom_delta: f32, anchor: Pos2) {
        self.translation += (anchor - self.translation) * (zoom_delta - 1.0);
        self.zoom *= zoom_delta;
    }

    pub fn zoom_in(&mut self) {
        self.zoom(ZOOM_FACTOR, self.translation);
    }

    pub fn zoom_out(&mut self) {
        self.zoom(ZOOM_FACTOR.recip(), self.translation);
    }
}
