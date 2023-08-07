use eframe::{
    emath::RectTransform,
    epaint::{vec2, Pos2, Rect, Vec2},
};

const PAN_FACTOR: f32 = 10.0;
const ZOOM_FACTOR: f32 = 1.25;

#[derive(Copy, Clone, Debug)]
pub struct Panzoom {
    translation: Pos2,
    zoom: f32,
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
    /// Construct a `RectTransform` that applies panzoom.
    pub fn transform(self, screen: Rect) -> RectTransform {
        let from = Rect::from_center_size(self.translation, screen.size() / self.zoom);
        RectTransform::from_to(from, screen)
    }

    /// Pan to the center and reset the zoom.
    pub fn reset(&mut self, size: Vec2, screen_size: Vec2) {
        self.translation = (size / 2.0).to_pos2();
        self.zoom = [
            50.0,
            screen_size.x / (size.x + 2.0),
            screen_size.y / (size.y + 2.0),
        ]
        .into_iter()
        .min_by(|x, y| x.partial_cmp(y).unwrap())
        .unwrap();
    }

    /// Pan by a vector (in screen coordinates).
    pub fn pan(&mut self, delta: Vec2) {
        self.translation -= delta / self.zoom;
    }

    /// Pan up by a constant amount.
    pub fn pan_up(&mut self) {
        self.pan(vec2(0.0, -PAN_FACTOR));
    }

    /// Pan down by a constant amount.
    pub fn pan_down(&mut self) {
        self.pan(vec2(0.0, PAN_FACTOR));
    }

    /// Pan left by a constant amount.
    pub fn pan_left(&mut self) {
        self.pan(vec2(-PAN_FACTOR, 0.0));
    }

    /// Pan right by a constant amount.
    pub fn pan_right(&mut self) {
        self.pan(vec2(PAN_FACTOR, 0.0));
    }

    /// Pan to location
    pub fn set_pan(&mut self, center: Pos2) {
        self.translation = center;
    }

    /// Zoom by a relative factor with the given anchor.
    pub fn zoom(&mut self, zoom_delta: f32, anchor: Pos2) {
        self.translation = anchor + (self.translation - anchor) / zoom_delta;
        self.zoom *= zoom_delta;
    }

    /// Zoom in by a constant factor.
    pub fn zoom_in(&mut self) {
        self.zoom(ZOOM_FACTOR, self.translation);
    }

    /// Zoom out by a constant factor.
    pub fn zoom_out(&mut self) {
        self.zoom(ZOOM_FACTOR.recip(), self.translation);
    }
}
