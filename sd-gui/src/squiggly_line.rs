use eframe::{
    egui::{self, show_tooltip_at_pointer, text_edit::TextEditOutput, Id, RichText},
    epaint::{Color32, FontId, Pos2, QuadraticBezierShape, Stroke},
};
use pest::{
    error::{Error, LineColLocation},
    RuleType,
};

fn is_in_line(cursor: usize, line_col: &LineColLocation) -> bool {
    // Pest lines are 1 indexed, egui are 0 ☹
    match line_col {
        LineColLocation::Pos((l, _)) => l - 1 == cursor,
        LineColLocation::Span((l1, _), (l2, _)) => l1 - 1 <= cursor && l2 - 1 <= cursor,
    }
}

fn lines_contained(line_col: &LineColLocation) -> Vec<usize> {
    match line_col {
        LineColLocation::Pos((l, _)) => vec![l - 1],
        LineColLocation::Span((l1, _), (l2, _)) => (*l1..=*l2).collect(),
    }
}

pub fn show_parse_error(ui: &egui::Ui, err: &Error<impl RuleType>, text_edit_out: &TextEditOutput) {
    let painter = ui.painter();
    for l in lines_contained(&err.line_col) {
        if let Some(row) = text_edit_out.galley.rows.get(l) {
            // Draw squiggly line under error line
            const SQUIGGLE_HEIGHT: f32 = 5.0;
            const SQUIGGLES_PER_CHAR: usize = 3;
            let left = row.rect.min.x;
            let right = row.rect.max.x;
            let base = row.rect.max.y;
            let count = u16::try_from(row.glyphs.len() * SQUIGGLES_PER_CHAR).unwrap();
            // Takes weighted average of 'left' and 'right' where
            // 0 <= i <= count
            let w_avg = |i: f32| {
                let count_f = f32::from(count);
                (left * (count_f - i) + right * i) / count_f
            };
            for i in 0..count {
                let start: Pos2 =
                    Pos2::from((w_avg(f32::from(i)), base)) + text_edit_out.text_draw_pos.to_vec2();
                let control: Pos2 = Pos2::from((
                    w_avg(f32::from(i) + 0.5),
                    base + SQUIGGLE_HEIGHT * (f32::from((i + 1) % 2) - 0.5),
                )) + text_edit_out.text_draw_pos.to_vec2();
                let end: Pos2 = Pos2::from((w_avg(f32::from(i + 1)), base))
                    + text_edit_out.text_draw_pos.to_vec2();
                painter.add(QuadraticBezierShape {
                    points: [start, control, end],
                    closed: false,
                    fill: Color32::TRANSPARENT,
                    stroke: Stroke::new(1.0, ui.style().visuals.error_fg_color),
                });
            }
        }
    }

    if let Some(x) = text_edit_out.response.hover_pos() {
        let pos = x - text_edit_out.text_draw_pos;

        if text_edit_out.galley.rect.contains((pos.x, pos.y).into()) {
            let cursor = text_edit_out.galley.cursor_from_pos(pos);
            if is_in_line(cursor.rcursor.row, &err.line_col) {
                show_tooltip_at_pointer(ui.ctx(), Id::new("hover_tooltip"), |ui| {
                    ui.label(RichText::new(err.to_string()).font(FontId::monospace(13.5)))
                });
            }
        }
    }
}
