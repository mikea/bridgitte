use ratatui::{buffer::Buffer, layout::Rect, widgets::Sparkline};

use ratatui::prelude::Widget;

pub(crate) fn render_sparkline(stats: &[u64]) -> String {
    let width = u16::try_from(stats.len()).unwrap();
    let mut buffer = Buffer::empty(Rect::new(0, 0, width, 1));
    let sparkline = Sparkline::default().data(stats);
    sparkline.render(*buffer.area(), &mut buffer);
    let mut result = String::new();
    for i in 0..width {
        result.push_str(buffer.cell((i, 0)).unwrap().symbol());
    }

    result
}
