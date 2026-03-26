use ratatui::layout::{Constraint, Layout};
use ratatui::prelude::*;

pub fn centered_rect(r: Rect, percent_x: u16, percent_y: u16) -> Rect {
    let popup_layout = Layout::vertical([
        Constraint::Percentage((100 - percent_y) / 2),
        Constraint::Percentage(percent_y),
        Constraint::Percentage((100 - percent_y) / 2),
    ])
    .split(r);

    Layout::horizontal([
        Constraint::Percentage((100 - percent_x) / 2),
        Constraint::Percentage(percent_x),
        Constraint::Percentage((100 - percent_x) / 2),
    ])
    .split(popup_layout[1])[1]
}

pub fn popup_rects(rect: Rect, percent_x: u16, percent_y: u16) -> (Rect, Rect, Rect) {
    let popup_rect = centered_rect(rect, percent_x, percent_y);
    let block_rect = popup_rect.inner(Margin::new(1, 1));
    let text_rect = block_rect.inner(Margin::new(3, 2));
    (popup_rect, block_rect, text_rect)
}
