use std::{
    io,
    ops::{Deref, DerefMut},
};

use anyhow::Result;
use crossterm::{
    cursor,
    event::{DisableMouseCapture, EnableMouseCapture, Event as CrosstermEvent, KeyEventKind},
    terminal::{disable_raw_mode, is_raw_mode_enabled, EnterAlternateScreen, LeaveAlternateScreen},
};
use futures::{FutureExt, StreamExt};
use ratatui::prelude::*;
use tokio::{
    sync::mpsc::{self, UnboundedReceiver, UnboundedSender},
    task::JoinHandle,
};

pub struct Terminal {
    terminal: ratatui::Terminal<CrosstermBackend<std::io::Stdout>>,
    task: JoinHandle<Result<()>>,
    event_rx: UnboundedReceiver<CrosstermEvent>,
    event_tx: UnboundedSender<CrosstermEvent>,
}

impl Deref for Terminal {
    type Target = ratatui::Terminal<CrosstermBackend<std::io::Stdout>>;

    fn deref(&self) -> &Self::Target {
        &self.terminal
    }
}

impl DerefMut for Terminal {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.terminal
    }
}

impl Terminal {
    pub fn new() -> Result<Self> {
        let ratatui_terminal = ratatui::Terminal::new(CrosstermBackend::new(std::io::stdout()))?;
        let (event_tx, event_rx) = mpsc::unbounded_channel();
        let task = tokio::task::spawn(async { Ok(()) });
        Ok(Self {
            terminal: ratatui_terminal,
            task,
            event_rx,
            event_tx,
        })
    }

    pub fn init(&mut self) -> Result<()> {
        crossterm::terminal::enable_raw_mode()?;
        crossterm::execute!(
            std::io::stdout(),
            EnterAlternateScreen,
            cursor::Hide,
            EnableMouseCapture
        )?;
        let event_tx = self.event_tx.clone();

        self.task = tokio::task::spawn(async move {
            let mut reader = crossterm::event::EventStream::new();
            loop {
                let crossterm_event = reader.next().fuse();
                tokio::select! {
                    event = crossterm_event => Self::handle_crossterm_event(event, &event_tx)?,
                }
            }
        });

        Ok(())
    }

    pub fn exit(&mut self) -> Result<()> {
        self.task.abort();

        if is_raw_mode_enabled()? {
            self.terminal.flush()?;
            crossterm::execute!(
                std::io::stdout(),
                LeaveAlternateScreen,
                cursor::Show,
                DisableMouseCapture
            )?;
            disable_raw_mode()?;
        }

        Ok(())
    }

    fn handle_crossterm_event(
        event: Option<Result<CrosstermEvent, io::Error>>,
        event_tx: &UnboundedSender<CrosstermEvent>,
    ) -> Result<()> {
        match event {
            Some(Ok(CrosstermEvent::Key(key))) => {
                if key.kind == KeyEventKind::Press {
                    event_tx.send(CrosstermEvent::Key(key))?;
                }
            }
            Some(Ok(event)) => event_tx.send(event)?,
            Some(Err(e)) => Err(e)?,
            _ => (),
        }
        Ok(())
    }

    pub async fn next(&mut self) -> Option<CrosstermEvent> {
        self.event_rx.recv().await
    }
}
