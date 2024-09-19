use std::{
    borrow::Cow,
    collections::{BTreeMap, HashMap},
    marker::PhantomData,
};

use crossterm::event::{KeyCode, KeyModifiers, MediaKeyCode, ModifierKeyCode};
use serde::{
    de::{self, Visitor},
    Deserialize, Serialize,
};

use crate::user_action::UserAction;

#[derive(Clone)]
pub struct KeybindsHolder<T: UserAction, Action: From<T> = T> {
    pub keybindings: Vec<Keybinding<T>>,
    pub map: HashMap<(KeyCode, KeyModifiers), Action>,
}

#[derive(Clone)]
pub struct Keybinding<T> {
    pub on: KeyCode,
    pub modifier: KeyModifier,
    pub action: T,
    pub show_in_help: bool,
}

impl<'de, T: Deserialize<'de> + UserAction, Action: From<T>> Deserialize<'de>
    for KeybindsHolder<T, Action>
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: de::Deserializer<'de>,
    {
        #[derive(Deserialize)]
        #[serde(field_identifier, rename_all = "lowercase")]
        enum Field {
            Keybindings,
        }

        struct KeybindsHolderVisitor<T: UserAction, A: From<T>> {
            phantom1: PhantomData<T>,
            phantom2: PhantomData<A>,
        }

        impl<'de, T: Deserialize<'de> + UserAction, Action: From<T>> Visitor<'de>
            for KeybindsHolderVisitor<T, Action>
        {
            type Value = KeybindsHolder<T, Action>;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("struct KeybindsHolder")
            }

            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: de::MapAccess<'de>,
            {
                let mut keybindings: Option<Vec<Keybinding<T>>> = None;

                while let Some(key) = map.next_key()? {
                    match key {
                        Field::Keybindings => {
                            if keybindings.is_some() {
                                return Err(de::Error::duplicate_field("keybindings"));
                            }
                            keybindings = Some(map.next_value()?);
                        }
                    }
                }

                let keybindings =
                    keybindings.ok_or_else(|| de::Error::missing_field("keybindings"))?;

                let mut map = HashMap::new();

                for keybinding in &keybindings {
                    map.insert(
                        (keybinding.on, keybinding.modifier.into()),
                        keybinding.action.into(),
                    );
                }

                Ok(KeybindsHolder { keybindings, map })
            }
        }

        const FIELDS: &[&str] = &["keybindings"];
        deserializer.deserialize_struct(
            "KeybindsHolder",
            FIELDS,
            KeybindsHolderVisitor {
                phantom1: PhantomData,
                phantom2: PhantomData,
            },
        )
    }
}

impl<T> Keybinding<T> {
    fn new(modifier: Option<KeyModifier>, on: KeyCode, action: T, show_in_help: bool) -> Self {
        Self {
            on,
            modifier: modifier.unwrap_or(KeyModifier::None),
            action,
            show_in_help,
        }
    }
}

impl<'de, T: Deserialize<'de>> Deserialize<'de> for Keybinding<T> {
    fn deserialize<D>(deserializer: D) -> std::prelude::v1::Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        #[derive(Deserialize)]
        #[serde(field_identifier, rename_all = "snake_case")]
        enum Field {
            On,
            Modifier,
            Action,
            ShowInHelp,
        }

        struct KeybindingVisitor<T> {
            phantom: PhantomData<T>,
        }

        impl<'de, T: Deserialize<'de>> Visitor<'de> for KeybindingVisitor<T> {
            type Value = Keybinding<T>;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("struct Keybinding")
            }

            fn visit_map<A>(self, mut map: A) -> std::prelude::v1::Result<Self::Value, A::Error>
            where
                A: serde::de::MapAccess<'de>,
            {
                let mut on = None;
                let mut modifier = None;
                let mut action = None;
                let mut show_in_help = None;
                while let Some(key) = map.next_key()? {
                    match key {
                        Field::On => {
                            if on.is_some() {
                                return Err(de::Error::duplicate_field("on"));
                            }
                            let key = map.next_value::<String>()?;

                            if key.len() == 1 {
                                on = Some(KeyCode::Char(key.chars().next().unwrap()));
                            } else if key.starts_with('F') && (key.len() == 2 || key.len() == 3) {
                                let which_f = key[1..].parse::<u8>().map_err(|_| {
                                    de::Error::invalid_value(
                                        de::Unexpected::Str(&key),
                                        &"something_correct",
                                    )
                                })?;
                                on = Some(KeyCode::F(which_f));
                            } else {
                                on = {
                                    match key.to_lowercase().as_str() {
                                        "enter" => Some(KeyCode::Enter),
                                        "esc" => Some(KeyCode::Esc),
                                        "up" => Some(KeyCode::Up),
                                        "down" => Some(KeyCode::Down),
                                        "left" => Some(KeyCode::Left),
                                        "right" => Some(KeyCode::Right),
                                        "home" => Some(KeyCode::Home),
                                        "end" => Some(KeyCode::End),
                                        "pageup" => Some(KeyCode::PageUp),
                                        "pagedown" => Some(KeyCode::PageDown),
                                        "tab" => Some(KeyCode::Tab),
                                        "backspace" => Some(KeyCode::Backspace),
                                        "delete" => Some(KeyCode::Delete),

                                        _ => {
                                            return Err(de::Error::invalid_value(
                                                de::Unexpected::Str(&key),
                                                &"something correct",
                                            ))
                                        }
                                    }
                                };
                            }
                        }
                        Field::Modifier => {
                            if modifier.is_some() {
                                return Err(de::Error::duplicate_field("modifier"));
                            }
                            modifier = Some(map.next_value());
                        }
                        Field::Action => {
                            if action.is_some() {
                                return Err(de::Error::duplicate_field("action"));
                            }
                            action = Some(map.next_value());
                        }
                        Field::ShowInHelp => {
                            if show_in_help.is_some() {
                                return Err(de::Error::duplicate_field("action"));
                            }
                            show_in_help = Some(map.next_value());
                        }
                    }
                }
                let on = on.ok_or_else(|| de::Error::missing_field("on"))?;
                let action = action.ok_or_else(|| de::Error::missing_field("action"))??;
                let modifier = modifier.transpose().unwrap();
                let show_in_help = show_in_help.transpose().unwrap().unwrap_or(true);

                if modifier.is_some() {
                    if let KeyCode::Char(char) = on {
                        if char.is_uppercase() {
                            return Err(de::Error::custom(
                                "you can't have a modifier with an uppercase letter, sorry",
                            ));
                        }
                    }
                }

                Ok(Keybinding::new(modifier, on, action, show_in_help))
            }
        }

        const FIELDS: &[&str] = &["on", "modifier", "action"];
        deserializer.deserialize_struct(
            "Keybinding",
            FIELDS,
            KeybindingVisitor {
                phantom: PhantomData,
            },
        )
    }
}

impl<T> Keybinding<T> {
    pub fn keycode_string_with_override(
        &self,
        override_fn: impl Fn(KeyCode) -> Option<Cow<'static, str>>,
    ) -> Cow<'static, str> {
        if let Some(key) = override_fn(self.on) {
            if !self.modifier.is_none() {
                format!("{}-{key}", self.modifier.to_str()).into()
            } else {
                key
            }
        } else {
            self.keycode_string()
        }
    }

    pub fn keycode_string(&self) -> Cow<'static, str> {
        let key = match self.on {
            KeyCode::Backspace => "Backspace".into(),
            KeyCode::Enter => "Enter".into(),
            KeyCode::Left => "Left".into(),
            KeyCode::Right => "Right".into(),
            KeyCode::Up => "Up".into(),
            KeyCode::Down => "Down".into(),
            KeyCode::Home => "Home".into(),
            KeyCode::End => "End".into(),
            KeyCode::PageUp => "PageUp".into(),
            KeyCode::PageDown => "PageDown".into(),
            KeyCode::Tab => "Tab".into(),
            KeyCode::BackTab => "BackTab".into(),
            KeyCode::Delete => "Delete".into(),
            KeyCode::Insert => "Insert".into(),
            KeyCode::F(i) => format!("F{i}").into(),
            KeyCode::Char(c) => {
                if c == ' ' {
                    Cow::Borrowed("Space")
                } else {
                    Cow::Owned(c.to_string())
                }
            }
            KeyCode::Null => "Null".into(),
            KeyCode::Esc => "Esc".into(),
            KeyCode::CapsLock => "CapsLock".into(),
            KeyCode::ScrollLock => "ScrollLock".into(),
            KeyCode::NumLock => "NumLock".into(),
            KeyCode::PrintScreen => "PrintScreen".into(),
            KeyCode::Pause => "Pause".into(),
            KeyCode::Menu => "Menu".into(),
            KeyCode::KeypadBegin => "KeypadBegin".into(),
            KeyCode::Media(media) => match media {
                MediaKeyCode::Play => "Play",
                MediaKeyCode::Pause => "Pause",
                MediaKeyCode::PlayPause => "PlayPause",
                MediaKeyCode::Reverse => "Reverse",
                MediaKeyCode::Stop => "Stop",
                MediaKeyCode::FastForward => "FastForward",
                MediaKeyCode::Rewind => "Rewind",
                MediaKeyCode::TrackNext => "TrackNext",
                MediaKeyCode::TrackPrevious => "TrackPrevious",
                MediaKeyCode::Record => "Record",
                MediaKeyCode::LowerVolume => "LowerVolume",
                MediaKeyCode::RaiseVolume => "RaiseVolume",
                MediaKeyCode::MuteVolume => "MuteVolume",
            }
            .into(),
            KeyCode::Modifier(modifier) => match modifier {
                ModifierKeyCode::LeftShift => "LeftShift",
                ModifierKeyCode::LeftControl => "LeftControl",
                ModifierKeyCode::LeftAlt => "LeftAlt",
                ModifierKeyCode::LeftSuper => "LeftSuper",
                ModifierKeyCode::LeftHyper => "LeftHyper",
                ModifierKeyCode::LeftMeta => "LeftMeta",
                ModifierKeyCode::RightShift => "RightShift",
                ModifierKeyCode::RightControl => "RightControl",
                ModifierKeyCode::RightAlt => "RightAlt",
                ModifierKeyCode::RightSuper => "RightSuper",
                ModifierKeyCode::RightHyper => "RightHyper",
                ModifierKeyCode::RightMeta => "RightMeta",
                ModifierKeyCode::IsoLevel3Shift => "IsoLevel3Shift",
                ModifierKeyCode::IsoLevel5Shift => "IsoLevel5Shift",
            }
            .into(),
        }
        .into();

        if !self.modifier.is_none() {
            format!("{}-{key}", self.modifier.to_str()).into()
        } else {
            key
        }
    }
}

#[derive(Serialize, Deserialize, Hash, Clone, Copy, PartialEq, Eq, Debug)]
pub enum KeyModifier {
    None,
    Ctrl,
    Shift,
    Alt,
    Super,
    Meta,
}

impl From<KeyModifier> for KeyModifiers {
    fn from(value: KeyModifier) -> Self {
        match value {
            KeyModifier::None => KeyModifiers::NONE,
            KeyModifier::Ctrl => KeyModifiers::CONTROL,
            KeyModifier::Shift => KeyModifiers::SHIFT,
            KeyModifier::Alt => KeyModifiers::ALT,
            KeyModifier::Super => KeyModifiers::SUPER,
            KeyModifier::Meta => KeyModifiers::META,
        }
    }
}

impl KeyModifier {
    fn to_str(self) -> &'static str {
        match self {
            KeyModifier::None => "",
            KeyModifier::Ctrl => "CTRL",
            KeyModifier::Shift => "SHIFT",
            KeyModifier::Alt => "ALT",
            KeyModifier::Super => "SUPER",
            KeyModifier::Meta => "META",
        }
    }

    fn is_none(self) -> bool {
        self == KeyModifier::None
    }
}

impl<T: UserAction, Action: From<T>> KeybindsHolder<T, Action> {
    const KEYS_DELIMITER: &'static str = ", ";

    pub fn get_keys_for_action_joined(&self, action: T) -> Option<String> {
        let keys = self.get_keys_for_action(action)?;
        Some(keys.join("/"))
    }

    pub fn get_keys_for_action(&self, action: T) -> Option<Vec<Cow<'static, str>>> {
        let mut keys = vec![];

        for keybinding in &self.keybindings {
            if action == keybinding.action {
                keys.push(keybinding.keycode_string());
            }
        }

        if keys.is_empty() {
            None
        } else {
            Some(keys)
        }
    }

    pub fn get_help_repr(&self) -> Vec<(String, &'static str)> {
        self._get_help_repr(None::<Box<dyn Fn(KeyCode) -> Option<Cow<'static, str>>>>)
    }

    pub fn get_help_repr_with_override(
        &self,
        override_fn: impl Fn(KeyCode) -> Option<Cow<'static, str>>,
    ) -> Vec<(String, &'static str)> {
        self._get_help_repr(Some(override_fn))
    }

    fn _get_help_repr(
        &self,
        override_fn: Option<impl Fn(KeyCode) -> Option<Cow<'static, str>>>,
    ) -> Vec<(String, &'static str)> {
        let mut keys: BTreeMap<&T, Vec<Cow<'static, str>>> = BTreeMap::new();
        for keybinding in &self.keybindings {
            if !keybinding.show_in_help {
                continue;
            }

            keys.entry(&keybinding.action).or_default().push(
                if let Some(override_fn) = &override_fn {
                    keybinding.keycode_string_with_override(override_fn)
                } else {
                    keybinding.keycode_string()
                },
            );
        }

        let mut new_keys = vec![];

        for (action, keycodes) in keys {
            new_keys.push((action, keycodes));
        }

        let mut res = vec![];
        let mut skip_next_loop = false;
        for (idx, (action, keycodes)) in new_keys.iter().enumerate() {
            if skip_next_loop {
                skip_next_loop = false;
                continue;
            }

            if let Some((next_action, next_keycodes)) = new_keys.get(idx + 1) {
                if let Some(merged_desc) = action.merge_desc_with(next_action) {
                    skip_next_loop = true;
                    let keys = format!(
                        "{} / {}",
                        keycodes.join(Self::KEYS_DELIMITER),
                        next_keycodes.join(Self::KEYS_DELIMITER)
                    );

                    res.push((keys, merged_desc));
                    continue;
                }
            }

            let keycode_string = keycodes.join(Self::KEYS_DELIMITER);
            let desc = action.desc();
            res.push((keycode_string, desc));
        }

        res
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn example_keymap() -> Keymap {
        toml::toml! {
            [general]
            keybindings = [
                { on = "Left", action = "Left" },
                { on = "Right", action = "Right" },
                { on = "Up", action = "Up" },
                { on = "Down", action = "Down" },
                { on = "q", action = "Quit", show_in_help = false },
                { on = "?", action = "ShowHelp" },
                { modifier = "Ctrl", on = "d", action = "PageDown" },
            ]

            [main_tab]
            keybindings = [
                { on = "w", action = "ShowWeatherInfo" },
            ]
        }
        .try_into()
        .expect("it's valid")
    }

    fn custom_keycode_to_str(keycode: KeyCode) -> Option<Cow<'static, str>> {
        match keycode {
            KeyCode::Left => Some("l".into()),
            KeyCode::Right => Some("r".into()),
            KeyCode::Up => Some("u".into()),
            KeyCode::Down => Some("d".into()),
            _ => None,
        }
    }

    #[derive(Deserialize, Debug, PartialEq, Eq, Hash, Clone, Copy, PartialOrd, Ord)]
    enum GeneralAction {
        Left,
        Right,
        Up,
        Down,
        ShowHelp,
        Quit,
        PageDown,
    }

    impl UserAction for GeneralAction {
        fn desc(&self) -> &'static str {
            match self {
                GeneralAction::Left => "move left",
                GeneralAction::Right => "move right",
                GeneralAction::Up => "move up",
                GeneralAction::Down => "move down",
                GeneralAction::ShowHelp => "show help",
                GeneralAction::Quit => "quit",
                GeneralAction::PageDown => "move page down",
            }
        }

        fn merge_desc_with(&self, other: &Self) -> Option<&'static str> {
            match (&self, other) {
                (Self::Left, Self::Right) => Some("move left / right"),
                (Self::Right, Self::Left) => Some("move right / left"),
                (Self::Up, Self::Down) => Some("move up / down"),
                (Self::Down, Self::Up) => Some("move down / up"),
                _ => None,
            }
        }
    }

    #[derive(Deserialize, PartialEq, Eq, Hash, Clone, Copy, Debug, PartialOrd, Ord)]
    enum MainTabAction {
        ShowWeatherInfo,
    }

    impl UserAction for MainTabAction {
        fn desc(&self) -> &'static str {
            match self {
                MainTabAction::ShowWeatherInfo => "show weather info",
            }
        }
    }

    #[derive(Deserialize)]
    struct Keymap {
        general: KeybindsHolder<GeneralAction>,
        main_tab: KeybindsHolder<MainTabAction>,
    }

    #[test]
    fn keycode_override_works() {
        let keymap: Keymap = example_keymap();

        assert_eq!(
            keymap.general.keybindings[0].keycode_string_with_override(custom_keycode_to_str),
            "l"
        );

        assert_eq!(
            keymap
                .general
                .keybindings
                .last()
                .unwrap()
                .keycode_string_with_override(custom_keycode_to_str),
            "CTRL-d"
        );
    }

    #[test]
    fn keybinding_works() {
        let keymap: Keymap = example_keymap();

        assert_eq!(keymap.general.keybindings.is_empty(), false);
        assert_eq!(keymap.main_tab.keybindings.is_empty(), false);

        let left_keybinding = &keymap.general.keybindings[0];
        assert_eq!(left_keybinding.modifier, KeyModifier::None);
        assert_eq!(left_keybinding.on, KeyCode::Left);
        assert_eq!(left_keybinding.show_in_help, true);

        let left_action = keymap
            .general
            .map
            .get(&(KeyCode::Left, KeyModifiers::NONE))
            .unwrap();
        assert_eq!(*left_action, GeneralAction::Left);

        let weather_action = keymap
            .main_tab
            .map
            .get(&(KeyCode::Char('w'), KeyModifiers::NONE))
            .unwrap();
        assert_eq!(*weather_action, MainTabAction::ShowWeatherInfo);

        let weather_keybinding = &keymap.main_tab.keybindings[0];
        assert_eq!(weather_keybinding.modifier, KeyModifier::None);
        assert_eq!(weather_keybinding.on, KeyCode::Char('w'));
    }

    #[test]
    fn keybinding_help_repr_works() {
        let keymap = example_keymap();
        let mut general_help = keymap.general.get_help_repr().into_iter();

        assert_eq!(
            general_help.next(),
            Some(("Left / Right".into(), "move left / right"))
        );

        assert_eq!(
            general_help.next(),
            Some(("Up / Down".into(), "move up / down"))
        );

        assert_eq!(general_help.next(), Some(("?".into(), "show help")));

        assert_eq!(
            general_help.next(),
            Some(("CTRL-d".into(), "move page down"))
        );

        assert_eq!(general_help.next(), None);
    }

    #[test]
    fn keybinding_single_works_properly() {
        let keybinding = Keybinding::new(
            Some(KeyModifier::Ctrl),
            KeyCode::Left,
            MainTabAction::ShowWeatherInfo,
            true,
        );

        assert_eq!(
            keybinding.keycode_string(),
            Cow::<'static, str>::Owned(String::from("CTRL-Left"))
        );

        assert_eq!(keybinding.on, KeyCode::Left);
        assert_eq!(keybinding.modifier, KeyModifier::Ctrl);
        assert_eq!(keybinding.show_in_help, true);
    }
}
