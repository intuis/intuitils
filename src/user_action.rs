use std::hash::Hash;

pub trait UserAction: Eq + Hash + Copy + Ord {
    fn desc(&self) -> &'static str;

    fn merge_desc_with(&self, other: &Self) -> Option<&'static str> {
        let _ = other;
        None
    }
}
