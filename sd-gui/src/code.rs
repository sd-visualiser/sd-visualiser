use std::{
    num::NonZeroUsize,
    ops::{Range, RangeBounds},
};

use eframe::egui::TextBuffer;
use itertools::Itertools;

#[derive(Debug, Clone)]
struct StringLines {
    contents: String,
    lines: NonZeroUsize,
}
#[derive(Debug)]
enum StringLinesError {
    Empty,
}

impl TryFrom<String> for StringLines {
    type Error = StringLinesError;

    fn try_from(contents: String) -> Result<Self, Self::Error> {
        if contents.is_empty() {
            Err(Self::Error::Empty)
        } else {
            let lines = (1 + contents.chars().filter(|&c| c == '\n').count())
                .try_into()
                .unwrap();
            Ok(StringLines { contents, lines })
        }
    }
}

#[derive(Debug, Clone, Default)]
struct Visible(Option<StringLines>);

impl Visible {
    /// Given a range i..j, drain the lines contained within.
    ///
    /// # Panics
    ///
    /// Panics if starting line is greater than number of lines.
    fn drain_lines(
        &mut self,
        range: impl RangeBounds<usize>,
    ) -> impl DoubleEndedIterator<Item = String> + '_ {
        let Some(inner) = self.0.as_mut() else { return vec![].into_iter(); };
        let mut removed = 0;
        let mut leading = false;
        let mut trailing = false;
        let end_char = match range.end_bound() {
            std::ops::Bound::Excluded(&l) if l == 0 => {
                return vec![].into_iter();
            }
            std::ops::Bound::Included(&l) => {
                removed += l + 1;
                trailing = true;
                inner
                    .contents
                    .chars()
                    .enumerate()
                    .filter_map(|(char_idx, c)| (c == '\n').then_some(char_idx))
                    .nth(l)
                    .unwrap()
                    + 1
            }
            std::ops::Bound::Excluded(&l) => {
                removed += l;
                trailing = true;
                inner
                    .contents
                    .chars()
                    .enumerate()
                    .filter_map(|(char_idx, c)| (c == '\n').then_some(char_idx))
                    .nth(l - 1)
                    .unwrap()
                    + 1
            }
            std::ops::Bound::Unbounded => {
                removed += inner.lines.get();
                inner.contents.chars().count()
            }
        }
        .clamp(0, inner.contents.len());
        let start_char = match range.start_bound() {
            std::ops::Bound::Included(&l) if l == 0 => 0,
            std::ops::Bound::Included(&l) => {
                removed -= l;
                leading = true;
                inner
                    .contents
                    .chars()
                    .enumerate()
                    .filter_map(|(char_idx, c)| (c == '\n').then_some(char_idx))
                    .nth(l - 1)
                    .unwrap_or(inner.contents.chars().count())
            }
            std::ops::Bound::Excluded(&l) => {
                removed -= l - 1;
                leading = true;
                inner
                    .contents
                    .chars()
                    .enumerate()
                    .filter_map(|(char_idx, c)| (c == '\n').then_some(char_idx))
                    .nth(l)
                    .unwrap_or(inner.contents.chars().count())
            }
            std::ops::Bound::Unbounded => 0,
        };
        let mut substring = inner
            .contents
            .drain(
                inner.contents.byte_index_from_char_index(start_char)
                    ..inner.contents.byte_index_from_char_index(end_char),
            )
            .skip(usize::from(leading))
            .collect::<String>();
        if trailing {
            assert_eq!(substring.pop(), Some('\n'));
        }
        if removed >= inner.lines.get() {
            self.0 = None;
        } else {
            inner.lines = (inner.lines.get() - removed).try_into().unwrap();
        }
        if removed > 0 {
            let result = substring
                .split('\n')
                .map(std::borrow::ToOwned::to_owned)
                .collect::<Vec<_>>();
            debug_assert_eq!(removed, result.len());
            result.into_iter()
        } else {
            vec![].into_iter()
        }
    }
}

impl Extend<String> for Visible {
    fn extend<T: IntoIterator<Item = String>>(&mut self, iter: T) {
        if let Some(inner) = &mut self.0 {
            for line in iter {
                inner.contents.push('\n');
                inner.contents.push_str(&line);
                inner.lines = inner.lines.saturating_add(1);
            }
        } else {
            let mut lines = iter.into_iter();
            if let Some(line) = lines.next() {
                self.0 = Some(StringLines {
                    contents: line,
                    lines: 1.try_into().unwrap(),
                });
                self.extend(lines);
            }
        }
    }
}

impl FromIterator<String> for Visible {
    fn from_iter<T: IntoIterator<Item = String>>(iter: T) -> Self {
        let mut visible = Visible(None);
        visible.extend(iter);
        visible
    }
}

impl IntoIterator for Visible {
    type Item = String;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        if let Some(inner) = self.0 {
            inner
                .contents
                .split('\n')
                .map(std::borrow::ToOwned::to_owned)
                .interleave(itertools::repeat_n("\n".to_owned(), inner.lines.into()))
                .collect::<Vec<_>>()
                .into_iter()
        } else {
            vec![].into_iter()
        }
    }
}

type Line = String;
#[derive(Debug, Clone, Default)]
pub(crate) struct Code {
    // code is pre + visible + reverse(post)
    // visible is a multiline string
    pre: Vec<Line>,
    visible: Visible,
    post: Vec<Line>,
}

impl Code {
    /// Returns a reference to the visible segment of this [`Code`].
    pub(crate) fn view(&self) -> &str {
        self.visible
            .0
            .as_ref()
            .map(|lines| lines.contents.as_str())
            .unwrap_or_default()
    }

    pub(crate) fn visible_len(&self) -> usize {
        self.visible
            .0
            .as_ref()
            .map(|stringlines| stringlines.lines.get())
            .unwrap_or_default()
    }

    /// Returns the number of lines in the [`Code`].
    pub(crate) fn len(&self) -> usize {
        self.pre.len() + self.visible_len() + self.post.len()
    }

    #[tracing::instrument(level = "trace")]
    pub(crate) fn reindex(&mut self, target_range: Range<usize>) {
        assert!(target_range.end <= self.len());
        match self.pre.len() .. self.pre.len() + self.visible_len() {
            visible_range if visible_range == target_range => {
                // do nothing
            }
            _ if target_range.is_empty() => {
                // only case for which self.visible should be None at the end
                // assume that target_range is of form (i .. i)
                tracing::trace!("empty target");
                match target_range.start {
                    pivot if pivot < self.pre.len() => {
                        tracing::trace!("move into pre");
                        self.post.extend(self.visible.drain_lines(..).rev());
                        // move some pre to post
                        self.post.extend(self.pre.drain(pivot..).rev());
                    },
                    pivot if pivot >= self.pre.len() + self.visible_len() => {
                        tracing::trace!("move into post");
                        let end = self.len();
                        self.pre.extend(self.visible.drain_lines(..));
                        // move some post to pre
                        self.pre.extend(self.post.drain(end - 1 - pivot ..).rev());
                    },
                    pivot if pivot >= self.pre.len() && pivot < self.pre.len() + self.visible_len() => {
                        tracing::trace!("split visible into pre and post");
                        // move some of lines to both pre and post
                        self.pre.extend(self.visible.drain_lines(.. pivot - self.pre.len()));
                        self.post.extend(self.visible.drain_lines(..).rev());
                    },
                    _ => unreachable!(),
                }
            }
            // one endpoint cases
            visible_range if target_range.start == visible_range.start && target_range.end < visible_range.end => {
                tracing::trace!("shrink visible from end");
                self.post.extend(self.visible.drain_lines(target_range.end - self.pre.len() ..).rev());
            }
            visible_range if target_range.start == visible_range.start && target_range.end > visible_range.end => {
                tracing::trace!("extend visible from end");
                self.visible.extend(self.post.drain(self.post.len() - (target_range.end - visible_range.end) .. self.post.len()).rev());
            }
            visible_range if target_range.start < visible_range.start && target_range.end == visible_range.end => {
                tracing::trace!("extend visible from start");
                let extension = self.pre.drain(target_range.start .. visible_range.start);
                if let Some(visible) = &mut self.visible.0 {
                    #[allow(unstable_name_collisions)]
                    let mut new_contents: String = extension.intersperse("\n".to_owned()).collect();
                    new_contents.push('\n');
                    new_contents.push_str(&visible.contents);
                    *visible = StringLines::try_from(new_contents).unwrap();
                } else {
                    self.visible = extension.collect();
                }
            }
            visible_range if target_range.start > visible_range.start && target_range.end == visible_range.end => {
                tracing::trace!("shrink visible from start");
                self.pre.extend(self.visible.drain_lines(visible_range.start - self.pre.len() .. target_range.start - self.pre.len())); }
            // two endpoint cases
            visible_range if target_range.end <= visible_range.start => {
                tracing::trace!("move visible entirely into pre");
                // shunt all of visible onto post
                self.post.extend(self.visible.drain_lines(..).rev());
                // shunt some of pre onto post
                self.post.extend(self.pre.drain(target_range.end ..).rev());
                // make last part of pre visible
                self.visible = self.pre.drain(target_range.start ..).collect();
            }
            visible_range if target_range.start >= visible_range.end => {
                tracing::trace!("move visible entirely into post");
                // shunt all of visible onto pre
                let post_start = self.pre.len() + self.visible_len();
                self.pre.extend(self.visible.drain_lines(..));
                // shunt some of post onto pre
                self.pre.extend(self.post.drain(target_range.end - post_start ..).rev());
                // make first part of post visible
                self.visible = self.post.drain(target_range.start - post_start ..).rev().collect();
            }
            // composite operations
            visible_range if
                // shrink from the end and extend visible from the start
                (target_range.start < visible_range.start && target_range.end > visible_range.start && target_range.end < visible_range.end)
                // shrink visible from both ends
                || (target_range.start > visible_range.start && target_range.end < visible_range.end)
                // extend visible from both ends
                || (target_range.start < visible_range.start && target_range.end > visible_range.end)
                => {
                    tracing::trace!("shrink from end and extend visible from start, shrink visible from both ends, extend visible from both ends");
                    self.reindex(visible_range.start .. target_range.end);
                    self.reindex(target_range.start .. target_range.end);
            }
            visible_range if (target_range.start > visible_range.start && target_range.start < visible_range.end && target_range.end > visible_range.end) => {
                tracing::trace!("shrink visible from start and grow from end, *in this order*");
                self.reindex(target_range.start .. visible_range.end);
                self.reindex(target_range.start .. target_range.end);
            }
            visible_range => {
                tracing::error!("unexpected reindex, visible_range: {visible_range:?}, target_range: {target_range:?}");
                unreachable!()
            }
        }
    }
}

impl ToString for Code {
    #[allow(unstable_name_collisions)]
    fn to_string(&self) -> String {
        self.pre
            .iter()
            .cloned()
            .chain(
                self.visible
                    .0
                    .as_ref()
                    .map(|visible| visible.contents.clone()),
            )
            .chain(self.post.iter().rev().cloned())
            .intersperse("\n".to_owned())
            .collect()
    }
}

impl From<String> for Code {
    fn from(str: String) -> Self {
        Code {
            pre: Vec::default(),
            visible: Visible(str.try_into().ok()),
            post: Vec::default(),
        }
    }
}

impl TextBuffer for Code {
    fn is_mutable(&self) -> bool {
        true
    }

    fn as_str(&self) -> &str {
        self.view()
    }

    fn insert_text(&mut self, text: &str, char_index: usize) -> usize {
        if let Some(stringlines) = &mut self.visible.0 {
            if let Some(lines) = stringlines
                .lines
                .checked_add(text.chars().filter(|&c| c == '\n').count())
            {
                stringlines.lines = lines;
            } else {
                panic!("too many lines");
            }
            stringlines.contents.insert_text(text, char_index)
        } else if text.is_empty() {
            0
        } else {
            self.visible = Visible(Some(text.to_owned().try_into().unwrap()));
            text.len()
        }
    }

    fn delete_char_range(&mut self, char_range: Range<usize>) {
        let visible_chars = self
            .visible
            .0
            .as_ref()
            .map(|sl| sl.contents.chars().count())
            .unwrap_or_default();
        let range = char_range.start.clamp(usize::MIN, visible_chars)
            ..char_range.end.clamp(usize::MIN, visible_chars);
        if let Some(stringlines) = &mut self.visible.0 {
            let lines = &stringlines.contents[range.clone()]
                .chars()
                .filter(|&c| c == '\n')
                .count();
            if let Ok(lines) = (stringlines.lines.get() - lines).try_into() {
                stringlines.lines = lines;
                stringlines.contents.delete_char_range(range);
            } else {
                self.visible.0.take();
            }
        }
    }

    fn clear(&mut self) {
        self.visible.0.take();
    }

    fn take(&mut self) -> String {
        self.visible
            .0
            .take()
            .map(|sl| sl.contents)
            .unwrap_or_default()
    }
}

#[cfg(test)]
mod tests {
    use proptest::{
        prelude::{any, prop},
        prop_assert_eq, prop_compose, proptest,
    };

    use super::{Code, StringLines, Visible};

    #[test]
    fn drain_lines() {
        let mut visible = Visible(StringLines::try_from("\n".to_owned()).ok());
        assert_eq!(
            visible.clone().drain_lines(..).collect::<Vec<_>>(),
            vec!["", ""]
        );
        assert_eq!(
            visible.clone().drain_lines(..1).collect::<Vec<_>>(),
            vec![""]
        );
        assert_eq!(
            visible.clone().drain_lines(1..).collect::<Vec<_>>(),
            vec![""]
        );
        assert_eq!(
            visible.drain_lines(2..).collect::<Vec<_>>(),
            Vec::<String>::default()
        );
        assert_eq!(
            visible.drain_lines(..0).collect::<Vec<_>>(),
            Vec::<String>::default()
        );
        assert_eq!(visible.drain_lines(..=0).collect::<Vec<_>>(), vec![""]);
        assert_eq!(visible.drain_lines(..).collect::<Vec<_>>(), vec![""]);

        let mut visible = Visible(StringLines::try_from("\n\n".to_owned()).ok());
        assert_eq!(
            visible.clone().drain_lines(..).collect::<Vec<_>>(),
            vec!["", "", ""]
        );
        assert_eq!(
            visible.clone().drain_lines(..1).collect::<Vec<_>>(),
            vec![""]
        );
        assert_eq!(
            visible.clone().drain_lines(1..).collect::<Vec<_>>(),
            vec!["", ""]
        );
        assert_eq!(
            visible.clone().drain_lines(1..2).collect::<Vec<_>>(),
            vec![""]
        );
        assert_eq!(
            visible.clone().drain_lines(..2).collect::<Vec<_>>(),
            vec!["", ""]
        );
        assert_eq!(visible.drain_lines(2..).collect::<Vec<_>>(), vec![""]);
    }

    prop_compose! {
        fn arb_code()(pre in prop::collection::vec(".*", 0..100), visible_str in r"(\PC|\n)*", post in prop::collection::vec(".*", 0..100)) -> Code {
            Code {
                pre,
                visible: Visible(visible_str.try_into().ok()),
                post,
            }
        }
    }

    proptest! {
        #[test]
        fn code_string_code_id(code in arb_code()) {
            println!("original: {code:#?}");
            let new_code = Code::from(code.to_string());
            println!("new: {new_code:#?}");
            println!();
            prop_assert_eq!(code.to_string(), new_code.to_string());
        }

        #[test]
        fn string_code_string_id(str: String) {
            prop_assert_eq!(str.clone(), Code::from(str).to_string());
        }

        #[test]
        fn line_count_correct(code in arb_code()) {
            println!("to_string: {:#?}", code.to_string());
            prop_assert_eq!(code.len(), {
                let str = code.to_string();
                str.lines().count() + str.chars().last().map(|c| usize::from(c == '\n')).unwrap_or_default()
            });
        }

        #[allow(clippy::str_to_string)]
        #[tracing_test::traced_test]
        #[test]
        fn reindex_preserves_contents(
            code in arb_code(),
            indices in prop::collection::vec(any::<(prop::sample::Index, prop::sample::Index)>(), 5..10)
        ) {
            let len = code.len();
            println!("original: {code:#?}");
            println!();
            for (i, j) in indices {
                let mut new_code = code.clone();
                let target_range = std::cmp::min(i.index(len), j.index(len)) .. std::cmp::max(i.index(len), j.index(len));
                println!("requested reindex: {target_range:#?}");
                new_code.reindex(target_range);
                println!("reindexed: {new_code:#?}");
                println!();
                prop_assert_eq!(len, new_code.len());
                prop_assert_eq!(code.to_string(), new_code.to_string());
            }
        }
    }
}
