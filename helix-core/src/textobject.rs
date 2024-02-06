use std::fmt::{Debug, Display};

use ropey::RopeSlice;
use tree_sitter::{Node, QueryCursor};

use crate::chars::{
    categorize_char, char_is_line_ending, char_is_punctuation, char_is_whitespace, CharCategory,
};
use crate::graphemes::{next_grapheme_boundary, prev_grapheme_boundary};
use crate::line_ending::rope_is_line_ending;
use crate::movement::Direction;
use crate::surround;
use crate::syntax::LanguageConfiguration;
use crate::Range;

fn word_boundary<P>(sp: TextPosition, direct: Direction, predicate: P) -> usize
where
    P: Fn(char) -> bool,
{
    use Direction::{Backward, Forward};
    match direct {
        Forward if sp.pos >= sp.text.len_chars().saturating_sub(1) => sp.text.len_chars(),
        Forward => {
            let kind = predicate(sp.text.char(sp.pos));
            sp.text
                .chars_at(sp.pos)
                .position(|c| kind != predicate(c))
                .map_or(sp.text.len_chars(), |x| x + sp.pos)
        }
        Backward if sp.pos == 0 => 0,
        Backward if sp.pos == sp.text.len_chars() => sp.text.len_chars(),
        Backward => {
            let kind = predicate(sp.text.char(sp.pos));
            sp.text
                .chars_at(sp.pos)
                .reversed()
                .position(|c| kind != predicate(c))
                .map_or(0, |x| sp.pos - x)
        }
    }
}

#[derive(Clone, Copy)]
struct TextPosition<'a> {
    text: RopeSlice<'a>,
    pos: usize,
}

fn is_word(ch: char, cs: &[char]) -> bool {
    ch.is_alphanumeric() || cs.contains(&ch)
}

fn is_long_word(ch: char) -> bool {
    categorize_char(ch) == CharCategory::Word || char_is_punctuation(ch)
}

pub fn textobject_word2(
    slice: RopeSlice,
    range: Range,
    textobject: TextObject,
    _count: usize,
    long: bool,
) -> Range {
    let mut tp = TextPosition {
        text: slice,
        pos: range.cursor(slice),
    };
    let word_chars = &['_'];

    use TextObject::*;
    match (textobject, long) {
        (Inside, true) => {
            let left = word_boundary(tp, Direction::Backward, is_long_word); // 让 newline 与任何类型都不同
            let right = word_boundary(tp, Direction::Forward, is_long_word);
            Range::new(left, right)
        }
        (Inside, false) => {
            let left = word_boundary(tp, Direction::Backward, |c| is_word(c, word_chars));
            let right = word_boundary(tp, Direction::Forward, |c| is_word(c, word_chars));
            Range::new(left, right)
        }
        (Around, true) => {
            let left = word_boundary(tp, Direction::Backward, is_long_word);
            let mut right = word_boundary(tp, Direction::Forward, is_long_word);
            if char_is_line_ending(slice.char(left)) || char_is_line_ending(slice.char(right)) {
                return Range::new(left, right);
            }
            tp.pos = right;
            right = word_boundary(tp, Direction::Forward, is_long_word);
            Range::new(left, right)
        }
        (Around, false) => {
            let left = word_boundary(tp, Direction::Backward, |c| is_word(c, word_chars));
            let mut right = word_boundary(tp, Direction::Forward, |c| is_word(c, word_chars));
            if char_is_whitespace(slice.char(left)) || char_is_whitespace(slice.char(right)) {
                tp.pos = right;
                right = word_boundary(tp, Direction::Forward, |c| is_word(c, word_chars));
            }
            return Range::new(left, right);
        }
        _ => unreachable!(),
    }
}

// Around/Inside: 是否包含起末空白符
// Long/Short: 是否包含换行符
// 当行尾有空白符时， Around Short 的换行符会被截断，从而表现与 Inside Short 一样。
pub fn textobject_line(
    slice: RopeSlice,
    range: Range,
    textobject: TextObject,
    _count: usize,
    long: bool,
) -> Range {
    let pos = range.cursor(slice);
    let mut tp = TextPosition { text: slice, pos };
    let mut right = word_boundary(tp, Direction::Forward, char_is_line_ending);
    let mut left = word_boundary(tp, Direction::Backward, char_is_line_ending);
    use TextObject::*;
    match (textobject, long) {
        (Around, false) => {
            if char_is_line_ending(slice.char(left)) {
                tp.pos = tp.pos.saturating_sub(1);
                left = word_boundary(tp, Direction::Backward, char_is_line_ending);
                right = right.saturating_sub(1);
            }
            Range::new(left, right)
        }
        (Around, true) => {
            if char_is_line_ending(slice.char(left)) {
                tp.pos = tp.pos.saturating_sub(1);
                left = word_boundary(tp, Direction::Backward, char_is_line_ending);
                Range::new(left, right)
            } else {
                Range::new(left, slice.len_chars().min(right + 1))
            }
        }
        (Inside, true) => todo!("search from both side that not white space"),
        (Inside, false) => todo!(),
        _ => unreachable!(),
    }
}

fn find_word_boundary(slice: RopeSlice, mut pos: usize, direction: Direction, long: bool) -> usize {
    use CharCategory::{Eol, Whitespace};

    let iter = match direction {
        Direction::Forward => slice.chars_at(pos),
        Direction::Backward => {
            let mut iter = slice.chars_at(pos);
            iter.reverse();
            iter
        }
    };

    let mut prev_category = match direction {
        Direction::Forward if pos == 0 => Whitespace,
        Direction::Forward => categorize_char(slice.char(pos - 1)),
        Direction::Backward if pos == slice.len_chars() => Whitespace,
        Direction::Backward => categorize_char(slice.char(pos)),
    };

    for ch in iter {
        match categorize_char(ch) {
            Eol | Whitespace => return pos,
            category => {
                if !long && category != prev_category && pos != 0 && pos != slice.len_chars() {
                    return pos;
                } else {
                    match direction {
                        Direction::Forward => pos += 1,
                        Direction::Backward => pos = pos.saturating_sub(1),
                    }
                    prev_category = category;
                }
            }
        }
    }

    pos
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum TextObject {
    Around,
    Inside,
    /// Used for moving between objects.
    Movement,
}

impl Display for TextObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::Around => "around",
            Self::Inside => "inside",
            Self::Movement => "movement",
        })
    }
}

// count doesn't do anything yet
pub fn textobject_word(
    slice: RopeSlice,
    range: Range,
    textobject: TextObject,
    _count: usize,
    long: bool,
) -> Range {
    let pos = range.cursor(slice);

    let word_start = find_word_boundary(slice, pos, Direction::Backward, long);
    // let word_end = find_word_boundary(slice, pos, Direction::Forward, long);
    let word_end = match slice.get_char(pos).map(categorize_char) {
        None | Some(CharCategory::Whitespace | CharCategory::Eol) => pos,
        _ => find_word_boundary(slice, pos + 1, Direction::Forward, long),
    };

    // Special case.
    if word_start == word_end {
        return Range::new(word_start, word_end);
    }

    match textobject {
        TextObject::Inside => Range::new(word_start, word_end),
        TextObject::Around => {
            let whitespace_count_right = slice
                .chars_at(word_end)
                .take_while(|c| char_is_whitespace(*c))
                .count();

            if whitespace_count_right > 0 {
                Range::new(word_start, word_end + whitespace_count_right)
            } else {
                let whitespace_count_left = {
                    let mut iter = slice.chars_at(word_start);
                    iter.reverse();
                    iter.take_while(|c| char_is_whitespace(*c)).count()
                };
                Range::new(word_start - whitespace_count_left, word_end)
            }
        }
        TextObject::Movement => unreachable!(),
    }
}

pub fn textobject_paragraph(
    slice: RopeSlice,
    range: Range,
    textobject: TextObject,
    count: usize,
) -> Range {
    let mut line = range.cursor_line(slice);
    let prev_line_empty = rope_is_line_ending(slice.line(line.saturating_sub(1)));
    let curr_line_empty = rope_is_line_ending(slice.line(line));
    let next_line_empty = rope_is_line_ending(slice.line(line.saturating_sub(1)));
    let last_char =
        prev_grapheme_boundary(slice, slice.line_to_char(line + 1)) == range.cursor(slice);
    let prev_empty_to_line = prev_line_empty && !curr_line_empty;
    let curr_empty_to_line = curr_line_empty && !next_line_empty;

    // skip character before paragraph boundary
    let mut line_back = line; // line but backwards
    if prev_empty_to_line || curr_empty_to_line {
        line_back += 1;
    }
    // do not include current paragraph on paragraph end (include next)
    if !(curr_empty_to_line && last_char) {
        let mut lines = slice.lines_at(line_back);
        lines.reverse();
        let mut lines = lines.map(rope_is_line_ending).peekable();
        while lines.next_if(|&e| e).is_some() {
            line_back -= 1;
        }
        while lines.next_if(|&e| !e).is_some() {
            line_back -= 1;
        }
    }

    // skip character after paragraph boundary
    if curr_empty_to_line && last_char {
        line += 1;
    }
    let mut lines = slice.lines_at(line).map(rope_is_line_ending).peekable();
    let mut count_done = 0; // count how many non-whitespace paragraphs done
    for _ in 0..count {
        let mut done = false;
        while lines.next_if(|&e| !e).is_some() {
            line += 1;
            done = true;
        }
        while lines.next_if(|&e| e).is_some() {
            line += 1;
        }
        count_done += done as usize;
    }

    // search one paragraph backwards for last paragraph
    // makes `map` at the end of the paragraph with trailing newlines useful
    let last_paragraph = count_done != count && lines.peek().is_none();
    if last_paragraph {
        let mut lines = slice.lines_at(line_back);
        lines.reverse();
        let mut lines = lines.map(rope_is_line_ending).peekable();
        while lines.next_if(|&e| e).is_some() {
            line_back -= 1;
        }
        while lines.next_if(|&e| !e).is_some() {
            line_back -= 1;
        }
    }

    // handle last whitespaces part separately depending on textobject
    match textobject {
        TextObject::Around => {}
        TextObject::Inside => {
            // remove last whitespace paragraph
            let mut lines = slice.lines_at(line);
            lines.reverse();
            let mut lines = lines.map(rope_is_line_ending).peekable();
            while lines.next_if(|&e| e).is_some() {
                line -= 1;
            }
        }
        TextObject::Movement => unreachable!(),
    }

    let anchor = slice.line_to_char(line_back);
    let head = slice.line_to_char(line);
    Range::new(anchor, head)
}

pub fn textobject_pair_surround(
    slice: RopeSlice,
    range: Range,
    textobject: TextObject,
    ch: char,
    count: usize,
) -> Range {
    textobject_pair_surround_impl(slice, range, textobject, Some(ch), count)
}

pub fn textobject_pair_surround_closest(
    slice: RopeSlice,
    range: Range,
    textobject: TextObject,
    count: usize,
) -> Range {
    textobject_pair_surround_impl(slice, range, textobject, None, count)
}

fn textobject_pair_surround_impl(
    slice: RopeSlice,
    range: Range,
    textobject: TextObject,
    ch: Option<char>,
    count: usize,
) -> Range {
    let pair_pos = match ch {
        Some(ch) => surround::find_nth_pairs_pos(slice, ch, range, count),
        // Automatically find the closest surround pairs
        None => surround::find_nth_closest_pairs_pos(slice, range, count),
    };
    pair_pos
        .map(|(anchor, head)| match textobject {
            TextObject::Inside => {
                if anchor < head {
                    Range::new(next_grapheme_boundary(slice, anchor), head)
                } else {
                    Range::new(anchor, next_grapheme_boundary(slice, head))
                }
            }
            TextObject::Around => {
                if anchor < head {
                    Range::new(anchor, next_grapheme_boundary(slice, head))
                } else {
                    Range::new(next_grapheme_boundary(slice, anchor), head)
                }
            }
            TextObject::Movement => unreachable!(),
        })
        .unwrap_or(range)
}

/// Transform the given range to select text objects based on tree-sitter.
/// `object_name` is a query capture base name like "function", "class", etc.
/// `slice_tree` is the tree-sitter node corresponding to given text slice.
pub fn textobject_treesitter(
    slice: RopeSlice,
    range: Range,
    textobject: TextObject,
    object_name: &str,
    slice_tree: Node,
    lang_config: &LanguageConfiguration,
    _count: usize,
) -> Range {
    let get_range = move || -> Option<Range> {
        let byte_pos = slice.char_to_byte(range.cursor(slice));

        let capture_name = format!("{}.{}", object_name, textobject); // eg. function.inner
        let mut cursor = QueryCursor::new();
        let node = lang_config
            .textobject_query()?
            .capture_nodes(&capture_name, slice_tree, slice, &mut cursor)?
            .filter(|node| node.byte_range().contains(&byte_pos))
            .min_by_key(|node| node.byte_range().len())?;

        let len = slice.len_bytes();
        let start_byte = node.start_byte();
        let end_byte = node.end_byte();
        if start_byte >= len || end_byte >= len {
            return None;
        }

        let start_char = slice.byte_to_char(start_byte);
        let end_char = slice.byte_to_char(end_byte);

        Some(Range::new(start_char, end_char))
    };
    get_range().unwrap_or(range)
}

#[cfg(test)]
mod test {
    use super::TextObject::*;
    use super::*;

    use crate::Range;
    use ropey::Rope;

    #[test]
    fn test_textobject_word() {
        // (text, [(char position, textobject, final range), ...])
        let tests = &[
            (
                "cursor at beginning of doc",
                vec![(0, Inside, (0, 6)), (0, Around, (0, 7))],
            ),
            (
                "cursor at middle of word",
                vec![
                    (13, Inside, (10, 16)),
                    (10, Inside, (10, 16)),
                    (15, Inside, (10, 16)),
                    (13, Around, (10, 17)),
                    (10, Around, (10, 17)),
                    (15, Around, (10, 17)),
                ],
            ),
            (
                "cursor between word whitespace",
                vec![(6, Inside, (6, 6)), (6, Around, (6, 6))],
            ),
            (
                "cursor on word before newline\n",
                vec![
                    (22, Inside, (22, 29)),
                    (28, Inside, (22, 29)),
                    (25, Inside, (22, 29)),
                    (22, Around, (21, 29)),
                    (28, Around, (21, 29)),
                    (25, Around, (21, 29)),
                ],
            ),
            (
                "cursor on newline\nnext line",
                vec![(17, Inside, (17, 17)), (17, Around, (17, 17))],
            ),
            (
                "cursor on word after newline\nnext line",
                vec![
                    (29, Inside, (29, 33)),
                    (30, Inside, (29, 33)),
                    (32, Inside, (29, 33)),
                    (29, Around, (29, 34)),
                    (30, Around, (29, 34)),
                    (32, Around, (29, 34)),
                ],
            ),
            (
                "cursor on #$%:;* punctuation",
                vec![
                    (13, Inside, (10, 16)),
                    (10, Inside, (10, 16)),
                    (15, Inside, (10, 16)),
                    (13, Around, (10, 17)),
                    (10, Around, (10, 17)),
                    (15, Around, (10, 17)),
                ],
            ),
            (
                "cursor on punc%^#$:;.tuation",
                vec![
                    (14, Inside, (14, 21)),
                    (20, Inside, (14, 21)),
                    (17, Inside, (14, 21)),
                    (14, Around, (14, 21)),
                    (20, Around, (14, 21)),
                    (17, Around, (14, 21)),
                ],
            ),
            (
                "cursor in   extra whitespace",
                vec![
                    (9, Inside, (9, 9)),
                    (10, Inside, (10, 10)),
                    (11, Inside, (11, 11)),
                    (9, Around, (9, 9)),
                    (10, Around, (10, 10)),
                    (11, Around, (11, 11)),
                ],
            ),
            (
                "cursor on word   with extra whitespace",
                vec![(11, Inside, (10, 14)), (11, Around, (10, 17))],
            ),
            (
                "cursor at end with extra   whitespace",
                vec![(28, Inside, (27, 37)), (28, Around, (24, 37))],
            ),
            (
                "cursor at end of doc",
                vec![(19, Inside, (17, 20)), (19, Around, (16, 20))],
            ),
        ];

        for (sample, scenario) in tests {
            let doc = Rope::from(*sample);
            let slice = doc.slice(..);
            for &case in scenario {
                let (pos, objtype, expected_range) = case;
                // cursor is a single width selection
                let range = Range::new(pos, pos + 1);
                let result = textobject_word(slice, range, objtype, 1, false);
                assert_eq!(
                    result,
                    expected_range.into(),
                    "\nCase failed: {:?} - {:?}",
                    sample,
                    case
                );
            }
        }
    }

    #[test]
    fn test_textobject_paragraph_inside_single() {
        let tests = [
            ("#[|]#", "#[|]#"),
            ("firs#[t|]#\n\nparagraph\n\n", "#[first\n|]#\nparagraph\n\n"),
            (
                "second\n\npa#[r|]#agraph\n\n",
                "second\n\n#[paragraph\n|]#\n",
            ),
            ("#[f|]#irst char\n\n", "#[first char\n|]#\n"),
            ("last char\n#[\n|]#", "#[last char\n|]#\n"),
            (
                "empty to line\n#[\n|]#paragraph boundary\n\n",
                "empty to line\n\n#[paragraph boundary\n|]#\n",
            ),
            (
                "line to empty\n\n#[p|]#aragraph boundary\n\n",
                "line to empty\n\n#[paragraph boundary\n|]#\n",
            ),
        ];

        for (before, expected) in tests {
            let (s, selection) = crate::test::print(before);
            let text = Rope::from(s.as_str());
            let selection = selection
                .transform(|r| textobject_paragraph(text.slice(..), r, TextObject::Inside, 1));
            let actual = crate::test::plain(s.as_ref(), &selection);
            assert_eq!(actual, expected, "\nbefore: `{:?}`", before);
        }
    }

    #[test]
    fn test_textobject_paragraph_inside_double() {
        let tests = [
            (
                "last two\n\n#[p|]#aragraph\n\nwithout whitespaces\n\n",
                "last two\n\n#[paragraph\n\nwithout whitespaces\n|]#\n",
            ),
            (
                "last two\n#[\n|]#paragraph\n\nwithout whitespaces\n\n",
                "last two\n\n#[paragraph\n\nwithout whitespaces\n|]#\n",
            ),
        ];

        for (before, expected) in tests {
            let (s, selection) = crate::test::print(before);
            let text = Rope::from(s.as_str());
            let selection = selection
                .transform(|r| textobject_paragraph(text.slice(..), r, TextObject::Inside, 2));
            let actual = crate::test::plain(s.as_ref(), &selection);
            assert_eq!(actual, expected, "\nbefore: `{:?}`", before);
        }
    }

    #[test]
    fn test_textobject_paragraph_around_single() {
        let tests = [
            ("#[|]#", "#[|]#"),
            ("firs#[t|]#\n\nparagraph\n\n", "#[first\n\n|]#paragraph\n\n"),
            (
                "second\n\npa#[r|]#agraph\n\n",
                "second\n\n#[paragraph\n\n|]#",
            ),
            ("#[f|]#irst char\n\n", "#[first char\n\n|]#"),
            ("last char\n#[\n|]#", "#[last char\n\n|]#"),
            (
                "empty to line\n#[\n|]#paragraph boundary\n\n",
                "empty to line\n\n#[paragraph boundary\n\n|]#",
            ),
            (
                "line to empty\n\n#[p|]#aragraph boundary\n\n",
                "line to empty\n\n#[paragraph boundary\n\n|]#",
            ),
        ];

        for (before, expected) in tests {
            let (s, selection) = crate::test::print(before);
            let text = Rope::from(s.as_str());
            let selection = selection
                .transform(|r| textobject_paragraph(text.slice(..), r, TextObject::Around, 1));
            let actual = crate::test::plain(s.as_ref(), &selection);
            assert_eq!(actual, expected, "\nbefore: `{:?}`", before);
        }
    }

    #[test]
    fn test_textobject_surround() {
        // (text, [(cursor position, textobject, final range, surround char, count), ...])
        let tests = &[
            (
                "simple (single) surround pairs",
                vec![
                    (3, Inside, (3, 3), '(', 1),
                    (7, Inside, (8, 14), ')', 1),
                    (10, Inside, (8, 14), '(', 1),
                    (14, Inside, (8, 14), ')', 1),
                    (3, Around, (3, 3), '(', 1),
                    (7, Around, (7, 15), ')', 1),
                    (10, Around, (7, 15), '(', 1),
                    (14, Around, (7, 15), ')', 1),
                ],
            ),
            (
                "samexx 'single' surround pairs",
                vec![
                    (3, Inside, (3, 3), '\'', 1),
                    (7, Inside, (7, 7), '\'', 1),
                    (10, Inside, (8, 14), '\'', 1),
                    (14, Inside, (14, 14), '\'', 1),
                    (3, Around, (3, 3), '\'', 1),
                    (7, Around, (7, 7), '\'', 1),
                    (10, Around, (7, 15), '\'', 1),
                    (14, Around, (14, 14), '\'', 1),
                ],
            ),
            (
                "(nested (surround (pairs)) 3 levels)",
                vec![
                    (0, Inside, (1, 35), '(', 1),
                    (6, Inside, (1, 35), ')', 1),
                    (8, Inside, (9, 25), '(', 1),
                    (8, Inside, (9, 35), ')', 2),
                    (20, Inside, (9, 25), '(', 2),
                    (20, Inside, (1, 35), ')', 3),
                    (0, Around, (0, 36), '(', 1),
                    (6, Around, (0, 36), ')', 1),
                    (8, Around, (8, 26), '(', 1),
                    (8, Around, (8, 36), ')', 2),
                    (20, Around, (8, 26), '(', 2),
                    (20, Around, (0, 36), ')', 3),
                ],
            ),
            (
                "(mixed {surround [pair] same} line)",
                vec![
                    (2, Inside, (1, 34), '(', 1),
                    (9, Inside, (8, 28), '{', 1),
                    (18, Inside, (18, 22), '[', 1),
                    (2, Around, (0, 35), '(', 1),
                    (9, Around, (7, 29), '{', 1),
                    (18, Around, (17, 23), '[', 1),
                ],
            ),
            (
                "(stepped (surround) pairs (should) skip)",
                vec![(22, Inside, (1, 39), '(', 1), (22, Around, (0, 40), '(', 1)],
            ),
            (
                "[surround pairs{\non different]\nlines}",
                vec![
                    (7, Inside, (1, 29), '[', 1),
                    (15, Inside, (16, 36), '{', 1),
                    (7, Around, (0, 30), '[', 1),
                    (15, Around, (15, 37), '{', 1),
                ],
            ),
        ];

        for (sample, scenario) in tests {
            let doc = Rope::from(*sample);
            let slice = doc.slice(..);
            for &case in scenario {
                let (pos, objtype, expected_range, ch, count) = case;
                let result = textobject_pair_surround(slice, Range::point(pos), objtype, ch, count);
                assert_eq!(
                    result,
                    expected_range.into(),
                    "\nCase failed: {:?} - {:?}",
                    sample,
                    case
                );
            }
        }
    }

    #[test]
    fn test_word_boundary() {
        // &[text, [(position, direction, expected-word-boundary)...]]
        let word_cases = &[
            (
                "  word  ",
                vec![],
                vec![
                    (0, Direction::Forward, 2usize),
                    (0, Direction::Backward, 0),
                    (2, Direction::Forward, 6),
                    (2, Direction::Backward, 2),
                    (5, Direction::Forward, 6),
                    (5, Direction::Backward, 2),
                    (6, Direction::Forward, 8),
                    (6, Direction::Backward, 6),
                    (7, Direction::Forward, 8),
                    (7, Direction::Backward, 6),
                    (8, Direction::Forward, 8),
                    (8, Direction::Backward, 8),
                ],
            ),
            (
                "a",
                vec![],
                vec![
                    (0, Direction::Forward, 1),
                    (0, Direction::Backward, 0),
                    (1, Direction::Forward, 1),
                    (1, Direction::Backward, 1),
                ],
            ),
            (
                "",
                vec![],
                vec![(0, Direction::Backward, 0), (0, Direction::Forward, 0)],
            ),
            (
                "hello\nworld!",
                vec![],
                vec![
                    (0, Direction::Forward, 5),
                    (4, Direction::Forward, 5),
                    (5, Direction::Forward, 6),
                    (5, Direction::Backward, 5),
                    (6, Direction::Forward, 11),
                    (6, Direction::Backward, 6),
                    (11, Direction::Forward, 12),
                ],
            ),
            (
                "hello-world!",
                vec!['-'],
                vec![
                    (0, Direction::Forward, 11),
                    (4, Direction::Forward, 11),
                    (5, Direction::Forward, 11),
                    (5, Direction::Backward, 0),
                    (6, Direction::Backward, 0),
                    (10, Direction::Backward, 0),
                    (11, Direction::Backward, 11),
                ],
            ),
        ];

        word_cases.iter().enumerate().for_each(|(i, t)| {
            let (txt, words, case) = t;
            case.iter().enumerate().for_each(|(j, &c)| {
                let (pos, direct, expect) = c;
                let rope = Rope::from_str(txt);
                let text = rope.slice(..);
                assert_eq!(
                    // word_boundary(slice, pos, direct, words),
                    word_boundary(TextPosition { text, pos }, direct, |c| is_word(c, words)),
                    expect,
                    "failed at case {i}.{j}",
                );
            });
        });

        let long_word_cases = &[(
            "  Hello?@#$%^&*()世界<>!  ",
            vec![
                (0, Direction::Forward, 2usize),
                (0, Direction::Backward, 0),
                (2, Direction::Forward, 22),
                (2, Direction::Backward, 2),
                (21, Direction::Forward, 22),
                (21, Direction::Backward, 2),
                (22, Direction::Forward, 24),
                (22, Direction::Backward, 22),
            ],
        )];
        long_word_cases.iter().enumerate().for_each(|(i, t)| {
            let (txt, case) = t;
            case.iter().enumerate().for_each(|(j, &c)| {
                let (pos, direct, expect) = c;
                let rope = Rope::from_str(txt);
                let text = rope.slice(..);
                assert_eq!(
                    // word_boundary(slice, pos, direct, words),
                    word_boundary(TextPosition { text, pos }, direct, char::is_whitespace),
                    expect,
                    "failed at case {i}.{j}",
                );
            });
        });

        // [(sample, [(pos, type, long-word, slice-range)...])...]
        let word_boundary_cases = &[
            (
                "  hello world  ",
                vec![
                    (0, TextObject::Inside, false, (0, 2)),
                    (2, TextObject::Inside, false, (2, 7)),
                    (0, TextObject::Around, false, (0, 7)),
                    (2, TextObject::Around, false, (2, 8)),
                ],
            ),
            (
                "  Hello-world!  ",
                vec![
                    (0, TextObject::Inside, false, (0, 2)),
                    (0, TextObject::Inside, true, (0, 2)),
                    (0, TextObject::Around, false, (0, 7)),
                    (0, TextObject::Around, true, (0, 14)),
                    (2, TextObject::Inside, false, (2, 7)),
                    (2, TextObject::Inside, true, (2, 14)),
                    (2, TextObject::Around, false, (2, 7)),
                    (2, TextObject::Around, true, (2, 16)),
                ],
            ),
            (
                "hello_world\ngood_bye",
                vec![
                    (0, TextObject::Inside, false, (0, 11)),
                    (0, TextObject::Inside, true, (0, 11)),
                    (0, TextObject::Around, false, (0, 11)),
                    (0, TextObject::Around, true, (0, 11)),
                    (11, TextObject::Inside, false, (11, 12)),
                    (11, TextObject::Inside, true, (11, 12)),
                    (11, TextObject::Around, false, (11, 12)),
                    (11, TextObject::Around, true, (11, 12)),
                ],
            ),
        ];
        word_boundary_cases.iter().for_each(|t| {
            let (sample, cases) = t;
            let rope = Rope::from_str(sample);
            let text = rope.slice(..);
            cases.iter().for_each(|&case| {
                let (pos, txt_obj, long_word, expected_slice) = case;
                let range = Range::new(pos, pos + 1);
                let res = textobject_word2(text, range, txt_obj, 1, long_word);
                assert_eq!(
                    res,
                    expected_slice.into(),
                    "\nfailed at {sample:?}, {case:?}",
                );
            })
        })
    }

    #[test]
    fn test_line() {
        //[(text, [(position, type, long, range)...])...]
        let test_cases = &[(
            "  first line\n  second line  \nthird line  ",
            //           c
            vec![
                (0, TextObject::Around, true, (0, 13)),
                (0, TextObject::Around, false, (0, 12)),
                // (0, TextObject::Inside, true, (2, 12)),
                // (0, TextObject::Inside, false, (0, 11)),

                // cursor at end of first line
                (11, TextObject::Around, true, (0, 13)),
                (11, TextObject::Around, false, (0, 12)),
                // (0, TextObject::Inside, true, (2, 12)),
                // (0, TextObject::Inside, false, (0, 11)),

                //cursor at line-breaking of first line
                (12, TextObject::Around, true, (0, 13)),
                (12, TextObject::Around, false, (0, 12)),
                // (12, TextObject::Inside, true, (2, 12)),
                // (12, TextObject::Inside, false, (0, 11)),

                //cursor at beginning of second line
                (13, TextObject::Around, true, (13, 29)),
                (13, TextObject::Around, false, (13, 28)),
                //cursor at beginning of third line
                (29, TextObject::Around, true, (29, 41)),
                (29, TextObject::Around, false, (29, 41)),
                //cursor at end of third line
                (40, TextObject::Around, true, (29, 41)),
            ],
        )];

        test_cases.iter().for_each(|t| {
            let (sample, cases) = t;
            let rope = Rope::from_str(sample);
            let text = rope.slice(..);
            cases.iter().for_each(|&case| {
                let (pos, txt_obj, long, expected) = case;
                let range = Range::new(pos, pos + 1);
                assert_eq!(
                    textobject_line(text, range, txt_obj, 1, long),
                    expected.into(),
                    "\nfailed at {sample:?}, {case:?}",
                );
            })
        })
    }
}
